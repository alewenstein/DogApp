import streamlit as st
import pandas as pd
import numpy as np
import random
import math
import sqlite3
import uuid
from datetime import datetime

# Initialize SQLite database
@st.cache_resource
def init_database():
    """Create and initialize the SQLite database"""
    conn = sqlite3.connect('dog_app_guesses.db', check_same_thread=False)
    cursor = conn.cursor()

    # Create table if it doesn't exist
    cursor.execute('''
        CREATE TABLE IF NOT EXISTS guesses (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            user_id TEXT NOT NULL,
            name TEXT NOT NULL,
            guess TEXT NOT NULL,
            correct_answer TEXT NOT NULL,
            is_correct INTEGER NOT NULL,
            timestamp TEXT NOT NULL
        )
    ''')

    conn.commit()
    return conn

def load_and_process_data():
    """Convert your R data processing to Python"""
    
    # Load NYC Dogs data
    nyc_dogs = pd.read_csv("NYC_Dog_Licensing_Dataset.csv")
    
    # Load NY names data (comma-separated: State,Gender,Year,Name,Count)
    ny_names = pd.read_csv("NY.TXT", header=None)
    ny_names.columns = ['V1', 'V2', 'V3', 'V4', 'V5']  # State, Gender, Year, Name, Count
    
    # Process dogs data (convert R logic)
    list_to_remove = ["UNKNOWN", "NAME NOT PROVIDED", "NONE", 'A', '.', 'NAME', 'NOT', "UNKNOWED"]
    
    dogs_filtered = nyc_dogs[
        (~nyc_dogs['AnimalName'].isin(list_to_remove)) & 
        (nyc_dogs['AnimalName'].notna()) &
        (~nyc_dogs['AnimalName'].str.contains(r'[^a-zA-Z\-]', na=False))
    ].copy()
    
    dogs_by_year = (dogs_filtered
                   .groupby('AnimalBirthYear')
                   .apply(lambda x: x.assign(dogs_in_year=len(x)))
                   .reset_index(drop=True)
                   .groupby(['AnimalBirthYear', 'dogs_in_year', 'AnimalName'])
                   .size()
                   .reset_index(name='dogs'))
    
    dogs_by_year['dog_prop'] = round(100 * dogs_by_year['dogs'] / dogs_by_year['dogs_in_year'], 1)
    
    # Process human names data
    human_names = (ny_names[ny_names['V3'] >= 1991]
                  .groupby('V3')
                  .apply(lambda x: x.assign(
                      people_in_year=x['V5'].sum(),
                      name=x['V4'].str.upper(),
                      human_prop=round(100 * x['V5'] / x['V5'].sum(), 1)
                  ))
                  .reset_index(drop=True)
                  [['V3', 'people_in_year', 'name', 'human_prop', 'V5']]
                  .rename(columns={'V3': 'year', 'V5': 'people'}))
    
    # Join datasets
    cross = pd.merge(human_names, dogs_by_year, 
                    left_on=['year', 'name'], 
                    right_on=['AnimalBirthYear', 'AnimalName'])
    cross = cross[cross['year'] >= 2000]
    
    # Calculate correlations and classifications
    check_corr = (cross.groupby('name')
                 .agg({
                     'people': 'sum',
                     'dogs': 'sum'
                 })
                 .reset_index()
                 .rename(columns={'people': 'people_tot', 'dogs': 'dogs_tot'}))
    
    check_corr['dog_angle_tot'] = np.round(np.degrees(np.arctan2(check_corr['people_tot'], check_corr['dogs_tot'])))
    check_corr['dogs_prop'] = check_corr['dogs_tot'] / check_corr['dogs_tot'].sum()
    check_corr['people_prop'] = check_corr['people_tot'] / check_corr['people_tot'].sum()
    check_corr['dog_angle_prop'] = np.round(np.degrees(np.arctan2(check_corr['people_prop'], check_corr['dogs_prop'])))
    
    # Classification logic
    def classify_dogginess_tot(angle):
        if 0 <= angle <= 34:
            return "Who's a good boy, yes you are!"
        elif 35 <= angle <= 55:
            return "Animorph"
        else:
            return "You're a human!!"
    
    def classify_dogginess_prop(angle):
        if 0 <= angle <= 34:
            return "Dog"
        elif 35 <= angle <= 55:
            return "Could be either"
        else:
            return "Human"
    
    check_corr['dogginess_tot'] = check_corr['dog_angle_tot'].apply(classify_dogginess_tot)
    check_corr['dogginess_prop'] = check_corr['dog_angle_prop'].apply(classify_dogginess_prop)
    check_corr['angle_diff'] = abs(check_corr['dog_angle_tot'] - check_corr['dog_angle_prop'])
    
    return check_corr

@st.cache_data
def get_processed_data():
    return load_and_process_data()

def save_guess_to_db(conn: sqlite3.Connection, user_id: str, name: str, guess: str, correct_answer: str):
    """Save user guess to SQLite database"""
    try:
        cursor = conn.cursor()
        cursor.execute('''
            INSERT INTO guesses (user_id, name, guess, correct_answer, is_correct, timestamp)
            VALUES (?, ?, ?, ?, ?, ?)
        ''', (
            user_id,
            name,
            guess,
            correct_answer,
            1 if guess == correct_answer else 0,
            datetime.now().isoformat()
        ))
        conn.commit()
        return True
    except Exception as e:
        st.error(f"Error saving to database: {e}")
        return False

def get_all_guesses(conn: sqlite3.Connection):
    """Retrieve all guesses from the database as a DataFrame"""
    try:
        query = "SELECT * FROM guesses ORDER BY timestamp DESC"
        df = pd.read_sql_query(query, conn)
        return df
    except Exception as e:
        st.error(f"Error reading from database: {e}")
        return pd.DataFrame()

def show_game_tab(data, db):
    """Display the main game interface"""
    st.subheader("Guess whether each name is a human name, a dog name, or somewhere in between")
    st.write("If you get them all right, you get a treat!")

    # Generate new set of names if needed
    if st.session_state.current_names is None:
        st.session_state.current_names = data.sample(12).reset_index()
        st.session_state.show_answers = False
        st.session_state.guesses_made = {}

    current_names = st.session_state.current_names

    # Create columns for layout
    col1, col2 = st.columns([3, 1])

    with col1:
        # Display names in a grid
        cols = st.columns(3)

        for i in range(12):
            with cols[i % 3]:
                name = current_names.loc[i, 'name']
                st.subheader(name)

                guess = st.radio(
                    "Your guess:",
                    ["Dog", "Human", "Could be either"],
                    key=f"guess_{i}",
                    disabled=st.session_state.show_answers
                )

                st.session_state.guesses_made[i] = guess

                if st.session_state.show_answers:
                    correct_answer = current_names.loc[i, 'dogginess_prop']
                    is_correct = guess == correct_answer

                    st.write(f"**Answer:** {correct_answer}")
                    if is_correct:
                        st.success("✓ Correct!")
                    else:
                        st.error("✗ Incorrect")

    with col2:
        st.subheader("Score")
        st.write(f"Correct: {st.session_state.score['correct']}")
        st.write(f"Total: {st.session_state.score['total']}")

        if not st.session_state.show_answers:
            if st.button("Submit", type="primary"):
                # Calculate score
                correct_count = 0

                for i in range(12):
                    guess = st.session_state.guesses_made.get(i)
                    if guess:
                        correct_answer = current_names.loc[i, 'dogginess_prop']
                        name = current_names.loc[i, 'name']

                        # Save to database
                        if db:
                            save_guess_to_db(db, st.session_state.user_id,
                                            name, guess, correct_answer)

                        if guess == correct_answer:
                            correct_count += 1

                # Update score
                st.session_state.score['correct'] += correct_count
                st.session_state.score['total'] += 12
                st.session_state.show_answers = True

                st.rerun()

        if st.button("Next Set of Names"):
            st.session_state.current_names = None
            st.session_state.show_answers = False
            st.session_state.guesses_made = {}
            st.rerun()

def show_name_lookup_tab(data):
    """Display name lookup interface"""
    st.subheader("Look up a specific name")

    # Search box
    search_name = st.text_input("Enter a name to search:", placeholder="e.g., MAX, BELLA, CHARLIE").upper()

    if search_name:
        result = data[data['name'] == search_name]

        if not result.empty:
            row = result.iloc[0]

            # Display results in a nice format
            st.success(f"✓ Found **{search_name}**")

            col1, col2 = st.columns(2)

            with col1:
                st.metric("Classification", row['dogginess_prop'])
                st.metric("Total People with Name", f"{int(row['people_tot']):,}")
                st.metric("Total Dogs with Name", f"{int(row['dogs_tot']):,}")

            with col2:
                st.metric("Dog Angle (Total)", f"{int(row['dog_angle_tot'])}°")
                st.metric("Dog Angle (Proportion)", f"{int(row['dog_angle_prop'])}°")
                st.metric("Angle Difference", f"{int(row['angle_diff'])}°")

            # Show explanation
            st.divider()
            st.write("**What does this mean?**")

            if row['dogginess_prop'] == "Dog":
                st.write(f"🐕 **{search_name}** is much more commonly used as a dog name than a human name!")
            elif row['dogginess_prop'] == "Human":
                st.write(f"👤 **{search_name}** is much more commonly used as a human name than a dog name!")
            else:
                st.write(f"🤷 **{search_name}** could go either way - it's popular for both dogs and humans!")

        else:
            st.warning(f"Name '{search_name}' not found in the dataset. Try another name!")

            # Show some suggestions
            st.write("**Popular names to try:**")
            sample_names = data.nlargest(20, 'dogs_tot')['name'].sample(10).tolist()
            st.write(", ".join(sample_names))

def show_data_explorer_tab(data):
    """Display data explorer with mobile-friendly visualizations"""
    st.subheader("Explore the Dataset")

    # Filters
    st.write("**Filter by classification:**")
    filter_type = st.multiselect(
        "Select classifications to show:",
        options=["Dog", "Human", "Could be either"],
        default=["Dog", "Human", "Could be either"]
    )

    filtered_data = data[data['dogginess_prop'].isin(filter_type)]

    # Summary stats
    col1, col2, col3 = st.columns(3)
    with col1:
        st.metric("Total Names", len(filtered_data))
    with col2:
        dog_count = len(filtered_data[filtered_data['dogginess_prop'] == 'Dog'])
        st.metric("Dog Names", dog_count)
    with col3:
        human_count = len(filtered_data[filtered_data['dogginess_prop'] == 'Human'])
        st.metric("Human Names", human_count)

    # Distribution chart
    st.divider()
    st.write("**Classification Distribution**")
    classification_counts = filtered_data['dogginess_prop'].value_counts()
    st.bar_chart(classification_counts)

    # Top names
    st.divider()
    st.write("**Top Names by Category**")

    view_by = st.radio("Sort by:", ["Most Popular (Dogs)", "Most Popular (People)", "Most Ambiguous"], horizontal=True)

    if view_by == "Most Popular (Dogs)":
        top_names = filtered_data.nlargest(20, 'dogs_tot')[['name', 'dogs_tot', 'people_tot', 'dogginess_prop']]
        st.dataframe(
            top_names.rename(columns={
                'name': 'Name',
                'dogs_tot': 'Total Dogs',
                'people_tot': 'Total People',
                'dogginess_prop': 'Classification'
            }),
            use_container_width=True,
            hide_index=True
        )
    elif view_by == "Most Popular (People)":
        top_names = filtered_data.nlargest(20, 'people_tot')[['name', 'people_tot', 'dogs_tot', 'dogginess_prop']]
        st.dataframe(
            top_names.rename(columns={
                'name': 'Name',
                'people_tot': 'Total People',
                'dogs_tot': 'Total Dogs',
                'dogginess_prop': 'Classification'
            }),
            use_container_width=True,
            hide_index=True
        )
    else:  # Most Ambiguous
        ambiguous = filtered_data[filtered_data['dogginess_prop'] == 'Could be either'].nlargest(20, 'dogs_tot')
        st.dataframe(
            ambiguous[['name', 'dogs_tot', 'people_tot', 'dog_angle_prop']].rename(columns={
                'name': 'Name',
                'dogs_tot': 'Total Dogs',
                'people_tot': 'Total People',
                'dog_angle_prop': 'Angle (closer to 45° = more ambiguous)'
            }),
            use_container_width=True,
            hide_index=True
        )

    # Searchable full dataset
    st.divider()
    st.write("**Browse All Names**")
    st.write(f"Showing {len(filtered_data)} names")

    st.dataframe(
        filtered_data[['name', 'dogginess_prop', 'dogs_tot', 'people_tot', 'dog_angle_prop']].sort_values('dogs_tot', ascending=False).rename(columns={
            'name': 'Name',
            'dogginess_prop': 'Classification',
            'dogs_tot': 'Total Dogs',
            'people_tot': 'Total People',
            'dog_angle_prop': 'Angle'
        }),
        use_container_width=True,
        hide_index=True,
        height=400
    )

def show_results_database_tab(db):
    """Display results database stats and export"""
    st.subheader("Game Results Database")

    if db:
        all_guesses = get_all_guesses(db)

        if not all_guesses.empty:
            col1, col2, col3 = st.columns(3)
            with col1:
                st.metric("Total Guesses", len(all_guesses))
            with col2:
                accuracy = (all_guesses['is_correct'].sum() / len(all_guesses) * 100)
                st.metric("Overall Accuracy", f"{accuracy:.1f}%")
            with col3:
                unique_users = all_guesses['user_id'].nunique()
                st.metric("Unique Players", unique_users)

            # Download button for CSV export
            csv = all_guesses.to_csv(index=False)
            st.download_button(
                label="Download All Results as CSV",
                data=csv,
                file_name="dog_app_results.csv",
                mime="text/csv"
            )

            # Show recent results
            st.divider()
            st.write("**Recent Results**")
            st.dataframe(all_guesses.head(50), use_container_width=True)
        else:
            st.info("No guesses recorded yet. Play the game to start collecting data!")
    else:
        st.error("Database connection failed.")

def main():
    st.set_page_config(page_title="Dog Name or Human Name?", layout="wide")

    st.title("🐕 Dog Name or Human Name?")

    # Initialize session state
    if 'user_id' not in st.session_state:
        st.session_state.user_id = str(uuid.uuid4())
    if 'current_names' not in st.session_state:
        st.session_state.current_names = None
    if 'score' not in st.session_state:
        st.session_state.score = {'correct': 0, 'total': 0}
    if 'show_answers' not in st.session_state:
        st.session_state.show_answers = False
    if 'guesses_made' not in st.session_state:
        st.session_state.guesses_made = {}

    # Load data
    try:
        data = get_processed_data()
    except Exception as e:
        st.error(f"Error loading data: {e}")
        st.stop()

    # Initialize SQLite database
    try:
        db = init_database()
    except Exception as e:
        st.warning("Database connection failed. Guesses won't be saved.")
        db = None

    # Create tabs
    tab1, tab2, tab3, tab4 = st.tabs(["🎮 Play Game", "🔍 Name Lookup", "📊 Data Explorer", "💾 Results Database"])

    with tab1:
        show_game_tab(data, db)

    with tab2:
        show_name_lookup_tab(data)

    with tab3:
        show_data_explorer_tab(data)

    with tab4:
        show_results_database_tab(db)

if __name__ == "__main__":
    main()