import streamlit as st
import pandas as pd
import numpy as np
import random
import math
from supabase import create_client, Client
import uuid
from datetime import datetime

# Initialize Supabase client
# You'll need to set these in Streamlit secrets
@st.cache_resource
def init_supabase():
    url = st.secrets["supabase"]["url"]
    key = st.secrets["supabase"]["anon_key"]
    return create_client(url, key)

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

def save_guess_to_db(supabase: Client, user_id: str, name: str, guess: str, correct_answer: str):
    """Save user guess to database"""
    try:
        data = {
            "user_id": user_id,
            "name": name,
            "guess": guess,
            "correct_answer": correct_answer,
            "is_correct": guess == correct_answer,
            "timestamp": datetime.now().isoformat()
        }
        result = supabase.table("guesses").insert(data).execute()
        return True
    except Exception as e:
        st.error(f"Error saving to database: {e}")
        return False

def main():
    st.set_page_config(page_title="Dog Name or Human Name?", layout="wide")
    
    st.title("Dog Name or Human Name?")
    st.subheader("Guess whether each name is a human name, a dog name, or somewhere in between")
    st.write("If you get them all right, you get a treat!")
    
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
    
    # Initialize Supabase
    try:
        supabase = init_supabase()
    except Exception as e:
        st.warning("Database connection failed. Guesses won't be saved.")
        supabase = None
    
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
                        if supabase:
                            save_guess_to_db(supabase, st.session_state.user_id, 
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

if __name__ == "__main__":
    main()