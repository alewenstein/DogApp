library(shiny)
library(tidyverse)
library(rCAT)
library(shinyjs)
library(shinythemes)

NY <- read.csv("NY.TXT", header=FALSE)
NYC_Dogs = read.csv("NYC_Dog_Licensing_Dataset.csv")

list_to_remove = c("UNKNOWN","NAME NOT PROVIDED","NONE",'A','.','NAME','NOT',"UNKNOWED")
dogs_by_year = NYC_Dogs %>%
  filter(!(AnimalName %in% list_to_remove) & !is.na(AnimalName) &
           !str_detect(AnimalName,'[^a-zA-Z\\-]' )) %>%
  group_by(AnimalBirthYear) %>% mutate(dogs_in_year = n()) %>%
  ungroup() %>%
  group_by(AnimalBirthYear,dogs_in_year,AnimalName) %>%
  summarise(dogs = n()) %>%
  mutate(dog_prop=round(100*dogs/dogs_in_year,1))


Human_names = NY %>% filter(V3 >= 1991) %>% group_by(V3) %>%
  mutate(people_in_year = sum(V5), name = str_to_upper(V4),
         human_prop = round(100*V5/people_in_year,1)) %>%
  select(V3,people_in_year,name,human_prop,V5) %>% rename(year = V3, people = V5)


Cross = inner_join(Human_names,dogs_by_year,by=c("year"="AnimalBirthYear","name"="AnimalName")) %>%
  filter(year>=2000)

check_corr = Cross %>% group_by(name) %>% 
  summarize(correlation =  0,people_tot = sum(people),
            dogs_tot = sum(dogs)) %>% ungroup() %>%
  mutate(dog_angle_tot = round(rad2deg(atan2(people_tot,dogs_tot))),
         dogs_prop = dogs_tot/sum(dogs_tot), people_prop = people_tot/sum(people_tot),
         dog_angle_prop = round(rad2deg(atan2(people_prop,dogs_prop)))) %>% 
  mutate(dogginess_tot = case_when(
    between(dog_angle_tot,0,34) ~ "Who's a good boy, yes you are!",
    between(dog_angle_tot,35,55) ~ "Animorph",
    between(dog_angle_tot,56,90) ~ "You're a human!!"),
    dogginess_prop = case_when(
      between(dog_angle_prop,0,34) ~ "Dog",
      between(dog_angle_prop,35,55) ~ "Could be either",
      between(dog_angle_prop,56,90) ~ "Human"),
    angle_diff = abs(dog_angle_tot-dog_angle_prop))


ui <- fluidPage(theme = shinytheme("journal"),
  titlePanel("Dog Name or Human Name?"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Instructions:"),
      p("Guess whether each name is a human name, a dog name, or somewhere in between"),
      p("If you get them all right, you get a treat!"),
      br(),
      actionButton("submit", "Submit"),
      actionButton("next_", "Next Set of Names"),
      br(),
      h4("Score:"),
      textOutput("score"),
      br(),
      div(id = "answer_key",
          h4("Answers:"),
          br(),
          tableOutput('answer_table')
          )
      ) %>% shinyjs::hidden()
      
    ,
    
    mainPanel(
      h2("Guess if each name is a dog name, a human name, or somewhere in between:"),
      fluidRow(
        lapply(1:12, function(i) {
        column(3,
               wellPanel(
                 div(
                   h2(textOutput(paste0("name_", i))),
                   radioButtons(paste0("guess_", i), label = h4("Make your guess"),
                                choices = list("Dog" = "Dog", "Human" = "Human", "Who knows" = "Could be either"))
                 )
                 )
               )       
        
      })
    )
  )
)
)

server <- function(input, output, session) {
  
  # Track the user's score
  score <- reactiveValues(correct = 0, total = 0)
  
  # Generate a set of 10 random letters and show them to the user
  game_group <- reactive({
    check_corr[sample(nrow(check_corr),12),]
  })
  names_list = reactive({game_group()[[1]]})
  answer_list = reactive({game_group()[[10]]})
  
  
  lapply(1:12, function(i) {
    output[[paste0("name_", i)]] <- renderText({
      names_list()[i]
    })
  })
  

  
  
  # Check if the user's guess for each letter is correct
  observeEvent(input$submit, {
    for (i in 1:12) {
      if (answer_list()[i] ==input[[paste0("guess_", i)]] ) {
        score$correct <- score$correct + 1
      }  
    }
    guess_list = reactive({c(input$guess_1,input$guess_2,input$guess_3,input$guess_4,input$guess_5,
                   input$guess_6,input$guess_7,input$guess_8,input$guess_9,input$guess_10,
                   input$guess_11, input$guess_12)})
    
    key = reactive({cbind(Name = names_list(),Guess =guess_list(), Answer = answer_list())})
    output$answer_table = renderTable(key())
    shinyjs::toggle("answer_key")
    
  })
  
  # Show the user's score
  output$score <- renderText({
    paste("Correct:", score$correct)
  })
  
  # Generate a new set of 10 random letters and reset the user's guesses
  observeEvent(input$next_, {
    session$reload()
  })
}

shinyApp(ui, server)

#rsconnect::deployApp('/Users/arilewenstein/Documents/Data/Dog App')


