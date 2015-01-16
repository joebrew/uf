
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  
  tags$img(src="https://admin.imodules.com/s/981/images/editor/College%20of%20Public%20Health%20Professions%20UF.jpg", width = 250),
  titlePanel("Xu and Brew's MET Calculator"),
  
  
  helpText(paste("Visualize, in both calories expended as well as",
                 "Metabolic equivalent tasks (MET's) exactly how you expend your energy.")),
  tags$div(
    HTML("<a href='https://github.com/joebrew/uf/tree/master/phc6711/metcalculator'>Code for this app</a>")
  ),
  tags$div(
    HTML("<a href='mailto:joebrew@gmail.com'>joebrew@gmail.com</a>")
  ),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      
      h5("BMI information:"),
      helpText(paste0("Provide your height and weight",
                      " (we need this to calculate calories).")),
      
      textInput(inputId = 'height',
                label = 'Height (inches)',
                value = 66),
      
      textInput(inputId = 'weight',
                label = 'Weight (pounds',
                value = 150), 
      
      
      tags$hr(),
      h5("Activity information:"),
      helpText(paste0("Scroll through the following activities.",
                      " If you've done any of them in the LAST 30 DAYS, fill in the number of times, intensity and average",
                      " duration.")),
      br(),
      helpText(paste0("If you did the activity 'vigorously' mark it as such.",
                      " If you did the activety only 'moderately,' leave the checkbox unmarked.",
                      " For duration, simply slide to the AVERAGE number of hours you ",
                      "spend doing the activity each session.")),
     
      textInput(inputId = 'Aerobics',
                label = 'Aerobics',
                value = '0'),
      
      checkboxInput('Aerobics_vig', label = 'Vigorous', value = FALSE),
      sliderInput('Aerobics_dur', label = 'Duration (in hours)', 
                  min=0, max=5, value=0, step=0.25),
      
      br(),       tags$hr(), 
      textInput(inputId = 'Baseball',
                label = 'Baseball',
                value = '0'),
      
      checkboxInput('Baseball_vig', label = 'Vigorous', value = FALSE),
      sliderInput('Baseball_dur', label = 'Duration (in hours)', 
                  min=0, max=5, value=0, step=0.25),
      
      br(),       tags$hr(), 
      textInput(inputId = 'Basketball',
                label = 'Basketball',
                value = '0'),
      
      checkboxInput('Basketball_vig', label = 'Vigorous', value = FALSE),
      sliderInput('Basketball_dur', label = 'Duration (in hours)', 
                  min=0, max=5, value=0, step=0.25),
      
      br(),       tags$hr(), 
      textInput(inputId = 'Bicycling',
                label = 'Bicycling',
                value = '0'),
      
      checkboxInput('Bicycling_vig', label = 'Vigorous', value = FALSE),
      sliderInput('Bicycling_dur', label = 'Duration (in hours)', 
                  min=0, max=5, value=0, step=0.25),
      
      br(),       tags$hr(), 
      textInput(inputId = 'Dance',
                label = 'Dance',
                value = '0'),
      
      checkboxInput('Dance_vig', label = 'Vigorous', value = FALSE),
      sliderInput('Dance_dur', label = 'Duration (in hours)', 
                  min=0, max=5, value=0, step=0.25),
      
      br(),       tags$hr(), 
      textInput(inputId = 'Football',
                label = 'Football',
                value = '0'),
      
      checkboxInput('Football_vig', label = 'Vigorous', value = FALSE),
      sliderInput('Football_dur', label = 'Duration (in hours)', 
                  min=0, max=5, value=0, step=0.25),
      
      br(),       tags$hr(), 
      textInput(inputId = 'Golf',
                label = 'Golf',
                value = '0'),
      
      checkboxInput('Golf_vig', label = 'Vigorous', value = FALSE),
      sliderInput('Golf_dur', label = 'Duration (in hours)', 
                  min=0, max=5, value=0, step=0.25),
      
      br(),       tags$hr(), 
      textInput(inputId = 'Hiking',
                label = 'Hiking',
                value = '0'),
      
      checkboxInput('Hiking_vig', label = 'Vigorous', value = FALSE),
      sliderInput('Hiking_dur', label = 'Duration (in hours)', 
                  min=0, max=5, value=0, step=0.25),
      
      br(),       tags$hr(), 
      textInput(inputId = 'Jogging',
                label = 'Jogging',
                value = '0'),
      
      checkboxInput('Jogging_vig', label = 'Vigorous', value = FALSE),
      sliderInput('Jogging_dur', label = 'Duration (in hours)', 
                  min=0, max=5, value=0, step=0.25),
      
      br(),       tags$hr(), 
      textInput(inputId = 'Kayaking',
                label = 'Kayaking',
                value = '0'),
      
      checkboxInput('Kayaking_vig', label = 'Vigorous', value = FALSE),
      sliderInput('Kayaking_dur', label = 'Duration (in hours)', 
                  min=0, max=5, value=0, step=0.25),
      
      br(),       tags$hr(), 
      textInput(inputId = 'Push_ups',
                label = 'Push_ups',
                value = '0'),
      
      checkboxInput('Push_ups_vig', label = 'Vigorous', value = FALSE),
      sliderInput('Push_ups_dur', label = 'Duration (in hours)', 
                  min=0, max=5, value=0, step=0.25),
      
      br(),       tags$hr(), 
      textInput(inputId = 'Racquetball',
                label = 'Racquetball',
                value = '0'),
      
      checkboxInput('Racquetball_vig', label = 'Vigorous', value = FALSE),
      sliderInput('Racquetball_dur', label = 'Duration (in hours)', 
                  min=0, max=5, value=0, step=0.25),
      
      br(),       tags$hr(), 
      textInput(inputId = 'Rowing',
                label = 'Rowing',
                value = '0'),
      
      checkboxInput('Rowing_vig', label = 'Vigorous', value = FALSE),
      sliderInput('Rowing_dur', label = 'Duration (in hours)', 
                  min=0, max=5, value=0, step=0.25),
      
      br(),       tags$hr(), 
      textInput(inputId = 'Running',
                label = 'Running',
                value = '0'),
      
      checkboxInput('Running_vig', label = 'Vigorous', value = FALSE),
      sliderInput('Running_dur', label = 'Duration (in hours)', 
                  min=0, max=5, value=0, step=0.25),
      
      br(),       tags$hr(), 
      textInput(inputId = 'Sit_ups',
                label = 'Sit_ups',
                value = '0'),
      
      checkboxInput('Sit_ups_vig', label = 'Vigorous', value = FALSE),
      sliderInput('Sit_ups_dur', label = 'Duration (in hours)', 
                  min=0, max=5, value=0, step=0.25),
      
      br(),       tags$hr(), 
      textInput(inputId = 'Skating',
                label = 'Skating',
                value = '0'),
      
      checkboxInput('Skating_vig', label = 'Vigorous', value = FALSE),
      sliderInput('Skating_dur', label = 'Duration (in hours)', 
                  min=0, max=5, value=0, step=0.25),
      
      br(),       tags$hr(), 
      textInput(inputId = 'Soccer',
                label = 'Soccer',
                value = '0'),
      
      checkboxInput('Soccer_vig', label = 'Vigorous', value = FALSE),
      sliderInput('Soccer_dur', label = 'Duration (in hours)', 
                  min=0, max=5, value=0, step=0.25),
      
      br(),       tags$hr(), 
      textInput(inputId = 'Stair_Climbing',
                label = 'Stair_Climbing',
                value = '0'),
      
      checkboxInput('Stair_Climbing_vig', label = 'Vigorous', value = FALSE),
      sliderInput('Stair_Climbing_dur', label = 'Duration (in hours)', 
                  min=0, max=5, value=0, step=0.25),
      
      br(),       tags$hr(), 
      textInput(inputId = 'Stretching',
                label = 'Stretching',
                value = '0'),
      
      checkboxInput('Stretching_vig', label = 'Vigorous', value = FALSE),
      sliderInput('Stretching_dur', label = 'Duration (in hours)', 
                  min=0, max=5, value=0, step=0.25),
      
      br(),       tags$hr(), 
      textInput(inputId = 'Swimming',
                label = 'Swimming',
                value = '0'),
      
      checkboxInput('Swimming_vig', label = 'Vigorous', value = FALSE),
      sliderInput('Swimming_dur', label = 'Duration (in hours)', 
                  min=0, max=5, value=0, step=0.25),
      
      br(),       tags$hr(), 
      textInput(inputId = 'Tennis',
                label = 'Tennis',
                value = '0'),
      
      checkboxInput('Tennis_vig', label = 'Vigorous', value = FALSE),
      sliderInput('Tennis_dur', label = 'Duration (in hours)', 
                  min=0, max=5, value=0, step=0.25),
      
      br(),       tags$hr(), 
      textInput(inputId = 'Volleyball',
                label = 'Volleyball',
                value = '0'),
      
      checkboxInput('Volleyball_vig', label = 'Vigorous', value = FALSE),
      sliderInput('Volleyball_dur', label = 'Duration (in hours)', 
                  min=0, max=5, value=0, step=0.25),
      
      br(),       tags$hr(), 
      textInput(inputId = 'Walking',
                label = 'Walking',
                value = '0'),
      
      checkboxInput('Walking_vig', label = 'Vigorous', value = FALSE),
      sliderInput('Walking_dur', label = 'Duration (in hours)', 
                  min=0, max=5, value=0, step=0.25),
      
      br(),       tags$hr(), 
      textInput(inputId = 'Weight_Lifting',
                label = 'Weight_Lifting',
                value = '0'),
      
      checkboxInput('Weight_Lifting_vig', label = 'Vigorous', value = FALSE),
      sliderInput('Weight_Lifting_dur', label = 'Duration (in hours)', 
                  min=0, max=5, value=0, step=0.25),
      
      br(),       tags$hr(), 
      textInput(inputId = 'Yard_Work',
                label = 'Yard_Work',
                value = '0'),
      
      checkboxInput('Yard_Work_vig', label = 'Vigorous', value = FALSE),
      sliderInput('Yard_Work_dur', label = 'Duration (in hours)', 
                  min=0, max=5, value=0, step=0.25),
      
      br(),       tags$hr(), 
      textInput(inputId = 'Frisbee',
                label = 'Frisbee',
                value = '0'),
      
      checkboxInput('Frisbee_vig', label = 'Vigorous', value = FALSE),
      sliderInput('Frisbee_dur', label = 'Duration (in hours)', 
                  min=0, max=5, value=0, step=0.25),
      
      br(),       tags$hr(), 
      textInput(inputId = 'Martial_Arts',
                label = 'Martial_Arts',
                value = '0'),
      
      checkboxInput('Martial_Arts_vig', label = 'Vigorous', value = FALSE),
      sliderInput('Martial_Arts_dur', label = 'Duration (in hours)', 
                  min=0, max=5, value=0, step=0.25),
      
      br(),       tags$hr(), 
      textInput(inputId = 'Yoga',
                label = 'Yoga',
                value = '0'),
      
      checkboxInput('Yoga_vig', label = 'Vigorous', value = FALSE),
      sliderInput('Yoga_dur', label = 'Duration (in hours)', 
                  min=0, max=5, value=0, step=0.25),
      
      br(),       tags$hr(), 
      textInput(inputId = 'Gymnastics',
                label = 'Gymnastics',
                value = '0'),
      
      checkboxInput('Gymnastics_vig', label = 'Vigorous', value = FALSE),
      sliderInput('Gymnastics_dur', label = 'Duration (in hours)', 
                  min=0, max=5, value=0, step=0.25),
      
      br(),       tags$hr(), 
      textInput(inputId = 'Skateboarding',
                label = 'Skateboarding',
                value = '0'),
      
      checkboxInput('Skateboarding_vig', label = 'Vigorous', value = FALSE),
      sliderInput('Skateboarding_dur', label = 'Duration (in hours)', 
                  min=0, max=5, value=0, step=0.25),
      
      br(),       tags$hr(), 
      textInput(inputId = 'Other',
                label = 'Other',
                value = '0'),
      
      checkboxInput('Other_vig', label = 'Vigorous', value = FALSE),
      sliderInput('Other_dur', label = 'Duration (in hours)', 
                  min=0, max=5, value=0, step=0.25),
      
      br()
      
      
      
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      tabsetPanel(
        tabPanel("Activity visuals",
                 #tableOutput("bmi_table"),
                 plotOutput("calorie_plot"),
                 plotOutput("met_plot")
                 
                 
    ),
    
    tabPanel("BMI visuals",
             plotOutput("bmi_plot")
    ),
    
    tabPanel("Activity data",
             dataTableOutput("my_table")
             )
    )
  )
  )
))
