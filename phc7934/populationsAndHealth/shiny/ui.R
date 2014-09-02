
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("PHC 7934 - Seminar: Populations and Health"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.tabs == 'Rose'",
        textInput("bla", "Bla")),
      
      conditionalPanel(
        condition = "input.tabs == 'Omran'",
        textInput("bla2", "Bla2")),
      
      conditionalPanel(
        condition = "input.tabs == 'Explore'",
        selectInput("country",
                    "Country:",
                    c("AUS",
                      "AUT",
                      #"BEL",
                      "BGR",
                      "BLR",
                      "CAN",
                      "CHE",
                      "CHL",
                      "CZE",
                      "DEUTE",
                      "DEUTNP",
                      "DEUTW",  
                      "DNK" ,    "ESP" ,    "EST",    
                      "FIN",     "FRACNP" , "FRATNP", 
                      'GBR_NIR' ,"GBR_NP",  "GBR_SCO",
                      "GBRCENW", "GBRTENW", "HUN" ,   
                      "IRL"  ,   "ISL" ,    "ISR",    
                      "ITA" ,    "JPN" ,    "LTU" ,   
                      "LUX" ,    "LVA" ,   "NLD" ,   
                      "NOR" ,    "NZL_MA" , "NZL_NM" ,
                      "NZL_NP" , "POL" ,    "PRT"  ,  
                      "RUS"  ,   "SVK" ,    "SVN" ,   
                      "SWE" ,    "TWN" ,    "UKR" ,   
                      "USA" )),
        sliderInput("year", "Year", 
                    min=1850, max=2013, value=2000, step=1,
                    animate=animationOptions(interval = 30, loop = FALSE,
                                             playButton = "Play", pauseButton = "Pause"))
        
        )
          ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id="tabs",
        tabPanel("Rose",
                 p("test")),
        tabPanel("Omran",
                 htmlOutput("motionchart1") 
        ),
        tabPanel("Explore",
                 plotOutput("plot3"),
                 plotOutput("plot4"),
                 plotOutput("plot1"),
                 plotOutput("plot2")
                 )
        )
    )
  )
))
