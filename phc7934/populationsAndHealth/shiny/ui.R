
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
        condition = "input.tabs == 'Welcome'",
        p("joebrew@gmail.com"),
        tags$div(
          HTML("<a href='github.com/joebrew/phc7934/'>Code for this site</a>")
          )),
      
      conditionalPanel(
        condition = "input.tabs == 'Rose'",
        p("joebrew@gmail.com"),
        tags$div(
          HTML("<a href='github.com/joebrew/phc7934/'>Code for this site</a>")
        )
        ),
      
      conditionalPanel(
        condition = "input.tabs == 'Rose example'",
        helpText("Pick a population (or both)"),
        checkboxInput("british", label = "British civil servant", value = FALSE),
        checkboxInput("kenyan", label = "Kenyan nomad", value = FALSE),
        br(), br(),
        
        p("joebrew@gmail.com"),
        tags$div(
          HTML("<a href='github.com/joebrew/phc7934/'>Code for this site</a>")
        )
      ),
      
      conditionalPanel(
        condition = "input.tabs == 'Omran'",
        p("joebrew@gmail.com"),
        tags$div(
          HTML("<a href='github.com/joebrew/phc7934/'>Code for this site</a>")
        )        ),
      
      conditionalPanel(
        condition = "input.tabs == 'Omran example'",
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
                                             playButton = "Play", pauseButton = "Pause")),
        p("joebrew@gmail.com"),
        tags$div(
          HTML("<a href='github.com/joebrew/phc7934/'>Code for this site</a>")
        )
        
        )
          ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id="tabs",
                  tabPanel("Welcome",
                           tags$div(
                             HTML('<iframe src="https://docs.google.com/presentation/d/1if_Dug2Q0M2lo7i_vP1C7VezfmxxTR2iiOlVxRr6w14/embed?start=false&loop=false&delayms=3000" frameborder="0" width="480" height="374" allowfullscreen="true" mozallowfullscreen="true" webkitallowfullscreen="true"></iframe>')
                           ),
                      
                           p("The below is NOT an original chart.  Credit goes to GoogleVis:"),
                           p("http://www.r-bloggers.com/googlevis-0-5-5-released/"),
                           htmlOutput("motionchart1") ),
                  
        tabPanel("Rose",
                 h4("Reading notes:"),
                 tags$div(
                   HTML('<iframe src="https://docs.google.com/document/d/1YoAmjr8zyJ8R-tMcfmjJC_K2Rhs8F5KiC212Kp1OMVw/pub?embedded=true" width="600" height="600" ></iframe>')
                 )),
        
        tabPanel("Rose example",
                 plotOutput("plot5") 
                 ),
        tabPanel("Omran",
                 h4("Reading notes:"),
                 tags$div(
                   HTML('<iframe src="https://docs.google.com/document/d/1_aho8TyRyNXp1bFd3qaZpPAR4zEPkrivfXT26xFhelQ/pub?embedded=true" width="600" height="600"></iframe>')
                 )
        ),
        tabPanel("Omran example",
                 p("These data come from the Human Mortality Database, but the visualizations are original."),
                 plotOutput("plot3"),
                 plotOutput("plot4"),
                 plotOutput("plot1"),
                 plotOutput("plot2")
                 )
        )
    )
  )
))
