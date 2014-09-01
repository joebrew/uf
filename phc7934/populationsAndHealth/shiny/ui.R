
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Old Faithful Geyser Data"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
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
                    "USA" ))
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot1"),
      plotOutput("plot2")
    )
  )
))
