
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
#setwd("C:/Users/BrewJR/Documents/uf/phc7934/populationsAndHealth/shiny")
source("function.R")
source("wb.R")

shinyServer(function(input, output) {

  output$plot1 <- renderPlot({
    MortFun(input$country, bar = FALSE, line = TRUE)
    
  })
  
  
  output$plot2 <- renderPlot({
    MortFun(input$country)
    
  })
  
  output$plot3 <- renderPlot({
    MortFun(input$country, bar = FALSE, line = FALSE, time = TRUE,
            year = input$year)
    
  })
  
  output$plot4 <- renderPlot({
    MortFun(input$country, bar = FALSE, line = FALSE, time = FALSE,
            year = input$year, scatter = TRUE)
    
  })
  
  output$motionchart1 <- renderGvis({
    
    gvisMotionChart(WorldBank,
                    idvar="country", timevar="year",
                    xvar="life.expectancy", 
                    yvar="fertility.rate",
                    colorvar="region", 
                    sizevar="population",
                    options=list(width=550, height=500))
    
  })

})
