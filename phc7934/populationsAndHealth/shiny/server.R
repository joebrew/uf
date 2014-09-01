
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
setwd("C:/Users/BrewJR/Documents/uf/phc7934/populationsAndHealth/shiny")
source("function.R")

shinyServer(function(input, output) {

  output$plot1 <- renderPlot({
    MortFun(input$country, bar = FALSE, line = TRUE)
    
  })
  
  
  output$plot2 <- renderPlot({
    MortFun(input$country)
    
  })

})
