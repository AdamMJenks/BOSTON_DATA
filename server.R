#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  getData <- reactive({
    if(input$type != 'All'){
      e <- subset(Energy_Parsed_Df, `Property Type` == input$type & year_built >= input$year)
    } else{
      e <- subset(Energy_Parsed_Df, year_built >= input$year)
    }
    return(e)
  })
    
  
  output$energy_table <- renderDataTable({
    
    # generate bins based on input$bins from ui.R
    the_data <- getData()
    
    # draw the histogram with the specified number of bins
    the_data
    
  }, options = list(scrollX = TRUE))
  
})
