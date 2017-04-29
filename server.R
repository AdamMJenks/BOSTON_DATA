#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  getData <- reactive({
    if(input$type != 'All'){
      e <- subset(Energy_Parsed_Df, `Property Type` == input$type)
      return(e)
    } else {
      return(Energy_Parsed_Df)
    }
    
    
    
  })
    
  
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    the_data <- getData()
    
    # draw the histogram with the specified number of bins
    hist(as.numeric(the_data$`Site EUI (kBTU/sf)`), col = 'darkgray', border = 'white')
    
  })
  
})
