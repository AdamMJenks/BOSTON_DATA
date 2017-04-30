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
library(plotrix)
library(rgdal)

formatMoney  <- function(x, ...) {
  paste0("$", formatC(as.numeric(x), format="f", digits=2, big.mark=","))
}

spToGeoJSON <- function(sp_obj){
  temp_file<-tempfile()
  writeOGR(sp_obj, temp_file,layer = "geojson", driver = "GeoJSON")
  geojs <- paste(readLines(temp_file), collapse=" ")
  file.remove(temp_file)
  return(geojs)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  getData <- reactive({
    if(input$type != 'All'){
      e <- subset(Energy_Parsed_Df, `Property Type` == input$type & year_built >= input$year)
    } else{
      e <- subset(Energy_Parsed_Df, year_built >= input$year)
    }
    e <- subset(e, select=c(Address, Property_Name, `Property Type`, year_built, Kwh_potential, sunlight_hours, sqft_available, Cost_of_installation_gross, lat, lng))
    #e$Cost_of_installation_gross <- formatMoney(e$Cost_of_installation_gross)
    
    return(e)
  })
    
  
  output$energy_table <- renderDataTable({
    
    # generate bins based on input$bins from ui.R
    the_data <- getData()
    
    # draw the histogram with the specified number of bins
    the_data
    
    the_data <- the_data %>% 
      datatable() %>%
      formatRound(columns=c('Kwh_potential', 'Cost_of_installation_gross'), digits=0)
    
  }, options = list(scrollX = TRUE, order = list(list(4, 'desc'))))
  
  output$mymap <- renderLeaflet({
  
    the_data <- getData()
    Boston_shape@data = left_join(Boston_shape@data, the_data, by = 'Property_Name', copy = TRUE)
    Boston_shape@data$height <- rescale(Boston_shape@data$Kwh_potential, c(0, 400)) 
    rbPal <- colorRampPalette(c('green','red'))
    Boston_shape@data$color <- rbPal(10)[cut(as.numeric(Boston_shape@data$Cost_of_installation_gross),breaks = 10)]
    Boston_shape@data <- subset(Boston_shape@data, select=-c(Pct_Gas, Pct_Electricity, Pct_Steam))
    i <- which(is.na(Boston_shape@data$Kwh_potential))
    Boston_shape <- Boston_shape[-i]
  
    geojsonio::geojson_write(Boston_shape, file="data.geojson")
    
    pal <- colorNumeric("viridis", NULL)

    leaflet(Boston_shape) %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                  fillColor = ~pal(Kwh_potential)) %>%
      addLegend(pal = pal, values = ~(Kwh_potential), opacity = 1.0)
  })
  
})
  
