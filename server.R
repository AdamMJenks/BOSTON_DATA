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
library(dplyr)
library(plotrix)
library(rgdal)
library(shinyWidgets)
library(spatialEco)

formatMoney  <- function(x, ...) {
  paste0("$", formatC(as.numeric(x), format="f", digits=2, big.mark=","))
}

shinyServer(function(input, output) {
   
  getData <- reactive({
      if(input$type != 'All'){
        e <- subset(Energy_Parsed_Df, `Property Type` == input$type & year_built >= input$year)
      } else{
        e <- subset(Energy_Parsed_Df, year_built >= input$year)
      }
      
      e <- subset(e, select=c(Address, Property_Name, `Property Type`, year_built, Kwh_potential, sunlight_hours, sqft_available, Cost_of_installation_gross, lat, lng))

    
    return(e)
  })
  
  output$energy_table <- renderDataTable({
    
    # generate bins based on input$bins from ui.R
    the_data <- getData()
    
    the_data <- the_data %>% 
      datatable() %>%
      formatRound(columns=c('Kwh_potential', 'Cost_of_installation_gross'), digits=0)
    
  }, options = list(scrollX = TRUE, order = list(list(4, 'desc'))))
  
  output$scenario_lists <- renderUI({
    if(input$land_building == "Boston Buildings"){
      pickerInput(
        label = "Choose Property Types", inputId = 'prop_types',multiple = TRUE, selected = 'Boston All',
        choices = c('Boston All',unique(Energy_Parsed_Df$`Property Type`))
      )
    } else if (input$land_building == "BPDA Owned Land"){
      pickerInput(
        label = "Choose Property Types", inputId = 'prop_types',multiple = TRUE, selected = 'BPDA All',
        choices = c('BPDA All',unique(as.character(land_parcels$type)))
      )
    } else if (input$land_building == 'Both'){
      pickerInput(
        label = "Choose Property Types", inputId = 'prop_types',multiple = TRUE, selected = 'All',
        choices = c('All','Boston All', 'BPDA All',unique(land_parcels$type, unique(Energy_Parsed_Df$`Property Type`)))
      )
    }
  })
  
  output$scenario_table <- renderDataTable({
    
    # generate bins based on input$bins from ui.R
    the_data <- getData()
    
    the_data <- the_data %>% 
      datatable() %>%
      formatRound(columns=c('Kwh_potential', 'Cost_of_installation_gross'), digits=0)
    
  }, options = list(scrollX = TRUE, order = list(list(4, 'desc'))))
  
  ### Leaflet map for energy tab
  output$mymap <- renderLeaflet({
 
     #browser()
    
    the_data <- getData()
     Boston_shape@data = data.frame(Boston_shape@data, the_data[match(Boston_shape@data$Property_Name, the_data$Property_Name),])

     rbPal <- colorRampPalette(c('green','red'))
     Boston_shape@data$score <- as.numeric(Boston_shape@data$Cost_of_installation_gross) / Boston_shape@data$Kwh_potential
     Boston_shape@data$score <- rescale(Boston_shape@data$score, c(0, 100))
     Boston_shape@data <- subset(Boston_shape@data, select=-c(Pct_Gas, Pct_Electricity, Pct_Steam))
     ## Remove some outliers
     Boston_shape@data$score [which(Boston_shape@data$score > 30)] <- NA
     Boston_shape@data$score <- rescale(Boston_shape@data$score, c(0, 100))
     Boston_shape@data$height <-Boston_shape@data$score
     Boston_shape@data$color <- rbPal(10)[cut(as.numeric(Boston_shape@data$score),breaks = 10)]

     Boston_shape <- sp.na.omit(Boston_shape, 'Kwh_potential')

#      geojsonio::geojson_write(Boston_shape, file="www/data.geojson")

    pal <- colorNumeric("viridis", NULL)

    leaflet(Boston_shape) %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                  fillColor = ~pal(Kwh_potential)) %>%
      addLegend(pal = pal, values = ~(Kwh_potential), opacity = 1.0)
  })
  
  ### Scenario action button and compute
  observeEvent(input$compute_button, {
    
    if(input$land_building == "Boston Buildings"){
      if('Boston All' %in% input$prop_types){
        calculate_df <- Energy_Parsed_Df %>% 
                                dplyr::select(Property_Name, `Property Type`, Address, Total_Site_Energy_Kwh, 
                                       sqft_available, sunlight_hours) %>%
                                mutate(Available_sqft_for_panels = sqft_available * (input$perc_roof_used/100),
                                       Number_of_300watt_Panels = Available_sqft_for_panels / 20.67,
                                       Kwh_potential = ((Number_of_300watt_Panels * 300)/1000) * sunlight_hours * 0.75,
                                       Cost_of_installation_gross = Number_of_300watt_Panels * 300 * input$price_p_watt)
        
        output$energy_requested <- renderInfoBox({
          infoBox("Energy Coverage Requested", (Total_energy_city_of_boston * (input$city_bost_perc/100)), icon = icon("list"), color = "purple")
          })
        output$produced_by_selection <- renderInfoBox({
          infoBox("Energy Produced", sum(calculate_df$Kwh_potential, na.rm = T), icon = icon("list"), color = "green")
          })
        output$percentage_covered <- renderInfoBox({
          infoBox("Percent Covered", ((sum(calculate_df$Kwh_potential, na.rm = T)/(Total_energy_city_of_boston * (input$city_bost_perc/100)))*100), icon = icon("list"), color = "blue")
          })
        output$cost_of_implementation <- renderInfoBox({
          infoBox("Cost of Implementation", sum(calculate_df$Cost_of_installation_gross, na.rm = T), icon = icon("list"), color = "orange")
          })
        
      } else {
      
        calculate_df <- Energy_Parsed_Df %>%
                                 dplyr::select(Property_Name, `Property Type`, Address, Total_Site_Energy_Kwh, 
                                               sqft_available, sunlight_hours) %>%
                                 subset(`Property Type` %in% input$prop_types) %>%
                                 mutate(Available_sqft_for_panels = sqft_available * (input$perc_roof_used/100),
                                        Number_of_300watt_Panels = Available_sqft_for_panels / 20.67,
                                        Kwh_potential = ((Number_of_300watt_Panels * 300)/1000) * sunlight_hours * 0.75,
                                        Cost_of_installation_gross = Number_of_300watt_Panels * 300 * input$price_p_watt)
        
        output$energy_requested <- renderInfoBox({
          infoBox("Energy Coverage Requested", (Total_energy_city_of_boston * (input$city_bost_perc/100)), icon = icon("list"), color = "purple")
        })
        output$produced_by_selection <- renderInfoBox({
          infoBox("Energy Produced", sum(calculate_df$Kwh_potential, na.rm = T), icon = icon("list"), color = "green")
        })
        output$percentage_covered <- renderInfoBox({
          infoBox("Percent Covered", ((sum(calculate_df$Kwh_potential, na.rm = T)/(Total_energy_city_of_boston * (input$city_bost_perc/100)))*100), icon = icon("list"), color = "blue")
        })
        output$cost_of_implementation <- renderInfoBox({
          infoBox("Cost of Implementation", sum(calculate_df$Cost_of_installation_gross, na.rm = T), icon = icon("list"), color = "orange")
        })
        

    }
  } else if (input$land_building == "BPDA Owned Land"){
    if('BPDA All' %in% input$prop_types){
      calculate_df <- land_parsed_df %>%
                                dplyr::select(address, neighborhood, type, sunlight_hours, sqft_available) %>%
                                mutate(Available_sqft_for_panels = sqft_available * (input$perc_roof_used/100),
                                       Number_of_300watt_Panels = Available_sqft_for_panels / 20.67,
                                       Kwh_potential = ((Number_of_300watt_Panels * 300)/1000) * sunlight_hours * 0.75,
                                       Cost_of_installation_gross = Number_of_300watt_Panels * 300 * input$price_p_watt)
      
      output$energy_requested <- renderInfoBox({
        infoBox("Energy Coverage Requested", (Total_energy_city_of_boston * (input$city_bost_perc/100)), icon = icon("list"), color = "purple")
      })
      output$produced_by_selection <- renderInfoBox({
        infoBox("Energy Produced", sum(calculate_df$Kwh_potential, na.rm = T), icon = icon("list"), color = "green")
      })
      output$percentage_covered <- renderInfoBox({
        infoBox("Percent Covered", ((sum(calculate_df$Kwh_potential, na.rm = T)/(Total_energy_city_of_boston * (input$city_bost_perc/100)))*100), icon = icon("list"), color = "blue")
      })
      output$cost_of_implementation <- renderInfoBox({
        infoBox("Cost of Implementation", sum(calculate_df$Cost_of_installation_gross, na.rm = T), icon = icon("list"), color = "orange")
      })
      
    } else {
      calculate_df <- land_parsed_df %>%
                                dplyr::select(address, neighborhood, type, sunlight_hours, sqft_available) %>%
                                subset(type %in% input$prop_types) %>%
                                mutate(Available_sqft_for_panels = sqft_available * (input$perc_roof_used/100),
                                       Number_of_300watt_Panels = Available_sqft_for_panels / 20.67,
                                       Kwh_potential = ((Number_of_300watt_Panels * 300)/1000) * sunlight_hours * 0.75,
                                       Cost_of_installation_gross = Number_of_300watt_Panels * 300 * input$price_p_watt)
      
      output$energy_requested <- renderInfoBox({
        infoBox("Energy Coverage Requested", (Total_energy_city_of_boston * (input$city_bost_perc/100)), icon = icon("list"), color = "purple")
      })
      output$produced_by_selection <- renderInfoBox({
        infoBox("Energy Produced", sum(calculate_df$Kwh_potential, na.rm = T), icon = icon("list"), color = "green")
      })
      output$percentage_covered <- renderInfoBox({
        infoBox("Percent Covered", ((sum(calculate_df$Kwh_potential, na.rm = T)/(Total_energy_city_of_boston * (input$city_bost_perc/100)))*100), icon = icon("list"), color = "blue")
      })
      output$cost_of_implementation <- renderInfoBox({
        infoBox("Cost of Implementation", sum(calculate_df$Cost_of_installation_gross, na.rm = T), icon = icon("list"), color = "orange")
      })
      
    }
   } else if (input$land_building == 'Both'){
     if('All' %in% input$prop_types){
       calculate_df <- Energy_Parsed_Df %>% 
                             dplyr::select(Property_Name, `Property Type`, Address, Total_Site_Energy_Kwh, 
                                           sqft_available, sunlight_hours) %>%
                             mutate(Available_sqft_for_panels = sqft_available * (input$perc_roof_used/100),
                                    Number_of_300watt_Panels = Available_sqft_for_panels / 20.67,
                                    Kwh_potential = ((Number_of_300watt_Panels * 300)/1000) * sunlight_hours * 0.75,
                                    Cost_of_installation_gross = Number_of_300watt_Panels * 300 * input$price_p_watt)
       
       bpda_calculate_df <- land_parsed_df %>%
                             dplyr::select(address, neighborhood, type, sunlight_hours, sqft_available) %>%
                             mutate(Available_sqft_for_panels = sqft_available * (input$perc_roof_used/100),
                                    Number_of_300watt_Panels = Available_sqft_for_panels / 20.67,
                                    Kwh_potential = ((Number_of_300watt_Panels * 300)/1000) * sunlight_hours * 0.75,
                                    Cost_of_installation_gross = Number_of_300watt_Panels * 300 * input$price_p_watt)
       
       output$energy_requested <- renderInfoBox({
         infoBox("Energy Coverage Requested", (Total_energy_city_of_boston * (input$city_bost_perc/100)), icon = icon("list"), color = "purple")
       })
       output$produced_by_selection <- renderInfoBox({
         infoBox("Energy Produced", sum(calculate_df$Kwh_potential, na.rm = T) + sum(bpda_calculate_df$Kwh_potential, na.rm = T), icon = icon("list"), color = "green")
       })
       output$percentage_covered <- renderInfoBox({
         infoBox("Percent Covered", (((sum(calculate_df$Kwh_potential, na.rm = T) + sum(bpda_calculate_df$Kwh_potential, na.rm = T))/(Total_energy_city_of_boston * (input$city_bost_perc/100)))*100), icon = icon("list"), color = "blue")
       })
       output$cost_of_implementation <- renderInfoBox({
         infoBox("Cost of Implementation", sum(calculate_df$Cost_of_installation_gross, na.rm = T), icon = icon("list"), color = "orange")
       })
       
     } else if ('BPDA All' %in% input$prop_types){
       calculate_df <- Energy_Parsed_Df %>%
                               dplyr::select(Property_Name, `Property Type`, Address, Total_Site_Energy_Kwh, 
                                             sqft_available, sunlight_hours) %>%
                               subset(`Property Type` %in% input$prop_types) %>%
                               mutate(Available_sqft_for_panels = sqft_available * (input$perc_roof_used/100),
                                      Number_of_300watt_Panels = Available_sqft_for_panels / 20.67,
                                      Kwh_potential = ((Number_of_300watt_Panels * 300)/1000) * sunlight_hours * 0.75,
                                      Cost_of_installation_gross = Number_of_300watt_Panels * 300 * input$price_p_watt)
       
       bpda_calculate_df <- land_parsed_df %>%
                             dplyr::select(address, neighborhood, type, sunlight_hours, sqft_available) %>%
                             mutate(Available_sqft_for_panels = sqft_available * (input$perc_roof_used/100),
                                    Number_of_300watt_Panels = Available_sqft_for_panels / 20.67,
                                    Kwh_potential = ((Number_of_300watt_Panels * 300)/1000) * sunlight_hours * 0.75,
                                    Cost_of_installation_gross = Number_of_300watt_Panels * 300 * input$price_p_watt)
       
       output$energy_requested <- renderInfoBox({
         infoBox("Energy Coverage Requested", (Total_energy_city_of_boston * (input$city_bost_perc/100)), icon = icon("list"), color = "purple")
       })
       output$produced_by_selection <- renderInfoBox({
         infoBox("Energy Produced", sum(calculate_df$Kwh_potential, na.rm = T) + sum(bpda_calculate_df$Kwh_potential, na.rm = T), icon = icon("list"), color = "green")
       })
       output$percentage_covered <- renderInfoBox({
         infoBox("Percent Covered", (((sum(calculate_df$Kwh_potential, na.rm = T) + sum(bpda_calculate_df$Kwh_potential, na.rm = T))/(Total_energy_city_of_boston * (input$city_bost_perc/100)))*100), icon = icon("list"), color = "blue")
       })
       output$cost_of_implementation <- renderInfoBox({
         infoBox("Cost of Implementation", sum(calculate_df$Cost_of_installation_gross, na.rm = T), icon = icon("list"), color = "orange")
       })
       
     } else if ('Boston All' %in% input$prop_types){
       calculate_df <- Energy_Parsed_Df %>% 
                                 dplyr::select(Property_Name, `Property Type`, Address, Total_Site_Energy_Kwh, 
                                               sqft_available, sunlight_hours) %>%
                                 mutate(Available_sqft_for_panels = sqft_available * (input$perc_roof_used/100),
                                        Number_of_300watt_Panels = Available_sqft_for_panels / 20.67,
                                        Kwh_potential = ((Number_of_300watt_Panels * 300)/1000) * sunlight_hours * 0.75,
                                        Cost_of_installation_gross = Number_of_300watt_Panels * 300 * input$price_p_watt)
       
       bpda_calculate_df <- land_parsed_df %>%
                                 dplyr::select(address, neighborhood, type, sunlight_hours, sqft_available) %>%
                                 subset(type %in% input$prop_types) %>%
                                 mutate(Available_sqft_for_panels = sqft_available * (input$perc_roof_used/100),
                                        Number_of_300watt_Panels = Available_sqft_for_panels / 20.67,
                                        Kwh_potential = ((Number_of_300watt_Panels * 300)/1000) * sunlight_hours * 0.75,
                                        Cost_of_installation_gross = Number_of_300watt_Panels * 300 * input$price_p_watt)
       
       output$energy_requested <- renderInfoBox({
         infoBox("Energy Coverage Requested", (Total_energy_city_of_boston * (input$city_bost_perc/100)), icon = icon("list"), color = "purple")
       })
       output$produced_by_selection <- renderInfoBox({
         infoBox("Energy Produced", sum(calculate_df$Kwh_potential, na.rm = T) + sum(bpda_calculate_df$Kwh_potential, na.rm = T), icon = icon("list"), color = "green")
       })
       output$percentage_covered <- renderInfoBox({
         infoBox("Percent Covered", (((sum(calculate_df$Kwh_potential, na.rm = T) + sum(bpda_calculate_df$Kwh_potential, na.rm = T))/(Total_energy_city_of_boston * (input$city_bost_perc/100)))*100), icon = icon("list"), color = "blue")
       })
       output$cost_of_implementation <- renderInfoBox({
         infoBox("Cost of Implementation", sum(calculate_df$Cost_of_installation_gross, na.rm = T), icon = icon("list"), color = "orange")
       })
     }
   }
    
    output$scenario_table <- renderDataTable({
        calculate_df
    }, options = list(scrollX = TRUE, order = list(list(4, 'desc'))))
    
  })
  
})
  
