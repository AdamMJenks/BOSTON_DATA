library(leaflet)
library(shiny)
library(shinydashboard)
library(shinyWidgets)

dashboardPage(skin = "green",
  dashboardHeader(title = "Boston Solar"),
  dashboardSidebar(
    sidebarMenu(
      a(img(src="img/logo.png", align="center", width="170px", class="nav_logo"), href="http://tcbanalytics.com", target="_blank"),
      menuItem("Home", tabName = "dashboard", icon = icon("home")),
      menuItem("Energy Skyline", tabName = "maps", icon = icon("map")),
      menuItem("Boston Solar Potential", tabName = "energy", icon = icon("bolt")),
      menuItem("Scenario Planner", tabName = "scenario", icon = icon("money")),
      menuItem("About", tabName='about', icon=icon("question"))
    )
  ),
  dashboardBody(
    tags$head(
    tags$link(href="https://api.tiles.mapbox.com/mapbox-gl-js/v0.36.0/mapbox-gl.css", rel="stylesheet"),
    tags$link(href="main.css", rel="stylesheet"),
    tags$script(src='https://api.tiles.mapbox.com/mapbox-gl-js/v0.36.0/mapbox-gl.js'),      
    tags$script(src='main.js')
    ),
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(width = 12,
                    img(src = 'img/boston.svg', height = 200, width = 200, class="city_logo"),
                    #img(src = 'img/logo.png', height = 200, width = 200, style="background-color: #5cb85c; padding: 30px;", class="logo"),
                    h1("Beantown Solar: The Future Never Looked So Bright", class="title"),
                    h2("Project: Track 1"),
                    p("Reducing Boston’s Carbon Footprint: Participants will use the Boston Energy Reporting and Disclosure Ordinance (BERDO), City of Boston Utility Billing Data, City of Boston Real-Time Energy Metering (coming soon!), and other relevant datasets from Analyze Boston to find new ways to promote energy efficiency and reduce greenhouse gas emissions in Boston. You might (for example) analyze the efficiency of individual buildings relative to their characteristics and usage, develop a new map to highlight Boston's greenest buildings, or create graphs of usage over time to see how Boston's demand for energy is evolving over time."),
                    h2("Team:"),
                    div( class="",
                      div( class="inline-block text-center",
                        img(src = 'img/pat.jpg', width = 200, height = 200, class="img-responsive img-circle"),
                        p("Patrick Brophy"),
                        p("Front End Developer at John Hancock")
                      ),
                      div( class="inline-block text-center",
                           img(src = 'img/tanya.jpg', width = 200, height = 200, class="img-responsive img-circle"),
                           p("Tanya Cashorali"),
                           p("Data Analytics Consultant at TCB Analytics")
                      ),
                      div( class="inline-block text-center",
                           img(src = 'img/adam.jpg', width = 200, height = 200, class="img-responsive img-circle"),
                           p("Adam Jenkins"),
                           p("Senior Data Scientist at Biogen")
                      ),
                      div( class="inline-block text-center",
                           img(src = 'img/jimmy.jpg' , width = 200, height = 200, class="img-responsive img-circle"),
                           p("James Mullen"),
                           p("Client Services Manager, Basho Technologies")
                      )
                    ),
                    h2("Description:"),
                    p("For this track, our team decided to help the City of Boston and others begin to comprehend the extent to which we can leverage
                      the square footage available on top of our buildings to offset our energy usage via solar power. Utilizing the BERDO and BPDA 
                      asset from Boston, along with Google Sunroof, we have produced advanced visualizations of Boston and a scenario planner
                      for future planning usage.")
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "maps",
              fluidRow(
                box (title = 'Maps', width = 12, class="mapbox", status = 'success', solidHeader = TRUE,
                  div(id = "map"),
                  div(class="solarLegend",
                    div(class="solarScale",
                        span(class="solarhigh", "High Solar Benefit"),
                        span(class="solarlow", "Low Solar Benefit")
                    )
                  )
                )
              ),
              fluidRow(
                box (title = "Legend", width = 12, status = 'success', solidHeader = TRUE,

                    div(
                        p("The skyline created by this visualization is not the Boston skyline you know."),
                        p("Instead, the heights of the buildings you see here are determined by how much energy they use: taller buildings use more Kwh of electricity."),
                        p("The colors of the buildings is a representation of how valuable that site's money could be if invested in solar energy.  For each dollar invested, what percentage of its electricity bill would be covered?  The greener the building, the higher the percentage."),
                        p("The tallest and greenest buildings stand to benefit the most from an investment in solar power."),
                        p("This solar skyline may not be what you see when you look out at Boston's buildings - but it will show you who can benefit the most by looking into the sky.")
                    )
                )
              )
      ),
      tabItem(tabName = "energy",
              h2("Energy"),
              fluidRow(
                box(title = 'Energy', width = 12, status = 'success', solidHeader = TRUE,
                  column(width = 6,
                    selectInput('type', label="Property Type", choices = c('All', sort(unique(Energy_Parsed_Df$`Property Type`)))),
                    numericInput('year', label="Year Built >=", min=min(Energy_Parsed_Df$year_built), max=max(Energy_Parsed_Df$year_built), value = 1900)
                  ),
                  column(width = 6,
                    numericInput('sqft_min', label = 'Minimum Sqft.',min=min(Energy_Parsed_Df$sqft_available, na.rm = T), max = max(Energy_Parsed_Df$sqft_available, na.rm = T), value = median(Energy_Parsed_Df$sqft_available)),
                    numericInput('kwh_min', label = 'Kwh Potential Minimum',min=min(Energy_Parsed_Df$Kwh_potential, na.rm = T), max = max(Energy_Parsed_Df$Kwh_potential, na.rm = T), value = median(Energy_Parsed_Df$Kwh_potential))
                  ),
                  box(width = 12,
                  tabsetPanel(
                    tabPanel('Table', HTML('<br>'), DT::dataTableOutput('energy_table')),
                    tabPanel('City Map',leafletOutput("mymap"))
                  )
                  )
                ))
              
      ),
      tabItem(tabName = "scenario",
        h2("Scenario Planner"),
        fluidRow(
          box(
              title = 'Input Values For Scenario', width = 12, status = 'success', solidHeader = TRUE,
              column(width = 6,
                  radioButtons('land_building', "Which Buildings to Optimize For:", choices=c("Boston Buildings", "BPDA Owned Land", 'Both')),
                  uiOutput("scenario_lists")
                  
              ),
              column(width = 6,
                  numericInput(inputId = 'perc_roof_used', label = 'Percentage Roof Usable',
                               min = 0, max = 100, value = 66, width = 250),
                  numericInput(inputId = "city_bost_perc", label = "Percentage of Electricity to Cover", 
                               min = 0, max = 100, value = 50, width = 250),
                  numericInput(inputId = "price_p_watt", label = 'Price per watt ($)',
                               min = 0, max = 5, value = 4.20, width = 250)
                  ),
              fluidRow(
                column(width = 4),
                column(width = 4, actionButton("compute_button", "Compute Scenario!"))
              ),
              tabsetPanel(
                tabPanel('Overview',
                         h2('Thank you for trying our scenario analysis tool!'),
                         HTML('<br>'),
                         h4('Here is some information that you may find useful. For each scenario you can choose whether to only use
                            buildings that are listed in Analyze Boston"s BERDO file or you can also include supplemental energy production
                            from BPDA assets. As well, you can mix and match what property types you want from one or the other.'),
                         HTML('<br>'),
                         h4('For additional metrics, you can choose what percentage of each roof that is usable according to Google Sunroof 
                            that can be used for solar.  Think: how much space do we need to remove for people to walk around, etc.  You
                            can also choose how much of energy of Boston you want to try and cover by solar and what the cost-per-watt of 
                            Solar is for your project!'),
                         HTML('<br>'),
                         h4('After you click the "Compute Scenario" button, the results tab will update with how much energy coverage is 
                            requested (in kilowatt hours), how much energy is produced by the solar scenario selected (in kilowatt hours),
                            the percentage that solar will cover of the amount requested, and lastly how much it would cost.  The "Scenario
                            Selected From" tab includes what BERDO file buildings were selected. If BPDA only is selected, this 
                            final tab will remain empty as the BPDA parcels are not included in the greater data, only as supplemental solar.'),
                         h4('ENJOY!')
                         ),
                tabPanel('Results',
                             infoBoxOutput("energy_requested"),
                             infoBoxOutput("produced_by_selection"),
                             infoBoxOutput("percentage_covered"),
                             infoBoxOutput("cost_of_implementation")
                         ),
                tabPanel('Scenario Selected From:',DT::dataTableOutput('scenario_table'))
              )
            )
          )
        ),
      tabItem(tabName = "about",
              fluidRow(
                box (title = 'About', width = 12, status = 'success', solidHeader = TRUE,
                     p("This application uses data from the following sources:"),
                     HTML("<ul><li><a href = 'https://data.boston.gov/dataset/building-energy-reporting-and-disclosure-ordinance'>Building Energy Reporting And Disclosure Ordinance (BERDO)</a></li>
                          <li><a href = 'https://www.google.com/get/sunroof'>Google Project Sunroof</a></li>
                          <li><a href = 'http://www.bostonplans.org/work-with-us/bpda-owned-land?viewall=1'>Boston Planning and Development Agency</a></li></ul>")
                )
              )
      )
    )
  )
)
  
