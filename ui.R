library(shiny)
library(shinydashboard)

dashboardPage( skin = "green",
  dashboardHeader(title = "Analyze Boston"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "dashboard", icon = icon("home")),
      menuItem("Maps", tabName = "maps", icon = icon("map")),
      menuItem("Energy", tabName = "energy", icon = icon("bolt")),
      menuItem("Savings", tabName = "savings", icon = icon("money"))
    )
  ),
  dashboardBody(
    tags$head(
    tags$link(href="https://api.tiles.mapbox.com/mapbox-gl-js/v0.36.0/mapbox-gl.css"),
    tags$style(HTML('
      .logo {
        display: block;
        margin: 0 auto;
      }
      .title {
        text-align: center;
      }
      .inline-block, .inline-block img {
        display: inline-block;
        margin: 0 20px;
      }
      #map,
      .mapboxgl-canvas { 
        width:100% !important;
        min-height: 800px;
      }
      .mapbox {
        min-height: 800px;
        padding: 0;
      }
    ')),
    tags$script(src='https://api.tiles.mapbox.com/mapbox-gl-js/v0.36.0/mapbox-gl.js'),      
    tags$script(src='main.js')
    ),
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(width = 12,
                    img(src = 'img/boston.svg', height = 200, width = 200, class="logo"),
                    h1("Analyze Boston", class="title"),
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
                    p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "maps",
              fluidRow(
                box (title = 'Maps', width = 12, class="mapbox", status = 'success', solidHeader = TRUE,
                  div(id = "map")
                )
              )
      ),
      tabItem(tabName = "energy",
              h2("Energy"),
              fluidRow(
                box(title = 'Energy', width = 12, status = 'success', solidHeader = TRUE,
                  selectInput('type', label="Property Type", choices = c('All', sort(unique(Energy_Parsed_Df$`Property Type`)))),
                  numericInput('year', label="Year Built >=", min=min(Energy_Parsed_Df$year_built), max=max(Energy_Parsed_Df$year_built), value = 1950),
                  DT::dataTableOutput('energy_table')
                )
              )
      ),
      tabItem(tabName = "savings",
        h2("Savings"),
        fluidRow(
          # A static infoBox
          infoBox("Taxes", 1 * 2, icon = icon("university")),
          # Dynamic infoBoxes
          infoBoxOutput("progressBox"),
          infoBoxOutput("approvalBox")
        ),
        
        # infoBoxes with fill=TRUE
        fluidRow(
          infoBox("Electricity", 10 * 2, icon = icon("bolt"), fill = TRUE),
          infoBoxOutput("progressBox2"),
          infoBoxOutput("approvalBox2")
        )
      )
    )
  )
)
