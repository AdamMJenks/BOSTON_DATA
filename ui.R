library(leaflet)
library(shiny)
library(shinydashboard)

dashboardPage(skin = "green",
  dashboardHeader(title = "Analyze Boston"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "dashboard", icon = icon("home")),
      menuItem("Maps", tabName = "maps", icon = icon("map")),
      menuItem("Energy", tabName = "energy", icon = icon("bolt")),
      menuItem("Scenario Planner", tabName = "scenario", icon = icon("money"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
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
    '))),
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
                box (title = 'Maps', width = 12, status = 'success', solidHeader = TRUE,
                  tags$iframe(src="https://www.google.com/maps/embed?pb=!1m18!1m12!1m3!1d94411.77160300482!2d-71.1273685766368!3d42.31335203482073!2m3!1f0!2f0!3f0!3m2!1i1024!2i768!4f13.1!3m3!1m2!1s0x89e3652d0d3d311b%3A0x787cbf240162e8a0!2sBoston%2C+MA!5e0!3m2!1sen!2sus!4v1492800915327", width="100%", height="800", frameborder="0", style="border:0")    
                )
              )
      ),
      tabItem(tabName = "energy",
              h2("Energy"),
              fluidRow(
                box(title = 'Energy', width = 12, status = 'success', solidHeader = TRUE,
                  selectInput('type', label="Property Type", choices = c('All', sort(unique(Energy_Parsed_Df$`Property Type`)))),
                  numericInput('year', label="Year Built >=", min=min(Energy_Parsed_Df$year_built), max=max(Energy_Parsed_Df$year_built), value = 1900),
                  DT::dataTableOutput('energy_table')
                ),
                leafletOutput("mymap")
              )
      ),
      tabItem(tabName = "scenario",
        h2("Scenario Planner"),
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
