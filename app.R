## IN DEVELOPMENT. 

library(tidyverse)
library(pins)
library(echarts4r)
library(reactable)
library(shiny)
library(tablerDash)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinybusy)
library(bs4Dash)

board_register("rsconnect",
               server = "srv-gcp-ms-connect:3939",
               key = Sys.getenv("CONNECT_API_KEY"))

domains <- c("Living Conditions", "Diet and Lifestyle",     
             "Education", "Demographics",
             "Mental Health and Wellbeing", "Smoking and Vaping",         
             "Alcohol Consumption", "Drug Use",                   
             "Sexual Health", "Safety",                  
             "Sustainability", "COVID-19")
names(domains) <- domains

# UI ----------------------------------------------------------------

ui <- tablerDashPage(
  title = "Dashboard", 
  navbar = tablerDashNav(
    id = "nav",
    src = "img/yphws_logo.png",
    tablerNavMenu(id = "tabs",
                  pickerInput("comp", label = "Select what to group by",
                              choices = list("Sex" = "sex", 
                                             "Year group" = "schyear", 
                                             "Ethnicity" = "ethnicity",
                                             "IMD Quintile" = "imd_quintile",
                                             "Sexuality" = "sexuality", 
                                             "Child looked after" = "cla",
                                             "Young carer" = "caring", 
                                             "Adopted" = "adopted", 
                                             "Smoker" = "smoke_ever",
                                             "Self-harm" = "selfharm_ever",
                                             "Bullied" = "bullied",
                                             "District" = "District"), 
                              selected = "sex", multiple = FALSE), 
                  tablerNavMenuItem(
                    "Key Points",
                    tabName = "KeyPoints"
                  ),
                  tablerNavMenuItem(
                    "Explore Data",
                    tabName = "ExploreData"
                  ),
                  tablerNavMenuItem(
                    "Inequalities",
                    tabName = "Inequalities"
                  ),
                  tablerNavMenuItem(
                    "Export",
                    tabName = "Export"
                  ),
                  tablerNavMenuItem(
                    "About",
                    tabName = "About"
                  )
                  
                  
    )
  ),
  body = tablerDashBody(
    tablerTabItems(
      key_mod("key"),
      explore_mod("explore"),
      inequalities_mod("ineq"),
      tablerTabItem(
        tabName = "Export",
        tablerCard(width = 12, title = "Data table",
                   reactableOutput("export")
        )
        
      ),
      tablerTabItem(
        tabName = "About"
      )
    )
  )
)




# Server ------------------------------------------------------------------

server <- function(input, output) {
  
  # Uncomment for testing
  # observe({
  #   
  #   if ("ethnicity" %in% input$comp) { browser() }
  #   
  # })
  
  # --Load all data-----
  rv <- reactiveValues()
  rv$params <- get_params() # params
  rv$data <- get_data(params = isolate(rv$params), data = isolate(params$data_pin)) # Raw data
  rv$data_old <- get_data(params = isolate(rv$params), data = isolate(params$prev_data_pin)) # Raw data, previous. 
  
  # --Generate statistics using raw data
  stats <- reactive({
    get_stats(
      data = rv$data$data,
      group = input$comp,
      q_coded = rv$data$q_coded
    )
  })
  
  diffs <- reactive({
    sel_comp <- rv$data$data %>% 
      select(input$comp) 
    sel_comp <- unique(na.omit(unlist(sel_comp)))
    
    get_stats_diffs(stats = stats(), 
                    levels = c("All Responses", sel_comp), 
                    compare_to_all = F) # TODO
  })
  
  # Key Points --------------------------------------------------------------
  
  key_mod_server("key", 
                 stats = stats(),
                 comp = input$comp)
  
  # Explore data --------------------------------------------------------------------

  explore_mod_server("explore",
                     stats = stats(),
                     diffs = diffs(),
                     comp = input$comp,
                     q_coded = rv$data$q_coded)
  
  # Inequalities ------------------------------------------------------------
  
  inequalities_mod_server("ineq",
                          params = rv$params,
                          q_coded = rv$data$q_coded,
                          diffs = diffs())
  
  # Export ------------------------------------------------------------------
  
  output$export <- renderReactable({
    
    reactable(stats())
    
  })
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)
