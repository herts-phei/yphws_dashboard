library(shiny)
library(shinyWidgets)
library(tablerDash)
library(bs4Dash)
library(dplyr)
library(echarts4r)
library(formattable)
library(ggplot2)
library(glue)
library(htmlwidgets)
library(knitr)
library(plotly)
library(purrr)
library(reactable)
library(rmarkdown)
library(rmdformats)
library(stringr)
library(sparkline)
library(viridis)

# UI ----------------------------------------------------------------

ui <- tablerDash::tablerDashPage(
  title = "Dashboard", 
  navbar = tablerDash::tablerDashNav(
    id = "nav",
    src = "img/yphws_logo_horizontal.png",
    tablerDash::tablerNavMenu(id = "tabs",
                              tags$head(shiny::includeScript("navAppend.js")),
                              #tags$head(shiny::includeHTML("google-analytics.html")),
                              shinyWidgets::pickerInput("year", label = "Year:", width = "100px", 
                                                        choices = list("2020" = "2020", 
                                                                       "2021" = "2021",
                                                                       "2022" = "2022"), 
                                                        selected = "2022", multiple = FALSE),
                              HTML('&nbsp;'),
                              shinyWidgets::pickerInput("comp", label = "Select what to group by:", width = "170px", 
                                                        choices = list("Gender" = "sex", 
                                                                       "Year group" = "schyear", 
                                                                       "Ethnicity" = "ethnicity",
                                                                       "IMD Quintile" = "imd_quintile",
                                                                       "Sexuality" = "sexuality", 
                                                                       "Young carer" = "caring", 
                                                                       #"Smoker" = "smoke_ever",
                                                                       #"Self-harm" = "selfharm_ever",
                                                                       "Bullied" = "bullied",
                                                                       "District" = "District"), 
                                                        selected = "sex", multiple = FALSE), 
                              tablerDash::tablerNavMenuItem(
                                "Key Points",
                                tabName = "KeyPoints"
                              ),
                              tablerDash::tablerNavMenuItem(
                                "Explore Data",
                                tabName = "ExploreData"
                              ),
                              tablerDash::tablerNavMenuItem(
                                "Inequalities",
                                tabName = "Inequalities"
                              ),
                              tablerDash::tablerNavMenuItem(
                                "Export",
                                tabName = "Export"
                              ),
                              tablerDash::tablerNavMenuItem(
                                "About",
                                tabName = "About"
                              )
    )
  ),
  body = tablerDash::tablerDashBody(
    tablerDash::tablerTabItems(
      key_mod("key"),
      explore_mod("explore"),
      inequalities_mod("ineq"),
      export_mod("export"),
      about_mod("about")
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
  rv <- shiny::reactiveValues()
  rv$params <- get_params() # params
  rv$data <- get_data() # Raw data, cumulative yearly
  
  # -- Filter data to breakdown selected ----
  shiny::observe({
    
    df_selected <- rv$data$data[[input$comp]] %>% 
      dplyr::mutate(value = formattable::percent(value, digits = 1),
                    lowercl = formattable::percent(lowercl, digits = 1),
                    uppercl = formattable::percent(uppercl, digits = 1),
                    lowereb = value - lowercl,
                    uppereb = uppercl - value)
    
    # Stats
    rv$stats_combined <- dplyr::select(df_selected, year, 1:12) %>% dplyr::distinct() # distinct because of repeated diffs that are now removed. 
    rv$stats <- dplyr::filter(rv$stats_combined, year == input$year)
    rv$stats_old <- dplyr::filter(rv$stats_combined, year == as.character(as.numeric(input$year) - 1))
    
    #TODO
    # Differences
    rv$diffs <- dplyr::filter(df_selected, year == input$year)
    rv$diffs_all <- df_selected
    
  })
  
  # Key Points --------------------------------------------------------------
  
  key_mod_server("key",
                 params = shiny::reactive(rv$params),
                 year = shiny::reactive(input$year),
                 stats = shiny::reactive(rv$stats),
                 stats_old = shiny::reactive(rv$stats_old),
                 stats_combined = shiny::reactive(rv$stats_combined),
                 q_coded = shiny::reactive(rv$data$q_coded),
                 grp_lookup = shiny::reactive(rv$data$grp_lookup),
                 comp = shiny::reactive(input$comp)
  )

  
  # Explore data --------------------------------------------------------------------
  
  explore_mod_server("explore",
                     params = shiny::reactive(rv$params),
                     year = shiny::reactive(input$year), 
                     stats = shiny::reactive(rv$stats),
                     stats_old = shiny::reactive(rv$stats_old),
                     diffs = shiny::reactive(rv$diffs),
                     comp = shiny::reactive(input$comp),
                     q_coded = shiny::reactive(rv$data$q_coded),
                     grp_lookup = shiny::reactive(rv$data$grp_lookup))
  
  # Inequalities ------------------------------------------------------------
  
  inequalities_mod_server("ineq",
                          params = shiny::reactive(rv$params),
                          year = shiny::reactive(input$year),
                          comp = shiny::reactive(input$comp), 
                          q_coded = shiny::reactive(rv$data$q_coded),
                          stats = shiny::reactive(rv$stats)
                          #diffs = shiny::reactive(rv$diffs)
                          )
  
  # Export ------------------------------------------------------------------
  
  export_mod_server(id = "export",
                    params = shiny::reactive(rv$params),
                    data = shiny::reactive(rv$data$data),
                    stats_combined = shiny::reactive(rv$stats_combined),
                    q_coded = shiny::reactive(rv$data$q_coded),
                    comp = shiny::reactive(input$comp))
  
  # About -------------------------------------------------------------------
  
  about_mod_server("about")
  
  # Send Feedback -----------------------------------------------------------
  
  output$feedback_link <- shiny::renderUI({
    shiny::tagList(a("Feedback", href="https://surveys.hertfordshire.gov.uk/s/YPHWS_Evaluation/", target="_blank"))
  })
  
}

# Run the application 
shiny::shinyApp(ui = ui, server = server)
