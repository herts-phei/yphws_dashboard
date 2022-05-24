library(bs4Dash)
library(crosstalk)
library(dplyr)
library(echarts4r)
library(formattable)
library(ggplot2)
library(glue)
library(htmlwidgets)
library(knitr)
library(PHEindicatormethods)
library(pins)
library(plotly)
library(plyr)
library(purrr)
library(reactable)
library(rlang)
library(rmarkdown)
library(rmdformats)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(stringr)
library(sparkline)
library(tablerDash)
library(tinytex)
library(viridis)
library(webshot)

year <- "2021"

domains <- c("Demographics", "Living Conditions", "Diet and Lifestyle",
             "Smoking and Vaping", "Alcohol Consumption", "Drug Use",
             "Sexual Health", "Mental Health and Wellbeing", "Safety",
             "Education", "Sustainability", "COVID-19")

names(domains) <- domains

# UI ----------------------------------------------------------------

ui <- tablerDash::tablerDashPage(
  title = "Dashboard", 
  navbar = tablerDash::tablerDashNav(
    id = "nav",
    src = "img/yphws_logo_horizontal.png",
    tablerDash::tablerNavMenu(id = "tabs",
                              tags$head(shiny::includeScript("navAppend.js")),
                              tags$head(shiny::includeHTML("google-analytics.html")),
                              shinyWidgets::pickerInput("comp", label = "Select what to group by:", width = "180px", 
                                                        choices = list("Sex" = "sex", 
                                                                       "Year group" = "schyear", 
                                                                       "Ethnicity" = "ethnicity",
                                                                       "IMD Quintile" = "imd_quintile",
                                                                       "Sexuality" = "sexuality", 
                                                                       "Young carer" = "caring", 
                                                                       #"Smoker" = "smoke_ever",
                                                                       "Self-harm" = "selfharm_ever",
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
                              )#, 
                              #tablerNavMenuItem(
                              #  uiOutput("feedback_link")
                              #)
    )
  ),
  body = tablerDash::tablerDashBody(
    tablerDash::tablerTabItems(
      key_mod("key"),
      explore_mod("explore"),
      inequalities_mod("ineq"),
      tablerDash::tablerTabItem(
        tabName = "Export",
        shiny::tagList(
          shiny::fluidRow(
            tablerDash::tablerCard(width = 12, title = "Data table", 
                                   closable = FALSE,
                                   shiny::uiOutput("exp_year"), 
                                   shiny::uiOutput("exp_breakdown"),
                                   shiny::uiOutput("exp_theme"),
                                   shiny::uiOutput("exp_question"),
                                   shiny::br(),
                                   downloadButton("exp_table", "Export table"),
                                   shiny::br(),
                                   reactableOutput("data_table")
            )
          ),
          shiny::fluidRow(
            tablerDash::tablerCard(title = "Export full report",
                                   width = 12, 
                                   closable = FALSE,
                                   shiny::uiOutput("exp_report_comp"),
                                   shiny::uiOutput("exp_report_cat"),
                                   shiny::downloadButton("exp_report", "Export report")
            )
          )
        ),
        
      ),
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
                    uppereb = uppercl - value,
                    value.y = formattable::percent(value.y, digits = 1))
    
    # Stats
    rv$stats_combined <- dplyr::select(df_selected, year, 1:12) %>% dplyr::distinct() # distinct because of repeated diffs that are now removed. 
    rv$stats <- dplyr::filter(rv$stats_combined, year == rv$params$year)
    rv$stats_old <- dplyr::filter(rv$stats_combined, year == as.character(as.numeric(rv$params$year) - 1))
    
    # Differences
    rv$diffs <- dplyr::filter(df_selected, year == rv$params$year)
    
  })
  
  # Key Points --------------------------------------------------------------
  
  key_mod_server("key",
                 params = shiny::reactive(rv$params),
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
                     stats = shiny::reactive(rv$stats),
                     stats_old = shiny::reactive(rv$stats_old),
                     diffs = shiny::reactive(rv$diffs),
                     comp = shiny::reactive(input$comp),
                     q_coded = shiny::reactive(rv$data$q_coded),
                     grp_lookup = shiny::reactive(rv$data$grp_lookup))
  
  # Inequalities ------------------------------------------------------------
  
  inequalities_mod_server("ineq",
                          params = shiny::reactive(rv$params),
                          comp = shiny::reactive(input$comp), 
                          q_coded = shiny::reactive(rv$data$q_coded),
                          stats = shiny::reactive(rv$stats),
                          diffs = shiny::reactive(rv$diffs))
  
  # Export ------------------------------------------------------------------
  
  output$exp_report_comp <- shiny::renderUI({
    
    shinyWidgets::pickerInput("exp_report_comp", label = "Select what to group by in your report",
                              choices = list("Sex" = "sex",
                                             "Year group" = "schyear",
                                             "Ethnicity" = "ethnicity",
                                             "IMD Quintile" = "imd_quintile",
                                             "Sexuality" = "sexuality",
                                             "Young carer" = "caring",
                                             #"Smoker" = "smoke_ever",
                                             "Self-harm" = "selfharm_ever",
                                             "Bullied" = "bullied",
                                             "District" = "District"),
                              selected = input$comp,
                              multiple = FALSE)
    
  })
  
  output$exp_report_cat <- shiny::renderUI({
    
    # choices <- rv$data$data[[input$exp_report_comp]] %>%
    #   select(input$exp_report_comp) %>%
    #   distinct() %>%
    #   pull(input$exp_report_comp)
    
    choices <- unique(rv$data$data[[input$exp_report_comp]]$breakdown) 
    
    choices <- choices[choices != "All Responses"]
    
    
    shinyWidgets::pickerInput("exp_report_cat", "Select the category from the selected group you are most interested in:",
                              choices = as.character(na.omit(choices)), multiple = FALSE,
                              selected = as.character(na.omit(choices)[1]))
    
  })
  
  
  output$exp_report <- shiny::downloadHandler(
    
    filename = function() {
      paste0("Hertfordshire YPHWS Report - ", input$exp_report_comp, " focusing on ", input$exp_report_cat, ".html")
    },
    
    content = function(file) {
      
      shiny::withProgress(message = "Producing the report. This can take some time...", {
        
        src <- normalizePath('report.Rmd')
        
        # temporarily switch to the temp dir, in case you do not have write permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, 'report.Rmd', overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(var = input$exp_report_comp,
                       cat = input$exp_report_cat,
                       rendered_by_shiny = TRUE)
        
        
        out <- rmarkdown::render('report.Rmd', params = params, envir = new.env())
        
        file.rename(out, file)
        
      })
    }
  )
  
  output$exp_year <- shiny::renderUI({
    
    shinyWidgets::awesomeCheckboxGroup(
      inputId = "exp_year",
      label = "Year:", 
      choices = unique(rv$stats_combined$year),
      selected = unique(rv$stats_combined$year),
      inline = TRUE, 
      status = "info"
    )
    
  })
  
  output$exp_breakdown <- shiny::renderUI({
    
    shinyWidgets::awesomeCheckboxGroup(
      inputId = "exp_breakdown",
      label = "Breakdown:", 
      choices = unique(rv$stats_combined$breakdown),
      selected = unique(rv$stats_combined$breakdown),
      inline = TRUE, 
      status = "info"
    )
    
  })
  
  output$exp_theme <- shiny::renderUI({
    
    shinyWidgets::pickerInput(
      inputId = "exp_theme",
      label = "Health topic:", 
      choices = na.omit(unique(rv$data$q_coded$question_theme)),
      selected = na.omit(unique(rv$data$q_coded$question_theme)),
      multiple = TRUE
    )
    
  })
  
  output$exp_question <- shiny::renderUI({
    
    filtered <- rv$data$q_coded %>% 
      dplyr::mutate(dplyr::across(where(is.character), ~dplyr::na_if(., "NA"))) %>% 
      dplyr::filter(question_theme %in% input$exp_theme, !is.na(survey_text)) %>% 
      dplyr::distinct() %>% 
      dplyr::pull(survey_text)
    
    shinyWidgets::pickerInput(
      inputId = "exp_question",
      label = "Question:", 
      choices = unique(filtered),
      selected = unique(filtered), 
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
      
    )
    
  })
  
  # Initial table data
  shiny::observe({
    
    rv$table_data <- rv$data$data[[input$comp]]  %>%
      dplyr::left_join(dplyr::select(rv$data$q_coded, question_coded, question_theme, survey_text), rv$data$q_coded, 
                       by = c("question" = "question_coded")) %>% 
      dplyr::select(year, breakdown, topic = question_theme, question = survey_text, 
                    `question option` = question_text, response, 
                    count, denominator, value, lowercl, uppercl) %>% 
      dplyr::distinct() %>% 
      dplyr::filter(year %in% input$exp_year,
                    breakdown %in% input$exp_breakdown,
                    topic %in% input$exp_theme,
                    question %in% input$exp_question)
    
  })
  
  #event reactive table
  shiny::observeEvent(input$export_button, {
    
    rv$table_data <- rv$data$data[[input$comp]]  %>%
      dplyr::left_join(dplyr::select(rv$data$q_coded, question_coded, question_theme, survey_text), rv$data$q_coded,
                       by = c("question" = "question_coded")) %>%
      dplyr::select(year, breakdown, topic = question_theme, question = survey_text,
                    `question option` = question_text, response,
                    count, denominator, value, lowercl, uppercl) %>%
      dplyr::distinct() %>%
      dplyr::filter(year %in% input$exp_year,
                    breakdown %in% input$exp_breakdown,
                    topic %in% input$exp_theme,
                    question %in% input$exp_question)
    
  })
  
  output$data_table <- reactable::renderReactable({
    
    rv$table_data %>% 
      reactable::reactable(filterable = TRUE, defaultPageSize = 10, 
                           columns = list(
                             value = reactable::colDef(format = reactable::colFormat(percent = TRUE, digits = 1)),
                             lowercl = reactable::colDef(format = reactable::colFormat(percent = TRUE, digits = 1)),
                             uppercl = reactable::colDef(format = reactable::colFormat(percent = TRUE, digits = 1))
                           ))
    
  })
  
  output$exp_table <- shiny::downloadHandler(
    
    filename = "data.csv",
    content = function(con) {
      
      data <- rv$table_data
      write.csv(data, con)
      
    }
    
  )
  
  # About -------------------------------------------------------------------
  
  about_mod_server("about")
  
  # Send Feedback -----------------------------------------------------------
  
  output$feedback_link <- shiny::renderUI({
    shiny::tagList(a("Feedback", href="https://surveys.hertfordshire.gov.uk/s/YPHWS_Evaluation/", target="_blank"))
  })
  
}

# Run the application 
shiny::shinyApp(ui = ui, server = server)
