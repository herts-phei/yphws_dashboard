library(devtools)
library(rmarkdown)
library(pins)
library(shiny)
library(tablerDash)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinybusy)
library(bs4Dash)
library(plotly)
library(echarts4r)
library(reactable)
library(sparkline)
library(glue)
library(plyr)
library(tidyverse)
library(rmdformats)
library(shinybusy)
library(knitr)
library(ggplot2)
library(htmlwidgets)
library(webshot)
library(PHEindicatormethods)
library(crosstalk)
library(reactable)
library(rmdformats)
library(tinytex)
library(remotes)
library(viridis)
library(formattable)
library(urbnthemes)
library(glue)
library(pacman)

year <- "2021"

domains <- c("Demographics", "Living Conditions", "Diet and Lifestyle",
             "Smoking and Vaping", "Alcohol Consumption", "Drug Use",
             "Sexual Health", "Mental Health and Wellbeing", "Safety",
             "Education", "Sustainability", "COVID-19")

names(domains) <- domains

# UI ----------------------------------------------------------------

ui <- tablerDashPage(
  title = "Dashboard", 
  navbar = tablerDashNav(
    id = "nav",
    src = "img/yphws_logo_horizontal.png",
    tablerNavMenu(id = "tabs",
                  pickerInput("comp", label = "Select what to group by",
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
        tagList(
          fluidRow(
            tablerCard(width = 12, title = "Data table",
                       closable = FALSE,
                       uiOutput("exp_year"), 
                       uiOutput("exp_breakdown"),
                       uiOutput("exp_theme"),
                       uiOutput("exp_question"),
                       actionBttn(
                         inputId = "export_button",
                         label = "Update table",
                         style = "minimal",
                         color = "danger"
                       ),
                       br(),
                       downloadButton("exp_table", "Export table"),
                       br(),
                       reactableOutput("data_table")
            )
          ),
          fluidRow(
            tablerCard(title = "Export full report (COMING SOON)",
                       width = 12, 
                       closable = FALSE,
                       # uiOutput("exp_report_comp"),
                       # uiOutput("exp_report_cat"),
                       downloadButton("exp_report", "Export report")
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
  rv <- reactiveValues()
  rv$params <- get_params() # params
  rv$data <- get_data() # Raw data, cumulative yearly

  # -- Filter data to breakdown selected ----
  observe({
    
    df_selected <- rv$data$data[[input$comp]] %>% 
      mutate(value = formattable::percent(value, digits = 1),
             lowercl = formattable::percent(lowercl, digits = 1),
             uppercl = formattable::percent(uppercl, digits = 1),
             lowereb = value - lowercl,
             uppereb = uppercl - value,
             value.y = formattable::percent(value.y, digits = 1))
    
    # Stats
    rv$stats_combined <- select(df_selected, year, 1:12) %>% distinct() # distinct because of repeated diffs that are now removed. 
    rv$stats <- filter(rv$stats_combined, year == rv$params$year)
    rv$stats_old <- filter(rv$stats_combined, year == as.character(as.numeric(rv$params$year) - 1))
    
    # Differences
    rv$diffs <- filter(df_selected, year == rv$params$year)

  })
  
  # Key Points --------------------------------------------------------------
  
  key_mod_server("key",
                 params = reactive(rv$params),
                 stats = reactive(rv$stats),
                 stats_old = reactive(rv$stats_old),
                 stats_combined = reactive(rv$stats_combined),
                 q_coded = reactive(rv$data$q_coded),
                 grp_lookup = reactive(rv$data$grp_lookup),
                 comp = reactive(input$comp)
                 )
  
  # Explore data --------------------------------------------------------------------
  
  explore_mod_server("explore",
                     params = reactive(rv$params),
                     stats = reactive(rv$stats),
                     stats_old = reactive(rv$stats_old),
                     diffs = reactive(rv$diffs),
                     comp = reactive(input$comp),
                     q_coded = reactive(rv$data$q_coded),
                     grp_lookup = reactive(rv$data$grp_lookup))
  
  # Inequalities ------------------------------------------------------------

  inequalities_mod_server("ineq",
                          params = reactive(rv$params),
                          comp = reactive(input$comp), 
                          q_coded = reactive(rv$data$q_coded),
                          stats = reactive(rv$stats),
                          diffs = reactive(rv$diffs))
  
  # Export ------------------------------------------------------------------

  output$exp_report_comp <- renderUI({

    pickerInput("exp_report_comp", label = "Select what to group by in your report",
                choices = list("Sex" = "sex",
                               "Year group" = "schyear",
                               "Ethnicity" = "ethnicity",
                               "IMD Quintile" = "imd_quintile",
                               "Sexuality" = "sexuality",
                               "Young carer" = "caring",
                               "Smoker" = "smoke_ever",
                               "Self-harm" = "selfharm_ever",
                               "Bullied" = "bullied",
                               "District" = "District"),
                selected = input$comp,
                multiple = FALSE)

  })

  output$exp_report_cat <- renderUI({

    choices <- rv$data$data[[input$exp_report_comp]] %>%
      select(input$exp_report_comp) %>%
      distinct() %>%
      pull(input$exp_report_comp)

    pickerInput("exp_report_cat", "Select the category from the selected group you are most interested in:",
                choices = as.character(na.omit(choices)), multiple = FALSE,
                selected = as.character(na.omit(choices)[1]))

  })

  output$exp_report <- downloadHandler(
    filename = "report.html",
    content = function(file) {
      tempReport <- file.path(tempdir(), "test.Rmd")
      file.copy("test.Rmd", tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      # params <- list(var = input$exp_report_comp,
      #                cat = input$exp_report_cat)

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      show_modal_spinner(text = "Rendering report. Please wait, this should take 1-2 minutes.")
      rmarkdown::render(tempReport, output_file = file,
                        # params = params,
                        envir = new.env(parent = globalenv())
      )
      remove_modal_spinner() # remove it when done

    }
  )

  output$exp_year <- renderUI({
    
    shinyWidgets::awesomeCheckboxGroup(
      inputId = "exp_year",
      label = "Year:", 
      choices = unique(rv$stats_combined$year),
      selected = unique(rv$stats_combined$year),
      inline = TRUE, 
      status = "info"
    )
    
  })
  
  output$exp_breakdown <- renderUI({
    
    shinyWidgets::awesomeCheckboxGroup(
        inputId = "exp_breakdown",
        label = "Breakdown:", 
        choices = unique(rv$stats_combined$breakdown),
        selected = unique(rv$stats_combined$breakdown),
        inline = TRUE, 
        status = "info"
      )
    
  })
  
  output$exp_theme <- renderUI({
    
    pickerInput(
      inputId = "exp_theme",
      label = "Health topic:", 
      choices = na.omit(unique(rv$data$q_coded$question_theme)),
      selected = na.omit(unique(rv$data$q_coded$question_theme)),
      multiple = TRUE
    )
    
  })
  
  output$exp_question <- renderUI({
    
    filtered <- rv$data$q_coded %>% 
      mutate(across(where(is.character), ~na_if(., "NA"))) %>% 
      filter(question_theme %in% input$exp_theme, !is.na(survey_text)) %>% 
      distinct() %>% 
      pull(survey_text)
      
    pickerInput(
      inputId = "exp_question",
      label = "Question:", 
      choices = unique(filtered),
      selected = unique(filtered), 
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
      
    )
    
  })
  
  # Initial table data
  observe({
    
    rv$table_data <- rv$data$data[[input$comp]]  %>%
      left_join(select(rv$data$q_coded, question_coded, question_theme, survey_text), rv$data$q_coded, 
                by = c("question" = "question_coded")) %>% 
      select(year, breakdown, topic = question_theme, question = survey_text, 
             `question option` = question_text, response, 
             count, denominator, value, lowercl, uppercl) %>% 
      distinct() %>% 
      filter(year %in% input$exp_year,
             breakdown %in% input$exp_breakdown,
             topic %in% input$exp_theme,
             question %in% input$exp_question)
    
  })
  
  #event reactive table
  observeEvent(input$export_button, {

    rv$table_data <- rv$data$data[[input$comp]]  %>%
      left_join(select(rv$data$q_coded, question_coded, question_theme, survey_text), rv$data$q_coded,
                by = c("question" = "question_coded")) %>%
      select(year, breakdown, topic = question_theme, question = survey_text,
             `question option` = question_text, response,
             count, denominator, value, lowercl, uppercl) %>%
      distinct() %>%
      filter(year %in% input$exp_year,
             breakdown %in% input$exp_breakdown,
             topic %in% input$exp_theme,
             question %in% input$exp_question)

  })
  
  output$data_table <- renderReactable({

     rv$table_data %>% 
      reactable(filterable = TRUE, defaultPageSize = 10, 
                columns = list(
                  value = colDef(format = colFormat(percent = TRUE, digits = 1)),
                  lowercl = colDef(format = colFormat(percent = TRUE, digits = 1)),
                  uppercl = colDef(format = colFormat(percent = TRUE, digits = 1))
                ))
      
  })

  output$exp_table <- downloadHandler(

    filename = "data.csv",
    content = function(con) {

      data <- rv$table_data
      write.csv(data, con)

      }

  )
  
  # About -------------------------------------------------------------------

  about_mod_server("about")
  
}

# Run the application 
shinyApp(ui = ui, server = server)


  