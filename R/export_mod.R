# UI ----------------------------------------------------------------------

export_mod <- function(id,
                       name = "Export") {
  
  ns <- NS(id)
  
  tablerTabItem(
    tabName = "Export",
    tagList(
      shiny::fluidRow(
        tablerDash::tablerCard(width = 12, title = "Data table", 
                               closable = FALSE,
                               uiOutput(ns("exp_year")), 
                               uiOutput(ns("exp_breakdown")),
                               uiOutput(ns("exp_theme")),
                               uiOutput(ns("exp_question")),
                               br(),
                               downloadButton(ns("exp_table"), "Export table"),
                               br(),
                               reactableOutput(ns("data_table"))
        )
      ),
      shiny::fluidRow(
        tablerDash::tablerCard(title = "Export custom full report",
                               width = 12, 
                               closable = FALSE,
                               # shiny::uiOutput(ns("exp_report_comp")),
                               # shiny::uiOutput(ns("exp_report_cat")),
                               shiny::uiOutput(ns("text"))
                               #shiny::uiOutput(ns("exp_report_year")),
                               #TODO
                               #shiny::downloadButton(ns("exp_report"), "Export report")
                               
                               
        )
      )
    )
  )
  
}

# Server ------------------------------------------------------------------

export_mod_server <- function(id,
                              params,
                              data, 
                              stats_combined,
                              q_coded, 
                              comp) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- NS(id)
      
      # Table export ------------------------------------------------------------
      
      output$exp_year <- renderUI({
        
        stats_combined <- stats_combined()
        
        shinyWidgets::awesomeCheckboxGroup(
          inputId = ns("exp_year"),
          label = "Year:", 
          choices = unique(stats_combined$year),
          selected = unique(stats_combined$year),
          inline = TRUE, 
          status = "info"
        )
        
      })
      
      output$exp_breakdown <- renderUI({
        
        stats_combined <- stats_combined()
        
        shinyWidgets::awesomeCheckboxGroup(
          inputId = ns("exp_breakdown"),
          label = "Breakdown:", 
          choices = unique(stats_combined$breakdown),
          selected = unique(stats_combined$breakdown),
          inline = TRUE, 
          status = "info"
        )
        
      })
      
      output$exp_theme <- renderUI({
        
        q_coded <- q_coded()
        
        pickerInput(
          inputId = ns("exp_theme"),
          label = "Health topic:", 
          choices = na.omit(unique(q_coded$question_theme)),
          selected = na.omit(unique(q_coded$question_theme)),
          multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE,
            liveSearch = TRUE),
        )
        
      })
      
      output$exp_question <- renderUI({
        
        q_coded <- q_coded()
        exp_theme <- input$exp_theme
        
        filtered <- q_coded %>% 
          mutate(across(where(is.character), ~na_if(., "NA"))) %>% 
          filter(question_theme %in% exp_theme, !is.na(survey_text)) %>% #
          distinct() %>% 
          pull(survey_text)
        
        pickerInput(
          inputId = ns("exp_question"),
          label = "Question:", 
          choices = unique(filtered),
          selected = unique(filtered), 
          multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE,
            liveSearch = TRUE),
          
        )
        
      })
      
      # Initial table data
      table_data <- reactive({
        
        data <- data()
        comp <- comp()
        q_coded <- q_coded()
        exp_year <- input$exp_year
        exp_breakdown <- input$exp_breakdown
        exp_theme <- input$exp_theme
        exp_question <- input$exp_question
        
        data[[comp]]  %>%
          left_join(select(q_coded, question_coded, question_theme, survey_text), q_coded, 
                    by = c("question" = "question_coded")) %>% 
          select(year, breakdown, topic = question_theme, question = survey_text, 
                 `question option` = question_text, response, 
                 count, denominator, value, lowercl, uppercl) %>% 
          distinct() %>% 
          filter(year %in% exp_year,
                 breakdown %in% exp_breakdown,
                 topic %in% exp_theme,
                 question %in% exp_question) 
        
      })
      
      # render reactable
      output$data_table <- renderReactable({
        
        table_data() %>% 
          reactable(filterable = TRUE, defaultPageSize = 10, 
                    columns = list(
                      value = colDef(format = colFormat(percent = TRUE, digits = 1)),
                      lowercl = colDef(format = colFormat(percent = TRUE, digits = 1)),
                      uppercl = colDef(format = colFormat(percent = TRUE, digits = 1))
                    ))
        
      })
      
      # download handler
      output$exp_table <- downloadHandler(
        
        filename = "data.csv",
        content = function(con) {
          
          # data <- output$data_table
          data <- table_data()
          write.csv(data, con)
          
        }
        
      ) 
      
      # Report export -----------------------------------------------------------
      
      output$exp_report_comp <- renderUI({
        
        comp <- comp()
        
        pickerInput(
          inputId = ns("exp_report_comp"), 
          label = "Select what to group by in your report:",
          choices = list("Gender" = "sex",
                         "Year group" = "schyear",
                         "Ethnicity" = "ethnicity",
                         "IMD Quintile" = "imd_quintile",
                         "Sexuality" = "sexuality",
                         "Young carer" = "caring",
                         "Bullied" = "bullied",
                         "District" = "District"),
          selected = comp,
          options = pickerOptions(
            liveSearch = TRUE),
          multiple = FALSE)
        
      })
      
      output$exp_report_cat <- shiny::renderUI({
        
        data <- data()
        
        choices <- unique(data[[input$exp_report_comp]]$breakdown) 
        choices <- choices[choices != "All Responses" & choices != "Non-white"]
        
        
        shinyWidgets::pickerInput(ns("exp_report_cat"), "Select the category from the selected group you are most interested in:",
                                  choices = as.character(na.omit(choices)), multiple = FALSE,
                                  options = pickerOptions(
                                    liveSearch = TRUE),
                                  selected = as.character(na.omit(choices)[1]))
        
      })
      
      # output$exp_report_year <- shiny::renderUI({
      #   
      #   stats_combined <- stats_combined()
      #   
      #   shinyWidgets::pickerInput(
      #     inputId = ns("exp_report_year"),
      #     label = "Select the year of interest:", 
      #     choices = c("2021", "2020"),
      #     selected = unique(stats_combined$year)[1],
      #     options = pickerOptions(
      #       liveSearch = TRUE),
      #     multiple = FALSE
      #   )
      #   
      # })
      
      output$text <- shiny::renderText({
        
        paste0("The 2023 data is currently being processed so that it can be used for customised reports.",
               " Customised reports allow you to export a full report with all indicators broken down by categories of your choosing (e.g. Those who have been bullied vs. not bullied).",
               " If you would like to be notified on when this functionality will available to use, please email YPHWS@hertfordshire.gov.uk")
        
      })
      
      
      # download handler
      output$exp_report <- downloadHandler(
        
        
        filename = function() {
          #TODO Temporary fix before 2023 lookup fix
          if (input$exp_report_comp == "sex" ) { brkdown <- "Gender" } else {brkdown <- input$exp_report_comp } 
          
          
          paste0("Hertfordshire YPHWS Report - ", brkdown, " focusing on ", input$exp_report_cat, "-2022", ".html")
        },
        
        content = function(file) {
          
          shiny::withProgress(message = "Producing the report. This can take some time...", {
            
            src <- normalizePath('report_app_short.Rmd')
            
            # temporarily switch to the temp dir, in case you do not have write permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, 'report_app_short.Rmd', overwrite = TRUE)
            
            # Set up parameters to pass to Rmd document
            params <- list(var = input$exp_report_comp,
                           cat = input$exp_report_cat,
                           #year = input$exp_report_year,
                           rendered_by_shiny = TRUE,
                           q_coded = q_coded(),
                           data = data(),
                           meta = params()$meta)
            
            
            out <- rmarkdown::render('report_app_short.Rmd', params = params, envir = new.env())
            
            file.rename(out, file)
            
          })
        }
      )
      
    }
  )
}
