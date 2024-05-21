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
                               shiny::uiOutput(ns("exp_report_comp")),
                               shiny::uiOutput(ns("exp_report_cat")),
                               shiny::uiOutput(ns("text")),
                               #shiny::uiOutput(ns("exp_report_year")),
                               #TODO
                               shiny::uiOutput(ns("export_report")),
                               shiny::uiOutput(ns("exp_report_button"))
                               
        )
    )
  )
  )
  
}

# Server ------------------------------------------------------------------

export_mod_server <- function(id,
                              params,
                              year,
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
        
        if (as.numeric(year()) <2023) {
          
          output$text <- shiny::renderText({
            
            paste0("Customised reports are currently only available for the latest year of data. Please select the current year at the top of the dashboard and come back to this panel to create your report.")
            
          })
          
        } else {
        
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
                         "Children looked after" = "cla",
                         "SEND/ADHD/Autism" = "condition_send_autism_adhd",
                         "District" = "district_clean"),
          selected = comp,
          options = pickerOptions(
            liveSearch = TRUE),
          multiple = FALSE)
       
        } 
      })
      
      output$exp_report_cat <- shiny::renderUI({
        
        if (as.numeric(year()) <2023) {
          
          output$text <- shiny::renderText({
            
            paste0("")
            
          })
          
        } else {
        
        data <- data()
        
        choices <- unique(data[[input$exp_report_comp]]$breakdown) 
        choices <- choices[choices != "All Responses" & choices != "Non-white"]
        
        if (input$exp_report_comp == "schyear") {
          choices <- c("Year 7", "Year 8", "Year 9", "Year 10", "Year 11", "Year 12", "Year 13", "Not at school/other")
          } else if (input$exp_report_comp == "sex") {
            choices <- c("Female", "Male", "Non-Binary", "Transgender", "Other sex", "Unsure", "Prefer not to say")
            } else if (input$exp_report_comp == "ethnicity") {
              choices <- c("Asian", "Black", "Mixed", "White", "Any other ethnic group")
              } else if (input$exp_report_comp == "sexuality") {
                choices <- c("Bisexual", "Heterosexual/Straight", "Homosexual/Gay Male", "Homsexual/Lesbian", "Questioning", "Unsure", "Other sexual orientations", "Prefer not to say")
                } else if (input$exp_report_comp == "caring") {
                    choices <- c("Young carer", "Non-carer")
                  } else if (input$exp_report_comp == "cla") {
                    choices <- c("Young person in care", "Young people not in care")
                    } else if (input$exp_report_comp == "condition_send_autism_adhd") {
                      choices <- c("SEND", "Non-SEND")
                    } else if (input$exp_report_comp == "district_clean") {
                      choices <- c("Broxbourne", "Dacorum", "East Hertfordshire", "Hertsmere", "North Hertfordshire",
                                   "St Albans", "Stevenage", "Three Rivers", "Watford", "Welwyn Hatfield", "Outside of Hertfordshire")
                      } else (input$exp_report_comp)
        
        shinyWidgets::pickerInput(ns("exp_report_cat"), "Select the category from the selected group you are most interested in:",
                                  choices = as.character(na.omit(choices)), multiple = FALSE,
                                  options = pickerOptions(
                                    liveSearch = TRUE),
                                  selected = as.character(na.omit(choices)[1]))
        }
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
        
        if (as.numeric(year()) <2023) {
          
          output$text <- shiny::renderText({
            
            paste0("")
            
          })
          
        } else {
        
        paste0(" Customised reports allow you to export a full report for the latest year of data, with all indicators broken down by group of interest (e.g. 'Year group') and category of most interest (e.g. 'Year 10').",
               " If you would like more information on this functionality, please email YPHWS@hertfordshire.gov.uk")
        }
      })
      
      # browser()
      # download handler
      
      output$exp_report <- downloadHandler(
          
        filename = function() {
          #TODO Temporary fix before 2023 lookup fix
          
          if (input$exp_report_comp == "sex" ) {
            brkdown <- "Gender" } else if (input$exp_report_comp == "cla") {
              brkdown <- "Children Looked After" } else if (input$exp_report_comp == "condition_send_autism_adhd") {
                brkdown <- "SEND ADHD Autism" } else if (input$exp_report_comp == "schyear") {
                  brkdown <- "School Year" } else if (input$exp_report_comp == "ethnicity") {
                    brkdown <- "Ethnicity" } else if (input$exp_report_comp == "imd_quintile") {
                      brkdown <- "IMD Quintile" } else if (input$exp_report_comp == "sexuality") {
                        brkdown <- "Sexuality" } else if (input$exp_report_comp == "caring") {
                          brkdown <- "Young Carer" } else if (input$exp_report_comp == "district_clean") {
                            brkdown <- "District"
              } else {brkdown <- input$exp_report_comp}
                
          paste0("Hertfordshire YPHWS Report - ", brkdown, " focusing on ", input$exp_report_cat, "-2023", ".html")
        },
        
        content = function(file) {
          
          shiny::withProgress(message = "Producing the report. This can take some time...", {
            
            src <- normalizePath('dashboard_custom_report.Rmd')
            
            # temporarily switch to the temp dir, in case you do not have write permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, 'dashboard_custom_report.Rmd', overwrite = TRUE)
            
            # Set up parameters to pass to Rmd document
            params <- list(var = isolate(input$exp_report_comp),
                           cat = isolate(input$exp_report_cat),
                           #year = input$exp_report_year,
                           rendered_by_shiny = TRUE,
                           q_coded = isolate(q_coded()),
                           data = isolate(data()),
                           meta = isolate(params()$meta))
            
            
            out <- rmarkdown::render('dashboard_custom_report.Rmd', params = params, envir = new.env())
            
            file.rename(out, file)
            
          
        }
      )
        
        })
      
      output$exp_report_button <- shiny::renderUI(
        
        if (as.numeric(year()) <2023) {
          
        } else {
  
          shiny::downloadButton(ns("exp_report"), "Export report")

          }
)
      
      
      }
  )
}
