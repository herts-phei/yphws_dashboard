# UI ----------------------------------------------------------------------

export_mod <- function(id,
                        name = "Export") {
  
  ns <- NS(id)
  
  tablerTabItem(
    tabName = "Export",
    tagList(
      fluidRow(
        tablerCard(width = 12, title = "Data table", 
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
      fluidRow(
        tablerCard(title = "Export full report (COMING SOON)",
                   width = 12, 
                   closable = FALSE
                   # uiOutput("exp_report_comp"),
                   # uiOutput("exp_report_cat"),
                   #downloadButton("exp_report", "Export report")
        )
      )
    )
  )
  
}



# Server ------------------------------------------------------------------

# NOTE: Selecting the Safety health topic will crash the app locally, but not on the server for some reason. 

export_mod_server <- function(id,
                              data, 
                              stats_combined,
                              q_coded, 
                              comp, 
                              exp_year, 
                              exp_breakdown,
                              exp_theme, 
                              exp_question, 
                              export_button) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      output$exp_report_comp <- renderUI({
      
      ns <- NS(id)
      comp <-  comp()
      
      pickerInput(ns("exp_report_comp"), label = "Select what to group by in your report",
                  choices = list("Sex" = "sex",
                                 "Year group" = "schyear",
                                 "Ethnicity" = "ethnicity",
                                 "IMD Quintile" = "imd_quintile",
                                 "Sexuality" = "sexuality",
                                 "Young carer" = "caring",
                                 # "Smoker" = "smoke_ever",
                                 "Self-harm" = "selfharm_ever",
                                 "Bullied" = "bullied",
                                 "District" = "District"),
                  selected = comp,
                  multiple = FALSE)
      
    })
      
      output$exp_report_cat <- shiny::renderUI({
        
        data <- data()
        
        choices <- unique(data[[input$exp_report_comp]]$breakdown) 
        
        choices <- choices[choices != "All Responses" & choices != "Non-white"]
        
        
        shinyWidgets::pickerInput("exp_report_cat", "Select the category from the selected group you are most interested in:",
                                  choices = as.character(na.omit(choices)), multiple = FALSE,
                                  selected = as.character(na.omit(choices)[1]))
        
      })
    
    
    output$exp_report <- downloadHandler(
      # filename = "report.html",
      # content = function(file) {
      #   withProgress(message = "Rendering, please wait!", {
      #     tempReport <- file.path(tempdir(), "test.Rmd")
      #     file.copy("test.Rmd", tempReport, overwrite = TRUE)
      #     
      #     # Set up parameters to pass to Rmd document
      #     # params <- list(var = input$exp_report_comp,
      #     #                cat = input$exp_report_cat)
      #     params <- list(rendered_by_shiny = TRUE)
      #     
      #     # Knit the document, passing in the `params` list, and eval it in a
      #     # child of the global environment (this isolates the code in the document
      #     # from the code in this app).
      #     # show_modal_spinner(text = "Rendering report. Please wait, this should take 1-2 minutes.")
      #     rmarkdown::render(tempReport, output_file = file,
      #                       params = params,
      #                       envir = new.env(parent = globalenv())
      #     )
      #     # remove_modal_spinner() # remove it when done
      #     
      #   })
      #   
      # }
      
      
      
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
    
    output$exp_year <- renderUI({
      
      stats_combined <- stats_combined()
      
      shinyWidgets::awesomeCheckboxGroup(
        inputId = "exp_year",
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
        inputId = "exp_breakdown",
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
        inputId = "exp_theme",
        label = "Health topic:", 
        choices = na.omit(unique(q_coded$question_theme)),
        selected = na.omit(unique(q_coded$question_theme)),
        multiple = TRUE
      )
      
    })
    
    output$exp_question <- renderUI({
      
      q_coded <- q_coded()
      exp_theme <- exp_theme()
      
      filtered <- q_coded %>% 
        mutate(across(where(is.character), ~na_if(., "NA"))) %>% 
        filter(question_theme %in% exp_theme, !is.na(survey_text)) %>% #
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
    table_data <- reactive({
      
      data <- data()
      comp <- comp()
      q_coded <- q_coded()
      exp_year <- exp_year()
      exp_breakdown <- exp_breakdown()
      exp_theme <- exp_theme()
      exp_question <- exp_question()
      
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
    
    #event reactive table
    # observeEvent(export_button(), {
    #   
    #   data <- data()
    #   comp <- comp()
    #   q_coded <- q_coded()
    #   exp_year <- exp_year()
    #   exp_breakdown <- exp_breakdown()
    #   exp_theme <- exp_theme()
    #   exp_question <- exp_question()
    #   
    #   #rv$
    #     table_data <- data[[comp]]  %>%
    #     left_join(select(q_coded, question_coded, question_theme, survey_text), q_coded,
    #               by = c("question" = "question_coded")) %>%
    #     select(year, breakdown, topic = question_theme, question = survey_text,
    #            `question option` = question_text, response,
    #            count, denominator, value, lowercl, uppercl) %>%
    #     distinct() %>%
    #     filter(year %in% exp_year,
    #            breakdown %in% exp_breakdown,
    #            topic %in% exp_theme,
    #            question %in% exp_question)
    #   
    # })
    
    # shiny::observeEvent(input$export_button, {
    eventReactive(export_button(),{
      
      data <- data()
      comp <- comp()
      q_coded <- q_coded()
      exp_year <- exp_year()
      exp_breakdown <- exp_breakdown()
      exp_theme <- exp_theme()
      exp_question <- exp_question()
      
      table_data <- data[[comp]]  %>%
        dplyr::left_join(dplyr::select(q_coded, question_coded, question_theme, survey_text), q_coded,
                         by = c("question" = "question_coded")) %>%
        dplyr::select(year, breakdown, topic = question_theme, question = survey_text,
                      `question option` = question_text, response,
                      count, denominator, value, lowercl, uppercl) %>%
        dplyr::distinct() %>%
        dplyr::filter(year %in% exp_year,
                      breakdown %in% exp_breakdown,
                      topic %in% exp_theme,
                      question %in% exp_question)
      
    })

        output$data_table <- renderReactable({
      
        table_data() %>% 
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
        # rv$
        data <- reactive({data_table()})
        write.csv(data, con)
        
      }
      
    ) 
      
    }
  )
}
