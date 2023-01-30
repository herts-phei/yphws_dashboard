# UI ----------------------------------------------------------------------

inequalities_mod <- function(id,
                             name = "Inequalities") {
  
  ns <- shiny::NS(id)
  
  tablerDash::tablerTabItem(
    tabName = name,
    shiny::tagList(
      shiny::fluidRow(
        tablerDash::tablerCard(title = "Introduction",
                               closable = FALSE,
                               width = 12, 
                               shiny::htmlOutput(ns("ineq_text"))
        )
      ),
      shiny::fluidRow(
        tablerDash::tablerCard(title = "Filters",
                               closable = FALSE,
                               width = 4,
                               shiny::uiOutput(ns("ineq_domains")),
                               shiny::uiOutput(ns("ineq_questions"))),
        shiny::column(
          width = 8,
          shiny::plotOutput(ns("tartan")))
      )
    )
    
  )
  
}


# Server ------------------------------------------------------------------

inequalities_mod_server <- function(id,
                                    params,
                                    year,
                                    comp, 
                                    q_coded,
                                    stats) {
  
  shiny::moduleServer(
    id, 
    function(input, output, session) {
      
      ns <- shiny::NS(id)
      
       #observe({if("Safety" %in% input$ineq_domains) {browser()}})
      
      # Intro text --------------------------------------------------------------------
      
      output$ineq_text <- shiny::renderText({
        paste0("This tab can be used to view significant differences between groups across different questions/indicators. ",
               "A tartan rug plot will be created based of the selected health topic/s. You can select which indicators (questions) ",
               "under the selected health topic/s you'd like to be added to the tartan rug. Please allow a few seconds after selection ", 
               "for the plot to update. <br><br> As shown in the legend, values coloured yellow are statistically similar to ", 
               "All Responses, while values that are dark blue or light blue were found to be statistically lower or statistically ",
               "higher compared to All Responses respectively. The statistical significance is determined by comparing the 95% ",
               "confidence intervals (CI) between groups and All Responses, as calculated using methods used by Public Health England. ",
               "All values are presented in percentages. To view the values of 95% CIs, see the Explore Data tab or Export tab.")
      })
      
      # UIs ---------------------------------------------------------------------
      
      output$ineq_domains <- shiny::renderUI({
        
        params <- params()
        
        shinyWidgets::awesomeRadio(
          inputId = ns("ineq_domains"),
          label = "Choose the health topic(s):", 
          choices = params$domains, 
          # bigger = TRUE,
          status = "info",
          # animation = "jelly",
          selected = params$domains[1]
        )
        
      })
      
      questions <- shiny::reactive({
        
        q_coded() %>%
          dplyr::mutate(survey_text = as.character(survey_text)) %>%
          dplyr::filter(question_theme %in% input$ineq_domains, 
                        response_of_interest == "TRUE",
                        question_coded != comp())
        
      })
      
      output$ineq_questions <- renderUI({
        
        shinyWidgets::pickerInput(
          inputId = ns("ineq_questions"),
          label = "Choose the indicators:",
          choices = as.character(unique(questions()$question_response)), 
          selected = as.character(unique(questions()$question_response)),
          options = pickerOptions(
            actionsBox = TRUE,
            liveSearch = TRUE),
          multiple = T
        )
        
      })
      
      for_height <- shiny::reactive(input$ineq_questions)
      
      # Tartan rug -------------------------------------------------------------
      
      output$tartan <- shiny::renderPlot({
        
        if (is.null(input$ineq_questions)) { return(NULL) }
        
        params <- params()
        q_coded <- q_coded()
        diffs <- get_stats_diffs(stats = stats(), 
                                 levels = unique(stats()$breakdown))
          
        df <- diffs %>% 
          dplyr::left_join(dplyr::select(q_coded, -question_text, -year), by = c("question" = "question_coded", 
                                                                          "response" = "response")) %>% 
          dplyr::filter(question_response %in% input$ineq_questions, response_of_interest == "TRUE") %>% 
          distinct()
        
        names(df) <- gsub("\\.x", "", names(df))
        
        if(comp() == "schyear"){
          
          categories <- unique(c("Year 7", "Year 8", "Year 9", "Year 10", "Year 11", "Year 12", "Year 13", "Not at school/other"))
          
        } else {
          
          categories <- as.character(unique(df$breakdown))
          categories <- categories[which(!grepl("All Responses", categories))]
          
        }
        
        rug_df <- df %>% 
          #filter(response %in% resp_interest) %>% 
          dplyr::mutate(Timeperiod = as.character(year()),
                        TimeperiodSortable = as.character(year()),
                        value = as.numeric(gsub("%", "", as.character(value))),
                        diff = ifelse(is.na(diff), "statistically similar", diff)) 
        
        if (nrow(rug_df) > 0) {
          
          tartan(df = rug_df,
                 indicators = unique(rug_df$question_response),
                 comparator_area = "All Responses",
                 areas = categories,
                 palette = "blues",
                 area_col = "breakdown",
                 period_col = "Timeperiod",
                 period_sort_col = "TimeperiodSortable",
                 indicator_col = "question_response",
                 value_col = "value",
                 upper_ci = "uppercl",
                 lower_ci = "lowercl")
          
        } else { return(NULL) }
        
        
        
      }, height = function() {60 * (max(1, length(for_height())))}, res = 96)
    }
  )
}
