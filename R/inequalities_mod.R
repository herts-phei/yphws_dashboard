# UI ----------------------------------------------------------------------

inequalities_mod <- function(id,
                             name = "Inequalities") {
  
  ns <- NS(id)
  
  tablerTabItem(
    tabName = name,
    tagList(
      fluidRow(
        tablerCard(title = "Introduction",
                   width = 12, 
                   htmlOutput(ns("ineq_text"))
        )
      ),
      fluidRow(
        tablerCard(title = "Filters",
                   width = 4, 
                   uiOutput(ns("ineq_domains")),
                   uiOutput(ns("ineq_questions"))),
        column(
                   width = 8,
                   plotOutput(ns("tartan")))
      )
    )
    
  )
  
}


# Server ------------------------------------------------------------------

inequalities_mod_server <- function(id,
                                    params,
                                    q_coded,
                                    stats,
                                    diffs) {
  
  moduleServer(
    id, 
    function(input, output, session) {
      
      ns <- NS(id)

      #observe({if("Education" %in% input$ineq_domains) {browser()}})
      
      # UIs ---------------------------------------------------------------------

      output$ineq_domains <- renderUI({
        
        params <- params()
    
        shinyWidgets::prettyCheckboxGroup(
            inputId = ns("ineq_domains"),
            label = "Choose the health topic(s):", 
            choices = params$domains, 
            bigger = TRUE,
            status = "info",
            animation = "jelly",
            selected = params$domains[1]
          )
        
      })

      output$ineq_questions <- renderUI({

        questions <- q_coded() %>%
          mutate(survey_text = as.character(survey_text)) %>%
          filter(question_theme %in% input$ineq_domains)

        checkboxGroupInput(
          inputId = ns("ineq_questions"),
          label = "Choose the indicators:", 
          choices = as.character(unique(questions$survey_text)),
          selected = as.character(unique(questions$survey_text))
        )

      })
      
      for_height <- reactive(input$ineq_questions)
      
      # Tartan rug -------------------------------------------------------------
      
      output$tartan <- renderPlot({
        
        params <- params()
        q_coded <- q_coded()
        diffs <- diffs()
        
        df <- diffs %>% 
          left_join(select(q_coded, -question_text), by = c("question" = "question_coded")) %>% 
          filter(survey_text %in% input$ineq_questions) %>% 
          group_by(breakdown.x, question, response) %>% 
          filter(grepl(response_of_interest, response)) %>% 
          ungroup()
        
        categories <- as.character(unique(df$breakdown.x))
        categories <- categories[which(!grepl("All Responses", categories))]
        
        #themes <- params$domains
        
        rug_df <- df %>% 
          mutate(Timeperiod = as.character(params$year),
                 TimeperiodSortable = as.character(params$year),
                 value.x = as.numeric(gsub("%", "", as.character(value.x))),
                 diff = ifelse(is.na(diff), "statistically similar", diff),
                 question_response = paste0(question_text.x, ": '", response, "'"))
        
        tartan(df = rug_df,
               indicators = unique(rug_df$question_response),
               comparator_area = "All Responses",
               areas = categories,
               palette = "blues",
               area_col = "breakdown.x",
               period_col = "Timeperiod",
               period_sort_col = "TimeperiodSortable",
               indicator_col = "question_response",
               value_col = "value.x",
               upper_ci = "uppercl.x",
               lower_ci = "lowercl.x")
        
        
      }, height = function() {70 * (length(for_height()))})
      
      # Intro text --------------------------------------------------------------------
      
      output$ineq_text <- renderText({
        paste0("This tab can be used to view significant differences between groups across different questions/indicators. ",
               "A tartan rug plot will be created based off of the selected health topic/s. You can select which indicators (questions) ",
               "under the selected health topic/s you'd like to be added to the tartan rug. Please allow a few seconds after selection ", 
               "for the plot to update. <br><br> As shown in the legend, values coloured yellow are statistically similar to ", 
               "All Responses, while values that are dark blue or light blue were found to be statistically lower or statistically ",
               "higher compared to All Responses, respectively. The statistical significance is determined by comparing the 95% ",
               "confidence intervals (CI) between groups and All Responses, as calculated using methods used by Public Health England. ",
               "All values are presented in percentages. To view the values of 95% CIs, see the Explore Data tab or Export tab.")
      })
      
    }
)
}
