# UI ----------------------------------------------------------------------

inequalities_mod <- function(id,
                             name = "Inequalities") {
  
  ns <- NS(id)
  
  tablerTabItem(
    tabName = name,
    tagList(
      fluidRow(
        tablerCard(title = "Filters",
                   width = 4, 
                   uiOutput(ns("ineq_domains")),
                   uiOutput(ns("ineq_questions"))),
        tablerCard(title = "Inequalities rug",
                   width = 8)
      ),
      fluidRow(
        tablerCard(title = "Demographics Indicators", 
                   width = 6, plotOutput(ns("tartan1"))),
        tablerCard(title = "Alcohole Indicators",
                   width = 6, plotOutput(ns("tartan2")))
      ),
      fluidRow(
        tablerCard(title = "Drug Indicators", 
                   width = 6, plotOutput(ns("tartan3"))),
        tablerCard(title = "COVID-19 Indicators", 
                   width = 6, plotOutput(ns("tartan4")))
      ),
      fluidRow(
        tablerCard(title = "Safety Indicators", 
                   width = 6, plotOutput(ns("tartan5"))),
        tablerCard(title = "Other Indicators",
                   width = 6, plotOutput(ns("tartan6")))
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

      observe({if("Education" %in% input$ineq_domains) {browser()}})
      # UIs ---------------------------------------------------------------------

      output$ineq_domains <- renderUI({
        
        params <- params()
    
        shinyWidgets::prettyCheckboxGroup(
            inputId = ns("ineq_domains"),
            label = "Choose the health topic(s):", 
            choices = params$domains,
            icon = icon("check-square-o"), 
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

        shinyWidgets::prettyCheckboxGroup(
          inputId = ns("ineq_questions"),
          label = "Choose the indicators:", 
          choices = as.character(unique(questions$survey_text)),
          selected = as.character(unique(questions$survey_text)),
          icon = icon("check-square-o"), 
          bigger = TRUE,
          status = "info",
          animation = "jelly"
        )

      })
      

      # Tartan rug -------------------------------------------------------------
      
      output$tartan <- renderPlot({
        
        params <- params()
        q_coded <- q_coded()
        diffs <- diffs()
        
        df <- diffs %>% 
          left_join(select(q_coded, -question_text), by = c("question" = "question_coded"))
        
        categories <- as.character(unique(df$breakdown.x))
        categories <- categories[which(!grepl("All Responses", categories))]
        
        #themes <- params$domains
        
        rug_df <- df %>% 
          filter(survey_text %in% input$`ineq-ineq_questions`) %>% #TODO
          mutate(Timeperiod = as.character(params$year),
                 TimeperiodSortable = as.character(params$year),
                 value.x = as.numeric(gsub("%", "", as.character(value.x))),
                 diff = ifelse(is.na(diff), "statistically similar", diff))
        
        tartan(df = rug_df,
               indicators = unique(rug_df$question),
               comparator_area = "All Responses",
               areas = categories,
               palette = "blues",
               area_col = "breakdown.x",
               period_col = "Timeperiod",
               period_sort_col = "TimeperiodSortable",
               indicator_col = "question",
               value_col = "value.x",
               upper_ci = "uppercl.x",
               lower_ci = "lowercl.x")
        
        
      })
      
      tartans <- reactive({
        

        

        
        t <- list()
        for (i in 1:length(themes)) {
          
          
          
          t[[i]] <- tartan(df = loop_df,
                           indicators = unique(loop_df$question),
                           comparator_area = "All Responses",
                           areas = categories,
                           palette = "rag",
                           area_col = "breakdown.x",
                           period_col = "Timeperiod",
                           period_sort_col = "TimeperiodSortable",
                           indicator_col = "question",
                           value_col = "value.x",
                           upper_ci = "uppercl.x",
                           lower_ci = "lowercl.x")
          
        } 
        return(t)
        
      })
      
      output$tartan1 <- renderPlot(tartans()[[1]])
      output$tartan2 <- renderPlot(tartans()[[2]])
      output$tartan3 <- renderPlot(tartans()[[3]])
      output$tartan4 <- renderPlot(tartans()[[4]])
      output$tartan5 <- renderPlot(tartans()[[5]])
      output$tartan6 <- renderPlot(tartans()[[6]])
      
    }
  )
  
}
