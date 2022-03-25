# UI ----------------------------------------------------------------------

explore_mod <- function(id,
                        name = "ExploreData") {
  
  ns <- NS(id)
  
  domains <- c("Demographics", "Living Conditions", "Diet and Lifestyle",
               "Smoking and Vaping", "Alcohol Consumption", "Drug Use",
               "Sexual Health", "Mental Health and Wellbeing", "Safety",
               "Education", "Sustainability", "COVID-19")
  
  names(domains) <- domains
  
  tablerTabItem(
    tabName = "ExploreData",
    fluidRow(
      column(2, tags$style(HTML(".col-sm-2{position:fixed; z-index:1; height: 75%; overflow-y:auto;}")),
             tagList(
               fluidRow(
                 tablerCard(width = 2,
                            htmlOutput(ns("explore_links")))
               ))
      ),
      column(offset = 3, 10, 
             shinyWidgets::prettyRadioButtons(
               inputId = ns("domains"),
               label = "Choose a topic:", 
               choices = domains,
               inline = TRUE, 
               status = "danger",
               fill = TRUE
             ),
             uiOutput(ns("explore_boxes")))
    )
  )
  
}



# Server ------------------------------------------------------------------

explore_mod_server <- function(id,
                               params,
                               stats,
                               stats_old,
                               diffs,
                               comp,
                               q_coded,
                               q_coded_old) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- NS(id)
      
      # Reactive UIs ------------------------------------------------------------
      
      # output$questions <- renderUI({
      # 
      #   stats <- stats()
      #   diffs <- diffs()
      #   comp <- comp()
      #   q_coded <- q_coded()
      # 
      #   questions <- isolate(q_coded) %>%
      #     mutate(survey_text = as.character(survey_text)) %>%
      #     filter(question_theme %in% input$domains)
      # 
      #   pickerInput(ns("questions"), label = "Select/type in a question (multiple can be selected)",
      #               choices = as.character(unique(questions$survey_text)), multiple = T,
      #               selected = as.character(unique(questions$survey_text)),
      #               options = list(`live-search` = TRUE))
      # })
      
      # observe({
      #   if ("Living Conditions" %in% input$domains ) {browser()}
      # })
      
      # Data --------------------------------------------------------------------
      
      chk_var <- reactive({
        
        q_coded <- q_coded()
        # vector of selected vars
        single <- q_coded %>% 
          arrange(question_raw) %>% 
          filter(question_theme %in% input$domains)
        
        #TODO deduplicate multicat questions.
        chk_var <- q_coded %>%
          filter(question_coded %in% single$question_coded,
                 !is.na(response)) %>%
          pull(question_coded_gen)
        
        return(unique(chk_var))
        
      })
      
      # filtered datasets
      chk_stats <- reactive({
        stats <- stats()
        stats %>% 
          left_join(select(q_coded(), -question_text), by = c("question" = "question_coded",
                                                              "response" = "response")) %>% 
          filter(question_coded_gen %in% chk_var())
        
      })
      
      chk_stats_old <- reactive({
        stats_old <- stats_old()
        stats_old %>%
          left_join(select(q_coded(), -question_text), by = c("question" = "question_coded",
                                                                  "response" = "response")) %>%
          filter(question_coded_gen %in% chk_var())
      })
      
      chk_diff <- reactive({
        diffs <- diffs()
        diffs %>% 
          left_join(select(q_coded(), -question_text), by = c("question" = "question_coded",
                                                                   "response" = "response")) %>% 
          filter(question_coded_gen %in% chk_var())
      })
      
      # Boxes -------------------------------------------------------------------
      boxes <- reactive({
        
        params <- params()
        stats <- stats()
        stats_old <- stats_old()
        diffs <- diffs()
        comp <- comp()
        q_coded <- q_coded()
        
        l <- list()
        for (i in 1:length(chk_var())){
          
          # Current question
          current <- filter(chk_stats(), question_coded_gen %in% chk_var()[i])
          current_old <- filter(chk_stats_old(), question_coded_gen %in% chk_var()[i]) %>% 
            mutate(multi_cat = as.logical(multi_cat),
                   multi_binary = as.logical(multi_binary),
                   year = "2020")
          
          multi <- ifelse(any(current$multi_cat, current$multi_binary), TRUE, FALSE) # check if multicat question
          multi_bin <- ifelse(all(current$multi_cat), FALSE, TRUE) # check if its multicat binary (yes/no)
          
          # --Create text and plots based on type of question--
          # --Multicat questions
          if (multi) { 
            
            # multicat style plot
            int_plot <- create_multi_plot(df = current,
                                          plot_title = "",
                                          binary = multi_bin)
            
            # trend table
            if (nrow(current_old) > 0) {
              
              current_old <- current_old %>% 
                mutate(year = as.character(as.numeric(params$year) - 1),
                       `2020` = value) %>% 
                filter(response_of_interest == "TRUE")
              
              names(current_old) <- paste0("prev_", names(current_old))
              
              stats_ <- current %>% 
                filter(response_of_interest == "TRUE")
              
              trend_plot <- create_trend_table(stats = stats_,
                                               stats_old = current_old,
                                               params = params)
              
            } else {
              
              trend_plot <- "Trend data cannot be generated as this question was not in last year's survey."
              
            }
            
            # text differs depending on type of question
            if(!multi_bin) {
            
              resp_interest <- paste(c("On most days", "I have never heard of it", "Agree", "Unsafe", "Yes"), 
                                     collapse = "|")
              resp_interest <- unique(current$response)[grepl(resp_interest, unique(current$response))]
              text <- create_sum_sentence(dataset = current,
                                          multi = multi,
                                          value_of_interest = resp_interest,
                                          full_data = chk_stats(),
                                          diffs = chk_diff(),
                                          custom_grp = unique(current$breakdown),
                                          group_of_interest = unique(current$breakdown)[2],
                                          q_coded = q_coded,
                                          top = NA)
              
            } else {
              
              if (any(grepl("internet_", current$question))) { top <- NA } else { top <- 5 }
              text <- create_sum_sentence(dataset = current,
                                          multi = multi,
                                          value_of_interest = "Yes",
                                          full_data = chk_stats(),
                                          diffs = chk_diff(),
                                          custom_grp = unique(current$breakdown),
                                          group_of_interest = unique(current$breakdown)[2],
                                          q_coded = q_coded,
                                          top = top)
              
            }
            
            # --Single cat questions
          } else { 
            
            # text summary
            text <- create_sum_sentence(dataset = current,
                                        multi = multi,
                                        value_of_interest = "Yes",
                                        full_data = chk_stats(),
                                        diffs = chk_diff(),
                                        custom_grp = unique(current$breakdown),
                                        group_of_interest = unique(current$breakdown)[2],
                                        q_coded = q_coded,
                                        top = NA)
            
            # interactive plot
            int_plot <- create_basic_plot(df = current,
                                          plot_custom_grp = unique(current$breakdown),
                                          plot_title = "")
            
            # trend table
            if (nrow(current_old) > 0) {
              
              current_old <- mutate(current_old, `2020` = value) %>% 
                filter(response_of_interest == "TRUE")
              
              names(current_old) <- paste0("prev_", names(current_old))
              
              stats_ <- current %>% 
                filter(response_of_interest == "TRUE")
              
              trend_plot <- create_trend_table(stats = stats_,
                                               stats_old = current_old,
                                               params = params)
              
            } else {
              
              trend_plot <- "Trend data cannot be generated as this question was not in last year's survey."
              
            }
          }
          
          # --Create boxes --
          l[[i]] <- tabItem("name", 
                            bs4TabCard(width = 12, side = "right", status = "success",
                                       collapsible = FALSE, 
                                       title = HTML(paste0("<a id='anchor-", current$question_coded_gen[1], "'></a>", current$heading[1],"<br>")),
                                       tabPanel("Summary", 
                                                HTML(
                                                  text
                                                ),
                                                br(),
                                                int_plot
                                       ),
                                       tabPanel(
                                         "Trend",
                                         br(),
                                         trend_plot
                                       ),
                                       tabPanel(
                                         "Table", 
                                         chk_stats() %>% 
                                           mutate(value = paste0(round(as.numeric(value) * 100, 2), "%"),
                                                  lowercl = paste0(round(as.numeric(lowercl) * 100, 2), "%"),
                                                  uppercl = paste0(round(as.numeric(uppercl) * 100, 2), "%")
                                           ) %>% 
                                           filter(question_coded_gen %in% chk_var()[i]) %>% 
                                           select(breakdown, question = question_text, response, value, count, denominator,
                                                  lowercl, uppercl) %>% 
                                           reactable(groupBy = c("breakdown", "question"),
                                                     columns = list(
                                                       value = colDef(maxWidth = 70),
                                                       count = colDef(maxWidth = 65),
                                                       denominator = colDef(maxWidth = 70),
                                                       lowercl = colDef(maxWidth = 70),
                                                       uppercl = colDef(maxWidth = 70)
                                                     ))
                                       )) )
        }
        
        return(l)
        
        
      })
      
      
      # TOC Links ---------------------------------------------------------------
      links <- reactive({

        stats <- stats()
        diffs <- diffs()
        comp <- comp()
        q_coded <- q_coded()

        l <- list()
        for (i in 1:length(chk_var())){

          # Current question
          current <- filter(chk_stats(), question_coded_gen %in% chk_var()[i])
          
          text <- q_coded$heading[q_coded$question_coded_gen %in% current$question_coded_gen][1] # for TOC

          l[[i]] <- paste0("<a href='#anchor-", current$question_coded_gen[i], "'>", text, "</a><br><br>")

        }

        output <- paste(unlist(l), collapse = "")

        return(output)

      })

      output$explore_boxes <- renderUI(boxes())
      output$explore_links <- renderText(links())
      
    }
  )
}
