# UI ----------------------------------------------------------------------

explore_mod <- function(id,
                        name = "ExploreData") {
  
  ns <- NS(id)
  
  domains <- c("Living Conditions", "Diet and Lifestyle",
               "Education", "Demographics",
               "Mental Health and Wellbeing", "Smoking and Vaping",
               "Alcohol Consumption", "Drug Use",
               "Sexual Health", "Safety",
               "Sustainability", "COVID-19")
  
  names(domains) <- domains
  
  tablerTabItem(
    tabName = "ExploreData",
    fluidRow(
      # column(2, tags$style(HTML(".col-sm-2{position:fixed; z-index:1; height: 75%; overflow-y:auto;}")),
      #        tagList(
      #          fluidRow(
      #            tablerCard(width = 2,
      #                       htmlOutput(ns("explore_links")))
      #          )
      #          )
      # ),
      column(12, 
             # pick survey topic
             pickerInput(
               inputId = ns("domains"), 
               label = "Select health topic/s:", 
               choices = c(domains),
               selected = "Safety", multiple = T
             ),
             uiOutput(ns("explore_boxes")))
    )
  )
  
}



# Server ------------------------------------------------------------------

explore_mod_server <- function(id,
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
      #   if ("Education" %in% input$domains ) {browser()}
      # })
      
      # Data --------------------------------------------------------------------
      
      chk_var <- reactive({
        
        q_coded <- q_coded()
        # vector of selected vars
        single <- q_coded %>% 
          filter(question_theme %in% input$domains)
        
        #TODO deduplicate multicat questions.
        chk_var <- q_coded %>%
          filter(question_coded %in% single$question_coded) %>%
          pull(question_coded_gen)
        
        return(unique(chk_var))
        
      })
      
      # filtered datasets
      chk_stats <- reactive({
        stats <- stats()
        stats %>% 
          left_join(select(q_coded(), -question_text), by = c("question" = "question_coded")) %>% 
          filter(question_coded_gen %in% chk_var())
        
      })
      
      chk_stats_old <- reactive({
        stats_old <- stats_old()
        stats_old %>%
          left_join(select(q_coded_old(), -question_text), by = c("question" = "question_coded")) %>%
          filter(question_coded_gen %in% chk_var())
      })
      
      chk_diff <- reactive({
        diffs <- diffs()
        diffs %>% 
          left_join(select(q_coded(), -question_text), by = c("question" = "question_coded")) %>% 
          filter(question_coded_gen %in% chk_var())
      })
      
      # Boxes -------------------------------------------------------------------
      boxes <- reactive({
        
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
                   year = "2021") %>% 
            bind_rows(mutate(current, year = "2020"))  
          
          multi <- ifelse(any(current$multi_cat, current$multi_binary), TRUE, FALSE) # check if multicat question
          multi_bin <- ifelse(all(current$multi_cat), FALSE, TRUE) # check if its multicat binary (yes/no)
          
          # --Create text and plots based on type of question--
          if (multi) { 
            int_plot <- create_multi_plot(df = current,
                                          plot_title = "",
                                          binary = multi_bin)
            
            trend_plot <- ""
            
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
                                          q_coded = q_coded)
              
            } else {
              
              text <- create_sum_sentence(dataset = current,
                                          multi = multi,
                                          value_of_interest = "Yes",
                                          full_data = chk_stats(),
                                          diffs = chk_diff(),
                                          custom_grp = unique(current$breakdown),
                                          group_of_interest = unique(current$breakdown)[2],
                                          q_coded = q_coded)
              
            }
            
          } else {
            
            text <- create_sum_sentence(dataset = current,
                                        multi = multi,
                                        value_of_interest = "Yes",
                                        full_data = chk_stats(),
                                        diffs = chk_diff(),
                                        custom_grp = unique(current$breakdown),
                                        group_of_interest = unique(current$breakdown)[2],
                                        q_coded = q_coded)
            
            int_plot <- create_basic_plot(df = current,
                                          plot_custom_grp = unique(current$breakdown),
                                          plot_title = "")
            
            trend_plot <- current_old %>% 
              ggplot(aes(x = response, y = value, group = year)) +
              geom_bar(
                aes(color = year, fill = year),
                stat = "identity", position = position_dodge(0.8),
                width = 0.7
              ) +
              geom_errorbar(aes(ymin = lowercl, ymax = uppercl, group = year), 
                            width = 0.2, colour = "black", alpha = 0.5,
                            position = position_dodge(0.95)) +
              facet_wrap(~breakdown) +
              theme_minimal()
            
            trend_plot <- ggplotly(trend_plot)
            
            
          }
          
          
          
          # --Create boxes --
          l[[i]] <- tabItem("name", 
                            tabBox(width = 12, side = "right", status = "success",
                                       collapsible = FALSE, 
                                       title = "",
                                       #HTML(paste0("<hr><br><a id='anchor-", current$question_coded_gen[1], "'></a>", chk_var()[i],"<br>")),
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
                                       )
                                       ) )
        }
        
        return(l)
        
        
      })
      
      
      # TOC Links ---------------------------------------------------------------
      # links <- reactive({
      #   
      #   stats <- stats()
      #   diffs <- diffs()
      #   comp <- comp()
      #   q_coded <- q_coded()
      #   
      #   l <- list()
      #   for (i in 1:length(chk_var())){
      #     
      #     # Current question
      #     current <- filter(chk_stats(), question_coded_gen %in% chk_var()[i])
      #     
      #     q_coded <- q_coded
      #     text <- q_coded$question_coded_gen[q_coded$question_coded_gen %in% current$question_coded_gen] # for TOC
      #     
      #     l[[i]] <- paste0("<a href='#anchor-", current$question_coded_gen[i], "'>", text, "</a><br><br>")
      #     
      #   }
      #   
      #   output <- paste(unlist(l), collapse = "")
      #   
      #   return(output)
      #   
      # })
      
      output$explore_boxes <- renderUI(boxes())
      #output$explore_links <- renderText(links())
      
    }
  )
}
