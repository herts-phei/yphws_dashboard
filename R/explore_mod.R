# UI ----------------------------------------------------------------------

explore_mod <- function(id,
                        name = "ExploreData") {
  
  ns <- shiny::NS(id)
  
  domains <- c("Demographics", "Living Conditions", "Diet and Lifestyle",
               "Smoking and Vaping", "Alcohol Consumption", "Drug Use",
               "Sexual Health", "Mental Health and Wellbeing", "Bullying", "Safety",
               "Education", "Sustainability", "COVID-19")
  
  names(domains) <- domains
  
  tablerDash::tablerTabItem(
    tabName = "ExploreData",
    shiny::fluidRow(
      shiny::column(2, tags$style(shiny::HTML(".col-sm-2{position:fixed; z-index:1; height: 75%; overflow-y:auto;}")),
                    shiny::tagList(
                      shiny::fluidRow(
                        tablerDash::tablerCard(width = 2,
                                               shiny::htmlOutput(ns("explore_links")))
                      ))
      ),
      shiny::column(offset = 3, 10, 
                    shinyWidgets::prettyRadioButtons(
                      inputId = ns("domains"),
                      label = "Choose a topic:", 
                      choices = domains,
                      inline = TRUE, 
                      status = "danger",
                      fill = TRUE
                    ),
                    shiny::uiOutput(ns("explore_boxes")))
    )
  )
  
}



# Server ------------------------------------------------------------------

# NOTE: Selecting the Safety health topic will crash the app locally, but not on the server for some reason. 

explore_mod_server <- function(id,
                               params,
                               year,
                               stats,
                               stats_old,
                               stats_combined,
                               diffs,
                               comp,
                               q_coded,
                               grp_lookup) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- shiny::NS(id)
      
      # Data --------------------------------------------------------------------
      
      chk_var <- shiny::reactive({
        
        q_coded <- q_coded()
        stats <- stats()
        
        # Filter to specific "rotas" after 2022 when intermittent questions were introduced
        # if (as.numeric(year()) > 2022) {
        #   
        #   rota <- ifelse(as.numeric(year()) %% 2 == 0, 2, 1)
        #   q_coded <- q_coded %>% 
        #     dplyr::mutate(rotation = as.character(rotation)) %>% 
        #     dplyr::filter(rotation %in% c("0", as.character(rota)),
        #                   year == year())
        #   
        # }

        # vector of selected vars
        single <- q_coded %>% 
          dplyr::arrange(question_raw) %>% 
          dplyr::filter(question_theme %in% input$domains)
        
        # filter out questions with only auto-filled No's
        multi_no_responses <- stats %>% 
          dplyr::select(question, response) %>% 
          dplyr::left_join(dplyr::select(q_coded, question_coded, response, multi_binary) %>% distinct(),
                           by = c("question" = "question_coded", 
                                  "response" = "response")) %>% 
          dplyr::filter(question %in% single$question_coded, multi_binary == "TRUE") %>% 
          dplyr::group_by(question) %>% 
          dplyr::mutate(exception = case_when(!"Yes" %in% response ~ FALSE, TRUE ~ TRUE)) %>% 
          dplyr::filter(!exception) %>% 
          dplyr::distinct()
        
        chk_var <- q_coded %>%
          dplyr::filter(question_coded %in% single$question_coded,
                        !is.na(response), !question_coded %in% multi_no_responses$question,
                        question_coded %in% unique(stats()$question)) %>%
          dplyr::pull(question_coded_gen)
        
        # if ("Mental Health and Wellbeing" %in% input$domains) {
        # 
        #   chk_var <- c("selfharm_ever", "worry")
        # }
        #TODO temporary 2022 solution for duplicated sex var. Remove during 2023 update
        ## if("sex" %in% chk_var & year() == "2022") { chk_var <- chk_var[chk_var != "sex"] }
        ## if("gender" %in% chk_var & year() != "2022") { chk_var <- chk_var[chk_var != "gender"] }
        
        return(unique(chk_var))
        
      })
      
      # filtered datasets
      chk_stats <- shiny::reactive({
        stats <- stats()
        q_coded <- dplyr::select(q_coded(), -question_text, -year)

        stats %>% 
          dplyr::left_join(q_coded, by = c("question" = "question_coded",
                                                           "response" = "response")) %>% 
          dplyr::filter(question_coded_gen %in% chk_var()) %>% 
          distinct()
        
      })
      
      chk_stats_old <- shiny::reactive({
        stats_old <- stats_old()
        stats_old %>%
          dplyr::left_join(dplyr::distinct(dplyr::select(q_coded(), -question_text, -year)), by = c("question" = "question_coded",
                                                                                                    "response" = "response")) %>%
          dplyr::filter(question_coded_gen %in% chk_var())
      })
      
      chk_trend <- shiny::reactive({
        stats <- stats_combined()
        
        q_coded_trend <- q_coded() %>% 
          dplyr::select(-polarity, -question_text, -rotation, -year,
                        -question_type, -order, -question_raw) %>% 
          dplyr::distinct()
        
        stats %>% 
          dplyr::left_join(q_coded_trend, 
                           by = c("question" = "question_coded", "response" = "response")) %>% 
          dplyr::filter(question_coded_gen %in% chk_var()) %>% 
          dplyr::distinct() # because of dupes caused by some years having same question_code
      })
      
      #observe(if(grepl("Smok", input$domains)) {browser()})
      
      # Boxes -------------------------------------------------------------------
      boxes <- shiny::reactive({
        
        l <- list()
        
        if (length(chk_var()) > 0) {
          
          params <- params()
          stats <- stats()
          stats_old <- stats_old()
          chk_trend <- chk_trend()
          diffs <- diffs()
          comp <- comp()
          q_coded <- q_coded()
          
          grp_lookup <- grp_lookup()
          
          for (i in 1:length(chk_var())){
            
            # Current question
            current <- dplyr::filter(chk_stats(), question_coded_gen %in% chk_var()[i]) %>% 
              dplyr::select(-question_raw) %>% 
              dplyr::distinct() %>% 
              dplyr::mutate(year = as.character(as.numeric(year())))
            
            current_old <- dplyr::filter(chk_stats_old(), question_coded_gen %in% chk_var()[i]) %>% 
              dplyr::select(-question_raw) %>% 
              dplyr::distinct() %>% 
              dplyr::mutate(year = as.character(as.numeric(year()) - 1))
            
            trend <- dplyr::filter(chk_trend, question_coded_gen %in% chk_var()[i]) %>% 
              dplyr::mutate(year = as.numeric(year))
            
            multi <- ifelse(any(as.logical(current$multi_cat), as.logical(current$multi_binary)), TRUE, FALSE) # check if multicat question
            multi_bin <- ifelse(all(as.logical(current$multi_cat)), FALSE, TRUE) # check if its multicat binary (yes/no)
            
            # Find group of interest 
            grp <- unique(current$breakdown)[grepl(paste0(unique(c(grp_lookup$value_reworded, grp_lookup$value_reworded2)), collapse = "|"), 
                                                   unique(current$breakdown))]
            grp <- ifelse(length(grp) == 0, NA, grp)
            
            # --Create text and plots based on type of question--
            # --Multicat questions
            if (multi) { 
              
              # response of interest (usually Yes)
              if(multi_bin) { resp_interest = "Yes" } else {
                
                resp_interest <- paste(c("low", "On most days", "60 or more minutes", "I have never heard of it", "Agree", "Unsafe", "Yes", "Currently attending"), 
                                       collapse = "|")
                resp_interest <- unique(current$response)[grepl(resp_interest, unique(current$response))]
                
              }
              
              if(comp == "schyear"){
                order <- c("Year 7", "Year 8", "Year 9", "Year 10", "Year 11", "Year 12", "Year 13", "All Responses", "Not at school/other")
                
                current_plot <- current 
                
                current_plot$breakdown <- factor(current$breakdown, levels = unique(order))
                
              } else {
                
                current_plot <- current
                order <- unique(current_plot$breakdown)
              }
              
              current_plot$response <- forcats::as_factor(current$response)
              
              if(!multi_bin & multi) {
                current_plot$response <- forcats::fct_relevel(current$response, c("low", "medium", "high", "very high", "I have never heard of it", "I have heard of it but know nothing about it", "It can be both treated and cured", "It can be treated but not cured"))
              }
              
              # multicat style plot
              int_plot <- create_multi_plot(df = current_plot,
                                            plot_title = "",
                                            binary = multi_bin)
              
              # trend table
              if (nrow(current_old) > 0) {
                
                trend_opts <- unique(trend$menu_text[trend$year == year()])
                
                trend_plot <- create_trend_plot(df = dplyr::filter(trend, response_of_interest == "TRUE"),
                                                plot_custom_grp = order,
                                                plot_title = "",
                                                year = year(),
                                                multi = TRUE)
                
                
              } else {
                
                trend_plot <- "Trend data cannot be generated as this question does not have enough yearly data."
                
              }
              
              # text differs depending on type of question
              if(!multi_bin) {
                
                text <- create_sum_sentence(dataset = current,
                                            dataset_old = current_old, 
                                            multi = T,
                                            value_of_interest = resp_interest,
                                            full_data = stats,
                                            # diffs = chk_stats(),
                                            custom_grp = unique(current$breakdown),
                                            group_of_interest = grp,
                                            q_coded = q_coded,
                                            top = 5)
                
              } else {
                
                if (any(grepl("internet_", current$question))) { top <- NA } else { top <- 5 }
                
                text <- create_sum_sentence(dataset = current,
                                            dataset_old = current_old, 
                                            multi = T,
                                            value_of_interest = "Yes",
                                            full_data = chk_stats(),
                                            # diffs = chk_diff(),
                                            custom_grp = unique(current$breakdown),
                                            group_of_interest = grp,
                                            q_coded = q_coded,
                                            top = top)
                
              }
              
              # --Single cat questions
            } else { 
              
              # text summary
              text <- create_sum_sentence(dataset = current,
                                          dataset_old = current_old,
                                          multi = multi,
                                          value_of_interest = NA,
                                          full_data = stats,
                                          # diffs = chk_diff(),
                                          custom_grp = unique(current$breakdown),
                                          group_of_interest = grp,
                                          q_coded = q_coded,
                                          top = NA)
              
              if(comp == "schyear"){
                
                order <- c("Year 7", "Year 8", "Year 9", "Year 10", "Year 11", "Year 12", "Year 13", "All Responses", "Not at school/other")
                
                current_plot <- current 
                
                current_plot$breakdown <- factor(current$breakdown, levels = unique(order))
                
              } else {
                
                current_plot <- current
                
                order <- unique(current_plot$breakdown)
              }
              
              # interactive plot
              int_plot <- create_basic_plot(df = current_plot,
                                            plot_custom_grp = order,
                                            plot_title = "")
              
              # trend table
              if (nrow(current_old) > 0) {

                m <- ifelse(length(unique(trend$response)) > 1, TRUE, FALSE)
                trend_plot <- create_trend_plot(df = trend,
                                                plot_custom_grp = order,
                                                plot_title = "",
                                                year = year(),
                                                multi = m)
                
              } else {
                
                trend_plot <- "Trend data cannot be generated as this question does not have enough yearly data."
                
              }
            }
            
            # --Create boxes --
            l[[i]] <- bs4Dash::tabItem("name", 
                                       bs4Dash::bs4TabCard(width = 12, side = "right", status = "success",
                                                           collapsible = FALSE, 
                                                           title = shiny::HTML(paste0("<a id='anchor-", current$question_coded_gen[1], "'></a>", current$heading[1],"<br>")),
                                                           shiny::tabPanel("Summary",
                                                                           shiny::HTML(
                                                                             text
                                                                           ),
                                                                           shiny::br(),
                                                                           int_plot
                                                           ),
                                                           shiny::tabPanel(
                                                             "Trend",
                                                             shiny::br(),
                                                             trend_plot
                                                           ),
                                                           shiny::tabPanel(
                                                             "Table",
                                                             chk_stats() %>% 
                                                               dplyr::mutate(value = paste0(round(as.numeric(value) * 100, 2), "%"),
                                                                             lowercl = paste0(round(as.numeric(lowercl) * 100, 2), "%"),
                                                                             uppercl = paste0(round(as.numeric(uppercl) * 100, 2), "%")
                                                               ) %>% 
                                                               dplyr::filter(question_coded_gen %in% chk_var()[i]) %>% 
                                                               dplyr::select(breakdown, question = question_text, response, percent = value, 
                                                                             count, total = denominator,
                                                                             `lower CI` = lowercl, `upper CI` = uppercl) %>% 
                                                               distinct() %>% 
                                                               reactable::reactable(groupBy = c("breakdown", "question"),
                                                                                    columns = list(
                                                                                      percent = reactable::colDef(maxWidth = 68),
                                                                                      count = reactable::colDef(maxWidth = 65),
                                                                                      total = reactable::colDef(maxWidth = 65),
                                                                                      `lower CI` = reactable::colDef(maxWidth = 75),
                                                                                      `upper CI` = reactable::colDef(maxWidth = 75)
                                                                                    ))
                                                           )
                                                           ) )
          }
          
        } else {
          
          l[[1]] <- bs4Dash::tabItem("name", 
                                     bs4Dash::bs4TabCard(width = 12, side = "right", status = "success",
                                                         collapsible = FALSE,
                                                         title = shiny::HTML(paste0(input$domains[1],"<br>")),
                                                         shiny::tabPanel(title = NULL, "No data available for the selected year.")))
          
        }
        
        
        return(l)
        
        
      })
      
      
      # TOC Links ---------------------------------------------------------------
      links <- shiny::reactive({
        
        stats <- stats()
        # diffs <- diffs()
        comp <- comp()
        q_coded <- q_coded()
        
        l <- list()
        
        if(length(chk_var() > 0)) {
          
          for (i in 1:length(chk_var())){
            
            # Current question
            current <- dplyr::filter(chk_stats(), question_coded_gen %in% chk_var()[i])
            
            text <- q_coded$heading[q_coded$question_coded_gen %in% current$question_coded_gen][1] # for TOC
            
            l[[i]] <- paste0("<a href='#anchor-", current$question_coded_gen[1], "'>", text, "</a><br><br>")
            
          }
          
        } else {
          
          l[[1]] <- paste0("")
          
        }
        
        output <- paste(unlist(l), collapse = "")
        return(output)
        
      })
      
      output$explore_boxes <- shiny::renderUI(boxes())
      output$explore_links <- shiny::renderText(links())
      
    }
  )
}