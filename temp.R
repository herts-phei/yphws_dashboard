
library(tidyverse)
library(HertsSPC)

chk_stats <- read_csv("chk_stats.csv")
chk_stats_old <- read_csv("chk_stats_old.csv")
chk_trend <- read_csv("chk_trend.csv")
diffs <- read_csv("diffs.csv")
stats <- read_csv("stats.csv")
stats_old <- read_csv("stats_old.csv")
grp_lookup <- read_csv("grp_lookup.csv")
q_coded <- read_csv("q_coded.csv")
params <- readRDS("params.rds")

chk_var <- c("mental_howaccess", "cope")
comp <- "sex"

data <- data.frame(test = NA)

if (length(chk_var()) > 0) {
  
  # params <- params
  # stats <- stats
  # stats_old <- stats_old
  # diffs <- diffs
  # comp <- comp
  # q_coded <- q_coded
  # grp_lookup <- grp_lookup
  
  for (i in 1:length(chk_var)){
    
    # Current question
    current <- dplyr::filter(chk_stats, question_coded_gen %in% chk_var[i]) %>% 
      dplyr::select(-question_raw) %>% 
      dplyr::distinct() %>% 
      dplyr::mutate(year = as.character(as.numeric(year)))
    
    current_old <- dplyr::filter(chk_stats_old, question_coded_gen %in% chk_var[i]) %>% 
      dplyr::select(-question_raw) %>% 
      dplyr::distinct() %>% 
      dplyr::mutate(year = as.character(as.numeric(year) - 1))
    
    trend <- dplyr::filter(chk_trend, question_coded_gen %in% chk_var[i]) %>% 
      dplyr::select(-question_raw, -question_response, -polarity, -order,
                    -question_type, -rotation, -response_of_interest) %>% 
      dplyr::distinct() %>% 
      dplyr::mutate(year = as.numeric(year),
                    #TODO
                    value = count/denominator)
    
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
        
        trend_edit <- dplyr::filter(chk_trend, question_coded_gen %in% chk_var[i]) %>% 
          dplyr::select(-question_raw, -question_response, -polarity, -order,
                        -question_type, -rotation) %>% 
          dplyr::distinct() %>% 
          dplyr::mutate(year = as.numeric(year),
                        #TODO
                        value = count/denominator) %>% 
          filter(response_of_interest) %>% 
          mutate(response = question_text)
        
        trend_plot <- create_trend_plot(df = trend_edit,
                                        plot_custom_grp = order,
                                        plot_title = "")
        
        
      } else {
        
        trend_plot <- "Trend data cannot be generated as this question was not in last year's survey."
        trend_text <- ""
        
      }
      
      # text differs depending on type of question
      if(!multi_bin) {
        
        text <- create_sum_sentence(dataset = current,
                                    dataset_old = current_old, 
                                    multi = T,
                                    value_of_interest = resp_interest,
                                    full_data = stats,
                                    # diffs = chk_stats,
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
                                    full_data = chk_stats,
                                    # diffs = chk_diff,
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
                                  # diffs = chk_diff,
                                  custom_grp = unique(current$breakdown),
                                  group_of_interest = grp,
                                  q_coded = q_coded,
                                  top = NA)
      
      if(comp == "schyear"){
        
        order <- c("Year 7", "Year 8", "Year 9", "Year 10", "Year 11", "Year 12", "Year 13", "All Responses", "Not at school/other")
        
        current_plot <- current 
        
        current_plot$breakdown <- factor(current$breakdown, levels = unique(order))
        
      }else{
        
        current_plot <- current
        
        order <- unique(current_plot$breakdown)
      }
      
      # interactive plot
      int_plot <- create_basic_plot(df = current_plot,
                                    plot_custom_grp = order,
                                    plot_title = "")

      # trend table
      if (nrow(current_old) > 0) {
        
        trend_plot <- create_trend_plot(df = trend,
                                        plot_custom_grp = order,
                                        plot_title = "")
        
      } else {
        
        trend_plot <- "Trend data cannot be generated as this question was not in last year's survey."
        
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
                                                                   shiny::br,
                                                                   int_plot
                                                   ),
                                                   shiny::tabPanel(
                                                     "Trend",
                                                     shiny::br,
                                                     trend_plot
                                                   ),
                                                   shiny::tabPanel(
                                                     "Table",
                                                     chk_stats %>% 
                                                       dplyr::mutate(value = paste0(round(as.numeric(value) * 100, 2), "%"),
                                                                     lowercl = paste0(round(as.numeric(lowercl) * 100, 2), "%"),
                                                                     uppercl = paste0(round(as.numeric(uppercl) * 100, 2), "%")
                                                       ) %>% 
                                                       dplyr::filter(question_coded_gen %in% chk_var[i]) %>% 
                                                       dplyr::select(breakdown, question = question_text, response, percent = value, 
                                                                     count, total = denominator,
                                                                     `lower CI` = lowercl, `upper CI` = uppercl) %>% 
                                                       distinct %>% 
                                                       reactable::reactable(groupBy = c("breakdown", "question"),
                                                                            columns = list(
                                                                              percent = reactable::colDef(maxWidth = 68),
                                                                              count = reactable::colDef(maxWidth = 65),
                                                                              total = reactable::colDef(maxWidth = 65),
                                                                              `lower CI` = reactable::colDef(maxWidth = 75),
                                                                              `upper CI` = reactable::colDef(maxWidth = 75)
                                                                            ))
                                                   )) )
  }
  
} else {
  
  l[[1]] <- bs4Dash::tabItem("name", 
                             bs4Dash::bs4TabCard(width = 12, side = "right", status = "success",
                                                 collapsible = FALSE,
                                                 title = shiny::HTML(paste0(input$domains[1],"<br>")),
                                                 shiny::tabPanel(title = NULL, "No data available for the selected year.")))
  
}