
# Functions

create_report_chunk <- function(df, 
                                theme
) {
  
  vars <- unique(vars_df$question_coded_gen[vars_df$question_theme == theme])
  
  # Exception for PA question that is formatted differently.
  if (all(c("pa_60", "pa") %in% vars)) vars <- vars[-which(vars == "pa")]
  
  # Determine which questions are simple vs. complex questions
  vars_simple <- vars_df %>% 
    filter(multi == FALSE, question_theme == theme) %>% 
    arrange(order) %>% 
    pull(question_coded_gen) %>% 
    unique()
  
  vars_multi <- vars_df %>% 
    filter(multi == TRUE, question_theme == theme) %>% 
    arrange(order) %>% 
    pull(question_coded_gen) %>% 
    unique()
  
  ## CHUNK list
  livcon_title_list <- list()
  livcon_summary_list <- list()
  livcon_diffs_list <- list()
  livcon_plot_list <- list()
  livcon_plot2_list <- list()
  livcon_table_list <- list()
  
  chk_var <- unique(vars_df$vars[vars_df$question_coded_gen == theme])
  
  for (i in vars) {
    
    simple <- ifelse(i %in% vars_simple, TRUE, FALSE)
    chk_var <- i
    
    # Suppress questions with less than 5 responses
    n_responses <- stats %>% 
      filter(question %in% chk_var, year == as.character(current_year), breakdown == "All Responses")
    
    if (nrow(n_responses) > 0) {
      if (max(n_responses$denominator) < 6) {
        
        plot_title <- vars_df$heading[vars_df$vars == chk_var][1]
        
        livcon_summary_list[[i]] <- glue::glue("Unfortunately this question had 5 or less responses so no data is presented.")
        
        livcon_diffs_list[[i]] <- glue::glue(" ")
        
        livcon_plot_list[[i]] <- glue::glue("Unfortunately this question had 5 or less responses so no data is presented.")
        
        livcon_plot2_list[[i]] <- glue::glue(" ")
        
        livcon_table_list[[i]] <- glue::glue("Unfortunately this question had 5 or less responses so no data is presented.")
        
        livcon_title_list[[i]] <- paste0("## ", plot_title, " {.tabset}\n\n**'", 
                                         vars_df$survey_text[vars_df$vars == i][1],
                                         "'**")
        
        list <- list(title_list = livcon_title_list,
                     summary_list = livcon_summary_list,
                     diffs_list = livcon_diffs_list,
                     plot_list = livcon_plot_list,
                     plot2_list = livcon_plot2_list,
                     table_list = livcon_table_list)
        
        return(list)
        
      }
    }
    
    # If there are enough responses, proceed with full set of contents
    if (simple) {
      
      chk_stats <- filter(stats, question %in% chk_var)
      chk_diff_sch <- filter(stats_diffs_sch, question %in% chk_var)
      chk_diff_yr <- filter(stats_diffs_yr, question %in% chk_var)
      chk_diff_gend <- filter(stats_diffs_sex, question %in% chk_var)
      
      plot_title <- vars_df$heading[vars_df$vars == chk_var][1]
      plot_angle <- ifelse(length(unique(chk_stats$response)) > 6, 60, 0)
      
      # bullet_df <- create_bullets(df = chk_diff_sch, 
      #                             df_yr = chk_diff_yr,
      #                             df_gend = chk_diff_gend,
      #                             district = district)
      # 
      # livcon_diffs_list[[i]] <- bullet_df$Table
      
      livcon_summary_list[[i]] <- paste0(create_sum_sentence_herts(dataset = chk_stats, 
                                                                   multi = F, 
                                                                   value_of_interest = F, 
                                                                   full_data = stats))
      # diffs = bullet_df$Data,
      # lookup = q_coded,
      # skip_sig_diffs = F,
      # var = chk_var))
      # 
      
      
      livcon_plot_list[[i]] <- create_basic_plot(df = chk_stats,
                                                 chk_diff_sch, 
                                                 plot_title = plot_title)
      
      # livcon_plot2_list[[i]] <- create_basic_plot(df = chk_stats, diff_df = chk_diff_sch, sex = F, plot_title = plot_title, year = current_year, textangle = plot_angle)
      
      livcon_table_list[[i]] <- create_tbl(chk_diff_sch)
      
      livcon_title_list[[i]] <- paste0("## ", plot_title, " {.tabset}\n\n**'", 
                                       vars_df$survey_text[vars_df$vars == i][1],
                                       "'**")
      
    } else {
      
      chk_var <- unique(vars_df$vars[vars_df$question_coded_gen == i])
      chk_stats <- filter(stats, question %in% chk_var)
      chk_diff <- dplyr::filter(stats_diffs, question %in% chk_var)
      chk_diff_sch <- filter(stats_diffs_sch, question %in% chk_var)
      chk_diff_yr <- filter(stats_diffs_yr, question %in% chk_var)
      chk_diff_gend <- filter(stats_diffs_sex, question %in% chk_var)
      plot_title <- vars_df$heading[vars_df$vars %in% chk_var][1]
      response_interest <- vars_df$value_of_interest[vars_df$question_coded_gen == i][1]
      
      # If chk_var needs filtering due to too many responses for the summary text
      if (!all(is.na(vars_df$response_filter_top[vars_df$question_coded_gen == i]))) {
        
        filter_to <- as.numeric(vars_df$response_filter_top[vars_df$question_coded_gen == i])
        
        chk_var_filtered <- chk_stats %>%
          filter(response == response_interest, breakdown == "All Responses") %>%
          arrange(desc(count)) %>%
          slice(1:filter_to) %>%
          pull(question)
        
      } else { chk_var_filtered <- chk_var }
      
      # bullet_df <- create_bullets(df = chk_diff_sch,
      #                             df_yr = chk_diff_yr,
      #                             df_gend = chk_diff_gend,
      #                             district = district)
      # 
      # livcon_diffs_list[[i]] <- bullet_df$Table
      
      livcon_summary_list[[i]] <- paste0(create_sum_sentence_herts(dataset = chk_stats, 
                                                                   multi = T, 
                                                                   value_of_interest = response_interest, 
                                                                   full_data = stats))
      # diffs = bullet_df$Data,
      # lookup = q_coded,
      # skip_sig_diffs = F,
      # var = chk_var_filtered))
      
      livcon_plot_list[[i]] <- create_multicat_plot_herts(df = chk_stats, plot_title, q_coded = q_coded)$result
      
      livcon_plot2_list[[i]] <- glue::glue(" ")
      
      livcon_table_list[[i]] <- create_tbl(chk_diff_sch, multi_response = T)
      
      livcon_title_list[[i]] <- paste0("## ", plot_title, " {.tabset}\n\n**'", 
                                       vars_df$survey_text_gen[vars_df$vars %in% chk_var][1],  "'**")
      
    }
    
  }
  
  list <- list(title_list = livcon_title_list,
               summary_list = livcon_summary_list,
               diffs_list = livcon_diffs_list,
               plot_list = livcon_plot_list,
               plot2_list = livcon_plot2_list,
               table_list = livcon_table_list)
  
  return(list)
  
}

paste_chunk_contents <- function(header, 
                                 name, 
                                 var) {
  
  paste0('\n\n', header, '\n\n### Summary\n\n`r ', name, '_summary_list[["', var,
         '"]]`\n\n```{r ,results="asis", echo=FALSE, warning=FALSE, message=FALSE}\n\n', name, '_diffs_list[["', var,
         '"]]\n\n```\n\n### Graph\n\n```{r ,results="asis", echo=FALSE, warning=FALSE, message=FALSE}\n\n', name, '_plot_list[["', var,
         '"]]\n\n```\n\n<br>\n\n```{r ,results="asis", echo=FALSE, warning=FALSE, message=FALSE}\n\n', name, '_plot2_list[["', var,
         '"]]\n\n```\n\n### Table\n\n```{r ,results="asis", echo=FALSE, warning=FALSE, message=FALSE}\n\n', name, '_table_list[["', var, '"]]\n\n```')
  
}

get_stats_diffs <- function(stats,
                            levels,
                            compare_to_all = F) {
  
  stats <- dplyr::filter(stats, breakdown %in% levels)
  
  # get differences (new cols)
  data_from <- stats
  data_to <- stats[stats[["school"]] %in% "All Schools", ]
  stats_diffs_all <- dplyr::inner_join(data_from, data_to, by = c(NULL, "question", "response")) %>%
    dplyr::mutate(diff = dplyr::case_when(lowercl.x > uppercl.y ~ "significantly higher than",
                                          uppercl.x < lowercl.y ~ "significantly lower than"),
                  image = paste0("graphics/", diff, ".png"))
  
  # clean factors
  stats_diffs_all$breakdown.x <- factor(stats_diffs_all$breakdown.x, levels = levels)
  stats_diffs_all$breakdown.x <- droplevels(stats_diffs_all$breakdown.x)
  
  if(compare_to_all) {
    stats_diffs_all <- stats_diffs_all %>%
      dplyr::filter(school.y == "All Schools") %>%
      dplyr::filter(!breakdown.x == "All Responses") %>%
      dplyr::filter(breakdown.y == "All Responses")
  }
  
  return(stats_diffs_all)
  
}


summarystats <- function(data, by, q_coded, omit_pivot = F) {
  # calculate summary metrics for all responses by specified groups
  # by should be a vector of columns to group by
  
  box::use(magrittr[`%>%`])
  
  if (omit_pivot) {
    
    data %>% 
      dplyr::group_by_at(dplyr::vars( {{ by }} , question)) %>%
      dplyr::rename(breakdown = by[2]) %>%
      dplyr::mutate(count = plyr::round_any(count, 5), # disclosure control - round to base 5
                    denominator = sum(count)) %>% # redo denom with altered count
      dplyr::ungroup() %>% 
      dplyr::filter(denominator >= 5) %>% # disclosure control & CI calculation - filter out less than 5
      PHEindicatormethods::phe_proportion(., count, denominator, type='standard', confidence = 0.998) %>% 
      dplyr::mutate(question_text = q_coded$question_text[match(question,  #add friendly names
                                                                q_coded$question_coded)]) %>% 
      dplyr::select(breakdown, school, question, question_text, dplyr::everything()) #reorder columns
    
  } else {
    
    data %>% 
      tidyr::pivot_longer(cols = -{{ by }} ,
                          names_to = c("question"), values_to = "response") %>%
      dplyr::group_by_all() %>%
      dplyr::filter(!is.na(response)) %>%
      dplyr::summarise(count = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::group_by_at(dplyr::vars( {{ by }} , question)) %>%
      dplyr::mutate(denominator = sum(count)) %>%
      dplyr::rename(breakdown = by[2]) %>%
      dplyr::mutate(count = plyr::round_any(count, 5), # disclosure control - round to base 5
                    denominator = sum(count)) %>% # redo denom with altered count
      dplyr::ungroup() %>% 
      dplyr::filter(denominator >= 5) %>% # disclosure control & CI calculation - filter out less than 5
      PHEindicatormethods::phe_proportion(., count, denominator, type='standard', confidence = 0.998) %>% 
      dplyr::mutate(question_text = q_coded$question_text[match(question,  #add friendly names
                                                                q_coded$question_coded)]) %>% 
      dplyr::select(breakdown, school, question, question_text, dplyr::everything()) #reorder columns
    
    
  }
  
}

differences <- function(data, by, from, to, join = NULL) {
  # looks for sig differences between report area and all responses
  # e.g. data, school, report_school, all_schools, schoolyear returns differences
  # between the respective year of the report school and all schools 
  # e.g. data, school, report_school, report_school returns differences between
  # each year of the report school (includes comaring year to itself)
  
  box::use(magrittr[`%>%`])
  
  data_from <- data[data[[by]] == from, ]
  data_to <- data[data[[by]] %in% to, ] 
  
  df <- dplyr::inner_join(data_from, data_to, by = c(join, "question", "response")) %>%
    dplyr::mutate(diff = dplyr::case_when(lowercl.x > uppercl.y ~ "significantly higher than",
                                          uppercl.x < lowercl.y ~ "significantly lower than"),
                  image = paste0("graphics/", diff, ".png"))
  
  return(df)
}

get_stats_diffs <- function(stats, 
                            levels, 
                            compare_to_all = F,
                            school = NA){
  
  box::use(magrittr[`%>%`])
  
  stats <- dplyr::filter(stats, breakdown %in% levels)
  
  # Check whether we are comparing one school to all schools
  if(!is.na(school)) {
    
    if (compare_to_all) {
      stats_diffs_all <- differences(stats, "school", school, "All Schools")
    } else {
      stats_diffs_all <- differences(stats, "school", school, c("All Schools", school))
    }
    
    
  } else {
    
    if(compare_to_all) {
      stats_diffs_all <- differences(stats, "school", "All Schools", "All Schools")
    } else {
      stats_diffs_all <- differences(stats, "school", "All Schools", c("All Schools", school))
    }
    
  }
  
  stats_diffs_all$breakdown.x <- factor(stats_diffs_all$breakdown.x, levels = levels)
  stats_diffs_all$breakdown.x <- droplevels(stats_diffs_all$breakdown.x)
  
  if(compare_to_all) {
    
    stats_diffs_all <- stats_diffs_all %>% 
      dplyr::filter(school.y == "All Schools") %>% 
      dplyr::filter(!breakdown.x == "All Responses") %>% 
      dplyr::filter(breakdown.y == "All Responses")
    
  }
  
  return(stats_diffs_all)
  
}


#
create_tbl <- function(stats_diff, multi_response = F, include_school = F) {
  
  stats_diff = chk_diff
  if (multi_response == F) {
    
    groupedby <- "breakdown"
    
    data1 <- stats_diff %>% 
      dplyr::rename(percentage = value,
                    `lower CI` = lowercl,
                    `upper CI` = uppercl) %>%
      #dplyr::left_join(select(q_coded, -response), by = c("question_text" = "question_text")) %>% 
      dplyr::select(breakdown, response, count, percentage, `lower CI`, `upper CI`) %>% 
      dplyr::mutate(`upper CI` = as.character(`upper CI`),
                    `lower CI` = as.character(`lower CI`))
    
    
  } else {
    
    groupedby <- c("breakdown", "question")
    data <- stats_diff %>% 
      #dplyr::mutate(difference = case_when(diff == "significantly higher than" ~ "higher",
      #diff == "significantly lower than" ~ "lower",
      #TRUE ~ "similar")) %>% 
      dplyr::rename(percentage = value,
                    `lower CI` = lowercl,
                    `upper CI` = uppercl) 
    
    if(length(unique(data$response)) <= 2) {
      
      data1 <- data %>% 
        dplyr::left_join(select(q_coded, question_coded, menu_text), by = c("question" = "question_coded")) %>% 
        dplyr::mutate(question = menu_text) %>% 
        dplyr::select(1, question, response, count, percentage, `lower CI`, `upper CI`) %>% 
        dplyr::distinct() %>% 
        dplyr::mutate(`upper CI` = as.character(`upper CI`),
                      `lower CI` = as.character(`lower CI`))
      
      
    } else {
      
      data1 <- data %>% 
        dplyr::left_join(select(q_coded, question_coded, menu_text), by = c("question" = "question_coded"))  %>% 
        dplyr::select(1, question=question_text, response, count, percentage, `lower CI`, `upper CI`) %>% 
        dplyr::distinct() %>% 
        dplyr::mutate(`upper CI` = as.character(`upper CI`),
                      `lower CI` = as.character(`lower CI`))
      
    }
  }
  
  if (nrow(data1) == 0) { # return the message below if there is nothing to show
    
    cat("Data suppressed due to low amount of responses.")
    
  } else {
    
    data1 %>% 
      dplyr::distinct() %>% 
      dplyr::arrange(breakdown) %>% 
      reactable::reactable(defaultPageSize = 10, groupBy = groupedby, striped = T,
                           columns = list(
                             percentage = reactable::colDef(format = reactable::colFormat(percent = T, digits = 1))
                           ))
  }
  
}

# Summary statistics ------------------------------------------------------

create_sum_sentence_cat_interest <- function(dataset = chk_stats, 
                                             multi = F, 
                                             value_of_interest = F, 
                                             full_data = stats,
                                             diffs = chk_diff,
                                             custom_grp = plot_custom_grp,
                                             group_of_interest = cat_of_interest) {
  
  if (nrow(dataset) == 0) { return ("") }
  
  if (multi == F) { # run the following if it's a simple one-choice question
    
    q_binary <- F
    
    data <- dataset
    # generate the values used for the sentences. 
    
    df <- dataset[dataset$breakdown == "All Responses", ]
    
    most_common <- df$response[df$count == max(df$count) & !is.na(df$question_text)]
    most_v <- df$value[df$count == max(df$count) & !is.na(df$question_text)]
    least_common <- df$response[df$count == min(df$count[df$count >= 0 & !is.na(df$question_text)])]
    least_v <- df$value[df$count == min(df$count[df$count >= 0 & !is.na(df$question_text)])][1]
    
    # If all students responded to this question, skip the sex breakdown. Include if not. 
    
    total_resp <- sum(dataset$count[dataset$breakdown == "All Responses"], na.rm = TRUE)
    max_resp <- max(full_data$denominator)
    
    perc <- round(total_resp / max_resp * 100, 1)
    
    total_resp <- ifelse(total_resp == max_resp, "every student", 
                         paste0(total_resp, " students", " (", perc, "%)"))
    
    # generate main sentence for All Responses
    
    if (is.na(total_resp)) { 
      
      return(paste0("Within the category, no students responded to this question."))
      
    } else {
      
      sentence <- paste0("Within Hertfordshire, ", total_resp, " responded to this question. ",
                         "<br> <br> The most common response for all respondents was '", most_common[1], "', which made up ", most_v[1], 
                         " of responses and the least common response was '", least_common[1], "', with ",
                         least_v[1], " of responses.", " For more detail, please see the Graph or Table tabs.")
    }
  } else{
    reps <- unique(dataset$question)
    data <- filter(dataset, !is.na(question_text))
    
    total_resp <- max(data$denominator)[1]
    max_resp <- max(full_data$denominator)[1]
    
    perc <- round(total_resp / max_resp * 100, 1)
    
    total_resp <- ifelse(total_resp == max_resp, "every student", 
                         paste0(total_resp, " students", " (", perc, "%)"))
    
    if (is.na(total_resp)) { 
      
      return(paste0("Within the school, no students responded to this question."))
      
    } else {
      
      # Start main sentence. 
      sentence <- paste0("Within Hertfordshire, ", total_resp, " responded to this question. ")
      
    }
  }
  
  
  return(sentence)
  
}



create_sum_sentence_herts <- function(dataset = chk_stats, 
                                      multi = F, 
                                      value_of_interest = F, 
                                      full_data = stats,
                                      diffs = chk_diff,
                                      custom_grp = plot_custom_grp,
                                      group_of_interest = cat_of_interest) {
  
  if (nrow(dataset) == 0) { return ("") }
  
  if (multi == F) { # run the following if it's a simple one-choice question
    
    q_binary <- F
    
    data <- dataset
    # generate the values used for the sentences. 
    
    df <- dataset[dataset$breakdown == "All Responses", ]
    
    most_common <- df$response[df$count == max(df$count) & !is.na(df$question_text)]
    most_v <- df$value[df$count == max(df$count) & !is.na(df$question_text)]
    least_common <- df$response[df$count == min(df$count[df$count >= 0 & !is.na(df$question_text)])]
    least_v <- df$value[df$count == min(df$count[df$count >= 0 & !is.na(df$question_text)])][1]
    
    # If all students responded to this question, skip the sex breakdown. Include if not. 
    
    total_resp <- sum(dataset$count[dataset$breakdown == "All Responses"], na.rm = TRUE)
    max_resp <- max(full_data$denominator)
    
    perc <- round(total_resp / max_resp * 100, 1)
    
    total_resp <- ifelse(total_resp == max_resp, "every student", 
                         paste0(total_resp, " students", " (", perc, "%)"))
    
    # generate main sentence for All Responses
    
    if (is.na(total_resp)) { 
      
      return(paste0("Within the school, no students responded to this question."))
      
    } else {
      
      if (!is.na(group_of_interest)[1]) {#& chk_var != group
        
        grp_df <- data %>% 
          dplyr::filter(breakdown %in% group_of_interest) %>% 
          dplyr::group_by(breakdown) %>% 
          dplyr::filter(denominator == max(denominator)) %>% 
          dplyr::ungroup() %>% 
          dplyr::select(breakdown, denominator) %>% 
          dplyr::distinct()
        
        prop <- paste0(round(grp_df$denominator / max(data$denominator, na.rm = T)[1] * 100, 1), "%")
        add <- paste0(" Among them, ", paste0(
          prop, " were ", grp_df$breakdown, collapse = ", "), ". ")
        
        sentence <- paste0("Within Hertfordshire, ", total_resp, " responded to this question." , add, 
                           "<br> <br> The most common response for all respondents was '", most_common[1], "', which made up ", most_v[1], 
                           " of responses and the least common response was '", least_common[1], "', with ",
                           least_v[1], " of responses.")
        
        df1 <- dataset[dataset$breakdown %in% group_of_interest , ]
        
        most_common <- df1 %>%
          dplyr::group_by(breakdown) %>% 
          dplyr::filter(count == max(count), !is.na(question_text)) %>% 
          dplyr::summarise(most_common = paste(response, collapse = "' or '"),
                           most_v = value) %>% 
          dplyr::ungroup() %>% 
          dplyr::distinct() 
        
        least_common <- df1 %>% 
          dplyr::group_by(breakdown) %>% 
          dplyr::filter(count == min(count), !is.na(question_text), ) %>% 
          dplyr::summarise(least_common = paste(response, collapse = "' or '"),
                           least_v = value) %>% 
          dplyr::ungroup() %>% 
          dplyr::distinct() 
        
        sentence <- paste(sentence, "<br><br>", paste0("The most common response for ", group_of_interest, " was '", most_common$most_common, 
                                                       "', which made up ", most_common$most_v, " of responses and the least common response for ", 
                                                       group_of_interest, " was '", least_common$least_common, "', with ", least_common$least_v, " of responses.", 
                                                       collapse = "<br><br>"))
        
      } else {
        
        sentence <- paste0("Within Hertfordshire, ", total_resp, " responded to this question. ",
                           "<br> <br> The most common response for all respondents was '", most_common[1], "', which made up ", most_v[1], 
                           " of responses and the least common response was '", least_common[1], "', with ",
                           least_v[1], " of responses.")
      }
      
    }
    
    
  } else { #run the following instead if it's a multi-check question
    
    reps <- unique(dataset$question)
    data <- filter(dataset, !is.na(question_text))
    
    total_resp <- max(data$denominator)[1]
    max_resp <- max(full_data$denominator)[1]
    
    perc <- round(total_resp / max_resp * 100, 1)
    
    total_resp <- ifelse(total_resp == max_resp, "every student", 
                         paste0(total_resp, " students", " (", perc, "%)"))
    
    if (is.na(total_resp)) { 
      
      return(paste0("Within the school, no students responded to this question."))
      
    } else {
      
      # Start main sentence. 
      sentence <- paste0("Within Hertfordshire, ", total_resp, " responded to this question. ")
      
      binary <- ifelse(all(unique(data$response) %in% c("Yes", "No")), TRUE, FALSE) # check if it's a Yes or No
      
      if(!is.na(group_of_interest)) {
        
        grp_df <- data %>% 
          dplyr::filter(breakdown %in% group_of_interest) %>% 
          dplyr::group_by(breakdown) %>% 
          dplyr::filter(denominator == max(denominator)) %>% 
          dplyr::ungroup() %>% 
          dplyr::select(breakdown, denominator) %>% 
          dplyr::distinct()
        
        prop <- paste0(round(grp_df$denominator / max(data$denominator, na.rm = T)[1] * 100, 2), "%")
        sentence <- paste0(sentence, "Among them, ", paste0(
          prop, " were ", grp_df$breakdown, collapse = ", "), ". ")
        
        for(group in 1:length(c("All Responses", group_of_interest))) {
          
          grp_df <- data %>% 
            dplyr::filter(breakdown == c("All Responses", group_of_interest)[group]) 
          
          group_name <- ifelse(unique(grp_df$breakdown) == "All Responses", "students", grp_df$breakdown)
          
          # generate the values used for the sentences. 
          df <- grp_df %>% 
            dplyr::filter(question %in% reps, response == value_of_interest) %>% 
            dplyr::arrange(desc(count))
          
          if (binary) {
            temp <- paste0("Out of responses from ", group_name, ", ", 
                           glue::glue_collapse(glue::glue("{df$value} selected '{df$question_text}'"), ", ", last = ", and "))
          } else{
            temp <- paste0("The number of ", group_name, " who stated '", df$response[1], "' was ",
                           glue::glue_collapse(glue::glue("{df$value} for '{df$question_text}'"), ", ", last = ", and "))
          }
          
          sentence <- paste0(sentence, temp, ".<br><br>")
          
        }
      }
    }
  }
  
  # Add a short prompt(?) sentence.
  sentence <- paste0(sentence, " For more detail, please see the Graph or Table tabs.")
  
  return(sentence)
  
}

create_basic_plot <- function(df = test, 
                              plot_custom_grp, 
                              plot_title,
                              rotate = 0) {
  
  groups <- c("All Responses", 
              plot_custom_grp)
  
  chk_var <- unique(vars_df$vars[vars_df$question_coded_gen == theme])
  
  # plot object
  df %>% 
    dplyr::filter(breakdown %in% groups) %>%
    dplyr::mutate(value = round(as.numeric(.$value), 3),
                  lowercl = round(as.numeric(.$lowercl), 3),
                  uppercl = round(as.numeric(.$uppercl), 3),
                  response = stringr::str_wrap(response, 15)) %>% 
    # breakdown = factor(breakdown, levels = groups)) %>% 
    dplyr::filter(question %in% chk_var, 
                  !is.na(question_text)) %>% 
    dplyr::group_by(breakdown) %>% 
    echarts4r::e_charts(response) %>% 
    echarts4r::e_bar(value, name = .$breakdown, tooltip = list(formatter = htmlwidgets::JS("
      function(params){
      return('value: ' + Math.round(params.value[1] * 100, 3) + '%' +
        '<br/>breakdown: ' + params.seriesName +
        '<br/>group: ' + params.value[params.encode.x[0]]) 
        }"))) %>%
    echarts4r::e_error_bar(lowercl, uppercl, name = .$breakdown,
                           itemStyle = list(opacity = 0.6),
                           tooltip = list(formatter = htmlwidgets::JS("
      function(params){
      return('breakdown: ' + params.seriesName +
      '</br>group: ' + params.value[params.encode.x[0]] +
      '<br/>upper CI: ' + Math.round(params.value[params.encode.y[1]] * 100, 3) +
        '%<br/>lower CI: ' + Math.round(params.value[params.encode.y[0]] * 100, 3) +
        '%') }"))) %>%
    echarts4r::e_tooltip(trigger = "item") %>% 
    echarts4r::e_grid(right = 180, left = 50) %>%
    echarts4r::e_y_axis(name = "Percent", nameLocation = "middle", nameGap = 35, max = 1, min = 0) %>% 
    echarts4r::e_x_axis(axisLabel = list(interval = 0, rotate = rotate)) %>% 
    echarts4r::e_format_y_axis(suffix = "%", formatter = echarts4r::e_axis_formatter("percent")) %>% 
    echarts4r::e_legend(show = TRUE, type = "scroll", orient = "vertical",
                        right = 10, top = 35, bottom = 10,
                        itemHeight = 10, itemWidth = 20,
                        textStyle = list(fontSize = 11)) %>%
    echarts4r::e_theme("westeros") %>%
    echarts4r::e_title(plot_title) %>% 
    echarts4r::e_image_g(right = 180, top = 0, z = -999, style = list(opacity = 0.5, width = 120,
                                                                      image = "https://www.hertshealthevidence.org/images/young-peoples-health-and-wellbeing-survey-logo-png-Cropped-448x190.png")) %>%
    echarts4r::e_toolbox_feature(feature = c("dataZoom", "restore"))
  
}

create_multicat_plot_herts <- function(df = chk_stats, title = plot_title, q_coded = q_coded, by = "percent") {
  
  df <- chk_stats %>% dplyr::filter(!is.na(question_text)) %>% 
    droplevels() 
  
  df <- df %>% dplyr::arrange(response) %>% 
    dplyr::left_join(select(q_coded, question_coded, menu_text), by = c("question" = "question_coded"))
  
  df$menu_text <- factor(df$menu_text)
  
  groups <- levels(df$menu_text)
  
  if (by == "percent") { groups <- unique(df$menu_text) } else { groups <- unique(df$breakdown) }
  
  button_list <- lapply(1:length(groups), function(x){
    list(method = "restyle",
         args = list("transforms[0].value", groups[x]),
         label = groups[x])
  })
  
  type_list <-  list(
    type = 'dropdown',
    active = 0,
    xanchor = 'left',
    yanchor = "top",
    pad = list('r'= 0, 't'= 0, 'b' = 0),
    y = 1.1,
    x = 1.02,
    buttons = button_list
  )
  
  if (by == "percent") {
    
    dplyr::group_by(df, breakdown, response, menu_text) %>%
      dplyr::arrange(response) %>%
      plotly::plot_ly(x = ~value, y = ~breakdown, type = "bar", name = ~response, color = ~response,
                      colors = "Blues", legendgroup = ~response, orientation = 'h',
                      hovertemplate = ~paste(stringr::str_wrap(paste0(count, " in the ", breakdown, " breakdown replied ", response, "<br>(", value,
                                                                      " [CI:", lowercl, " to ", uppercl, "])"), 30), "<extra></extra>"),
                      transforms = list(list(type = "filter", target = ~menu_text, operator = '=', value = groups[1]))) %>%
      plotly::layout(barmode = "stack", 
                     title = list(text = paste("<b>", plot_title, "</b>"), yanchor = "bottom", y = 1.3, x = 0, font = list(size= 12)),
                     xaxis = list(title = "Percent", tickformat = ".1%"),
                     updatemenus = list(type_list),
                     autosize = F, height = 600,
                     margin = list(r = 250,
                                   t = 0,
                                   l = 0,
                                   b = 0),
                     #hovermode = 'compare',
                     yaxis = list(title = "", autorange = "reversed")) %>%
      plotly::config(displaylogo = FALSE, modeBarButtons = list(list("toImage", "pan2d", "resetScale2d", "hoverClosestCartesian")))
    
    
  } else {
    
    dplyr::group_by(df, question_text, response, breakdown) %>%
      dplyr::filter(response == "Yes") %>% 
      dplyr::arrange(response) %>%
      dplyr::mutate(question_text = stringr::str_wrap(question_text, 25)) %>%
      plotly::plot_ly(x = ~count, y = ~question_text, type = "bar", name = ~response,
                      colors = "Blues", legendgroup = ~response, orientation = 'h',
                      transforms = list(list(type = "filter", target = ~breakdown, operator = '=', value = groups[1]))) %>%
      plotly::layout(barmode = "stack", title = list(text = paste("<b>", plot_title, "</b>"), xanchor = "left", y = 0.85, x = 0, font = list(size = 12)),
                     xaxis = list(title = "Count of 'Yes'"),
                     updatemenus = list(type_list),
                     autosize = F, height = 800,
                     margin = list(r = 400,
                                   t = 0,
                                   l = 0,
                                   b = 0),
                     hovermode = 'compare',
                     yaxis = list(title = "", autorange = "reversed")) %>%
      plotly::config(displaylogo = FALSE, modeBarButtons = list(list("toImage", "pan2d", "resetScale2d", "hoverClosestCartesian")))
    
  }
  
}

create_multicat_plot_herts <- purrr::quietly(create_multicat_plot_herts)
