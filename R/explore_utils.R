
# Generate text -----------------------------------------------------------

compare_last_yr <- function(df_new, 
                            df_old,
                            diffs_all,
                            multicat = F, 
                            response_interest = NA) {
  
  if (nrow(df_old) == 0) { return("") }
  
  # filtering to key groups
  df_new <- df_new %>%
    dplyr::filter(breakdown == "All Responses")
  
  df_old <- df_old %>%
    dplyr::filter(breakdown == "All Responses")
  
  df_old_comp <- dplyr::mutate(df_old, breakdown = "Previous All Responses")
  
  new_diffs <- diffs_all %>% 
    dplyr::filter(!is.na(breakdown)) %>% 
    dplyr::mutate(question_text = ifelse(is.na(question_text), question_text.y, question_text))
  
  if (nrow(new_diffs) == 0) { return("") }
  
  # match number of rows so cols can be binded
  if(nrow(df_new) > nrow(df_old)) {
    
    df_new <- df_new %>% 
      dplyr::semi_join(df_old, by = c("breakdown", "question", "response"))
    
  } else if (nrow(df_new) < nrow(df_old)) {
    
    df_old <- df_old %>% 
      dplyr::semi_join(df_new, by = c("breakdown", "question", "response"))
    
  }
  
  names(df_old) <- paste0(names(df_old), "_old")
  
  # category of interest may be different depending on type of question ( single vs multi-category ). Also needs extra cleaning. 
  if (multicat == F) { 
    
    resp_var <- "response" 
    
  } else { 
    
    resp_var <- "question" 
    df_old <- df_old %>% 
      dplyr::filter(response_old == response_interest)
    
    df_new <- df_new %>% 
      dplyr::filter(response == response_interest)
    
  }
  
  # main df with comparisons and correct text for sentences
  df <- dplyr::bind_cols(df_new, df_old) %>% 
    dplyr::filter(breakdown %in% c("All Responses")) %>% 
    dplyr::left_join(select(new_diffs, response, question, diff), by = unique(c(resp_var, "response"))) %>% 
    dplyr::mutate(val_comp = dplyr::case_when(value > value_old ~ "HIGHER than", 
                                              value < value_old ~ "LOWER than", 
                                              value == value_old ~ "the SAME as"),
                  val_diff = abs(value - value_old),
                  diff = dplyr::case_when(is.na(diff) ~ "statistically similar to", 
                                          diff == "significantly higher than" ~ "statistically HIGHER than", 
                                          diff == "significantly lower than" ~ "statistically LOWER than"),
                  question_text = ifelse(is.na(question_text), question_text_old, question_text_old)) %>% 
    dplyr::distinct(breakdown, 2, response, .keep_all = TRUE)
  
  # if response_interest isn't given, default to the category with highest proportion and those that had differences.
  if (is.na(response_interest)[1]) {
    
    main_grp <- c(df[[resp_var]][df$value == max(df$value)][1],
                  df[[resp_var]][df$diff != "statistically SIMILAR to"])
    main_grp <- sort(unique(main_grp))
    
  } else {
    
    main_grp <- c(response_interest, df[[resp_var]][df$diff != "statistically similar to"])
    main_grp <- sort(unique(main_grp))
    
  }
  
  # If question is multicategory, sentence needs to be changed
  if (multicat) { 
    addition <- paste0(" for '", df$question_text, "'")
    main_grp <- rep(response_interest, nrow(df))
  } else {
    addition <- rep("", length(main_grp))
  }
  
  # get rows of interest for the sentences.
  main_df <- df %>% 
    dplyr::filter(breakdown == "All Responses", response %in% main_grp) %>% 
    dplyr::arrange(!!dplyr::ensym(resp_var))
  
  main_val <- round(as.numeric(dplyr::pull(main_df, value)) * 100, 1) #percentage
  main_comp <- dplyr::pull(main_df, diff) #whether it's higher/lower/same
  main_val_old <- round(as.numeric(dplyr::pull(main_df, value_old)) * 100, 1) #percentage of previous year
  
  # main sentence
  sentence <- glue::glue("This year, {main_val[1]}% answered '{main_grp[1]}'{addition[1]}, which was {main_comp[1]} last year ({main_val_old[1]}%).")
  
  # if a vector is given for response_interest, adjust the sentence so it summarises more than one category.
  if (length(main_val) > 1) {
    
    if (length(main_val) == 2) { 
      
      sentence <- glue::glue("{sentence} Additionally, {main_val[-1]}% answered '{main_grp[-1]}'{addition[-1]}, which was {main_comp[-1]} last year ({main_val_old[-1]}%).")
      
    } else if (length(main_val) > 2) {
      
      main_df <- df %>% 
        dplyr::filter(breakdown == "All Responses", response %in% main_grp,
                      diff != "statistically similar to") %>% 
        dplyr::arrange(!!dplyr::ensym(resp_var))
      
      main_val <- round(as.numeric(dplyr::pull(main_df, value)) * 100, 1) #percentage
      main_comp <- dplyr::pull(main_df, diff) #whether it's higher/lower/same
      main_val_old <- round(as.numeric(dplyr::pull(main_df, value_old)) * 100, 1) #percentage of previous year
      
      main_comp <- gsub(" than| to", "", main_comp)
      
      sentence <- glue::glue("{sentence} Additionally, {
                 glue::glue_collapse(glue::glue(
                 'the proportion of young people that answered {glue::single_quote({main_grp[-1]})} {addition[-1]} was {main_comp[-1]}')
                 , ', ', last = ', and ')}.")
      
    }
  }
  
  return(sentence)
  
}

create_sum_sentence <- function(dataset, 
                                dataset_old, 
                                multi = F, 
                                value_of_interest = F, 
                                full_data,
                                # diffs,
                                custom_grp,
                                group_of_interest,
                                q_coded,
                                top = NA) {
  
  if (nrow(dataset) == 0) { return ("") }
  
  if (!multi) { # run the following if it's a simple one-choice question
    
    q_binary <- F
    
    data <- dataset
    df <- dataset[dataset$breakdown == "All Responses", ]
    
    # generate the values used for the sentences. 
    if(nrow(df) > 0) {
      
      most_common <- df$response[df$count == max(df$count) & !is.na(df$question_text)][1]
      most_v <- df$value[df$count == max(df$count) & !is.na(df$question_text)][1]
      least_common <- df$response[df$count == min(df$count[df$count >= 0 & !is.na(df$question_text)])][1]
      least_v <- df$value[df$count == min(df$count[df$count >= 0 & !is.na(df$question_text)])][1]
      
      # If all students responded to this question, skip the sex breakdown. Include if not. 
      
      total_resp <- dataset$denominator[dataset$breakdown == "All Responses"][1]
      max_resp <- max(full_data$denominator)
      
      perc <- round(total_resp / max_resp * 100, 1)
      
      total_resp <- ifelse(total_resp == max_resp, "every student", 
                           paste0(total_resp, " students", " (", perc, "%)"))
      
      # generate main sentence for All Responses
      sentence <- paste0("Respondents were asked '<b>", df$survey_text_gen[1], "</b>'.<br><br>")
      
      if (is.na(total_resp)) { 
        
        return(paste0("No students responded to this question."))
        
      } else {
        
        # trend <- compare_last_yr(df_new = dataset, 
        #                          df_old = dataset_old, 
        #                          multi = F,
        #                          diffs_all = diffs,
        #                          response_interest = NA)
        
        if (!is.na(group_of_interest)[1]) {
          
          grp_df <- data %>% 
            dplyr::filter(breakdown %in% group_of_interest) %>% 
            dplyr::group_by(breakdown) %>% 
            dplyr::filter(denominator == max(denominator)) %>% 
            dplyr::ungroup() %>% 
            dplyr::select(breakdown, denominator) %>% 
            dplyr::distinct()
          
          prop <- paste0(round(grp_df$denominator / max(data$denominator, na.rm = T)[1] * 100, 2), "%")
          add <- paste0(" Among them, ", paste0(
            prop, " were ", grp_df$breakdown, collapse = ", "), ". ")
          
          sentence <- paste0(sentence, "Within Hertfordshire, ", total_resp, " responded to this question. " , add, 
                             # trend, 
                             "<br> <br> The most common response for all respondents was '", most_common[1], "', which made up <b>", most_v[1], 
                             "</b> of responses and the least common response was '", least_common[1], "', with <b>",
                             least_v[1], "</b> of responses.")
          
          df1 <- dataset[dataset$breakdown %in% group_of_interest, ]
          
          most_common <- df1 %>%
            dplyr::group_by(breakdown) %>% 
            dplyr::filter(count == max(count), !is.na(question_text), !duplicated(response)) %>% 
            dplyr::summarise(most_common = paste(response, collapse = "' or '"), most_v = min(value)) %>% 
            dplyr::ungroup() %>% 
            dplyr::distinct() 
          
          least_common <- df1 %>% 
            dplyr::group_by(breakdown) %>% 
            dplyr::filter(count == min(count), !is.na(question_text), !duplicated(response)) %>% 
            dplyr::summarise(least_common = paste(response, collapse = "' or '"), least_v = min(value)) %>%
            dplyr::ungroup() %>% 
            dplyr::distinct() 
          
          sentence <- paste(sentence, "<br><br>", paste0("The most common response for <b>", group_of_interest, "s</b> was '", most_common$most_common, 
                                                         "', which made up ", most_common$most_v, " of responses and the least common response was '", least_common$least_common, "', with ", least_common$least_v, " of responses.", 
                                                         collapse = "<br><br>"))
          
        } else {
          
          sentence <- paste0("Within Hertfordshire, ", total_resp, " responded to this question. ", 
                             # trend,
                             "<br> <br> The most common response for all respondents was '", most_common[1], "', which made up <b>", most_v[1], 
                             "</b> of responses and the least common response was '", least_common[1], "', with <b>",
                             least_v[1], "</b> of responses.")
        }
        
      }
      
    } else {return(NULL)}
    
    
  } else { #run the following instead if it's a multi-check question
    
    q_coded <- dplyr::select(q_coded, order, question_raw, question_theme, question_text, 
                             reworded, question_coded) %>% 
      dplyr::distinct()
    
    reps <- unique(dataset$question)
    data <- dplyr::filter(dataset, !is.na(question_text))
    
    total_resp <- max(data$denominator)[1]
    max_resp <- max(full_data$denominator)[1]
    
    perc <- round(total_resp / max_resp * 100, 1)
    
    total_resp <- ifelse(total_resp == max_resp, "every student", 
                         paste0(total_resp, " students", " (", perc, "%)"))
    
    # generate main sentence for All Responses
    sentence <- paste0("Respondents were asked <b>'", data$survey_text_gen[1], "</b><br><br>")
    
    if (is.na(total_resp)) { 
      
      return(paste0("No students responded to this question."))
      
    } else {
      
      # Start main sentence. 
      sentence <- paste0(sentence, "Within Hertfordshire, ", total_resp, " responded to this question. ")
      
      binary <- ifelse(all(unique(data$response) %in% c("Yes", "No")), TRUE, FALSE) # check if it's a Yes or No
      
      # get trend sentence
      # trend <- compare_last_yr(df_new = dataset,
      #                          df_old = dataset_old,
      #                          diffs_all = diffs, 
      #                          multicat = T,
      #                          response_interest = value_of_interest)
      
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
          prop, " were ", grp_df$breakdown, collapse = ", "), ". " 
          # trend
          )
        
      } else {
        
        sentence <- paste0(sentence
                           # trend
                           )
        
      }
      
      for(group in 1:length(na.omit(c("All Responses", group_of_interest)))) {
        
        grp_df <- data %>% 
          dplyr::filter(breakdown == c("All Responses", group_of_interest)[group]) 
        
        group_name <- ifelse(unique(grp_df$breakdown) == "All Responses", "all students", 
                             paste0("<b>", grp_df$breakdown, "s</b>"))
        
        # generate the values used for the sentences. 
        df <- grp_df %>% 
          dplyr::filter(question %in% reps, response_of_interest == "TRUE") %>% 
          dplyr::left_join(select(q_coded, reworded, question_coded), by = c("question" = "question_coded")) %>% 
          #drop_na(reworded) %>% 
          dplyr::arrange(dplyr::desc(count)) %>%
          dplyr::select(-tidyselect::contains(".y"), -order, -question_type, -polarity, -rotation) %>%
          dplyr::distinct()
        
        # if we only want the top N responses, subset df
        
        if (!is.na(top)) { 
          df <- df %>% 
            dplyr::arrange(desc(value)) %>% 
            dplyr::slice(1:top)
        } 
        
        if (binary) {
          
          temp <- paste0("Out of responses from ", group_name, ", ", 
                         glue::glue_collapse(glue::glue("<b>{df$value}</b> selected '{df$question_text}'"), ", ", last = ", and "))
        } else {
          temp <- paste0("The number of ", group_name, " who stated '", df$response[1], "' was ",
                         glue::glue_collapse(glue::glue("<b>{df$value}</b> for '{df$question_text}'"), ", ", last = ", and "))
        }
        
        sentence <- paste0(sentence, temp, ".<br><br>")
        
      }
      
    }
  }
  
  # Add a short prompt(?) sentence.
  sentence <- paste0(sentence, " For more detail, please see the graph or table. Please note that all count data is rounded to the nearest 5 for data confidentiality purposes.")
  
  return(sentence)
  
}

# Graphs --------------------------------------------------------------

create_basic_plot <- function(df, 
                              plot_custom_grp, 
                              plot_title,
                              rotate = 0) {
  
  
  groups <- unique(c("All Responses", 
                     plot_custom_grp))
  
  rotate <- ifelse(length(unique(df$response)) > 7, 45, 0)
  
  # plot object
  df %>% 
    dplyr::filter(breakdown %in% groups) %>% 
    dplyr::mutate(value = round(as.numeric(.$value), 3),
                  lowercl = round(as.numeric(.$lowercl), 3),
                  uppercl = round(as.numeric(.$uppercl), 3),
                  response = stringr::str_wrap(response, 15),
                  breakdown = factor(breakdown, levels = groups)) %>% 
    dplyr::filter(!is.na(question_text)) %>% 
    dplyr::group_by(breakdown) %>% 
    echarts4r::e_charts(response) %>% 
    echarts4r::e_bar(value, name = .$breakdown, tooltip = list(formatter = htmlwidgets::JS("
      function(params){
      return('value: ' + params.value[1] * 100 + '%' +
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
    echarts4r::e_y_axis(name = "Percent", nameLocation = "middle", nameGap = 35, min = 0) %>% 
    echarts4r::e_x_axis(axisLabel = list(interval = 0, rotate = rotate)) %>% 
    echarts4r::e_format_y_axis(suffix = "%", formatter = echarts4r::e_axis_formatter("percent")) %>% 
    echarts4r::e_legend(show = TRUE, type = "scroll", orient = "vertical",
                        right = 10, top = 35, bottom = 10,
                        itemHeight = 10, itemWidth = 20,
                        textStyle = list(fontSize = 12)) %>%
    echarts4r::e_theme("westeros") %>%
    echarts4r::e_title(plot_title) %>% 
    echarts4r::e_image_g(right = 180, top = 0, z = -999, style = list(opacity = 0.5, width = 120,
                                                                      image = "https://www.hertshealthevidence.org/images/young-peoples-health-and-wellbeing-survey-logo-png-Cropped-448x190.png")) %>%
    echarts4r::e_toolbox_feature(feature = c("dataZoom", "restore")) %>% 
    echarts4r::e_theme_custom("phei.json")
  
}

create_multi_plot <- function(df,
                              plot_title,
                              binary) {
  
  df <- df %>%
    dplyr::filter(!is.na(question_text)) %>%
    droplevels()

  if (binary) {

    df %>%
      dplyr::filter(response == "Yes") %>%
      dplyr::arrange(response) %>%
      plotly::plot_ly(x = ~value, y = ~question_text, type = "bar", name = ~breakdown, color = ~breakdown,
                      colors = "viridis", legendgroup = ~breakdown, orientation = 'h',
                      hovertemplate = ~paste(stringr::str_wrap(paste0(value, " (", count, ") in the ", breakdown, " breakdown replied ",
                                                                      response, "<br>(CI:", lowercl, " to ",
                                                                      uppercl, ")"), 30), "<extra></extra>")) %>%
      plotly::layout(title = list(text = paste("<b>", plot_title, "</b>"),
                                  yanchor = "bottom", y = 1.3, x = 0, font = list(size= 12)),
                     xaxis = list(title = "Percent", tickformat = ".1%"),
                     yaxis = list(title = "", autorange = "reversed")) %>%
      plotly::config(displaylogo = FALSE,
                     modeBarButtons = list(list("toImage", "zoomIn2d", "zoomOut2d", "pan2d", "resetScale2d", "hoverClosestCartesian")))

  } else {

    groups <- unique(df$menu_text)

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

    df %>%
      dplyr::arrange(response) %>%
      plotly::plot_ly(x = ~value, y = ~breakdown, type = "bar", name = ~response, color = ~response,
                      colors = "viridis", legendgroup = ~response, orientation = 'h',
                      hovertemplate = ~paste(stringr::str_wrap(paste0(value, " (", count, ") in the ", breakdown, " breakdown replied ",
                                                                      response, "<br>(CI:", lowercl, " to ",
                                                                      uppercl, ")"), 30), "<extra></extra>"),
                      transforms = list(list(type = "filter", target = ~menu_text, operator = '=', value = groups[1]))) %>%
      plotly::layout(barmode = "stack",
                     title = list(text = paste("<b>", plot_title, "</b>"), yanchor = "bottom", y = 1.3, x = 0, font = list(size= 12)),
                     xaxis = list(title = "Percent", tickformat = ".1%"),
                     updatemenus = list(type_list),
                     yaxis = list(title = "", autorange = "reversed")) %>%
      plotly::config(displaylogo = FALSE,
                     modeBarButtons = list(list("toImage", "zoomIn2d", "zoomOut2d", "pan2d", "resetScale2d", "hoverClosestCartesian")))

  }
  
  
}

create_trend_plot <- function(df, 
                              plot_custom_grp, 
                              plot_title,
                              year,
                              multi = FALSE) {

  # df=trend
  # plot_custom_grp = order
  # plot_title = ""
  # year = year()

    groups <- unique(c("All Responses", 
                       plot_custom_grp))
    
    rotate <- ifelse(length(unique(df$response)) > 7, 45, 0)
    
    if (multi) {
      
      trend_opts <- unique(df$menu_text[df$year == year])
      
      # If it's actually a simple question with multiple responses of interest
      if (all(is.na(unique(trend_opts)))) {
        trend_opts <- unique(df$response[df$year == year])
        
        base <- df %>% 
          dplyr::filter(breakdown == "All Responses",
                        response %in% trend_opts) %>% 
          dplyr::group_by(response) %>% 
          dplyr::mutate(temp_sum = 1,
                        keep = case_when(sum(temp_sum) > 3 ~ TRUE,
                                         TRUE ~ FALSE)) %>% 
          dplyr::ungroup() %>% 
          dplyr::filter(keep) %>% 
          dplyr::mutate(value = round(as.numeric(.$value), 3),
                        lowercl = round(as.numeric(.$lowercl), 3),
                        uppercl = round(as.numeric(.$uppercl), 3),
                        response = stringr::str_wrap(response, 15),
                        breakdown = factor(breakdown, levels = groups),
                        year = factor(year, levels = sort(unique(df$year)))) %>% 
          dplyr::arrange(year) %>% 
          dplyr::filter(!is.na(question_text)) %>% 
          dplyr::group_by(response)
        
        legend_length <- ifelse(max(nchar(df$response) > 40), -100, 10)
        
      } else {
        
        base <- df %>% 
          dplyr::filter(breakdown == "All Responses",
                        menu_text %in% trend_opts) %>% 
          dplyr::group_by(menu_text) %>% 
          dplyr::mutate(temp_sum = 1,
                        keep = case_when(sum(temp_sum) > 3 ~ TRUE,
                                         TRUE ~ FALSE)) %>% 
          dplyr::ungroup() %>% 
          dplyr::filter(keep) %>% 
          dplyr::mutate(value = round(as.numeric(.$value), 3),
                        lowercl = round(as.numeric(.$lowercl), 3),
                        uppercl = round(as.numeric(.$uppercl), 3),
                        response = stringr::str_wrap(response, 15),
                        breakdown = factor(breakdown, levels = groups),
                        year = factor(year, levels = sort(unique(df$year)))) %>% 
          dplyr::arrange(year) %>% 
          dplyr::filter(!is.na(question_text)) %>% 
          dplyr::group_by(menu_text)
        
        legend_length <- ifelse(max(nchar(df$menu_text) > 40), 100, 10)
        
      }

      if(nrow(base) == 0) return("Trend data cannot be generated as this question does not have enough yearly data.")
      
      base %>% 
        echarts4r::e_charts(year) %>% 
        echarts4r::e_line(value, name = .$breakdown, tooltip = list(formatter = htmlwidgets::JS("
      function(params){
      return('value: ' + params.value[1] * 100 + '%' +
        '<br/>breakdown: ' + params.seriesName +
        '<br/>group: ' + params.value[params.encode.x[0]]) 
        }"))) %>% 
        echarts4r::e_tooltip(trigger = "item") %>% 
        echarts4r::e_grid(right = 180, left = 50) %>%
        echarts4r::e_y_axis(name = "Percent", nameLocation = "middle", nameGap = 35, min = 0) %>% 
        echarts4r::e_x_axis(axisLabel = list(interval = 0, rotate = rotate)) %>% 
        echarts4r::e_format_y_axis(suffix = "%", formatter = echarts4r::e_axis_formatter("percent")) %>% 
        echarts4r::e_legend(show = TRUE, type = "scroll", orient = "vertical",
                            right = legend_length, top = 55, bottom = 10,
                            itemHeight = 10, itemWidth = 20,
                            textStyle = list(fontSize = 12)) %>%
        #echarts4r::e_theme("westeros") %>%
        echarts4r::e_title(plot_title) %>% 
        echarts4r::e_image_g(right = 180, top = 0, z = -999, style = list(opacity = 0.5, width = 120,
                                                                          image = "https://www.hertshealthevidence.org/images/young-peoples-health-and-wellbeing-survey-logo-png-Cropped-448x190.png")) %>%
        echarts4r::e_toolbox_feature(feature = c("dataZoom", "restore"))
      
    } else {
      
      trend_opts <- unique(df$breakdown[df$year == year])
      
      legend_length <- ifelse(max(nchar(df$breakdown) > 40), -100, 10)
      
      df %>% 
        dplyr::filter(breakdown %in% trend_opts) %>% 
        dplyr::group_by(breakdown) %>% 
        dplyr::mutate(temp_sum = 1,
                      keep = case_when(sum(temp_sum) > 3 ~ TRUE,
                                       TRUE ~ FALSE)) %>% 
        dplyr::ungroup() %>% 
        dplyr::filter(keep) %>%
        dplyr::mutate(value = round(as.numeric(.$value), 3),
                      lowercl = round(as.numeric(.$lowercl), 3),
                      uppercl = round(as.numeric(.$uppercl), 3),
                      response = stringr::str_wrap(response, 15),
                      breakdown = factor(breakdown, levels = groups),
                      year = factor(year, levels = sort(unique(df$year)))) %>% 
        dplyr::arrange(year) %>% 
        dplyr::filter(!is.na(question_text)) %>% 
        dplyr::group_by(breakdown) %>% 
        echarts4r::e_charts(year) %>% 
        echarts4r::e_line(value, name = .$breakdown, tooltip = list(formatter = htmlwidgets::JS("
      function(params){
      return('value: ' + params.value[1] * 100 + '%' +
        '<br/>breakdown: ' + params.seriesName +
        '<br/>group: ' + params.value[params.encode.x[0]]) 
        }"))) %>% 
        echarts4r::e_tooltip(trigger = "item") %>% 
        echarts4r::e_grid(right = 180, left = 50) %>%
        echarts4r::e_y_axis(name = "Percent", nameLocation = "middle", nameGap = 35, min = 0) %>% 
        echarts4r::e_x_axis(axisLabel = list(interval = 0, rotate = rotate)) %>% 
        echarts4r::e_format_y_axis(suffix = "%", formatter = echarts4r::e_axis_formatter("percent")) %>% 
        echarts4r::e_legend(show = TRUE, type = "scroll", orient = "vertical",
                            right = legend_length, top = 35, bottom = 10,
                            itemHeight = 10, itemWidth = 20,
                            textStyle = list(fontSize = 12)) %>%
        echarts4r::e_theme("westeros") %>%
        echarts4r::e_title(plot_title) %>% 
        echarts4r::e_image_g(right = 180, top = 0, z = -999, style = list(opacity = 0.5, width = 120,
                                                                          image = "https://www.hertshealthevidence.org/images/young-peoples-health-and-wellbeing-survey-logo-png-Cropped-448x190.png")) %>%
        echarts4r::e_toolbox_feature(feature = c("dataZoom", "restore")) 
      
    }
  
}

# Tables ------------------------------------------------------------------

create_tbl <- function(stats_diff, 
                       multi_response = F, 
                       include_school = F) {
  
  if (district_comparison == T) {stats_diff <- filter(stats_diff, school.y != paste0(district, " Schools"),
                                                      breakdown != "All Responses in District")}
  
  if (multi_response == F) {
    
    groupedby <- "breakdown"
    district_level <- ifelse(unique(stats_diff$breakdown.x)[2] %in% plot_dis_grp, T, F)
    
    data <- stats_diff %>% 
      dplyr::mutate(difference = dplyr::case_when(lowercl.x > uppercl.y ~ "higher",
                                                  uppercl.x < lowercl.y ~ "lower",
                                                  TRUE ~ "similar")) 
    
    data1 <- data %>% 
      dplyr::rename(count = count.x, percentage = value.x, `all schools` = value.y,
                    `comparison to all schools` = difference, `lower CI` = lowercl.x,
                    `upper CI` = uppercl.x) %>% 
      dplyr::left_join(q_coded, by = c("question_text.x" = "question_text")) %>% 
      dplyr::select(1, response, count, percentage, `lower CI`, `upper CI`, 
                    `all schools`, `comparison to all schools`) %>% 
      dplyr::mutate(`upper CI` = as.character(`upper CI`),
                    `lower CI` = as.character(`lower CI`),
                    `all schools` = as.character(`all schools`),
                    percentage = as.character(percentage)) %>% 
      rename(breakdown = breakdown.x)
    
    data_ <- data %>% 
      dplyr::select(breakdown.y, response, count.y, value.y, lowercl.y, uppercl.y) %>% 
      dplyr::rename(breakdown = breakdown.y, count = count.y, percentage = value.y, 
                    `lower CI` = lowercl.y, `upper CI` = uppercl.y) %>% 
      dplyr::mutate(`upper CI` = as.character(`upper CI`),
                    `lower CI` = as.character(`lower CI`),
                    percentage = as.character(percentage)) 
    
    
  } else {
    
    groupedby <- c("breakdown", "question")
    data <- stats_diff %>% 
      mutate(difference = case_when(lowercl.x > uppercl.y ~ "higher",
                                    uppercl.x < lowercl.y ~ "lower",
                                    TRUE ~ "similar")) %>% 
      dplyr::rename(count = count.x, percentage = value.x, `all schools` = value.y,
                    `comparison to all schools` = difference, 
                    `lower CI` = lowercl.x,
                    `upper CI` = uppercl.x) 
    
    if(length(unique(data$response)) <= 2) {
      
      data1 <- data %>% 
        dplyr::left_join(q_coded, by = c("question" = "question_coded")) %>% 
        dplyr::mutate(question = menu_text) %>% 
        dplyr::select(1, question, response, count, percentage, `lower CI`, `upper CI`, 
                      `all schools`, `comparison to all schools`) %>% 
        dplyr::distinct() %>% 
        dplyr::mutate(`upper CI` = as.character(`upper CI`),
                      `lower CI` = as.character(`lower CI`),
                      `all schools` = as.character(`all schools`),
                      percentage = as.character(percentage))
      
      
    } else {
      
      data1 <- data %>% 
        dplyr::left_join(q_coded, by = c("question" = "question_text")) %>% 
        dplyr::select(1, question, response, count, percentage, `lower CI`, `upper CI`, 
                      `all schools`, `comparison to all schools`) %>% 
        dplyr::distinct() %>% 
        dplyr::mutate(`upper CI` = as.character(`upper CI`),
                      `lower CI` = as.character(`lower CI`),
                      `all schools` = as.character(`all schools`),
                      percentage = as.character(percentage))
      
    }
    
    data_ <- data %>% 
      dplyr::left_join(q_coded, by = c("question" = "question_coded")) %>% 
      dplyr::select(breakdown.y, menu_text, response, count.y, `all schools`, lowercl.y, uppercl.y) %>% 
      dplyr::rename(breakdown = breakdown.y, count = count.y, percentage = `all schools`, 
                    `lower CI` = lowercl.y, `upper CI` = uppercl.y, question = menu_text) %>% 
      dplyr::mutate(`upper CI` = as.character(`upper CI`),
                    `lower CI` = as.character(`lower CI`),
                    percentage = as.character(percentage))
    
  }
  
  if (nrow(data) == 0) { # return the message below if there is nothing to show
    
    cat("Data suppressed due to low amount of responses.")
    
  } else {
    
    names(data1)[1] <- "breakdown"
    
    if (include_school == T) { # if school report, include schools percentage
      
      data1 %>%
        dplyr::arrange(breakdown) %>% 
        reactable::reactable(groupBy = groupedby,
                             columns = list(
                               percentage = reactable::colDef(format = reactable::colFormat(percent = T, digits = 1)),
                               `all schools` = reactable::colDef(format = reactable::colFormat(percent = T, digits = 1)),
                               `comparison to all schools` = reactable::colDef(style = JS(
                                 "function(rowInfo) {
                var value = rowInfo.row['comparison to all schools']
                if (value == 'similar') {
                var color = '#ffc000'} else {
                if (value == 'lower') {
                var color = '#5555e5' } else {
                var color = '#bed2ff' }
                }
                return { color: color, fontWeight: 'bold' }
                }"), minWidth = 100)
                             ))
      
    } else { # if overview report, exclude schools percentage
      
      data <- data1 %>% 
        dplyr::select(-`comparison to all schools`, -`all schools`) %>% 
        dplyr::bind_rows(data_)
      
      data %>% 
        dplyr::distinct() %>% 
        dplyr::arrange(breakdown) %>% 
        reactable::reactable(defaultPageSize = 10, groupBy = groupedby, striped = T,
                             columns = list(
                               percentage = reactable::colDef(format = reactable::colFormat(percent = T, digits = 1))
                             ))
      
      
    }
    
    
  }
  
}

create_trend_table <- function(stats,
                               stats_old,
                               year) {
  
  prev_yr <- as.character(as.numeric(year) - 1)
  
  table_df <- stats %>%
    dplyr::mutate(year = year, 
                  !!dplyr::ensym(year) := value) %>%
    dplyr::left_join(stats_old, by = c("breakdown" = "prev_breakdown",
                                       "question" = "prev_question",
                                       "response" = "prev_response")) %>%
    dplyr::mutate(!!dplyr::ensym(prev_yr) := ifelse(!is.na(prev_value), round(as.numeric(prev_value), 4) * 100, 0),
                  !!dplyr::ensym(year) := ifelse(!is.na(!!dplyr::ensym(year)), round(as.numeric(!!dplyr::ensym(year)), 4) * 100, 0),
                  !!dplyr::ensym(prev_yr) := ifelse(is.na(!!dplyr::ensym(prev_yr)), 0, !!dplyr::ensym(prev_yr)),
                  !!dplyr::ensym(prev_yr) := ifelse(is.na(!!dplyr::ensym(prev_yr)), 0, !!dplyr::ensym(prev_yr)),
                  Trend = purrr::map2(!!dplyr::ensym(prev_yr), !!dplyr::ensym(year), c), 
                  Change = round(!!dplyr::ensym(year) - !!dplyr::ensym(prev_yr), 2)) %>%
    dplyr::select(Indicator = question_response, Group = breakdown, !!dplyr::ensym(prev_yr), !!dplyr::ensym(year), 
                  Trend, Change) %>% 
    dplyr::mutate(!!dplyr::ensym(prev_yr) := paste0(!!dplyr::ensym(prev_yr), "%"),
                  !!dplyr::ensym(year) := paste0(!!dplyr::ensym(year), "%")) %>% 
    dplyr::filter(!is.na(Indicator))
  
  table_df %>%
    dplyr::distinct() %>% 
    reactable::reactable(defaultSorted = c("Indicator", "Group"), defaultPageSize = 100,
                         sortable = FALSE,
                         columns = list(
                           Indicator = reactable::colDef(
                             sortable = F,
                             style = htmlwidgets::JS("function(rowInfo, colInfo, state) {
        const firstSorted = state.sorted[0]
        // Merge cells if unsorted or sorting by school
        if (!firstSorted || firstSorted.id === 'Indicator') {
          const prevRow = state.pageRows[rowInfo.viewIndex - 1]
          if (prevRow && rowInfo.row['Indicator'] === prevRow['Indicator']) {
            return { visibility: 'hidden' }
          }
        }
      }")),
      Trend = reactable::colDef(
        cell = function(value, index) {
          sparkline::sparkline(table_df$Trend[[index]], 
                               chartRangeMin = 0, chartRangeMax = 100)
        }
      ),
      Change = reactable::colDef(
        header = shiny::span("Change", class = "sr-only"),
        align = "center",
        width = 40,
        cell = function(value) trend_indicator(value)
      )),
      outlined = TRUE
    )
  
}

# Icon to indicate trend: unchanged, up, down, or new
trend_indicator <- function(value) {
  
  label <- ifelse(value > 0, "up", 
                  ifelse(value < 0, "down", "unchanged"))
  
  # Add img role and tooltip/label for accessibility
  args <- list(role = "img", title = label)
  
  if (label == "unchanged") {
    args <- c(args, list("–", style = "color: #666; font-weight: 700"))
  } else if (label == "up") {
    args <- c(args, list(shiny::icon("caret-up"), style = "color: #1ed760"))
  } else if (label == "down") {
    args <- c(args, list(shiny::icon("caret-down"), style = "color: #cd1a2b"))
  } else {
    args <- c(args, list(shiny::icon("circle"), style = "color: #2e77d0; font-size: 10px"))
  }
  do.call(span, args)
}

