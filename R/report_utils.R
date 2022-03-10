
# Generate text -----------------------------------------------------------

create_sum_sentence <- function(dataset, 
                                multi = F, 
                                value_of_interest = F, 
                                full_data,
                                diffs,
                                custom_grp,
                                group_of_interest,
                                q_coded) {
  
  if (nrow(dataset) == 0) { return ("") }
  
  if (multi == F) { # run the following if it's a simple one-choice question
    
    q_binary <- F
    
    data <- dataset
    # generate the values used for the sentences. 
    
    df <- dataset[dataset$breakdown == "All Responses" & dataset$school == "All Schools", ]
    
    most_common <- df$response[df$count == max(df$count) & !is.na(df$question_text)]
    most_v <- df$value[df$count == max(df$count) & !is.na(df$question_text)]
    least_common <- df$response[df$count == min(df$count[df$count >= 0 & !is.na(df$question_text)])]
    least_v <- df$value[df$count == min(df$count[df$count >= 0 & !is.na(df$question_text)])][1]
    
    # If all students responded to this question, skip the sex breakdown. Include if not. 
    
    total_resp <- sum(dataset$count[dataset$breakdown == "All Responses" &
                                      dataset$school == "All Schools"], na.rm = TRUE)
    max_resp <- max(full_data$denominator)
    
    perc <- round(total_resp / max_resp * 100, 1)
    
    total_resp <- ifelse(total_resp == max_resp, "every student", 
                         paste0(total_resp, " students", " (", perc, "%)"))
    
    # generate main sentence for All Responses
    
    if (is.na(total_resp)) { 
      
      return(paste0("Within the school, no students responded to this question."))
      
    } else {
      
      if (!is.na(group_of_interest)[1]) {
        
        grp_df <- data %>% 
          filter(breakdown %in% group_of_interest) %>% 
          group_by(breakdown) %>% 
          filter(denominator == max(denominator)) %>% 
          ungroup() %>% 
          select(breakdown, denominator) %>% 
          distinct()
        
        prop <- paste0(round(grp_df$denominator / max(data$denominator, na.rm = T)[1] * 100, 2), "%")
        add <- paste0(" Among them, ", paste0(
          prop, " were ", grp_df$breakdown, collapse = ", "), ". ")
        
        sentence <- paste0("Within Hertfordshire, ", total_resp, " responded to this question." , add, 
                           "<br> <br> The most common response for all respondents was '", most_common[1], "', which made up ", most_v[1], 
                           " of responses and the least common response was '", least_common[1], "', with ",
                           least_v[1], " of responses.")
        
        df1 <- dataset[dataset$breakdown %in% group_of_interest & dataset$school == "All Schools", ]
        
        most_common <- df1 %>%
          group_by(breakdown) %>% 
          filter(count == max(count), !is.na(question_text)) %>% 
          summarise(most_common = paste(response, collapse = "' or '"),
                    most_v = value) %>% 
          ungroup() %>% 
          distinct() 
        
        least_common <- df1 %>% 
          group_by(breakdown) %>% 
          filter(count == min(count), !is.na(question_text), ) %>% 
          summarise(least_common = paste(response, collapse = "' or '"),
                    least_v = value) %>% 
          ungroup() %>% 
          distinct() 
        
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
          filter(breakdown %in% group_of_interest) %>% 
          group_by(breakdown) %>% 
          filter(denominator == max(denominator)) %>% 
          ungroup() %>% 
          select(breakdown, denominator) %>% 
          distinct()
        
        prop <- paste0(round(grp_df$denominator / max(data$denominator, na.rm = T)[1] * 100, 2), "%")
        sentence <- paste0(sentence, "Among them, ", paste0(
          prop, " were ", grp_df$breakdown, collapse = ", "), ". ")
        
        for(group in 1:length(c("All Responses", group_of_interest))) {
          
          grp_df <- data %>% 
            filter(breakdown == c("All Responses", group_of_interest)[group]) 
          
          group_name <- ifelse(unique(grp_df$breakdown) == "All Responses", "students", grp_df$breakdown)
          
          # generate the values used for the sentences. 
          df <- grp_df %>% 
            filter(question %in% reps, response == value_of_interest) %>% 
            left_join(q_coded, by = c("question" = "question_coded")) %>% 
            #drop_na(reworded) %>% 
            arrange(desc(count))
          
          if (binary) {
            temp <- paste0("Out of responses from ", group_name, ", ", 
                           glue_collapse(glue("{df$value} selected '{df$question_text.x}'"), ", ", last = ", and "))
          } else{
            temp <- paste0("The number of ", group_name, " who stated '", df$response[1], "' was ",
                           glue_collapse(glue("{df$value} for '{df$question_text.x}'"), ", ", last = ", and "))
          }
          
          sentence <- paste0(sentence, temp, ".<br><br>")
          
        }
      }
    }
  }
  
  # Add a short prompt(?) sentence.
  sentence <- paste0(sentence, " For more detail, please see the graph or table.")
  
  return(sentence)
  
}

compare_last_yr <- function(df_new, 
                            df_old,
                            var,
                            sch = "All Schools", 
                            multicat = F, 
                            response_interest = NA) {
  
  # df_new <- stats
  # df_old <- stats_old
  # var <- chk_var
  # if there were no responses to this question, print message:   
  if(!sch %in% unique(df_new$school[df_new$question == chk_var])) { return("") }
  
  if (!sch %in% df_old$school) { return("") }
  # filtering to key groups
  df_new <- df_new %>%
    dplyr::filter(question %in% var, breakdown == "All Responses")
  
  df_old <- df_old %>%
    dplyr::filter(question %in% var, breakdown == "All Responses")
  
  df_old_comp <- mutate(df_old, breakdown = "Previous All Responses")
  
  new_diffs <- yphws$get_stats_diffs(bind_rows(df_new, df_old_comp), 
                                     levels = c("All Responses", "Previous All Responses"), 
                                     school = sch) %>% 
    filter(breakdown.x == "All Responses", 
           breakdown.y == "Previous All Responses",
           school.y != "All Schools") %>% 
    mutate(question_text.x = case_when(is.na(question_text.x) ~ question_text.y, TRUE ~ question_text.x))
  
  names(new_diffs) <- gsub(".x", "", names(new_diffs))
  
  if (nrow(new_diffs) == 0) { return("") }
  
  # match number of rows so cols can be binded
  if(nrow(df_new) > nrow(df_old)) {
    
    df_new <- df_new %>% 
      dplyr::semi_join(df_old, by = c("breakdown", "school", "question", "response"))
    
  } else if (nrow(df_new) < nrow(df_old)) {
    
    df_old <- df_old %>% 
      dplyr::semi_join(df_new, by = c("breakdown", "school", "question", "response"))
    
  }
  
  names(df_old) <- paste0(names(df_old), "_old")
  
  # category of interest may be different depending on type of question ( single vs multi-category ). Also needs extra cleaning. 
  if (multicat == F) { 
    
    resp_var <- "response" 
    
  } else { 
    
    resp_var <- "question" 
    df_old <- df_old %>% 
      filter(response_old == response_interest)
    
    df_new <- df_new %>% 
      filter(response == response_interest)
    
  }
  
  # main df with comparisons and correct text for sentences
  df <- dplyr::bind_cols(df_new, df_old) %>% 
    dplyr::filter(breakdown %in% c("All Responses"),
                  school == sch) %>% 
    left_join(select(new_diffs, response, question, diff), by = unique(c(resp_var, "response"))) %>% 
    dplyr::mutate(school = dplyr::case_when(school == "All Schools" ~ "Hertfordshire", TRUE ~ school),
                  school_old = dplyr::case_when(school_old == "All Schools" ~ "Hertfordshire", TRUE ~ school_old),
                  val_comp = case_when(value > value_old ~ "HIGHER than", 
                                       value < value_old ~ "LOWER than", 
                                       value == value_old ~ "the SAME as"),
                  val_diff = abs(value - value_old),
                  diff = case_when(is.na(diff) ~ "statistically similar to", 
                                   diff == "significantly higher than" ~ "statistically HIGHER than", 
                                   diff == "significantly lower than" ~ "statistically LOWER than"),
                  question_text = case_when(is.na(question_text) ~ question_text_old, TRUE ~ question_text_old)) %>% 
    dplyr::distinct()
  
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
    addition <- paste0("")
  }
  
  # get rows of interest for the sentences.
  main_df <- df %>% 
    dplyr::filter(school == sch, breakdown == "All Responses", response %in% main_grp) %>% 
    arrange(!!ensym(resp_var))
  
  main_val <- round(as.numeric(dplyr::pull(main_df, value)) * 100, 1) #percentage
  main_comp <- dplyr::pull(main_df, diff) #whether it's higher/lower/same
  main_val_old <- round(as.numeric(dplyr::pull(main_df, value_old)) * 100, 1) #percentage of previous year
  
  if (multicat == FALSE) { addition <- rep("", length(main_val)) }
  
  # main sentence
  sentence <- glue::glue("This year, {main_val[1]}% of those who responded from {sch} answered '{main_grp[1]}'{addition[1]}, which was {main_comp[1]} last year ({main_val_old[1]}%).")
  
  # if a vector is given for response_interest, adjust the sentence so it summarises more than one category.
  if (length(main_val) > 1) {
    
    if (length(main_val) == 2) { 
      
      sentence <- glue::glue("{sentence} Additionally, {main_val[-1]}% answered '{main_grp[-1]}'{addition[-1]}, which was {main_comp[-1]} last year ({main_val_old[-1]}%).")
      
    } else if (length(main_val) > 2) {
      
      main_comp <- gsub(" than| to", "", main_comp)
      
      glue::glue("{sentence} Additionally, {
                 glue::glue_collapse(glue::glue(
                 'the proportion of young people that answered {glue::single_quote({main_grp[-1]})} {addition[-1]} was {main_comp[-1]}')
                 , ', ', last = ', and ')}.") -> sentence
      
    }
  }
  
  return(sentence)
  
}

# Graphs --------------------------------------------------------------

create_basic_plot <- function(df, 
                              plot_custom_grp, 
                              plot_title,
                              rotate = 0) {
  
  groups <- unique(c("All Responses", 
                     plot_custom_grp))
  
    # plot object
    df %>% 
      filter(breakdown %in% groups) %>% 
      mutate(value = round(as.numeric(.$value), 3),
             lowercl = round(as.numeric(.$lowercl), 3),
             uppercl = round(as.numeric(.$uppercl), 3),
             response = str_wrap(response, 15),
             breakdown = factor(breakdown, levels = groups)) %>% 
      filter(!is.na(question_text)) %>% 
      group_by(breakdown) %>% 
      e_charts(response) %>% 
      e_bar(value, name = .$breakdown, tooltip = list(formatter = htmlwidgets::JS("
      function(params){
      return('value: ' + Math.round(params.value[1] * 100, 3) + '%' +
        '<br/>breakdown: ' + params.seriesName +
        '<br/>group: ' + params.value[params.encode.x[0]]) 
        }"))) %>%
      e_error_bar(lowercl, uppercl, name = .$breakdown,
                  itemStyle = list(opacity = 0.6),
                  tooltip = list(formatter = htmlwidgets::JS("
      function(params){
      return('breakdown: ' + params.seriesName +
      '</br>group: ' + params.value[params.encode.x[0]] +
      '<br/>upper CI: ' + Math.round(params.value[params.encode.y[1]] * 100, 3) +
        '%<br/>lower CI: ' + Math.round(params.value[params.encode.y[0]] * 100, 3) +
        '%') }"))) %>%
      e_tooltip(trigger = "item") %>% 
      e_grid(right = 180, left = 50) %>%
      e_y_axis(name = "Percent", nameLocation = "middle", nameGap = 35, max = 1, min = 0) %>% 
      e_x_axis(axisLabel = list(interval = 0, rotate = rotate)) %>% 
      e_format_y_axis(suffix = "%", formatter = e_axis_formatter("percent")) %>% 
      e_legend(show = TRUE, type = "scroll", orient = "vertical",
               right = 10, top = 35, bottom = 10,
               itemHeight = 10, itemWidth = 20,
               textStyle = list(fontSize = 12)) %>%
      e_theme("westeros") %>%
      e_title(plot_title) %>% 
      e_image_g(right = 180, top = 0, z = -999, style = list(opacity = 0.5, width = 120,
                                                             image = "https://www.hertshealthevidence.org/images/young-peoples-health-and-wellbeing-survey-logo-png-Cropped-448x190.png")) %>%
      e_toolbox_feature(feature = c("dataZoom", "restore")) %>% 
      e_theme_custom("phei.json")

  }

create_multi_plot <- function(df,
                              plot_title,
                              binary) {
  
  df <- df %>% 
    filter(!is.na(question_text)) %>% 
    droplevels()
  
  if (binary) {
    
    df %>% 
      filter(response == "Yes") %>% 
      arrange(response) %>%
      plot_ly(x = ~value, y = ~question_text, type = "bar", name = ~breakdown, color = ~breakdown,
              colors = "Blues", legendgroup = ~breakdown, orientation = 'h',
              hovertemplate = ~paste(stringr::str_wrap(paste0(value, " (", count, ") in the ", breakdown, " breakdown replied ", 
                                                              response, "<br>(CI:", lowercl, " to ", 
                                                              uppercl, ")"), 30), "<extra></extra>")) %>%
      layout(title = list(text = paste("<b>", plot_title, "</b>"), 
                          yanchor = "bottom", y = 1.3, x = 0, font = list(size= 12)),
             xaxis = list(title = "Percent", tickformat = "%", 
                          range = list(0, 1)),
             yaxis = list(title = "", autorange = "reversed")) %>%
      plotly::config(displaylogo = FALSE, 
                     modeBarButtons = list(list("toImage", "pan2d", "resetScale2d", "hoverClosestCartesian")))
    
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
      arrange(response) %>%
      plot_ly(x = ~value, y = ~breakdown, type = "bar", name = ~response, color = ~response,
              colors = "Blues", legendgroup = ~response, orientation = 'h',
              hovertemplate = ~paste(stringr::str_wrap(paste0(value, " (", count, ") in the ", breakdown, " breakdown replied ", 
                                                              response, "<br>(CI:", lowercl, " to ", 
                                                              uppercl, ")"), 30), "<extra></extra>"),
              transforms = list(list(type = "filter", target = ~menu_text, operator = '=', value = groups[1]))) %>%
      layout(barmode = "stack", 
             title = list(text = paste("<b>", plot_title, "</b>"), yanchor = "bottom", y = 1.3, x = 0, font = list(size= 12)),
             xaxis = list(title = "Percent", tickformat = "%"),
             updatemenus = list(type_list),
             yaxis = list(title = "", autorange = "reversed")) %>%
      plotly::config(displaylogo = FALSE, 
                     modeBarButtons = list(list("toImage", "pan2d", "resetScale2d", "hoverClosestCartesian")))
    
  }
  
  
}

tartan <- function(df,
                   indicators, 
                   comparator_area,
                   areas, 
                   palette,
                   area_col,
                   period_col,
                   period_sort_col,
                   indicator_col,
                   value_col, 
                   upper_ci,
                   lower_ci) {
  
  # colors
  col_lb <- '#bed2ff' #light blue
  col_db <- '#5555e5' #dark blue
  col_grey <- "#e3e3e3" #grey
  col_r <- '#c00000' #red
  col_y <- '#ffc000' #yellow
  col_g <- '#92d050' #green
  
  if (palette == "rag") {
    
    higher <- col_r
    lower <- col_g
    labels <- c("Significantly Better", "Significantly Worse", "Similar")
    
  } else if (palette == "blues") {
    
    higher <- col_lb
    lower <- col_db
    labels <- c("Significantly Lower", "Significantly Higher", "Similar")
    
  } else ( stop("Incorrect palette. Please enter either 'rag' or 'blues'.") )
  
  # duplicate a dummy "Area" that has period values so it can be a column in the tartan rug 
  value_vector <- df[[period_col]][df[[indicator_col]] %in% indicators & df[[area_col]] == comparator_area]
  date_rows <- df %>% 
    dplyr::filter(!!ensym(area_col) == comparator_area,
                  !!ensym(indicator_col) %in% indicators) %>% 
    mutate(diff = NA)
  
  # the following doesn't work unless indexed for some reason
  date_rows[[area_col]] <- "Period"
  date_rows[[value_col]] <- value_vector
  
  # make data frame for plot
  df[[value_col]] <- round(as.numeric(df[[value_col]]), 1)
  df[[value_col]] <- as.character(df[[value_col]])
  
  # str_wrap will reformat factor levels, so make new levels
  new_levels <- stringr::str_wrap(c("Period", comparator_area, areas), 25)
  new_ind_levels <- stringr::str_wrap(indicators, 30)
  new_comparator_area <- stringr::str_wrap(comparator_area, 25)
  
  p_data <- df %>% 
    dplyr::filter(!!ensym(indicator_col) %in% indicators) %>% 
    dplyr::group_by(!!ensym(indicator_col)) %>%
    dplyr::filter(!!ensym(period_sort_col) == max(!!ensym(period_sort_col))) %>%
    dplyr::ungroup() %>%
    dplyr::bind_rows(date_rows) %>%
    dplyr::filter(!!ensym(area_col) %in% c("Period", comparator_area, new_comparator_area, areas)) %>% 
    dplyr::select(!!ensym(area_col), !!ensym(indicator_col), !!ensym(period_col), !!ensym(value_col), !!ensym(upper_ci), !!ensym(lower_ci), diff) %>% 
    dplyr::mutate(colour = dplyr::case_when(diff == "significantly higher than" ~ higher, 
                                            diff == "significantly lower than" ~ lower, 
                                            TRUE ~ col_y),
                  colour = dplyr::case_when(!!ensym(area_col) %in% c(comparator_area, new_comparator_area) ~ col_grey, 
                                            is.na(!!ensym(value_col)) ~ "#f5f5f5",
                                            is.na(diff) ~ "#f5f5f5",
                                            TRUE ~ colour), 
                  diff = dplyr::case_when(!!ensym(area_col) %in% c(comparator_area, new_comparator_area) ~ "comparator", 
                                          TRUE ~ diff),
                  text_colour = dplyr::case_when(colour %in% c(col_r, col_db) ~ "white", TRUE ~ "black")) %>% 
    dplyr::distinct()
  
  # use str_wrap in advance
  p_data[[area_col]] <- stringr::str_wrap(p_data[[area_col]], 25)
  p_data[[indicator_col]] <- stringr::str_wrap(p_data[[indicator_col]], 30)
  
  p_data[[value_col]][is.na(p_data[[value_col]])] <- ""
  p_data[[area_col]] <- ordered(p_data[[area_col]], levels = new_levels)
  p_data[[indicator_col]] <- ordered(p_data[[indicator_col]], levels = new_ind_levels)
  
  p_data %>% 
    ggplot2::ggplot(aes(!!ensym(area_col), 
                        !!ensym(indicator_col), fill = colour)) +
    ggplot2::geom_tile(show.legend = T, colour = "white", size = 0.8) +
    ggplot2::geom_text(aes(label = !!ensym(value_col), colour = text_colour), 
                       show.legend = FALSE, size = 3.5) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::scale_fill_manual(values = c("#e3e3e3" = "#e3e3e3", "#ffc000" ="#ffc000", 
                                          "#92d050" = "#92d050", "#c00000" = "#c00000",
                                          "#f5f5f5" = "#f5f5f5", '#bed2ff' = '#bed2ff',
                                          '#5555e5' = '#5555e5'),
                               breaks = c(lower, higher, "#ffc000"),
                               labels = labels) + 
    ggplot2::scale_colour_manual(values = c("white" = "white", "black" = "black"), guide = FALSE) +
    ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 0, face = "bold"),
                   axis.text.y = element_text(face = "bold"),
                   axis.ticks = element_blank(),
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank(),
                   legend.title = element_blank(),
                   panel.background = element_rect(fill = "#f8f8f8"),
                   legend.position = "bottom") 
  
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
      mutate(difference = case_when(lowercl.x > uppercl.y ~ "higher",
                                    uppercl.x < lowercl.y ~ "lower",
                                    TRUE ~ "similar")) 
    
    data1 <- data %>% 
      dplyr::rename(count = count.x, percentage = value.x, `all schools` = value.y,
                    `comparison to all schools` = difference, `lower CI` = lowercl.x,
                    `upper CI` = uppercl.x) %>% 
      left_join(q_coded, by = c("question_text.x" = "question_text")) %>% 
      select(1, response, count, percentage, `lower CI`, `upper CI`, 
             `all schools`, `comparison to all schools`) %>% 
      mutate(`upper CI` = as.character(`upper CI`),
             `lower CI` = as.character(`lower CI`),
             `all schools` = as.character(`all schools`),
             percentage = as.character(percentage)) %>% 
      rename(breakdown = breakdown.x)
    
    data_ <- data %>% 
      select(breakdown.y, response, count.y, value.y, lowercl.y, uppercl.y) %>% 
      rename(breakdown = breakdown.y, count = count.y, percentage = value.y, 
             `lower CI` = lowercl.y, `upper CI` = uppercl.y) %>% 
      mutate(`upper CI` = as.character(`upper CI`),
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
        left_join(q_coded, by = c("question" = "question_coded")) %>% 
        mutate(question = menu_text) %>% 
        select(1, question, response, count, percentage, `lower CI`, `upper CI`, 
               `all schools`, `comparison to all schools`) %>% 
        distinct() %>% 
        mutate(`upper CI` = as.character(`upper CI`),
               `lower CI` = as.character(`lower CI`),
               `all schools` = as.character(`all schools`),
               percentage = as.character(percentage))
      
      
    } else {
      
      data1 <- data %>% 
        left_join(q_coded, by = c("question" = "question_text")) %>% 
        select(1, question, response, count, percentage, `lower CI`, `upper CI`, 
               `all schools`, `comparison to all schools`) %>% 
        distinct() %>% 
        mutate(`upper CI` = as.character(`upper CI`),
               `lower CI` = as.character(`lower CI`),
               `all schools` = as.character(`all schools`),
               percentage = as.character(percentage))
      
    }
    
    data_ <- data %>% 
      left_join(q_coded, by = c("question" = "question_coded")) %>% 
      select(breakdown.y, menu_text, response, count.y, `all schools`, lowercl.y, uppercl.y) %>% 
      rename(breakdown = breakdown.y, count = count.y, percentage = `all schools`, 
             `lower CI` = lowercl.y, `upper CI` = uppercl.y, question = menu_text) %>% 
      mutate(`upper CI` = as.character(`upper CI`),
             `lower CI` = as.character(`lower CI`),
             percentage = as.character(percentage))
    
  }
  
  if (nrow(data) == 0) { # return the message below if there is nothing to show
    
    cat("Data suppressed due to low amount of responses.")
    
  } else {
    
    names(data1)[1] <- "breakdown"
    
    if (include_school == T) { # if school report, include schools percentage
      
      data1 %>%
        arrange(breakdown) %>% 
        reactable(groupBy = groupedby,
                  columns = list(
                    percentage = colDef(format = colFormat(percent = T, digits = 1)),
                    `all schools` = colDef(format = colFormat(percent = T, digits = 1)),
                    `comparison to all schools` = colDef(style = JS(
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
      
      data <- data1 %>% select(-`comparison to all schools`, -`all schools`) %>% 
        bind_rows(data_)
      
      data %>% 
        distinct() %>% 
        arrange(breakdown) %>% 
        reactable(defaultPageSize = 10, groupBy = groupedby, striped = T,
                  columns = list(
                    percentage = colDef(format = colFormat(percent = T, digits = 1))
                  ))
      
      
    }
    
    
  }
  
}



