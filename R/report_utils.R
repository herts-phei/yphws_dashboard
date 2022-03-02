
# Generate text -----------------------------------------------------------

create_sum_sentence <- function(dataset, 
                                multi = F, 
                                value_of_interest = F, 
                                full_data,
                                diffs,
                                custom_grp,
                                group_of_interest) {
  
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
            drop_na(reworded) %>% 
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
  sentence <- paste0(sentence, " For more detail, please see the Graph or Table tabs.")
  
  return(sentence)
  
}

# Bar graphs --------------------------------------------------------------

create_int_plot <- function(df, 
                            sex = FALSE, 
                            title = plot_title, 
                            fontsize = 11, 
                            textangle = 0, 
                            factor_levels = F,
                            include_school = F){
  
  if (district_comparison == T) {df <- filter(df, school != paste0(district, " Schools"))}
  
  # if (factor_levels[1] == F) { 
  #   
  #   factor_levels <- unique(str_wrap(df$response, 10))
  #   
  # } else {
  #     
  #   factor_levels <- str_wrap(factor_levels, 10)
  #   
  #   }
  
  # clean data so no denominator is less than 20 responses
  df <- df %>% 
    complete(expand(., nesting(breakdown, school, response))) 
  #mutate(response = ordered(str_wrap(.$response, 10), levels = c(factor_levels)))
  
  if(sex == TRUE) {
    
    data <- df %>%
      filter(breakdown %in% c("All Responses", "Female", "Male", "Other", "Prefer not to say"))
    
    t <- table(data$breakdown[data$count < 6]) #find group that consistently has less than 20 responses
    remove <- names(t)[t == 14]
    
    data <- data[data$breakdown %nin% remove, ]
    
  } else {
    
    data <- df %>%
      filter(breakdown %in% c("All Responses", "Year 7", "Year 8", "Year 9",
                              "Year 10", "Year 11", "Year 12", "Year 13"))
    
    t <- table(data$breakdown[data$count < 6]) #find group that consistently has less than 20 responses
    remove <- names(t)[t == 14]
    
    data <- data[data$breakdown %nin% remove, ]
    
    g_levels <- c('Year 13',
                  'Year 12',
                  'Year 11',
                  'Year 10',
                  'Year 9',
                  'Year 8',
                  'Year 7',
                  'All Responses')
    
    g_levels <- g_levels[!g_levels %in% remove]
    
    data$breakdown <- ordered(data$breakdown, levels = g_levels) 
    
  }
  
  data_all <- data %>% 
    filter(school == "All Schools") 
  
  # data_all$value[data_all$denominator < 6] <- 0
  # data_all$lowereb[data_all$denominator < 6] <- 0
  # data_all$uppereb[data_all$denominator < 6] <- 0
  # data_all$lowercl[data_all$denominator < 6] <- 0
  # data_all$uppercl[data_all$denominator < 6] <- 0
  
  
  p_all <- data_all %>%
    dplyr::arrange(breakdown) %>%
    plot_ly(x = ~str_wrap(response, 10), y = ~value, type = "bar", width = 900, height = 500,
            name = ~breakdown, color = ~breakdown, colors = "viridis", 
            legendgroup = ~breakdown,
            error_y = list(array = ~uppereb, arrayminus = ~lowereb, 
                           symmetric = F, color = '#000000', opacity = 0.3),
            hovertemplate = ~paste0(count, ", ", value_cens, " (CI: ", uppercl, " - ", lowercl, ")")) %>%
    layout(yaxis = list(title = 'Percent', tickformat = ',.0%'),
           xaxis = list(tickangle = textangle, tickfont = list(size = fontsize), title = "All Schools"),
           barmode = 'group',
           hovermode = 'compare')
  
  # data_school$value[data_school$denominator < 6] <- 0
  # data_school$lowereb[data_school$denominator < 6] <- 0
  # data_school$uppereb[data_school$denominator < 6] <- 0
  # data_school$lowercl[data_school$denominator < 6] <- 0
  # data_school$uppercl[data_school$denominator < 6] <- 0
  
  if (include_school == T) {
    
    data_school <- data %>% 
      filter(school == sch)
    
    p_sch <- data_school %>%
      dplyr::arrange(breakdown) %>%
      plot_ly(x = ~str_wrap(response, 10), y = ~value, type = "bar", width = 1100, height = 500,
              name = ~breakdown, color = ~breakdown, colors = "viridis", 
              legendgroup = ~breakdown, 
              showlegend = F,
              error_y = list(array = ~uppereb, arrayminus = ~lowereb,
                             symmetric = F, color = '#000000', opacity = 0.3),
              hovertemplate = ~paste0(count, ", ", value_cens, " (CI: ", uppercl, " - ", lowercl, ")")) %>%
      layout(yaxis = list(title = 'Percent', tickformat = ',.0%'),
             barmode = 'group', xaxis = list(title = sch),
             xaxis = list(tickangle = textangle, tickfont = list(size = fontsize)),
             hovermode = 'compare')
    
    p <- plotly::subplot(p_sch, p_all, shareY = T, titleX = TRUE) %>%
      layout(title = list(text = paste(title, "| YPHWS", year),
                          x = 0.05, font = list(color = col_db)),
             images = list(source = base64enc::dataURI(file ="graphics/logo_phei.png"),
                           x = 1, y = 1.05, sizex = 0.15, sizey = 0.15, xanchor = "left",
                           yanchor = "top"),
             legend = list(y = 0.8),
             xaxis = list(tickangle = textangle, tickfont = list(size = fontsize))) %>%
      plotly::config(displaylogo = FALSE, modeBarButtons = list(list("toImage", "pan2d", "resetScale2d", "hoverCompareCartesian", "hoverClosestCartesian"))) %>%
      partial_bundle()
    
    return(p)
    
  }
  
  
  return(p_all)
  
  
}

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
    e_toolbox_feature(feature = c("dataZoom", "restore"))
  
}

mean_responses_plot <- function(df = data, vars = chk_var){
  
  df_year <- df %>%
    select(breakdown = schyear, vars) %>%
    mutate(across(vars, ~as.numeric(ifelse(.x == "Yes", 1, 0)))) %>%
    mutate(total = rowSums(across(vars), na.rm = T)) %>%
    group_by(breakdown) %>%
    phe_mean(total) %>%
    ungroup()
  
  df_sex <- df %>%
    select(breakdown = sex, vars) %>%
    mutate(across(vars, ~as.numeric(ifelse(.x == "Yes", 1, 0)))) %>%
    mutate(total = rowSums(across(vars), na.rm = T)) %>%
    group_by(breakdown) %>%
    phe_mean(total) %>%
    ungroup()
  
  df_eth <- df %>%
    select(breakdown = ethnicity, vars) %>%
    mutate(across(chk_var, ~as.numeric(ifelse(.x == "Yes", 1, 0)))) %>%
    mutate(total = rowSums(across(vars), na.rm = T)) %>%
    group_by(breakdown) %>%
    phe_mean(total) %>%
    ungroup() %>%
    filter(breakdown != "Prefer not to say")
  
  df <- bind_rows(df_year, df_sex, df_eth) %>% filter(!is.na(breakdown))
  
  df %>%
    e_charts(breakdown) %>% 
    e_bar(value, name = .$breakdown, tooltip = list(formatter = htmlwidgets::JS("
      function(params){
      return('value: ' + Math.round(params.value[1], 2) +
        '<br/>group: ' + params.value[params.encode.x[0]])
        }"))) %>%
    e_error_bar(lowercl, uppercl, name = .$breakdown,
                itemStyle = list(opacity = 0.4),
                tooltip = list(formatter = htmlwidgets::JS("
      function(params){
      return('group: ' + params.value[params.encode.x[0]] +
      '<br/>upper CI: ' + Math.round(params.value[params.encode.y[1]], 2) +
        '<br/>lower CI: ' + Math.round(params.value[params.encode.y[0]], 2)
        )}"))) %>%
    e_tooltip(trigger = "item") %>% 
    e_legend(show = F) %>%
    e_y_axis(name = "Mean", nameLocation = "middle", nameGap = 35) %>% 
    e_x_axis(axisLabel = list(interval = 0, rotate = 90)) %>%
    e_title(paste("Mean number of", tolower(plot_title))) %>%
    e_image_g(right = 80, top = 5, z = -999, style = list(opacity = 0.5, width = 120,
                                                          image = "https://www.hertshealthevidence.org/images/young-peoples-health-and-wellbeing-survey-logo-png-Cropped-448x190.png")) %>%
    e_toolbox_feature(feature = c("dataZoom", "saveAsImage")) %>%
    e_grid(bottom = 150)
  
}

# Tables ------------------------------------------------------------------

create_tbl <- function(stats_diff, multi_response = F, include_school = F) {
  
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



