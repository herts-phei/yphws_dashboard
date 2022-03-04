

# General -----------------------------------------------------------------

summarystats <- function(data, by, q_coded, omit_pivot = F) {
  # calculate summary metrics for all responses by specified groups
  # by should be a vector of columns to group by
  
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


get_stats <- function(data,
                      group,
                      sch = NA,
                      q_coded) { 
  
  box::use(magrittr[`%>%`])
  box::use(dplyr[recode, case_when, n, everything])
  box::use(tidyr[complete, expand, nesting])
  
  # recode scale to categories
  data <- data %>%
    dplyr::mutate(life_satisfied = recode(life_satisfied, "0"="low","1"="low","2"="low","3"="low","4"="low","5"="medium","6"="medium","7"="high","8"="high","9"="very high","10"="very high"),
                  life_worthwhile = recode(life_worthwhile, "0"="low","1"="low","2"="low","3"="low","4"="low","5"="medium","6"="medium","7"="high","8"="high","9"="very high","10"="very high"),
                  life_happyyesterday = recode(life_happyyesterday, "0"="low","1"="low","2"="low","3"="low","4"="low","5"="medium","6"="medium","7"="high","8"="high","9"="very high","10"="very high"),
                  life_satisfied_before_covid = recode(life_satisfied_before_covid, "0"="low","1"="low","2"="low","3"="low","4"="low","5"="medium","6"="medium","7"="high","8"="high","9"="very high","10"="very high"),
                  alcohol_perday = recode(alcohol_perday, "0"="none","1"="1-2","2"="1-2","3"="3-4","4"="3-4","5"="5-6","6"="5-6","7"="7-9","8"="7-9","9"="7-9","10"="10+"),
                  pa_60 = recode(pa_60, "0"="none","1"="1-3","2"="1-3","3"="1-3","4"="4-5","5"="4-5","6"="6-7","7"="6-7"),
                  pa_30 = recode(pa_30, "0"="none","1"="1-3","2"="1-3","3"="1-3","4"="4-5","5"="4-5","6"="6-7","7"="6-7"))
  
  data_l <- list()
  
  # summary statistics for all schools. 
  data_l[["data_all_schools"]] <- data %>% 
    dplyr::mutate(school = "All Schools", schyear = "All Responses") %>% 
    summarystats(c('school', 'schyear'), q_coded = q_coded)
  
  # summary statistics by group variable(s)
  for (g in 1:length(group)) {
    
    data_l[[paste0("data_by", group[g])]] <- data %>% 
      dplyr::mutate(school = "All Schools") %>%  
      summarystats(c('school', group[g]), q_coded = q_coded)
    
  }
  
  # IF WE WANT TO GROUP BY DISTRICTS, WE NEED SOME ADDITIONAL PRE-PROCESSING. 
  if (any(grepl("District", group))) {
    
    responses_per_district <- data %>% 
      dplyr::select(school, District) %>%  
      dplyr::group_by(District) %>% 
      dplyr::mutate(school_count = n()) %>% 
      dplyr::distinct() %>% 
      dplyr::summarise(District, school_count, schools_per_district = n()) %>% 
      dplyr::distinct() %>% 
      dplyr::mutate(comparison = case_when(school_count >= 250 & schools_per_district > 1 ~ TRUE, 
                                           TRUE ~ FALSE))
    
    data_d <- dplyr::filter(data, District %in% responses_per_district$District[responses_per_district$comparison == T])
    
    # bind to list
    data_l[[paste0("data_by", "District")]] <- data_d %>% 
      dplyr::mutate(school = "All Schools") %>%  
      summarystats(c('school', "District"), q_coded = q_coded)
    
  }
  
  omit_cols <- c("Ended", "Started", "UserID", "postcode", "postcode2", "sch_hidden", "school_other_hidden") #certain columns are not needed for reporting.
  
  stats <- dplyr::bind_rows(data_l)
  
  # IF RUNNING SCHOOL REPORT (e.g. school = "Tring")
  if (!is.na(sch[1])) {
    
    data_l_d <- list()
    
    # summary statistics for school of interest
    data_l_d[["data_school"]] <- data %>%
      dplyr::filter(school == sch) %>%
      dplyr::mutate(schyear = "All Responses") %>% # report schools all years
      tidyr::pivot_longer(cols = -c('school', 'schyear') ,
                          names_to = c("question"), values_to = "response") %>%
      
      # temporarily bind full data for complete
      dplyr::bind_rows(dplyr::select(stats, school, question, response) %>% dplyr::filter(school == "All Schools")) %>% 
      complete(expand(., school, schyear, 
                      nesting(question, response))) %>% 
      dplyr::group_by_all() %>%
      dplyr::filter(!is.na(response)) %>%
      dplyr::summarise(count = n()) %>%
      dplyr::ungroup() %>% 
      dplyr::filter(school == sch) %>% # remove the temporarily binded stats
      summarystats(c('school','schyear'), q_coded = q_coded, omit_pivot = T)
    
    # summary statistics by group variable(s) for school of interest
    for (g in 1:length(group)) {
      
      data_l_d[[paste0("data_school_by", group[g])]] <- data %>% 
        dplyr::filter(school == sch) %>%  
        tidyr::pivot_longer(cols = -c('school', group[g]) ,
                            names_to = c("question"), values_to = "response") %>%
        
        # temporarily bind full data for complete
        dplyr::bind_rows(dplyr::select(stats, school, question, response) %>% dplyr::filter(school == "All Schools")) %>% 
        complete(expand(., school, .data[[group[g]]], 
                        nesting(question, response))) %>% 
        dplyr::group_by_all() %>%
        dplyr::filter(!is.na(response)) %>%
        dplyr::summarise(count = dplyr::n()) %>%
        dplyr::ungroup() %>% 
        dplyr::filter(school == sch) %>% # remove the temporarily binded stats
        summarystats(c('school', group[g]), q_coded = q_coded, omit_pivot = T)
      
    }
    
    # additionally, if working with school data we should exclude "Not at school/other" 
    stats_d <- dplyr::bind_rows(data_l_d) %>% 
      dplyr::filter(breakdown != "Not at school/other")
    
    stats <- dplyr::bind_rows(stats, stats_d)
    
  }
  
  stats <- stats %>%
    dplyr::mutate(value = formattable::percent(value, digits = 1),
                  lowercl = formattable::percent(lowercl, digits = 1),
                  uppercl = formattable::percent(uppercl, digits = 1),
                  lowereb = value - lowercl,
                  uppereb = uppercl - value)  %>% 
    dplyr::filter(!is.na(breakdown))
  
  return(stats)
  
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


# Summary statistics ------------------------------------------------------

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

create_basic_plot <- function(df, 
                              plot_custom_grp, 
                              plot_title,
                              rotate = 0) {
  
  groups <- c("All Responses", 
              plot_custom_grp)
  
  # plot object
  df %>% 
    filter(breakdown %in% groups) %>% 
    mutate(value = round(as.numeric(.$value), 3),
           lowercl = round(as.numeric(.$lowercl), 3),
           uppercl = round(as.numeric(.$uppercl), 3),
           response = str_wrap(response, 15),
           breakdown = factor(breakdown, levels = groups)) %>% 
    filter(question %in% chk_var, 
           !is.na(question_text)) %>% 
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

mean_responses_plot <- function(df = data, vars = chk_var, custom_grp = group){
  
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
  
  df_custom <- df %>%
    select(breakdown := !!ensym(custom_grp), vars) %>%
    mutate(across(chk_var, ~as.numeric(ifelse(.x == "Yes", 1, 0)))) %>%
    mutate(total = rowSums(across(vars), na.rm = T)) %>%
    group_by(breakdown) %>%
    phe_mean(total) %>%
    ungroup() %>%
    filter(breakdown != "Prefer not to say")
  
  df <- bind_rows(df_year, df_sex, df_custom) %>% filter(!is.na(breakdown))
  
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

create_multicat_plot_herts <- function(df = chk_stats, title = plot_title, q_coded = q_coded, by = "percent") {
  
  df <- chk_stats %>% filter(!is.na(question_text)) %>% 
    droplevels() 
  
  df <- df %>% dplyr::arrange(response) %>% 
    left_join(q_coded, by = c("question" = "question_coded"))
  
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
    
    group_by(df, breakdown, response, menu_text) %>%
      arrange(response) %>%
      plot_ly(x = ~value, y = ~breakdown, type = "bar", name = ~response, color = ~response,
              colors = "Blues", legendgroup = ~response, orientation = 'h',
              hovertemplate = ~paste(stringr::str_wrap(paste0(count, " in the ", breakdown, " breakdown replied ", response, "<br>(", value,
                                                              " [CI:", lowercl, " to ", uppercl, "])"), 30), "<extra></extra>"),
              transforms = list(list(type = "filter", target = ~menu_text, operator = '=', value = groups[1]))) %>%
      layout(barmode = "stack", 
             title = list(text = paste("<b>", plot_title, "</b>"), yanchor = "bottom", y = 1.3, x = 0, font = list(size= 12)),
             xaxis = list(title = "Percent", tickformat = "%"),
             updatemenus = list(type_list),
             autosize = F, height = 600,
             margin = list(r = 250,
                           t = 0,
                           l = 0,
                           b = 0),
             #hovermode = 'compare',
             yaxis = list(title = "", autorange = "reversed"),
             images = list(source = base64enc::dataURI(file = "www/logo_phei.png"),
                           x = 0.80, y = 1.1, sizex = 0.15, sizey = 0.15, xanchor = "left",
                           yanchor = "top")) %>%
      plotly::config(displaylogo = FALSE, modeBarButtons = list(list("toImage", "pan2d", "resetScale2d", "hoverClosestCartesian")))
    
    
  } else {
    
    group_by(df, question_text, response, breakdown) %>%
      filter(response == "Yes") %>% 
      arrange(response) %>%
      mutate(question_text = str_wrap(question_text, 25)) %>%
      plot_ly(x = ~count, y = ~question_text, type = "bar", name = ~response,
              colors = "Blues", legendgroup = ~response, orientation = 'h',
              transforms = list(list(type = "filter", target = ~breakdown, operator = '=', value = groups[1]))) %>%
      layout(barmode = "stack", title = list(text = paste("<b>", plot_title, "</b>"), xanchor = "left", y = 0.85, x = 0, font = list(size = 12)),
             xaxis = list(title = "Count of 'Yes'"),
             updatemenus = list(type_list),
             autosize = F, height = 800,
             margin = list(r = 400,
                           t = 0,
                           l = 0,
                           b = 0),
             hovermode = 'compare',
             yaxis = list(title = "", autorange = "reversed"),
             images = list(source = base64enc::dataURI(file = "www/logo_phei.png"),
                           x = 0.70, y = 1.25, sizex = 0.3, sizey = 0.3, xanchor = "left",
                           yanchor = "top")) %>%
      plotly::config(displaylogo = FALSE, modeBarButtons = list(list("toImage", "pan2d", "resetScale2d", "hoverClosestCartesian")))
    
  }
  
}

create_multicat_plot_herts <- purrr::quietly(create_multicat_plot_herts)

# Tables ------------------------------------------------------------------

create_tbl <- function(stats_diff, multi_response = F, include_school = F) {
  
  #stats_diff = chk_diff
  if (multi_response == F) {
    
    groupedby <- "breakdown"
    
    data1 <- stats_diff %>% 
      dplyr::rename(breakdown = breakdown.x, count = count.x, percentage = value.x, 
                    `lower CI` = lowercl.x,
                    `upper CI` = uppercl.x) %>% 
      left_join(q_coded, by = c("question_text.x" = "question_text")) %>% 
      select(breakdown, response, count, percentage, `lower CI`, `upper CI`) %>% 
      mutate(`upper CI` = as.character(`upper CI`),
             `lower CI` = as.character(`lower CI`))
    
    
  } else {
    
    groupedby <- c("breakdown", "question")
    data <- stats_diff %>% 
      mutate(difference = case_when(lowercl.x > uppercl.y ~ "higher",
                                    uppercl.x < lowercl.y ~ "lower",
                                    TRUE ~ "similar")) %>% 
      dplyr::rename(breakdown = breakdown.x, count = count.x, percentage = value.x,
                    `lower CI` = lowercl.x,
                    `upper CI` = uppercl.x) 
    
    if(length(unique(data$response)) <= 2) {
      
      data1 <- data %>% 
        left_join(q_coded, by = c("question" = "question_coded")) %>% 
        mutate(question = menu_text) %>% 
        select(1, question, response, count, percentage, `lower CI`, `upper CI`) %>% 
        distinct() %>% 
        mutate(`upper CI` = as.character(`upper CI`),
               `lower CI` = as.character(`lower CI`))
      
      
    } else {
      
      data1 <- data %>% 
        left_join(q_coded, by = c("question" = "question_text")) %>% 
        select(1, question=question_text.x, response, count, percentage, `lower CI`, `upper CI`) %>% 
        distinct() %>% 
        mutate(`upper CI` = as.character(`upper CI`),
               `lower CI` = as.character(`lower CI`))
      
    }
  }
  
  if (nrow(data1) == 0) { # return the message below if there is nothing to show
    
    cat("Data suppressed due to low amount of responses.")
    
  } else {
    
    data1 %>% 
      distinct() %>% 
      arrange(breakdown) %>% 
      reactable(defaultPageSize = 10, groupBy = groupedby, striped = T,
                columns = list(
                  percentage = colDef(format = colFormat(percent = T, digits = 1))
                ))
  }
  
}


# Bullet points -----------------------------------------------------------

create_bullets_herts <- function(df, grouped_by_question = F) {
  
  keep_these <- q_coded$question_coded[!is.na(q_coded$reworded) & q_coded$reworded != "-"]
  
  df <- df %>%
    filter(question %in% keep_these) %>%
    left_join(select(q_coded, question_coded, question_theme), by = c("question" = "question_coded")) %>%
    rename(Theme = question_theme)
  
  df <- df[!is.na(df$diff), ]
  
  l <- list()
  
  if(nrow(df) == 0){
    
    l[["Table"]] <- glue::glue("No significant differences")
    
    l[["Data"]] <- glue::glue(" ")
    
    return(l)
    
  } else {
    
    #empty list for sentences
    sentences <- list()
    
    # creates a bullet for each row in the stats_diffs_sch, with icons
    for (i in seq(nrow(df))) {
      
      current <- df[i,]
      
      current$diff <- ifelse(current$diff == "significantly higher than", "significantly <b>HIGHER</b> than", "significantly <b>LOWER</b> than")
      
      cust_break <- current$breakdown.x
      
      current$school.y <- current$breakdown.y
      df$breakdown.y[i] <- current$breakdown.y
      
      # creates a template to populate with values from the stats_diffs_sch
      template <- q_coded$reworded[q_coded$question_coded == current$question][1]
      
      template <- gsub("(.*)students ", "\\1", template)
      
      sentences[[i]] <- paste0(sprintf(template, cust_break, current$response, current$value.x,
                                       current$question_text.x, current$diff,
                                       current$school.y, current$value.y))
    }
    
    df$sentences <- unlist(sentences)
    
    df <- rename(df, "Differences found" = sentences, "Breakdown" = breakdown.x,
                 Question = question_text.x)
    
    prevent_dupes <- df %>%
      group_by(question, response) %>%
      summarise(count = n()) %>%
      filter(response %in% c("Yes", "No")) %>%
      arrange(question)
    
    remove_q <- prevent_dupes$question[duplicated(prevent_dupes$question)]
    
    df <- df[!(df$question %in% remove_q & df$response == "No"), ]
    
    df$Question  <- ifelse(df$question %in% q_coded$question_coded,
                           q_coded$survey_text[match(df$question, q_coded$question_coded)], NA)
    
    groupedc <- c("Breakdown", "Theme")
    
    #make the table
    l[["Table"]] <- reactable(select(df, Breakdown, Theme, Question, `Differences found`),
                              groupBy = groupedc, highlight = T, striped = T,
                              pagination = F,
                              columns = list(`Differences found` = colDef(minWidth = 200, html = T)  #minimum width 200
                              ))
    
    l[["Data"]] <- df
    
    return(l)
    
  }
}
