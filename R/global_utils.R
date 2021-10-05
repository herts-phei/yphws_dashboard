
# General functions -------------------------------------------------------

# wrapper function for longer titles in plots
wrapper_html <- function(x, ...) {
  # Wrapper function for plot titles
  paste(strwrap(x, ...), collapse = "<br/>")
}

wrapper <- function(x, ...) {
  # Wrapper function for plot titles
  paste(strwrap(x, ...), collapse = "\n")
}

substrRight <- function(x, n){
  
  substr(x, nchar(x) - n + 1, nchar(x))
  
}

# Summary statistics ------------------------------------------------------

summarystats <- function(data, by) {
  # calculate summary metrics for all responses by specified groups
  # by should be a vector of columns to group by
  t <- data %>% 
    pivot_longer(cols = -{{ by }} ,
                 names_to=c("question"), values_to="response") %>%
    group_by_all() %>%
    filter(!is.na(response)) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    group_by_at(vars( {{ by }} , question)) %>%
    mutate(denominator = sum(count)) %>%
    rename(breakdown = by[2]) %>%
    mutate(count = plyr::round_any(count, 5), # disclosure control - round to base 5
           denominator = sum(count)) %>% # redo denom with altered count
    ungroup() %>% 
    filter(denominator >= 5) %>% # disclosure control & CI calculation - filter out less than 5
    PHEindicatormethods::phe_proportion(., count, denominator, type='standard', confidence = 0.998) %>% 
    mutate(question_text = q_coded$question_text[match(question,  #add friendly names
                                                       q_coded$question_coded)]) %>% 
    select(breakdown, school, question, question_text, everything()) #reorder columns
}

differences <- function(data, by, from, to, join = NULL) {
  # looks for sig differences between report area and all responses
  # e.g. data, school, report_school, all_schools, schoolyear returns differences
  # between the respective year of the report school and all schools 
  # e.g. data, school, report_school, report_school returns differences between
  # each year of the report school (includes comaring year to itself)
  data_from <- data[data[[by]] == from, ]
  data_to <- data[data[[by]] == to, ] 
  
  df <- inner_join(data_from, data_to, by = c(join, "question", "response")) %>%
    mutate(diff = case_when(lowercl.x > uppercl.y ~ "significantly higher than",
                            uppercl.x < lowercl.y ~ "significantly lower than"),
           image = paste0("graphics/", diff, ".png"))
  
  return(df)
}

create_bullets_herts <- function(df, grouped_by_question = F) {
  
  keep_these <- q_coded$question_coded[!is.na(q_coded$reworded) & q_coded$reworded != "-"]
  
  df <- df %>% 
    filter(question %in% keep_these) %>% 
    left_join(select(q_coded, question_coded, question_theme), by = c("question" = "question_coded")) %>% 
    rename(Theme = question_theme)
  
  df <- df[!is.na(df$diff), ]
  
  l <- list()
  
  if(nrow(df) == 0){
    
    df <- tibble(`Between Groups` = NA, response = NA, Question = NA, `Differences found` = "None", diff = NA) 
    
    #make the table 
    l[["Table"]] <- select(df, -diff, -response) %>% reactable(highlight = T, striped = T, 
                                                               columns = list(
                                                                 `Differences found` = colDef(minWidth = 200, html = T)  #minimum width 200
                                                               ))
    
    l[["Data"]] <- df
    
    return (l)
    
  } else {
    
    #empty list for sentences
    sentences <- list() 
    
    # creates a bullet for each row in the stats_diffs_sch, with icons
    for (i in seq(nrow(df))) {
      
      current <- df[i,]
      
      current$diff <- ifelse(current$diff == "significantly higher than", "significantly <b>HIGHER</b> than", "significantly <b>LOWER</b> than")
      
      if (grepl(" in ", current$breakdown.x)) {
        
        cust_break <- gsub("All Schools in ", "", current$school.x)
        df$school.x[i] <- gsub("All Schools in ", "", current$school.x)
        
        current$school.y <- "all schools"
        df$school.y[i] <- "all schools"
        
      } else {
        
        cust_break <- current$breakdown.x
        
        current$school.y <- current$breakdown.y
        df$breakdown.y[i] <- current$breakdown.y
        
      }
      # creates a template to populate with values from the stats_diffs_sch
      template <- q_coded$reworded[q_coded$question_coded == current$question][1]
      
      template <- gsub("(.*)students ", "\\1", template)
      
      sentences[[i]] <- paste0(sprintf(template, cust_break, current$response, current$value.x,
                                       current$question_text.x, current$diff,
                                       current$school.y, current$value.y))
    }
    
    df$sentences <- unlist(sentences)
    
    df <- rename(df, "Differences found" = sentences, "Breakdown" = breakdown, 
                 "Between..." = breakdown.x, Question = question_text.x)
    
    prevent_dupes <- df %>% 
      group_by(question, response) %>% 
      summarise(count = n()) %>% 
      filter(response %in% c("Yes", "No")) %>% 
      arrange(question)
    
    remove_q <- prevent_dupes$question[duplicated(prevent_dupes$question)]
    
    df <- df[!(df$question %in% remove_q & df$response == "No"), ]
    
    df$Question  <- ifelse(df$question %in% q_coded$question_coded, 
                           q_coded$survey_text[match(df$question, q_coded$question_coded)], NA) 
    
    groupedc <- c("Between...")
    
    if (grouped_by_question == T) { groupedc <- c("Between...", "Question") }
    
    #make the table 
    l[["Table"]] <- reactable(select(df, `Between...`, Question, `Differences found`), 
                              groupBy = groupedc, highlight = T, striped = T, 
                              pagination = F, 
                              columns = list(
                                `Between...` = colDef(width = 130, html = T),
                                `Differences found` = colDef(minWidth = 200, html = T)  #minimum width 200
                              ))
    
    l[["Data"]] <- df
    
    return(l)
    
  }
}

# Server functions --------------------------------------------------------

get_params <- function() {
  
  output <- list()
  
  output$year <- 2020 #year of survey
  
  #default colors
  
  output$col_lb <- '#248cdc' #'rgba(36,140,220,1)' #light blue
  output$col_db <- '#001874' #'rgba(0,24,116,1)' #dark blue
  output$col_r <- '#fa003c' #'rgba(250,0,60,1)' #reddish pink
  output$col_y <- '#ffaf04' #'rgba(255,175,4,1)' #yellow
  output$col_g <- '#59d002' #'rgba(89,208,2,1)' #green 
  
  #color vectors
  output$yphws_col <- c(output$col_lb, output$col_db, output$col_r, output$col_y, output$col_g, output$col_g)
  output$yphws_col2 <- c(output$col_lb, output$col_db, output$col_r,"#FF69B4", output$col_y, output$col_g, output$col_g, "#228B22", "#20B2AA", "#2F4F4F")
  output$yphws_pal <- colorRampPalette(c(output$col_lb, output$col_db, output$col_r, output$col_y, output$col_g, output$col_g))
  
  #set dig colors
  output$clr_notdiff <- output$col_y
  output$clr_higher <- output$col_lb
  output$clr_lower <- output$col_db
  
  return(output)
  
}

get_data <- function(data, q_coded, school_data) {
  
  # colname_vector <- as.character(names(data)) #column names of second row headers
  # names(data)[grep("^X", names(data))] <- colname_vector #replace missing headers with the above vector
  # 
  # omit_cols <- c("Ended", "Started", "UserID", "postcode", "postcode2", "sch_hidden", "school_other_hidden") #certain columns are not needed for reporting.
  # 
  # data <- data[-c(1, 2), -c(2:6)] %>% # removing the first 2 rows.  
  #   data.table::setnames(q_coded$question_raw, q_coded$question_coded, skip_absent = T) %>%
  #   left_join(school_data[, 5:6], by = c("school" = "School")) %>% 
  #   select(-all_of(omit_cols)) %>% 
  #   mutate(age = as.character(trunc(as.numeric(age))))
  
  data$schyear <- ordered(data$schyear, 
                          levels = c('Year 7', 
                                     'Year 8',
                                     'Year 9', 
                                     'Year 10', 
                                     'Year 11', 
                                     'Year 12', 
                                     'Year 13', 
                                     'Not at school/other'))
  
  data <- data[!colnames(data) == ""] # QC
  
  # some questions autofill with "No" even though the question was skipped/not applicable.
  data[data$smoke_ever %in% c("I have never smoked", "I have tried smoking once or twice") | is.na(data$smoke_ever), 
       c(which(names(data) %in% c("smoke_reduce", "vaping_reducesmoke")), grep("buycig_", names(data)))] <- NA
  
  data[data$alcohol_ever == "Never" | is.na(data$alcohol_ever), 
       c(which(names(data) %in% c("alcohol_reduce", "alcohol_6more", "alcohol_perday")), grep("buyalc_", names(data)))] <- NA
  
  data[data$drug_ever == "I have never taken drugs" | is.na(data$drug_ever), 
       c(which(names(data) %in% c("drug_reduce", "drugs_alcohol")), grep("buydrug_", names(data)), grep("_exp", names(data)))] <- NA
  
  data[data$bullied != "Yes" | is.na(data$bullied), 
       c(grep("bullied_", names(data)))] <- NA
  
  data[data$approached_worry %in% c("No", "Prefer not say") | is.na(data$approached_worry), 
       c(grep("approached_", names(data))[-1])] <- NA
  
  # recode scale to categories
  data <- data %>%
    mutate(life_satisfied = recode(life_satisfied, "0"="low","1"="low","2"="low","3"="low","4"="low","5"="medium","6"="medium","7"="high","8"="high","9"="very high","10"="very high"),
           life_worthwhile = recode(life_worthwhile, "0"="low","1"="low","2"="low","3"="low","4"="low","5"="medium","6"="medium","7"="high","8"="high","9"="very high","10"="very high"),
           life_happyyesterday = recode(life_happyyesterday, "0"="low","1"="low","2"="low","3"="low","4"="low","5"="medium","6"="medium","7"="high","8"="high","9"="very high","10"="very high"),
           life_satisfied_before_covid = recode(life_satisfied_before_covid, "0"="low","1"="low","2"="low","3"="low","4"="low","5"="medium","6"="medium","7"="high","8"="high","9"="very high","10"="very high"),
           alcohol_perday = recode(alcohol_perday, "0"="none","1"="1-2","2"="1-2","3"="3-4","4"="3-4","5"="5-6","6"="5-6","7"="7-9","8"="7-9","9"="7-9","10"="10+"),
           pa_60 = recode(pa_60, "0"="none","1"="1-3","2"="1-3","3"="1-3","4"="4-5","5"="4-5","6"="6-7","7"="6-7"),
           pa_30 = recode(pa_30, "0"="none","1"="1-3","2"="1-3","3"="1-3","4"="4-5","5"="4-5","6"="6-7","7"="6-7"))
  
  # remove those under Year 10 for certain sexual health questions
  
  censor_these <- c("sh_pressure", "sh_wait", "sh_serious", "sh_condoms", "sh_pregnancy", "sh_16", "condoms_free")
  censor_yr <- c("Year 7", "Year 8", "Year 9", "Not at school/other")
  
  data[data$schyear %in% censor_yr, which(names(data) %in% censor_these)] <- NA
  
  return(data)
  
}

get_sum_stats <- function(data, var) {
  
  all <- data %>%
    mutate(school = "All Schools", schyear = "All Responses") %>% # all schools all years
    summarystats(c('school','schyear'))
  
  byvar <- data %>%
    mutate(school = "All Schools") %>% # all schools by sex
    summarystats(c('school', var[1]))
  
  if (length(var) > 1) {
    
    for (i in 2:length(var)) {
      
      byvar_i <- data %>%
        mutate(school = "All Schools") %>% # all schools by sex
        summarystats(c('school', var[i]))
      
      byvar <- bind_rows(byvar, byvar_i)
      
    }
  }
  
  all <- bind_rows(all, byvar) %>% 
    mutate(breakdown = recode(breakdown, "1" = "Male", "2" = "Female"),
           value = formattable::percent(value, digits = 1),
           lowercl = formattable::percent(lowercl, digits = 1),
           uppercl = formattable::percent(uppercl, digits = 1),
           lowereb = value - lowercl,
           uppereb = uppercl - value) %>% 
    complete(expand(., breakdown, school, 
                    nesting(question, response)),
             fill = list(count = 0, denominator = 0, value = 0,
                         lowercl = 0, uppercl = 0, lowereb = 0, uppereb = 0)) %>% 
    mutate(school = case_when(breakdown == "All Responses" ~ "All Schools", 
                              TRUE ~ school))
  
  return(all)
  
}

get_diffs <- function(stats, var) {
  
  stats_diffs_sex <- filter(stats, breakdown %in% c("Female", "Male", "Other")) %>%
    differences("school", "All Schools", "All Schools") %>% 
    filter(count.x > 4 & count.y > 4) 
  
  stats_diffs_sex$breakdown.x <- factor(stats_diffs_sex$breakdown.x)
  stats_diffs_sex$breakdown.x <- droplevels(stats_diffs_sex$breakdown.x)
  
  return(stats_diffs_sex)
  
}

# Generate text -----------------------------------------------------------

create_sum_sentence_herts <- function(dataset = chk_stats, 
                                      multi = F, 
                                      value_of_interest = F, 
                                      full_data = stats,
                                      diffs = chk_diff_sch) {
  
  if (nrow(dataset) == 0) { return ("") }
  
  if (multi == F) { # run the following if it's a simple one-choice question
    
    q_binary <- F
    
    data <- dataset
    # generate the values used for the sentences. 
    
    df <- dataset[dataset$breakdown == "All Responses" & dataset$school == "All Schools", ]
    
    most_common <- df$response[df$count == max(df$count) & !is.na(df$question_text)]
    most_v <- df$value_cens[df$count == max(df$count) & !is.na(df$question_text)]
    least_common <- df$response[df$count == min(df$count[df$count >= 0 & !is.na(df$question_text)])]
    least_v <- df$value_cens[df$count == min(df$count[df$count >= 0 & !is.na(df$question_text)])][1]
    
    if ("Female" %in% unique(data$breakdown) & "Male" %in% unique(data$breakdown)) {
      
      df <- dataset[dataset$breakdown == "Female" & dataset$school == "All Schools", ]
      
      most_common[2] <- df$response[df$count == max(df$count) & !is.na(df$question_text)]
      most_v[2] <- df$value_cens[df$count == max(df$count) & !is.na(df$question_text)]
      least_common[2] <- df$response[df$count == min(df$count[df$count >= 0 & !is.na(df$question_text)])]
      least_v[2] <- df$value_cens[df$count == min(df$count[df$count >= 0 & !is.na(df$question_text)])]
      f_count <- df$denominator[!is.na(df$question_text) & !is.na(df$question_text)][1]
      
      df <- dataset[dataset$breakdown == "Male" & dataset$school == "All Schools", ]
      
      most_common[3] <- df$response[df$count == max(df$count) & !is.na(df$question_text)]
      most_v[3] <- df$value_cens[df$count == max(df$count) & !is.na(df$question_text)]
      least_common[3] <- df$response[df$count == min(df$count[df$count >= 0 & !is.na(df$question_text)])]
      least_v[3] <- df$value_cens[df$count == min(df$count[df$count >= 0 & !is.na(df$question_text)])]
      m_count <- df$denominator[!is.na(df$question_text)][1]
      
      total_resp <- sum(dataset$count[dataset$breakdown %in% c("Female", "Male", "Other") &
                                        !is.na(dataset$question_text) &
                                        dataset$school == "All Schools"])
      o_count <- total_resp - (f_count + m_count) 
      
    }
    
    # If all students responded to this question, skip the sex breakdown. Include if not. 
    
    total_resp <- sum(dataset$count[dataset$breakdown %in% c("Female", "Male", "Other") &
                                      !is.na(dataset$question_text) &
                                      dataset$school == "All Schools"])
    max_resp <- max(full_data$denominator)
    
    perc <- round(total_resp / max_resp * 100, 1)
    percent_cens <- perc #ifelse(perc >= 95, ">=95%", ifelse(perc <= 5, "<=5%", perc))
    
    total_resp <- ifelse(total_resp == max_resp, "every student", 
                         paste0(total_resp, " students", " (", percent_cens, "%)"))
    
    add_sex_breakdown <- ""
    
    if ("Female" %in% unique(data$breakdown) & "Male" %in% unique(data$breakdown)) {
      
      add_sex_breakdown <- ifelse(total_resp != "every student", 
                                  paste0(" Of these, ", f_count, " were female, ", m_count,
                                         " were male, and ", o_count, " stated 'Other' for sex. "), "")
      
      # sometimes due to low response rate for each sex, NAs appear due to frequent rounding to 0. If this occurs, omit breakdown.
      if (is.na(f_count) | is.na(m_count)) { add_sex_breakdown <- ""}
      
    }
    
    # generate main sentence for All Responses
    
    if (is.na(total_resp)) { 
      
      return(paste0("Within the school, no students responded to this question."))
      
    } else {
      
      sentence <- paste0("Within Hertfordshire, ", total_resp, " responded to this question." , add_sex_breakdown,
                         "<br> <br> The most common response for all respondents was '", most_common[1], "', which made up ", most_v[1], 
                         " of responses and the least common response was '", least_common[1], "', with ",
                         least_v[1], " of responses.")
      
    }
    
    # generate additional sentences if male or female values for most common values differ
    if (max(data$denominator[data$breakdown == "Female" & data$school == "All Schools"])[1] != 0 &
        max(data$denominator[data$breakdown == "Male" & data$school == "All Schools"])[1] != 0 &
        "Female" %in% unique(data$breakdown) & "Male" %in% unique(data$breakdown)) {
      
      if (most_common[1] != most_common[2]) {
        
        sentence <- paste0(sentence, 
                           " For female students, the most common response instead was '", most_common[2], "' (", most_v[2], ").")
        
      }
      
      if (most_common[1] != most_common[3]) {
        
        sentence <- paste0(sentence, 
                           " For male students, the most common response instead was '", most_common[3], "' (", most_v[3], ").")
        
      }
      
    }
    
    # Repeat same process for ethnic groups if there are at least 2 unique ethnic groups that responded. 
    
    if (sum(unique(data$breakdown[data$denominator > 0]) %in% plot_ethn_grp) > 2) {
      
      e_sentence <- NA
      
      loop_grps <- unique(data$breakdown[data$denominator > 0 & data$breakdown %in% plot_ethn_grp])
      
      spot_commons <- data %>% 
        filter(denominator > 0 , breakdown %in% loop_grps) %>% 
        group_by(breakdown) %>%
        filter(count == max(count)) %>% 
        ungroup() %>% 
        select(breakdown, response, count, value) 
      
      spot_commons$unique <- ifelse(spot_commons$response != 
                                      spot_commons$response[spot_commons$breakdown == "All Responses"], 1, 0)
      
      loop <- ifelse(sum(spot_commons$unique) > 0, T, F)
      
      # get all ethnicity groups that have a different common response. 
      loop_grps <- unique(as.character(spot_commons$breakdown[spot_commons$unique == 1]))
      
      # loop to generate sentence for each ethnic group in loop_grps
      if (length(loop_grps) > 0) {
        
        for (e in 1:length(loop_grps)) {
          
          current <- as.character(loop_grps[e])
          
          df <- dataset[dataset$breakdown == current & dataset$school == "All Schools", ] %>% 
            filter(count == max(.$count))
          
          if (current %in% c("Any other ethnic group", "Mixed")) { 
            
            current <- paste0("respondents that selected '", current, "' when asked for their ethnicity")
            
            t_sentence <- c(glue::glue(" The most common response for {current} was '{df$response}' ({df$value})."))
            
            if (length(t_sentence) > 1) { 
              
              t_sentence <- paste(c(gsub(").", ") ", t_sentence[1]), 
                                    gsub(paste0("The most common response for respondents that selected '", loop_grps[e], 
                                                "' when asked for their ethnicity was "), "", 
                                         t_sentence[2:length(t_sentence)])), collapse = "and")  
              
            }
            
          } else {
            
            current <- paste(current, "respondents")
            
            t_sentence <- c(glue::glue(" The most common response for {current} was '{df$response}' ({df$value})."))
            
            if (length(t_sentence) > 1) { 
              
              t_sentence <- paste(c(gsub(").", ") ", t_sentence[1]), 
                                    gsub(paste0("The most common response for ", loop_grps[e], " respondents was "), "", 
                                         t_sentence[2:length(t_sentence)])), collapse = "and")  
              
            }
            
          }
          
          e_sentence[e] <- t_sentence
          
        }
        
        # most_common <- spot_commons$response[spot_commons$breakdown == "All Responses"]
        
        sentence <- paste0(sentence, paste(e_sentence, collapse = " "))
        
      }
      
    }
    
    
  } else { #run the following instead if it's a multi-check question
    
    # for these questions we summarise either one or a vector of responses of interest 
    #(for example, for diet we are interested in whether they say "on most days" for water, and "rarely" for sweets.)
    
    reps <- chk_var
    data <- filter(dataset, !is.na(question_text))
    
    vector_interest <- ifelse(length(value_of_interest) > 1, T, F) # if there are multiple responses of interest, T
    q_binary <- ifelse(length(unique(dataset$response)) == 2, T, F) # if the responses is just Yes or No, T
    
    if (q_binary == T) { reps <- reps[reps %in% unique(data$question[data$count != 0 & 
                                                                       data$response %in% value_of_interest])] }
    
    t <- data %>% 
      filter(breakdown %in% c("Female", "Male", "Other"), !is.na(question_text)) %>% 
      group_by(breakdown) %>% 
      summarise(total = max(denominator))
    
    total_resp <- sum(t$total)
    max_resp <- max(full_data$denominator)
    
    if ("Female" %in% unique(data$breakdown) & "Male" %in% unique(data$breakdown)) {
      
      f_count <- max(data$denominator[data$breakdown == "Female"], na.rm = T)
      m_count <- max(data$denominator[data$breakdown == "Male"], na.rm = T)
      
      t <- data %>% 
        filter(breakdown %in% c("Female", "Male", "Other"), !is.na(question_text)) %>% 
        group_by(breakdown) %>% 
        summarise(total = max(denominator))
      
      total_resp <- sum(t$total)
      o_count <- total_resp - (f_count + m_count)
      
    }
    
    perc <- round(total_resp / max_resp * 100, 1)
    percent_cens <- perc #ifelse(perc >= 95, ">=95%", ifelse(perc <= 5, "<=5%", perc))
    
    total_resp <- ifelse(total_resp == max_resp, "every student", 
                         paste0(total_resp, " students", " (", percent_cens, "%)"))
    
    # if not everyone has responded, add sex breakdown. 
    
    add_sex_breakdown <- ""
    
    if ("Female" %in% unique(data$breakdown) & "Male" %in% unique(data$breakdown)) {
      
      add_sex_breakdown <- ifelse(total_resp != "every student", 
                                  paste0(" Of these, ", f_count, " were female, ", m_count,
                                         " were male, and ", o_count, " either preferred not to say, or stated 'Other' for sex. "), "")
      
      # sometimes due to low response rate for each sex, NAs appear due to frequent rounding to 0. If this occurs, omit breakdown.
      if (is.na(f_count) | is.na(m_count)) { add_sex_breakdown <- ""}
      
    }
    
    
    if (is.na(total_resp)) { 
      
      return(paste0("Within the school, no students responded to this question."))
      
    } else {
      
      # Start main sentence. 
      sentence <- paste0("Within Hertfordshire, ", total_resp, " responded to this question." , add_sex_breakdown, "<br> <br>")
      
    }
    
    if (vector_interest == T) { reps <- na.omit(reps[!is.na(value_of_interest)]) } # if there is NA in vector of variables of interest, remove the choice from the loop. 
    
    temp <- " "
    
    if (length(reps) > 1) { #reorder by the most to least number of responses
      
      if (q_binary == T) {
        
        t <- data %>%  
          filter(question %in% reps, response == "Yes", breakdown == "All Responses") %>% 
          arrange(desc(value)) %>% 
          distinct()
        
        reps <- t$question
        
      } 
      
      for (q in 1:length(reps)) {
        
        # generate the values used for the sentences. 
        
        df <- data[data$breakdown == "All Responses" & data$question == reps[q], ] %>% 
          left_join(q_coded, by = c("question" = "question_coded")) %>% 
          drop_na(reworded)
        
        if (vector_interest == T) {
          
          df <- filter(df, response == na.omit(value_of_interest)[q])
          
        } else {
          
          df <- filter(df, response == value_of_interest)
          
          df <- mutate(df, question_text.x = menu_text)
          
        }
        
        # generate main sentence for All Responses
        
        if (q_binary == F) {
          
          temp <- paste0(temp, "When asked about ", df$question_text.x, ", ", df$count, " students answered with '", df$response, "'. ")
          
        } else if (q_binary == T & q != length(reps) & df$count > 0) {
          
          posneg_starter <- ifelse(df$response == "Yes", paste0(df$question_text.x),
                                   paste0("not ", df$question_text.x))
          
          if (q_binary == T) { df <- filter(df, response == value_of_interest) }
          
          temp <- paste0(temp, df$count, " students selected '",
                         posneg_starter, "', ")  
          
        } else if (q_binary == T & q == length(reps) & df$count > 0) {
          
          posneg_starter <- ifelse(df$response == "Yes", df$question_text.x,
                                   paste0("not ", df$question_text.x))
          
          if (q_binary == T) { df <- filter(df, response == value_of_interest) }
          
          temp <- paste0(temp, "and ", df$count, " students selected '",
                         posneg_starter, "'. ")  
          
        }
        
      }
      
    } else if (length(reps) == 1) {
      
      df <- data[data$breakdown == "All Responses" & data$question == reps[1], ] %>% 
        left_join(q_coded, by = c("question" = "question_coded")) %>% 
        drop_na(reworded) 
      
      if (q_binary == T) { df <- filter(df, response == value_of_interest) }
      
      posneg_starter <- ifelse(df$response == "Yes", paste0(df$question_text.x),
                               paste0("not ", df$question_text.x))
      
      temp <- paste0(temp, df$count, " students selected '",
                     posneg_starter, "', ")  
      
    }
    
    sentence <- paste0(sentence, temp)
    
  }
  
  # Add a short prompt(?) sentence.
  
  sentence <- paste0(sentence, " For more detail, please see the Graph or Table tabs.")
  
  # Add significant difference count sentences if they exist.
  
  keep_these <- q_coded$question_coded[!is.na(q_coded$reworded)]
  
  df <- data
  
  if (unique(df$question) %in% keep_these) {
    
    diffs_yr <- nrow(filter(create_bullets(chk_diff_yr, "between year groups")$Data, `Differences found` != "None"))
    diffs_sex <- nrow(filter(create_bullets(chk_diff_sex, "between female and male young people")$Data, `Differences found` != "None"))
    diffs_ethn <- nrow(filter(create_bullets(chk_diff_ethn, "ethnicity")$Data, `Differences found` != "None"))
    diffs_dis <- nrow(filter(create_bullets(chk_diff_dis, "District")$Data, `Differences found` != "None"))
    
    diffs_all <- sum(diffs_yr, diffs_sex, diffs_ethn, diffs_dis)
    
    diffs_table <- c(diffs_yr, diffs_sex, diffs_ethn, diffs_dis)
    names(diffs_table) <- c("year groups", "sexes", "ethnic groups", "districts")
    loop_grp <- names(diffs_table)[diffs_table > 0]
    
  } else {
    
    return(sentence)
    
  }
  
  # if its a yes or no question the "no" row is omitted so if this is the case, omit 1 from the count. 
  # if (q_binary == T) { 
  #   
  #   #diffs_sch <- nrow(filter(create_bullets(chk_diff_sch, "between schools")$Data, response == "Yes"))
  #   diffs_yr <- nrow(filter(create_bullets(chk_diff_yr, "between year groups")$Data, `Differences found` != "None"))
  #   
  # }
  
  if (diffs_all > 0) {
    
    sentence <- paste0(sentence, "<br> <br> Overall there were ", diffs_all, " significant difference(s) found between groups. ")
    
    for (s in loop_grp) {
      
      current <- which(names(diffs_table) == s)
      sentence <- paste0(sentence, "<br> <br> There were ", diffs_table[current], 
                         " significant difference(s) found between ", names(diffs_table)[current], ". ")
      
    }
    
    sentence <- paste0(sentence, "<br><br>For more detail, please see the Comparisons tab to see the full breakdown. ")
    
  } else {
    
    sentence <- paste0(sentence, "<br> <br> No statistical differences between groups were found for this question.")
    
  }
  
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
                              chk_var, 
                              rotate = 0) {
  
  df_temp <- as.vector(c(rep(F, length(unique(df$breakdown)))))

  df_temp[1] <- T

  df %>% 
    mutate(value = round(as.numeric(.$value), 3),
           lowercl = round(as.numeric(.$lowercl), 3),
           uppercl = round(as.numeric(.$uppercl), 3),
           response = str_wrap(response, 15)) %>% 
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
    e_x_axis(axisLabel = list(interval = 0, rotate = 0)) %>% 
    e_format_y_axis(suffix = "%", formatter = e_axis_formatter("percent")) %>% 
    e_legend(show = TRUE, selected = df_temp, type = "scroll", orient = "vertical",
             right = 10, top = 35, bottom = 10,
             itemHeight = 10, itemWidth = 20,
             textStyle = list(fontSize = 12)) %>% 
  # TODO
    #e_theme_custom("www/phei.json") %>%
    e_title("Demo plot") %>%
    # e_image_g(right = 180, top = 0, z = -999, style = list(opacity = 0.5, width = 120,
    #                                                        image = "https://www.hertshealthevidence.org/images/young-peoples-health-and-wellbeing-survey-logo-png-Cropped-448x190.png")) %>%
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



