# Server functions --------------------------------------------------------

get_params <- function(board = board) {
  
  output <- list()
  
  output$year <- "2021" #year of survey
  output$unique_schools <- "55" # number of unique schools
  
  output$districts <- c("Broxbourne", "Dacorum", "East Hertfordshire", "Hertsmere", "North Hertfordshire",
                        "St Albans", "Stevenage", "Three Rivers", "Watford", "Welwyn Hatfield")
  
  output$domains <- c("Demographics", "Living Conditions", "Diet and Lifestyle",
                      "Smoking and Vaping", "Alcohol Consumption", "Drug Use",
                      "Sexual Health", "Mental Health and Wellbeing", "Safety",
                      "Education", "Sustainability", "COVID-19")
  
  return(output)
  
}

# PIN VERSION 
# get_data <- function(data, 
#                      params) {
#   
#   output <- list()
#   
#   # questions lookup
#   output$q_coded <- pin_get(params$q_coded, board = "rsconnect") %>%
#     purrr::map_dfr(~ as.character(.)) %>% 
#     mutate(multi_cat = as.logical(multi_cat),
#            multi_binary = as.logical(multi_binary))
#   
#   # survey data
#   output$data <- pin_get(data, board = "rsconnect") %>%
#     purrr::map_dfr(~ as.character(.)) %>% 
#     mutate(
#       imd_quintile = case_when(imd_quintile %in% "1" ~ "Quintile 1 - Most Deprived", TRUE ~ imd_quintile),
#       imd_quintile = case_when(imd_quintile %in% "2" ~ "Quintile 2", TRUE ~ imd_quintile),
#       imd_quintile = case_when(imd_quintile %in% "3" ~ "Quintile 3", TRUE ~ imd_quintile),
#       imd_quintile = case_when(imd_quintile %in% "4" ~ "Quintile 4", TRUE ~ imd_quintile),
#       imd_quintile = case_when(imd_quintile %in% "5" ~ "Quintile 5 - Least Deprived", TRUE ~ imd_quintile)
#     ) 
#   
#   return(output)
#   
# }

get_data <- function() {
  
    output <- list()

    # questions lookup
    output$q_coded <- read_csv("data-raw/q_coded.csv") %>%
      purrr::map_dfr(~ as.character(.)) %>%
      dplyr::mutate(across(where(is.character), ~na_if(., "NA")))
    
    # group lookup
    output$grp_lookup <- read_csv("data-raw/grp_lookup.csv") %>%
      purrr::map_dfr(~ as.character(.)) %>%
      dplyr::mutate(across(where(is.character), ~na_if(., "NA")))

    # survey data
    output$data <- readRDS("data-raw/stats.rds") 

    return(output)
  
}

# get_stats <- function(data, 
#                       group, 
#                       q_coded) {
#   
#   # recode scale to categories
#   data <- data %>%
#     dplyr::mutate(life_satisfied = recode(life_satisfied, "0"="low","1"="low","2"="low","3"="low","4"="low","5"="medium","6"="medium","7"="high","8"="high","9"="very high","10"="very high"),
#                   life_worthwhile = recode(life_worthwhile, "0"="low","1"="low","2"="low","3"="low","4"="low","5"="medium","6"="medium","7"="high","8"="high","9"="very high","10"="very high"),
#                   life_happyyesterday = recode(life_happyyesterday, "0"="low","1"="low","2"="low","3"="low","4"="low","5"="medium","6"="medium","7"="high","8"="high","9"="very high","10"="very high"),
#                   life_satisfied_before_covid = recode(life_satisfied_before_covid, "0"="low","1"="low","2"="low","3"="low","4"="low","5"="medium","6"="medium","7"="high","8"="high","9"="very high","10"="very high"),
#                   alcohol_perday = recode(alcohol_perday, "0"="none","1"="1-2","2"="1-2","3"="3-4","4"="3-4","5"="5-6","6"="5-6","7"="7-9","8"="7-9","9"="7-9","10"="10+"),
#                   pa_60 = recode(pa_60, "0"="none","1"="1-3","2"="1-3","3"="1-3","4"="4-5","5"="4-5","6"="6-7","7"="6-7"),
#                   pa_30 = recode(pa_30, "0"="none","1"="1-3","2"="1-3","3"="1-3","4"="4-5","5"="4-5","6"="6-7","7"="6-7"))
#   
#   data_l <- list()
#   
#   # summary statistics for all schools. 
#   data_l[["data_all_schools"]] <- data %>% 
#     dplyr::mutate(school = "All Schools", schyear = "All Responses") %>% 
#     summarystats(c('school', 'schyear'), q_coded = q_coded)
#   
#   # summary statistics by group variable(s)
#   for (g in 1:length(group)) {
#     
#     data_l[[paste0("data_by", group[g])]] <- data %>% 
#       dplyr::mutate(school = "All Schools") %>%  
#       summarystats(c('school', group[g]), q_coded = q_coded)
#     
#   }
#   
#   # IF WE WANT TO GROUP BY DISTRICTS, WE NEED SOME ADDITIONAL PRE-PROCESSING. 
#   if (any(grepl("District", group))) {
#     
#     responses_per_district <- data %>% 
#       dplyr::select(school, District) %>%  
#       dplyr::group_by(District) %>% 
#       dplyr::mutate(school_count = n()) %>% 
#       dplyr::distinct() %>% 
#       dplyr::summarise(District, school_count, schools_per_district = n()) %>% 
#       dplyr::distinct() %>% 
#       dplyr::mutate(comparison = case_when(school_count >= 250 & schools_per_district > 1 ~ TRUE, 
#                                            TRUE ~ FALSE))
#     
#     data_d <- dplyr::filter(data, District %in% responses_per_district$District[responses_per_district$comparison == T])
#     
#     # bind to list
#     data_l[[paste0("data_by", "District")]] <- data_d %>% 
#       dplyr::mutate(school = "All Schools") %>%  
#       summarystats(c('school', "District"), q_coded = q_coded)
#     
#   }
#   
#   omit_cols <- c("Ended", "Started", "UserID", "postcode", "postcode2", "sch_hidden", "school_other_hidden") #certain columns are not needed for reporting.
#   
#   stats <- dplyr::bind_rows(data_l)
#   
#   stats <- stats %>%
#     dplyr::mutate(value = formattable::percent(value, digits = 1),
#                   lowercl = formattable::percent(lowercl, digits = 1),
#                   uppercl = formattable::percent(uppercl, digits = 1),
#                   lowereb = value - lowercl,
#                   uppereb = uppercl - value)  %>% 
#     dplyr::filter(!is.na(breakdown))
#   
# }
# 
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

# General functions -------------------------------------------------------

# quickly convert a string into an expression
run_string <- function(x) {
  eval(parse(text = x))
}

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

#' differences <- function(data, by, from, to, join = NULL) {
#'   # looks for sig differences between report area and all responses
#'   # e.g. data, school, report_school, all_schools, schoolyear returns differences
#'   # between the respective year of the report school and all schools 
#'   # e.g. data, school, report_school, report_school returns differences between
#'   # each year of the report school (includes comaring year to itself)
#'   
#'   data_from <- data[data[[by]] == from, ]
#'   data_to <- data[data[[by]] %in% to, ] 
#'   
#'   df <- dplyr::inner_join(data_from, data_to, by = c(join, "question", "response")) %>%
#'     dplyr::mutate(diff = dplyr::case_when(lowercl.x > uppercl.y ~ "significantly higher than",
#'                                           uppercl.x < lowercl.y ~ "significantly lower than"),
#'                   image = paste0("graphics/", diff, ".png"))
#'   
#'   return(df)
#' }
#' 
#' summarystats <- function(data, by, q_coded, omit_pivot = F) {
#'   # calculate summary metrics for all responses by specified groups
#'   # by should be a vector of columns to group by
#'   
#'   if (omit_pivot) {
#'     
#'     data %>% 
#'       dplyr::group_by_at(dplyr::vars( {{ by }} , question)) %>%
#'       dplyr::rename(breakdown = by[2]) %>%
#'       dplyr::mutate(count = plyr::round_any(count, 5), # disclosure control - round to base 5
#'                     denominator = sum(count)) %>% # redo denom with altered count
#'       dplyr::ungroup() %>% 
#'       dplyr::filter(denominator >= 5) %>% # disclosure control & CI calculation - filter out less than 5
#'       PHEindicatormethods::phe_proportion(., count, denominator, type='standard', confidence = 0.998) %>% 
#'       dplyr::mutate(question_text = q_coded$question_text[match(question,  #add friendly names
#'                                                                 q_coded$question_coded)]) %>% 
#'       dplyr::select(breakdown, school, question, question_text, dplyr::everything()) #reorder columns
#'     
#'   } else {
#'     
#'     data %>% 
#'       tidyr::pivot_longer(cols = -{{ by }} ,
#'                           names_to = c("question"), values_to = "response") %>%
#'       dplyr::group_by_all() %>%
#'       dplyr::filter(!is.na(response)) %>%
#'       dplyr::summarise(count = dplyr::n()) %>%
#'       dplyr::ungroup() %>%
#'       dplyr::group_by_at(dplyr::vars( {{ by }} , question)) %>%
#'       dplyr::mutate(denominator = sum(count)) %>%
#'       dplyr::rename(breakdown = by[2]) %>%
#'       dplyr::mutate(count = plyr::round_any(count, 5), # disclosure control - round to base 5
#'                     denominator = sum(count)) %>% # redo denom with altered count
#'       dplyr::ungroup() %>% 
#'       dplyr::filter(denominator >= 5) %>% # disclosure control & CI calculation - filter out less than 5
#'       PHEindicatormethods::phe_proportion(., count, denominator, type='standard', confidence = 0.998) %>% 
#'       dplyr::mutate(question_text = q_coded$question_text[match(question,  #add friendly names
#'                                                                 q_coded$question_coded)]) %>% 
#'       dplyr::select(breakdown, school, question, question_text, dplyr::everything()) #reorder columns
#'     
#'     
#'   }
#'   
#' }
#' 
#' # Data processing functions --------------------------------------------------------
#' 
#' #' Generates a dataset of summary statistics - counts, percentages, CIs
#' 
#' summarystats <- function(data, by, q_coded, omit_pivot = F) {
#'   # calculate summary metrics for all responses by specified groups
#'   # by should be a vector of columns to group by
#'   
#'   box::use(magrittr[`%>%`])
#'   
#'   if (omit_pivot) {
#'     
#'     data %>% 
#'       dplyr::group_by_at(dplyr::vars( {{ by }} , question)) %>%
#'       dplyr::rename(breakdown = by[2]) %>%
#'       dplyr::mutate(count = plyr::round_any(count, 5), # disclosure control - round to base 5
#'                     denominator = sum(count)) %>% # redo denom with altered count
#'       dplyr::ungroup() %>% 
#'       dplyr::filter(denominator >= 5) %>% # disclosure control & CI calculation - filter out less than 5
#'       PHEindicatormethods::phe_proportion(., count, denominator, type='standard', confidence = 0.998) %>% 
#'       dplyr::mutate(question_text = q_coded$question_text[match(question,  #add friendly names
#'                                                                 q_coded$question_coded)]) %>% 
#'       dplyr::select(breakdown, school, question, question_text, dplyr::everything()) #reorder columns
#'     
#'   } else {
#'     
#'     data %>% 
#'       tidyr::pivot_longer(cols = -{{ by }} ,
#'                           names_to = c("question"), values_to = "response") %>%
#'       dplyr::group_by_all() %>%
#'       dplyr::filter(!is.na(response)) %>%
#'       dplyr::summarise(count = dplyr::n()) %>%
#'       dplyr::ungroup() %>%
#'       dplyr::group_by_at(dplyr::vars( {{ by }} , question)) %>%
#'       dplyr::mutate(denominator = sum(count)) %>%
#'       dplyr::rename(breakdown = by[2]) %>%
#'       dplyr::mutate(count = plyr::round_any(count, 5), # disclosure control - round to base 5
#'                     denominator = sum(count)) %>% # redo denom with altered count
#'       dplyr::ungroup() %>% 
#'       dplyr::filter(denominator >= 5) %>% # disclosure control & CI calculation - filter out less than 5
#'       PHEindicatormethods::phe_proportion(., count, denominator, type='standard', confidence = 0.998) %>% 
#'       dplyr::mutate(question_text = q_coded$question_text[match(question,  #add friendly names
#'                                                                 q_coded$question_coded)]) %>% 
#'       dplyr::select(breakdown, school, question, question_text, dplyr::everything()) #reorder columns
#'     
#'     
#'   }
#'   
#' }
#' 
#' #' Returns dataset with differences found as new columns 
#' 
#' differences <- function(data, by, from, to, join = NULL) {
#'   # looks for sig differences between report area and all responses
#'   # e.g. data, school, report_school, all_schools, schoolyear returns differences
#'   # between the respective year of the report school and all schools 
#'   # e.g. data, school, report_school, report_school returns differences between
#'   # each year of the report school (includes comaring year to itself)
#'   
#'   box::use(magrittr[`%>%`])
#'   
#'   data_from <- data[data[[by]] == from, ]
#'   data_to <- data[data[[by]] %in% to, ] 
#'   
#'   df <- dplyr::inner_join(data_from, data_to, by = c(join, "question", "response")) %>%
#'     dplyr::mutate(diff = dplyr::case_when(lowercl.x > uppercl.y ~ "significantly higher than",
#'                                           uppercl.x < lowercl.y ~ "significantly lower than"),
#'                   image = paste0("graphics/", diff, ".png"))
#'   
#'   return(df)
#' }
#' 
#' #' Returns summary statistics of survey data - count, percentage, 95% confidence intervals. The function will recode rating variables
#' #' (e.g. scoring 1 - 10) into categorical variables before this. For district level summary statistics, only districts with >= 250 responses from
#' #' at least 2 schools will be kept. 
#' 
#' get_stats <- function(data,
#'                       group,
#'                       sch = NA,
#'                       q_coded) { 
#'   
#'   box::use(magrittr[`%>%`])
#'   box::use(dplyr[mutate, recode, select, group_by, summarise, case_when, distinct, filter, bind_rows, n, everything])
#'   box::use(tidyr[complete, expand, nesting])
#'   
#'   # recode scale to categories
#'   data <- data %>%
#'     mutate(life_satisfied = recode(life_satisfied, "0"="low","1"="low","2"="low","3"="low","4"="low","5"="medium","6"="medium","7"="high","8"="high","9"="very high","10"="very high"),
#'            life_worthwhile = recode(life_worthwhile, "0"="low","1"="low","2"="low","3"="low","4"="low","5"="medium","6"="medium","7"="high","8"="high","9"="very high","10"="very high"),
#'            life_happyyesterday = recode(life_happyyesterday, "0"="low","1"="low","2"="low","3"="low","4"="low","5"="medium","6"="medium","7"="high","8"="high","9"="very high","10"="very high"),
#'            life_satisfied_before_covid = recode(life_satisfied_before_covid, "0"="low","1"="low","2"="low","3"="low","4"="low","5"="medium","6"="medium","7"="high","8"="high","9"="very high","10"="very high"),
#'            alcohol_perday = recode(alcohol_perday, "0"="none","1"="1-2","2"="1-2","3"="3-4","4"="3-4","5"="5-6","6"="5-6","7"="7-9","8"="7-9","9"="7-9","10"="10+"),
#'            pa_60 = recode(pa_60, "0"="none","1"="1-3","2"="1-3","3"="1-3","4"="4-5","5"="4-5","6"="6-7","7"="6-7"),
#'            pa_30 = recode(pa_30, "0"="none","1"="1-3","2"="1-3","3"="1-3","4"="4-5","5"="4-5","6"="6-7","7"="6-7"))
#'   
#'   data_l <- list()
#'   
#'   # summary statistics for all schools. 
#'   data_l[["data_all_schools"]] <- data %>% 
#'     mutate(school = "All Schools", schyear = "All Responses") %>% 
#'     summarystats(c('school', 'schyear'), q_coded = q_coded)
#'   
#'   # summary statistics by group variable(s)
#'   for (g in 1:length(group)) {
#'     
#'     data_l[[paste0("data_by", group[g])]] <- data %>% 
#'       mutate(school = "All Schools") %>%  
#'       summarystats(c('school', group[g]), q_coded = q_coded)
#'     
#'   }
#'   
#'   # IF WE WANT TO GROUP BY DISTRICTS, WE NEED SOME ADDITIONAL PRE-PROCESSING. 
#'   if (any(grepl("District", group))) {
#'     
#'     responses_per_district <- data %>% 
#'       select(school, District) %>%  
#'       group_by(District) %>% 
#'       mutate(school_count = n()) %>% distinct() %>% 
#'       summarise(District, school_count, schools_per_district = n()) %>% distinct() %>% 
#'       mutate(comparison = case_when(school_count >= 250 & schools_per_district > 1 ~ TRUE, 
#'                                     TRUE ~ FALSE))
#'     
#'     data_d <- filter(data, District %in% responses_per_district$District[responses_per_district$comparison == T])
#'     
#'     # bind to list
#'     data_l[[paste0("data_by", "District")]] <- data_d %>% 
#'       mutate(school = "All Schools") %>%  
#'       summarystats(c('school', "District"), q_coded = q_coded)
#'     
#'   }
#'   
#'   omit_cols <- c("Ended", "Started", "UserID", "postcode", "postcode2", "sch_hidden", "school_other_hidden") #certain columns are not needed for reporting.
#'   
#'   stats <- bind_rows(data_l)
#'   
#'   # IF RUNNING SCHOOL REPORT (e.g. school = "Tring")
#'   if (!is.na(sch[1])) {
#'     
#'     data_l_d <- list()
#'     
#'     # summary statistics for school of interest
#'     data_l_d[["data_school"]] <- data %>%
#'       filter(school == sch) %>%
#'       mutate(schyear = "All Responses") %>% # report schools all years
#'       tidyr::pivot_longer(cols = -c('school', 'schyear') ,
#'                           names_to = c("question"), values_to = "response") %>%
#'       
#'       # temporarily bind full data for complete
#'       dplyr::bind_rows(select(stats, school, question, response) %>% filter(school == "All Schools")) %>% 
#'       complete(expand(., school, schyear, 
#'                       nesting(question, response))) %>% 
#'       dplyr::group_by_all() %>%
#'       dplyr::filter(!is.na(response)) %>%
#'       dplyr::summarise(count = dplyr::n()) %>%
#'       dplyr::ungroup() %>% 
#'       dplyr::filter(school == sch) %>% # remove the temporarily binded stats
#'       summarystats(c('school','schyear'), q_coded = q_coded, omit_pivot = T)
#'     
#'     # summary statistics by group variable(s) for school of interest
#'     for (g in 1:length(group)) {
#'       
#'       data_l_d[[paste0("data_school_by", group[g])]] <- data %>% 
#'         filter(school == sch) %>%  
#'         tidyr::pivot_longer(cols = -c('school', group[g]) ,
#'                             names_to = c("question"), values_to = "response") %>%
#'         
#'         # temporarily bind full data for complete
#'         dplyr::bind_rows(select(stats, school, question, response) %>% filter(school == "All Schools")) %>% 
#'         complete(expand(., school, .data[[group[g]]], 
#'                         nesting(question, response))) %>% 
#'         dplyr::group_by_all() %>%
#'         dplyr::filter(!is.na(response)) %>%
#'         dplyr::summarise(count = dplyr::n()) %>%
#'         dplyr::ungroup() %>% 
#'         dplyr::filter(school == sch) %>% # remove the temporarily binded stats
#'         summarystats(c('school', group[g]), q_coded = q_coded, omit_pivot = T)
#'       
#'     }
#'     
#'     # additionally, if working with school data we should exclude "Not at school/other" 
#'     stats_d <- bind_rows(data_l_d) %>% 
#'       filter(breakdown != "Not at school/other")
#'     
#'     stats <- bind_rows(stats, stats_d)
#'     
#'   }
#'   
#'   stats <- stats %>%
#'     mutate(value = formattable::percent(value, digits = 1),
#'            lowercl = formattable::percent(lowercl, digits = 1),
#'            uppercl = formattable::percent(uppercl, digits = 1),
#'            lowereb = value - lowercl,
#'            uppereb = uppercl - value)  
#'   
#'   return(stats)
#'   
#' }
#' 
#' #' Accepts output from get_stats() and outputs comparisons between groups based off of 95% confidence intervals. Additional columns are added that 
#' #' specify if one group is statistically higher/lower/similar to another group. This is done for every group, question, and response.
#' 
#' get_stats_diffs <- function(stats, 
#'                             levels, 
#'                             compare_to_all = F,
#'                             school = NA){
#'   
#'   box::use(magrittr[`%>%`])
#'   
#'   stats <- dplyr::filter(stats, breakdown %in% levels)
#'   
#'   # Check whether we are comparing one school to all schools
#'   if(!is.na(school)) {
#'     
#'     if (compare_to_all) {
#'       stats_diffs_all <- differences(stats, "school", school, "All Schools")
#'     } else {
#'       stats_diffs_all <- differences(stats, "school", school, c("All Schools", school))
#'     }
#'     
#'     
#'   } else {
#'     
#'     if(compare_to_all) {
#'       stats_diffs_all <- differences(stats, "school", "All Schools", "All Schools")
#'     } else {
#'       stats_diffs_all <- differences(stats, "school", "All Schools", c("All Schools", school))
#'     }
#'     
#'   }
#'   
#'   stats_diffs_all$breakdown.x <- factor(stats_diffs_all$breakdown.x, levels = levels)
#'   stats_diffs_all$breakdown.x <- droplevels(stats_diffs_all$breakdown.x)
#'   
#'   if(compare_to_all) {
#'     
#'     stats_diffs_all <- stats_diffs_all %>% 
#'       dplyr::filter(school.y == "All Schools") %>% 
#'       dplyr::filter(!breakdown.x == "All Responses") %>% 
#'       dplyr::filter(breakdown.y == "All Responses")
#'     
#'   }
#'   
#'   return(stats_diffs_all)
#'   
#' }

