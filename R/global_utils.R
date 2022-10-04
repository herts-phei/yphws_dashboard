# Server functions --------------------------------------------------------

get_params <- function(board = board) {
  
  readRDS("data-raw/params.rds") 
  
}

# PIN VERSION 
# get_data <- function(data, 
#                      params) {
#   
#   output <- list()
#   
#   # questions lookup
#   output$q_coded <- pin_read(board, params$q_coded) %>%
#     purrr::map_dfr(~ as.character(.)) %>% 
#     mutate(multi_cat = as.logical(multi_cat),
#            multi_binary = as.logical(multi_binary))
#   
#   # survey data
#   output$data <- pin_read(board, data) %>%
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
    output$q_coded <- read.csv("data-raw/q_coded.csv") %>%
      purrr::map_dfr(~ as.character(.)) %>%
      dplyr::mutate(dplyr::across(where(is.character), ~na_if(., "NA")))
    
    # group lookup
    output$grp_lookup <- read.csv("data-raw/grp_lookup.csv") %>%
      purrr::map_dfr(~ as.character(.)) %>%
      dplyr::mutate(dplyr::across(where(is.character), ~na_if(., "NA")))

    # survey data
    output$data <- readRDS("data-raw/stats.rds") 
    
    return(output)
  
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
