
differences <- function(data, by, from, to, join = NULL) {
  # looks for sig differences between report area and all responses
  # e.g. data, school, report_school, all_schools, schoolyear returns differences
  # between the respective year of the report school and all schools 
  # e.g. data, school, report_school, report_school returns differences between
  # each year of the report school (includes comaring year to itself)
  
  data_from <- data[data[[by]] %in% from, ]
  data_to <- data[data[[by]] %in% to, ] 
  
  df <- dplyr::inner_join(data_from, data_to, by = c(join, "question", "response")) %>%
    dplyr::mutate(diff = dplyr::case_when(lowercl.x > uppercl.y ~ "significantly higher than",
                                          uppercl.x < lowercl.y ~ "significantly lower than"),
                  image = paste0("graphics/", diff, ".png"))
  
  return(df)
}

get_stats_diffs <- function(stats, 
                            levels){
  
  stats <- dplyr::filter(stats, breakdown %in% levels)
  
  stats_diffs_all <- differences(stats, "breakdown", c(unique(stats$breakdown)), "All Responses")
  
  stats_diffs_all$breakdown.x <- factor(stats_diffs_all$breakdown.x, levels = levels)
  stats_diffs_all$breakdown.x <- droplevels(stats_diffs_all$breakdown.x)

  return(stats_diffs_all)
  
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
    dplyr::filter(!!dplyr::ensym(area_col) == comparator_area,
                  !!dplyr::ensym(indicator_col) %in% indicators) %>% 
    dplyr::mutate(diff = NA)
  
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
    dplyr::filter(!!dplyr::ensym(indicator_col) %in% indicators) %>% 
    dplyr::group_by(!!dplyr::ensym(indicator_col)) %>%
    dplyr::filter(!!dplyr::ensym(period_sort_col) == max(!!dplyr::ensym(period_sort_col))) %>%
    dplyr::ungroup() %>%
    dplyr::bind_rows(date_rows) %>%
    #dplyr::filter(!!dplyr::ensym(area_col) %in% c("Period", comparator_area, new_comparator_area, areas)) %>% 
    dplyr::select(!!dplyr::ensym(area_col), !!dplyr::ensym(indicator_col), !!dplyr::ensym(period_col), 
                  !!dplyr::ensym(value_col), !!dplyr::ensym(upper_ci), !!dplyr::ensym(lower_ci), diff) %>% 
    dplyr::mutate(colour = dplyr::case_when(diff == "significantly higher than" ~ higher, 
                                            diff == "significantly lower than" ~ lower, 
                                            TRUE ~ col_y),
                  colour = dplyr::case_when(!!ensym(area_col) %in% c(comparator_area, new_comparator_area) ~ col_grey, 
                                            is.na(!!ensym(value_col)) ~ "#f5f5f5",
                                            is.na(diff) ~ "#f5f5f5",
                                            TRUE ~ colour), 
                  diff = ifelse(!!dplyr::ensym(area_col) %in% c(comparator_area, new_comparator_area), "comparator", diff),
                  text_colour = ifelse(colour %in% c(col_r, col_db), "white", "black")) %>% 
    dplyr::distinct()
  
  # use str_wrap in advance
  p_data[[area_col]] <- stringr::str_wrap(p_data[[area_col]], 25)
  p_data[[indicator_col]] <- stringr::str_wrap(p_data[[indicator_col]], 30)
  
  p_data[[value_col]][is.na(p_data[[value_col]])] <- ""
  p_data[[area_col]] <- ordered(p_data[[area_col]], levels = new_levels)
  p_data[[indicator_col]] <- ordered(p_data[[indicator_col]], levels = new_ind_levels)
  
  p_data %>% 
    ggplot2::ggplot(aes(!!dplyr::ensym(area_col), 
                        !!dplyr::ensym(indicator_col), fill = colour)) +
    ggplot2::geom_tile(show.legend = T, colour = "white", size = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = !!dplyr::ensym(value_col), colour = text_colour), 
                       show.legend = FALSE, size = 3.5) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::scale_fill_manual(values = c("#e3e3e3" = "#e3e3e3", "#ffc000" ="#ffc000", 
                                          "#92d050" = "#92d050", "#c00000" = "#c00000",
                                          "#f5f5f5" = "#f5f5f5", '#bed2ff' = '#bed2ff',
                                          '#5555e5' = '#5555e5'),
                               breaks = c(lower, higher, "#ffc000"),
                               labels = labels) + 
    ggplot2::scale_colour_manual(values = c("white" = "white", "black" = "black"), guide = FALSE) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 0, face = "bold"),
                   axis.text.y = ggplot2::element_text(face = "bold"),
                   axis.ticks = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(), 
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "#f8f8f8"),
                   legend.position = "bottom") 
  
}
