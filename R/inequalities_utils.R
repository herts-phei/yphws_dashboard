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