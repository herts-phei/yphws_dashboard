
add_year_diff <- function(diffs,
                           stats) {
  
  #TODO currently only supports 2 time periods. 
  diffs_ <- diffs %>% 
    filter(breakdown == breakdown.y,
           year != year.y) 
  
  stats %>% 
    left_join(select(diffs_, breakdown, question, response, diff), 
              by = c("breakdown", "question", "response"))
  
}

create_yearly_plot <- function(stats,
                               question_p,
                               response_p,
                               title, 
                               subtitle,
                               group_id,
                               legend = F, 
                               connect = F) {
  
  rotate <- ifelse(length(unique(stats$breakdown)) > 4, 45, 0)
  
  p <- stats %>%
    dplyr::filter(question == question_p,
                  response == response_p,
                  breakdown != "All Responses") %>%
    dplyr::mutate(value = as.numeric(value),
                  lowercl = as.numeric(lowercl),
                  uppercl = as.numeric(uppercl)) %>%
    dplyr::group_by(year) %>%
    echarts4r::e_charts(breakdown) %>%
    echarts4r::e_bar(value) %>%
    # echarts4r::e_error_bar(lowercl, uppercl, name = .$breakdown,
    #                        itemStyle = list(opacity = 0.6)) %>%
    echarts4r::e_tooltip(trigger = "axis") %>%
    echarts4r::e_y_axis(name = "Percent", nameLocation = "middle", nameGap = 35, min = 0) %>%
    echarts4r::e_x_axis(axisLabel = list(interval = 0, rotate = rotate)) %>%
    echarts4r::e_format_y_axis(suffix = "%", formatter = e_axis_formatter("percent")) %>%
    echarts4r::e_grid(bottom = 100) %>%
    echarts4r::e_title(title,
                       subtitle) %>%
    echarts4r::e_theme("westeros") %>%
    echarts4r::e_group(group_id) 
  
  # Add markers if there are significant differences between years
  if(any(!is.na(stats$diff))) {
    
    # clean df with just differences
    m_df <- stats %>%  
      filter(!is.na(diff),
             year == "2021",
             question == question_p,
             response == response_p,
             breakdown != "All Responses") %>% 
      select(
        xAxis = breakdown,
        yAxis = value,
        value = diff
      ) 
    
    for (i in 1:nrow(m_df)) {
      
      # whether its pointing downwards or upwards
      direction <- ifelse(m_df$value[i] == "significantly higher than", 180, 0)
      
      # whether to present arrow as green or red
      polarity <- ifelse(m_df$value[i] == "significantly higher than", "green", "red")
      
      marker <- m_df %>% 
        mutate(value = "") %>% 
        slice(i) %>% 
        as.list()
      
      p <- p %>% echarts4r::e_mark_point(serie = "2021", data = marker, 
                                         symbol = "arrow", symbolSize = 20, symbolRotate = direction,
                                         symbolOffset = c(0, -20),
                                         itemStyle = list(color = polarity, opacity = 0.3)) 
    }
  }
  
  if(!legend) {
    p <- p %>% echarts4r::e_legend(show = F) 
  }
  
  if(connect) {
    p %>% echarts4r::e_connect_group("mh")
  } else {
    return(p)
  }
  
  
}
