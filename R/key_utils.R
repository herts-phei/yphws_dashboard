
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
                               q_coded, 
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
                  response %in% response_p,
                  breakdown != "All Responses") %>%
    dplyr::mutate(value = round(as.numeric(value), 2),
                  lowercl = as.numeric(lowercl),
                  uppercl = as.numeric(uppercl)) %>%
    dplyr::group_by(year) %>%
    echarts4r::e_charts(breakdown) %>%
    echarts4r::e_bar(value, barWidth = "10%", name = .$year, 
                     tooltip = list(formatter = htmlwidgets::JS("
      function(params){
      return('<b>value</b>: ' + Math.round(params.value[1] * 100, 4) + '%' +
        '<br/><b>year</b>: ' + params.seriesName +
        '<br/><b>group</b>: ' + params.value[params.encode.x[0]]) 
        }"
))) %>%
    # echarts4r::e_error_bar(lowercl, uppercl, name = .$breakdown,
    #                        itemStyle = list(opacity = 0.6)) %>%
    echarts4r::e_tooltip(trigger = "item") %>%
    echarts4r::e_y_axis(name = "Percent", nameLocation = "middle", nameGap = 35, min = 0) %>%
    echarts4r::e_x_axis(axisLabel = list(interval = 0, rotate = rotate)) %>%
    echarts4r::e_format_y_axis(suffix = "%", formatter = e_axis_formatter("percent")) %>%
    echarts4r::e_grid(bottom = 100) %>%
    echarts4r::e_title(title,
                       wrapper(subtitle)) %>%
    echarts4r::e_theme("westeros") %>%
    echarts4r::e_group(group_id) 
  
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
  
  # Add markers if there are significant differences between years
  if(nrow(m_df) > 0) {

    direction <- vector()

    for (i in 1:nrow(m_df)) {

      # whether its pointing downwards or upwards
      if (m_df$value[i] == "significantly higher than") direction <- c(direction, 180) else direction <- c(direction, 0)

      # whether to present arrow as green or red
      rag <- q_coded$polarity[q_coded$question_coded == question_p & q_coded$response == response_p]
      polarity <- ifelse((rag == "RAG - Low is good" & m_df$value[i] == "significantly higher than") |
                           (rag == "RAG - High is good" & m_df$value[i] == "significantly lower than"), 
                         "green", "red")

      # if there are two directions in the plot, need to change serie
      if (length(unique(m_df$value)) > 1) {
        serie_unique <- ifelse(direction[i] == 180, "2021", "2020")
      } else {
        serie_unique <- "2021"
      }
      
      marker <- m_df %>%
        mutate(value = "") %>%
        slice(i) %>%
        as.list()

      p <- p %>% echarts4r::e_mark_point(serie = serie_unique, data = marker,
                                         symbol = "arrow", symbolSize = 13, symbolRotate = direction,
                                         symbolOffset = c(0, -20),
                                         itemStyle = list(color = polarity, opacity = 0.3))
    }
  }
  
  if(!legend) {
    p <- p %>% echarts4r::e_legend(show = F) 
  }
  
  if(connect) {
    p %>% echarts4r::e_connect_group(group_id)
  } else {
    return(p)
  }
  
  
}
