
# Summary tables ----------------------------------------------------------

create_trend_table <- function(stats,
                               stats_old,
                               colors = c("#ffffff", "#ff013c")) {
  
  table_df <- stats %>%
    mutate(`2021` = value) %>%
    left_join(stats_old, by = c("breakdown" = "prev_breakdown",
                                "question" = "prev_question",
                                "response" = "prev_response")) %>%
    mutate(`2020` = round(as.numeric(prev_value), 2),
           `2021` = round(as.numeric(`2021`), 2))
  
  table_df <- table_df %>% 
    mutate(`2020` = case_when(is.na(`2020`) ~ 0, TRUE ~ `2020`),
           `2020` = case_when(is.na(`2020`) ~ 0, TRUE ~ `2020`),
           Trend = `2021` - `2020`,
           question_merged = paste(question, ":", response)) %>%
    select(Indicator = question_merged, Group = breakdown, `2020`, `2021`, Trend)
  
  red_pal <- function(x) rgb(colorRamp(colors)(x), maxColorValue = 255)
  
  table_df %>%
    reactable(defaultSorted = c("Indicator", "Group"), defaultPageSize = 100,
              columns = list(
                Indicator = colDef(
                  style = JS("function(rowInfo, colInfo, state) {
        const firstSorted = state.sorted[0]
        // Merge cells if unsorted or sorting by school
        if (!firstSorted || firstSorted.id === 'Indicator') {
          const prevRow = state.pageRows[rowInfo.viewIndex - 1]
          if (prevRow && rowInfo.row['Indicator'] === prevRow['Indicator']) {
            return { visibility: 'hidden' }
          }
        }
      }")),
      `2020` = colDef(
        style = function(value) {
          normalized <- (value - min(table_df$`2020`)) / (max(table_df$`2020`) - min(table_df$`2020`))
          color <- red_pal(normalized)
          list(background = color)
        },
        maxWidth = 80
      ),
      `2021` = colDef(
        style = function(value) {
          normalized <- (value - min(table_df$`2021`)) / (max(table_df$`2021`) - min(table_df$`2021`))
          color <- red_pal(normalized)
          list(background = color)
        },
        maxWidth = 80
      ),
      Trend = colDef(
        cell = function(value) {
          if (value >= 0) paste0("+", value) else value
        },
        style = function(value) {
          color <- if (value > 0) {
            "#e00000"
          } else if (value < 0) {
            "#008000"
          }
          list(fontWeight = 600, color = color)
        },
        maxWidth = 80
      )),
      outlined = TRUE
    )
  
}

