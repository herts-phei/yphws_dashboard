
params <- readRDS("params_temp.Rds")

current_year = "2023"
group <- "schyear"
cat_of_interest <- "Year 9"
q_coded <- params$q_coded


data <- params$data
data <- data[[group]]
data <- data %>% dplyr::filter(question != "lsoa_code")

# cat_of_interest <- params$cat # Only applicable if group parameter is custom and you'd like to highlight a specific category within the group. Otherwise, state NA
char_year = as.character(current_year)


df <- data
theme <- "Sexual Health"

# vars <- unique(df$question_coded_gen[df$question_theme == theme])

# Exception for PA question that is formatted differently.
# if (all(c("pa_60", "pa") %in% vars)) vars <- vars[-which(vars == "pa")]

# Determine which questions are simple vs. complex questions
# vars_simple <- df %>%
#   filter(multi == FALSE, question_theme == theme) %>%
#   arrange(order) %>%
#   pull(question_coded_gen) %>%
#   unique()
# 
# vars_multi <- df %>%
#   filter(multi == TRUE, question_theme == theme) %>%
#   arrange(order) %>%
#   pull(question_coded_gen) %>%
#   unique()

## CHUNK list
livcon_title_list <- list()
livcon_summary_list <- list()
livcon_diffs_list <- list()
livcon_plot_list <- list()
livcon_plot2_list <- list()
livcon_table_list <- list()


q_coded <- q_coded %>%
  dplyr::filter(year == as.character(current_year)) %>% 
  dplyr::select("order", "question_raw", "question_theme", "question_coded", "question_text", "reworded", "survey_text", "menu_text", "multi_cat", "multi_binary", "question_coded_gen", "survey_text_gen", "heading", "rotation") %>%
  dplyr::mutate(survey_text_gen = ifelse(survey_text_gen == "Rate how safe you feel when<U+0085>", "Rate how safe you feel when:", survey_text_gen)) %>% 
  distinct()


stats <- data %>% 
  dplyr::filter(question != "lsoa_code",
                year == current_year) 
vars_df <- data.frame(vars = unique(stats$question))

rotation_number <- ifelse(as.numeric(current_year) %% 2 == 0, 2, 1)

vars_df <- left_join(vars_df, 
                     select(q_coded, order, question_theme, question_raw, question_coded,
                            heading, question_coded_gen, survey_text, survey_text_gen,
                            multi_binary, multi_cat, rotation), 
                     by = c("vars" = "question_coded")) %>% 
  filter(#!is.na(question_raw), 
    !question_coded_gen %in% c("dfe", "school"),
    rotation %in% c(0, rotation_number)) %>% 
  mutate(multi = case_when(multi_cat == "TRUE"|multi_binary == "TRUE" ~ TRUE, 
                           TRUE ~ FALSE),
         order = as.numeric(order),
         response_filter_top = case_when(question_coded_gen %in% c("buycig", "buyalc",
                                                                   "buydrug","drugsupp",
                                                                   "shinfo", "internet",
                                                                   "mental", "skipschool",
                                                                   "clubs", "prospects",
                                                                   "prospects_support", 
                                                                   "prospects_info",
                                                                   "environ", "environ_barriers",
                                                                   "pa_barriers", "sh") ~ 3,
                                         question_coded_gen %in% c("taken", "offered",
                                                                   "worry", "cope",
                                                                   "findiff") ~ 5,
                                         TRUE ~ NA),
         value_of_interest = case_when(question_coded_gen %in% c("diet") ~ "On most days",
                                       question_coded_gen %in% c("std") ~ "I have never heard of it",
                                       question_coded_gen %in% c("sh") ~ "Agree",
                                       question_coded_gen %in% c("life") ~ "low",
                                       question_coded_gen %in% c("safety") ~ "Unsafe",
                                       question_coded_gen %in% c("clubs") ~ "Currently attending",
                                       TRUE ~ "Yes")) %>% 
  arrange(order)

chk_var <- vars_df$vars[which(vars_df$question_coded_gen %in% "sh")]
# chk_var <- unique(df$vars[df$question_coded_gen == i])
chk_stats <- filter(stats, question %in% chk_var)
# chk_diff <- dplyr::filter(stats_diffs, question %in% chk_var)
# chk_diff_sch <- filter(stats_diffs_sch, question %in% chk_var)
# chk_diff_yr <- filter(stats_diffs_yr, question %in% chk_var)
# chk_diff_gend <- filter(stats_diffs_sex, question %in% chk_var)
plot_title <- vars_df$heading[vars_df$vars %in% chk_var][1]
# response_interest <- df$value_of_interest[df$question_coded_gen == i][1]

# create_multicat_plot_herts <- function(df = chk_stats, title = "test", q_coded = q_coded, by = "percent") {
#   
#   df <- df %>% dplyr::filter(!is.na(question_text)) %>% 
#     droplevels() 
#   
#   df <- df %>% dplyr::arrange(response) %>% 
#     dplyr::left_join(select(q_coded, question_coded, menu_text), by = c("question" = "question_coded"))
#   
#   df$breakdown <- factor(df$breakdown)
#   
#   groups <- levels(df$breakdown)
#   
#   button_list <- lapply(1:length(groups), function(x){
#     list(method = "restyle",
#          args = list("transforms[0].value", groups[x]),
#          label = groups[x])
#   })
#   
#   type_list <-  list(
#     type = 'dropdown',
#     active = 0,
#     xanchor = 'left',
#     yanchor = "top",
#     pad = list('r'= 0, 't'= 0, 'b' = 0),
#     y = 1.1,
#     x = 1.02,
#     buttons = button_list
#   )
#   
#   if (by == "percent") {
#     
#     dplyr::group_by(df, breakdown, response, menu_text) %>%
#       dplyr::arrange(response) %>%
#       plotly::plot_ly(x = ~value, y = ~breakdown, type = "bar", name = ~response, color = ~response,
#                       colors = "Blues", legendgroup = ~response, orientation = 'h',
#                       hovertemplate = ~paste(stringr::str_wrap(paste0(count, " in the ", breakdown, " breakdown replied ", response, "<br>(", value,
#                                                                       " [CI:", lowercl, " to ", uppercl, "])"), 30), "<extra></extra>"),
#                       transforms = list(list(type = "filter", target = ~menu_text, operator = '=', value = groups[1]))) %>%
#       plotly::layout(barmode = "stack", 
#                      title = list(text = paste("<b>", plot_title, "</b>"), yanchor = "bottom", y = 1.3, x = 0, font = list(size= 12)),
#                      xaxis = list(title = "Percent", tickformat = ".1%"),
#                      updatemenus = list(type_list),
#                      autosize = T,
#                      # height = 600,
#                      # margin = list(r = 250,
#                      #               t = 0,
#                      #               l = 0,
#                      #               b = 0),
#                      #hovermode = 'compare',
#                      yaxis = list(title = "", autorange = "reversed")) %>%
#       plotly::config(displaylogo = FALSE, modeBarButtons = list(list("toImage", "pan2d", "resetScale2d", "hoverClosestCartesian")))
#     
#     
#   } else {
#     
#     dplyr::group_by(df, question_text, response, breakdown) %>%
#       dplyr::filter(response == "Yes") %>% 
#       dplyr::arrange(response) %>%
#       dplyr::mutate(question_text = stringr::str_wrap(question_text, 25)) %>%
#       plotly::plot_ly(x = ~count, y = ~question_text, type = "bar", name = ~response,
#                       colors = "Blues", legendgroup = ~response, orientation = 'h',
#                       transforms = list(list(type = "filter", target = ~breakdown, operator = '=', value = groups[1]))) %>%
#       plotly::layout(barmode = "stack", title = list(text = paste("<b>", plot_title, "</b>"), xanchor = "left", y = 0.85, x = 0, font = list(size = 12)),
#                      xaxis = list(title = "Count of 'Yes'"),
#                      updatemenus = list(type_list),
#                      autosize = T,
#                      # height = 800,
#                      # margin = list(r = 400,
#                      #               t = 0,
#                      #               l = 0,
#                      #               b = 0),
#                      hovermode = 'compare',
#                      yaxis = list(title = "", autorange = "reversed")) %>%
#       plotly::config(displaylogo = FALSE, modeBarButtons = list(list("toImage", "pan2d", "resetScale2d", "hoverClosestCartesian")))
#     
#   }
#   
# }
# 
# create_multicat_plot_herts <- purrr::quietly(create_multicat_plot_herts)
# 
# create_report_chunk <- function(df,
#                                 theme
# ) {
#   
#   vars <- unique(df$question_coded_gen[df$question_theme == theme])
#   
#   # Exception for PA question that is formatted differently.
#   if (all(c("pa_60", "pa") %in% vars)) vars <- vars[-which(vars == "pa")]
#   
#   # Determine which questions are simple vs. complex questions
#   vars_simple <- df %>%
#     filter(multi == FALSE, question_theme == theme) %>%
#     arrange(order) %>%
#     pull(question_coded_gen) %>%
#     unique()
#   
#   vars_multi <- df %>%
#     filter(multi == TRUE, question_theme == theme) %>%
#     arrange(order) %>%
#     pull(question_coded_gen) %>%
#     unique()
#   
#   ## CHUNK list
#   livcon_title_list <- list()
#   livcon_summary_list <- list()
#   livcon_diffs_list <- list()
#   livcon_plot_list <- list()
#   livcon_plot2_list <- list()
#   livcon_table_list <- list()
#   
#   for (i in vars) {
#     
#     simple <- ifelse(i %in% vars_simple, TRUE, FALSE)
#     chk_var <- i
#     
#     # Suppress questions with less than 5 responses
#     n_responses <- stats %>%
#       filter(question %in% chk_var, year == as.character(current_year), breakdown == "All Responses")
#     
#     if (nrow(n_responses) > 0) {
#       if (max(n_responses$denominator) < 6) {
#         
#         plot_title <- df$heading[df$vars == chk_var][1]
#         
#         livcon_summary_list[[i]] <- glue::glue("Unfortunately this question had 5 or less responses so no data is presented.")
#         
#         livcon_diffs_list[[i]] <- glue::glue(" ")
#         
#         livcon_plot_list[[i]] <- glue::glue("Unfortunately this question had 5 or less responses so no data is presented.")
#         
#         livcon_plot2_list[[i]] <- glue::glue(" ")
#         
#         livcon_table_list[[i]] <- glue::glue("Unfortunately this question had 5 or less responses so no data is presented.")
#         
#         livcon_title_list[[i]] <- paste0("## ", plot_title, " {.tabset}\n\n**'",
#                                          df$survey_text[df$vars == i][1],
#                                          "'**")
#         
#         list <- list(title_list = livcon_title_list,
#                      summary_list = livcon_summary_list,
#                      diffs_list = livcon_diffs_list,
#                      plot_list = livcon_plot_list,
#                      plot2_list = livcon_plot2_list,
#                      table_list = livcon_table_list)
#         
#         return(list)
#         
#       }
#     }
#     
#     # If there are enough responses, proceed with full set of contents
#     if (simple) {
#       
#       chk_stats <- filter(stats, question %in% chk_var)
#       chk_diff <- dplyr::filter(stats_diffs, question %in% chk_var)
#       # chk_diff_sch <- filter(stats_diffs_sch, question %in% chk_var)
#       chk_diff_yr <- filter(stats_diffs_yr, question %in% chk_var)
#       chk_diff_gend <- filter(stats_diffs_sex, question %in% chk_var)
#       
#       plot_title <- df$heading[df$vars == chk_var][1]
#       plot_angle <- ifelse(length(unique(chk_stats$response)) > 6, 60, 0)
#       
#       # bullet_df <- create_bullets(df = chk_diff_sch,
#       #                             df_yr = chk_diff_yr,
#       #                             df_gend = chk_diff_gend,
#       #                             district = district)
#       #
#       # livcon_diffs_list[[i]] <- bullet_df$Table
#       
#       livcon_summary_list[[i]] <- paste0(create_sum_sentence_herts(dataset = chk_stats,
#                                                                    multi = F,
#                                                                    value_of_interest = F,
#                                                                    full_data = stats,
#                                                                    diffs = chk_diff,
#                                                                    custom_grp = plot_custom_grp,
#                                                                    group_of_interest = cat_of_interest))
#       # diffs = bullet_df$Data,
#       # lookup = q_coded,
#       # skip_sig_diffs = F,
#       # var = chk_var))
#       #
#       
#       
#       livcon_plot_list[[i]] <- create_basic_plot(df = chk_stats,
#                                                  plot_custom_grp,
#                                                  plot_title = plot_title)
#       
#       # livcon_plot2_list[[i]] <- create_basic_plot(df = chk_stats, diff_df = chk_diff_sch, sex = F, plot_title = plot_title, year = current_year, textangle = plot_angle)
#       
#       
#       livcon_table_list[[i]] <- create_tbl(chk_diff)
#       
#       livcon_title_list[[i]] <- paste0("## ", plot_title, " {.tabset}\n\n**'",
#                                        df$survey_text[df$vars == i][1],
#                                        "'**")
#       
#     } else {
#       
#       chk_var <- unique(df$vars[df$question_coded_gen == i])
#       chk_stats <- filter(stats, question %in% chk_var)
#       chk_diff <- dplyr::filter(stats_diffs, question %in% chk_var)
#       # chk_diff_sch <- filter(stats_diffs_sch, question %in% chk_var)
#       chk_diff_yr <- filter(stats_diffs_yr, question %in% chk_var)
#       chk_diff_gend <- filter(stats_diffs_sex, question %in% chk_var)
#       plot_title <- df$heading[df$vars %in% chk_var][1]
#       response_interest <- df$value_of_interest[df$question_coded_gen == i][1]
#       
#       # If chk_var needs filtering due to too many responses for the summary text
#       if (!all(is.na(df$response_filter_top[df$question_coded_gen == i]))) {
#         
#         filter_to <- as.numeric(df$response_filter_top[df$question_coded_gen == i])
#         
#         chk_var_filtered <- chk_stats %>%
#           filter(response == response_interest, breakdown == "All Responses") %>%
#           arrange(desc(count)) %>%
#           slice(1:filter_to) %>%
#           pull(question)
#         
#       } else { chk_var_filtered <- chk_var }
#       
#       # bullet_df <- create_bullets(df = chk_diff_sch,
#       #                             df_yr = chk_diff_yr,
#       #                             df_gend = chk_diff_gend,
#       #                             district = district)
#       #
#       # livcon_diffs_list[[i]] <- bullet_df$Table
#       
#       livcon_summary_list[[i]] <- paste0(create_sum_sentence_herts(dataset = chk_stats,
#                                                                    multi = T,
#                                                                    value_of_interest = response_interest,
#                                                                    full_data = stats,
#                                                                    diffs = chk_diff,
#                                                                    custom_grp = plot_custom_grp,
#                                                                    group_of_interest = cat_of_interest))
#       # diffs = bullet_df$Data,
#       # lookup = q_coded,
#       # skip_sig_diffs = F,
#       # var = chk_var_filtered))
#       
#       livcon_plot_list[[i]] <- create_multicat_plot_herts(df = chk_stats, plot_title, q_coded = q_coded)$result
#       
#       livcon_plot2_list[[i]] <- glue::glue(" ")
#       
#       
#       livcon_table_list[[i]] <- create_tbl(chk_diff, multi_response = T)
#       
#       livcon_title_list[[i]] <- paste0("## ", plot_title, " {.tabset}\n\n**'",
#                                        df$survey_text_gen[df$vars %in% chk_var][1],  "'**")
#       
#       
#       # plot_title <- df$heading[df$vars == chk_var][1]
#       # 
#       #   livcon_summary_list[[i]] <- glue::glue("Unfortunately this question had 5 or less responses so no data is presented.")
#       # 
#       #   livcon_diffs_list[[i]] <- glue::glue(" ")
#       # 
#       #   livcon_plot_list[[i]] <- glue::glue("Unfortunately this question had 5 or less responses so no data is presented.")
#       # 
#       #   livcon_plot2_list[[i]] <- glue::glue(" ")
#       # 
#       #   livcon_table_list[[i]] <- glue::glue("Unfortunately this question had 5 or less responses so no data is presented.")
#       # 
#       #   livcon_title_list[[i]] <- paste0("## ", plot_title, " {.tabset}\n\n**'",
#       #                                    df$survey_text[df$vars == i][1],
#       #                                    "'**")
#       
#     }
#     
#   }
#   
#   list <- list(title_list = livcon_title_list,
#                summary_list = livcon_summary_list,
#                # diffs_list = livcon_diffs_list,
#                plot_list = livcon_plot_list,
#                plot2_list = livcon_plot2_list,
#                table_list = livcon_table_list)
#   
#   return(list)
#   
# }
# 
# paste_chunk_contents <- function(header,
#                                  name,
#                                  var) {
#   
#   paste0('\n\n', header, '\n\n### Summary\n\n`r ', name, '_summary_list[["', var,
#          '"]]`\n\n### Graph\n\n```{r ,results="asis", echo=FALSE, warning=FALSE, message=FALSE}\n\n', name, '_plot_list[["',
#          # var,
#          # '"]]\n\n```\n\n<br>\n\n```{r ,results="asis", echo=FALSE, warning=FALSE, message=FALSE}\n\n', name, '_plot2_list[["',
#          var,
#          '"]]\n\n```\n\n### Table\n\n```{r ,results="asis", echo=FALSE, warning=FALSE, message=FALSE}\n\n', name, '_table_list[["', var, '"]]\n\n```')
#   
# }

# get_stats_diffs <- function(stats,
#                             levels,
#                             compare_to_all = F) {
#
#   stats <- dplyr::filter(stats, breakdown %in% levels)
#
#   # get differences (new cols)
#   data_from <- stats
#   data_to <- stats[stats[["school"]] %in% "All Schools", ]
#   stats_diffs_all <- dplyr::inner_join(data_from, data_to, by = c(NULL, "question", "response")) %>%
#     dplyr::mutate(diff = dplyr::case_when(lowercl.x > uppercl.y ~ "significantly higher than",
#                                           uppercl.x < lowercl.y ~ "significantly lower than"),
#                   image = paste0("graphics/", diff, ".png"))
#
#   # clean factors
#   stats_diffs_all$breakdown.x <- factor(stats_diffs_all$breakdown.x, levels = levels)
#   stats_diffs_all$breakdown.x <- droplevels(stats_diffs_all$breakdown.x)
#
#   if(compare_to_all) {
#     stats_diffs_all <- stats_diffs_all %>%
#       dplyr::filter(school.y == "All Schools") %>%
#       dplyr::filter(!breakdown.x == "All Responses") %>%
#       dplyr::filter(breakdown.y == "All Responses")
#   }
#
#   return(stats_diffs_all)
#
# }


# summarystats <- function(data, by, q_coded, omit_pivot = F) {
#   # calculate summary metrics for all responses by specified groups
#   # by should be a vector of columns to group by
#   
#   box::use(magrittr[`%>%`])
#   
#   if (omit_pivot) {
#     
#     data %>%
#       dplyr::group_by_at(dplyr::vars( {{ by }} , question)) %>%
#       dplyr::rename(breakdown = by[2]) %>%
#       dplyr::mutate(count = plyr::round_any(count, 5), # disclosure control - round to base 5
#                     denominator = sum(count)) %>% # redo denom with altered count
#       dplyr::ungroup() %>%
#       dplyr::filter(denominator >= 5) %>% # disclosure control & CI calculation - filter out less than 5
#       PHEindicatormethods::phe_proportion(., count, denominator, type='standard', confidence = 0.998) %>%
#       dplyr::mutate(question_text = q_coded$question_text[match(question,  #add friendly names
#                                                                 q_coded$question_coded)]) %>%
#       dplyr::select(breakdown, school, question, question_text, dplyr::everything()) #reorder columns
#     
#   } else {
#     
#     data %>%
#       tidyr::pivot_longer(cols = -{{ by }} ,
#                           names_to = c("question"), values_to = "response") %>%
#       dplyr::group_by_all() %>%
#       dplyr::filter(!is.na(response)) %>%
#       dplyr::summarise(count = dplyr::n()) %>%
#       dplyr::ungroup() %>%
#       dplyr::group_by_at(dplyr::vars( {{ by }} , question)) %>%
#       dplyr::mutate(denominator = sum(count)) %>%
#       dplyr::rename(breakdown = by[2]) %>%
#       dplyr::mutate(count = plyr::round_any(count, 5), # disclosure control - round to base 5
#                     denominator = sum(count)) %>% # redo denom with altered count
#       dplyr::ungroup() %>%
#       dplyr::filter(denominator >= 5) %>% # disclosure control & CI calculation - filter out less than 5
#       PHEindicatormethods::phe_proportion(., count, denominator, type='standard', confidence = 0.998) %>%
#       dplyr::mutate(question_text = q_coded$question_text[match(question,  #add friendly names
#                                                                 q_coded$question_coded)]) %>%
#       dplyr::select(breakdown, school, question, question_text, dplyr::everything()) #reorder columns
#     
#     
#   }
#   
# }
# 
# differences <- function(data, by, from, to, join = NULL) {
#   # looks for sig differences between report area and all responses
#   # e.g. data, school, report_school, all_schools, schoolyear returns differences
#   # between the respective year of the report school and all schools
#   # e.g. data, school, report_school, report_school returns differences between
#   # each year of the report school (includes comaring year to itself)
#   
#   box::use(magrittr[`%>%`])
#   
#   data_from <- data[data[[by]] == from, ]
#   data_to <- data[data[[by]] %in% to, ]
#   
#   df <- dplyr::inner_join(data_from, data_to, by = c(join, "question", "response")) %>%
#     dplyr::mutate(diff = dplyr::case_when(lowercl.x > uppercl.y ~ "significantly higher than",
#                                           uppercl.x < lowercl.y ~ "significantly lower than"),
#                   image = paste0("graphics/", diff, ".png"))
#   
#   return(df)
# }
# 
# get_stats_diffs <- function(stats,
#                             levels,
#                             compare_to_all = F,
#                             school = NA){
#   
#   box::use(magrittr[`%>%`])
#   
#   stats <- dplyr::filter(stats, breakdown %in% levels)
#   
#   # Check whether we are comparing one school to all schools
#   if(!is.na(school)) {
#     
#     if (compare_to_all) {
#       stats_diffs_all <- differences(stats, "school", school, "All Schools")
#     } else {
#       stats_diffs_all <- differences(stats, "school", school, c("All Schools", school))
#     }
#     
#     
#   } else {
#     
#     if(compare_to_all) {
#       stats_diffs_all <- differences(stats, "school", "All Schools", "All Schools")
#     } else {
#       stats_diffs_all <- differences(stats, "school", "All Schools", c("All Schools", school))
#     }
#     
#   }
#   
#   stats_diffs_all$breakdown.x <- factor(stats_diffs_all$breakdown.x, levels = levels)
#   stats_diffs_all$breakdown.x <- droplevels(stats_diffs_all$breakdown.x)
#   
#   if(compare_to_all) {
#     
#     stats_diffs_all <- stats_diffs_all %>%
#       dplyr::filter(school.y == "All Schools") %>%
#       dplyr::filter(!breakdown.x == "All Responses") %>%
#       dplyr::filter(breakdown.y == "All Responses")
#     
#   }
#   
#   return(stats_diffs_all)
#   
# }
# 
# current_year <- 2023
# year_report <- 2023
# #year_report <- params$year#2021 # year of survey
# school_number <- params$meta
# school_number <- school_number %>%
#   dplyr::filter(year %in% year_report)
# school_number <- school_number$unique_schools
# # group <- params$var # Which variable to break down by. Usually ethnicity.
# 
# 
# 
# 
# districts <- c(
#   "Broxbourne", "Dacorum", "East Hertfordshire", "Hertsmere", "North Hertfordshire",
#   "St Albans", "Stevenage", "Three Rivers", "Watford", "Welwyn Hatfield"
# )
# 
# rotation_number <- ifelse(as.numeric(current_year) %% 2 == 0, 2, 1)
# 
# q_coded <- q_coded %>%
#   dplyr::filter(year == as.character(current_year)) %>% 
#   dplyr::select("order", "question_raw", "question_theme", "question_coded", "question_text", "reworded", "survey_text", "menu_text", "multi_cat", "multi_binary", "question_coded_gen", "survey_text_gen", "heading", "rotation") %>%
#   distinct()
# 
# # data <- params$data
# # data <- data[[group]]
# perc_stats <- data %>% 
#   dplyr::filter(question != "lsoa_code",
#                 year == current_year) %>% 
#   dplyr::mutate(across(c("value", "lowercl", "uppercl", "lowereb", "uppereb")))
# 
# stats <- data %>% 
#   dplyr::filter(question != "lsoa_code",
#                 year == current_year) 
# 
# #### stats_diffs data frames ------------------------------ ####
# stats_diffs <- data %>%
#   dplyr::mutate(year = as.numeric(year)) %>%
#   dplyr::filter(year == year_report)
# 
# stats_diffs_sch <- get_stats_diffs(stats, unique(stats$breakdown), compare_to_all = FALSE) %>% 
#   dplyr::filter(year.x == as.character(current_year),
#                 year.y == as.character(current_year))
# 
# stats_diffs_yr <- get_stats_diffs(stats = stats, 
#                                   compare_to_all = F,
#                                   levels = c("All Responses", "Year 7", "Year 8","Year 9",
#                                              "Year 10", "Year 11","Year 12", "Year 13")) %>%
#   dplyr::filter(count.x > 4, count.y > 4, school.y == "All Schools",
#                 year.x == as.character(current_year),
#                 year.y == as.character(current_year))
# 
# stats_diffs_sex <- get_stats_diffs(stats = stats,
#                                    compare_to_all = F,
#                                    levels = c("Female", "Male", "Other")) %>%
#   dplyr::filter(count.x > 4 & count.y > 4, school.y == "All Schools",
#                 year.x == as.character(current_year),
#                 year.y == as.character(current_year)) 
# 
# 
# custom_filter <- ifelse(!is.na(cat_of_interest), cat_of_interest, "All Responses")
# 
# # Worries
# worry_list <- c("worry_school", "worry_bully", "worry_phys", "worry_mental", "worry_family", "worry_money", 
#                 "worry_look", "worry_socialmedia", "worry_relationships", "worry_drugsalcohol", "worry_environment", 
#                 "worry_crime", "worry_gambling", "worry_friends", "worry_lonely",
#                 "worry_future", "worry_incrime", "worry_lgbt", "worry_racism", "worry_siblings",
#                 "worry_treatgirls", "worry_violenceboys", "worry_violencegirls")
# 
# worries_ <- stats %>% # count of responses for each answer in descending order
#   dplyr::filter(breakdown == custom_filter & question %in% worry_list & response == "Yes") %>%
#   dplyr::group_by(question) %>%
#   dplyr::arrange(desc(count))
# 
# 
# worries_$question_text <- gsub("Whether they worried about", "", worries_$question_text)
# worries_ <- worries_ %>%
#   dplyr::mutate(answer_text = dplyr::case_when(
#     question == "worry_school" ~ "school", question == "worry_look" ~ "their appearance", question == "worry_mental" ~ "their mental health",
#     question == "worry_lonely" ~ "loneliness", question == "worry_friends" ~ "their friends", question == "worry_phys" ~ "their physical health",
#     question == "worry_environment" ~ "the environment", question == "worry_family" ~ "their family", question == "worry_money" ~ "money",
#     question == "worry_safety" ~ "their safety", question == "worry_socialmedia" ~ "social media", question == "worry_bully" ~ "bullying",
#     question == "worry_relationships" ~ "relationships and/or their sexual health", question == "worry_crime" ~ "crime",
#     question == "worry_drugsalcohol" ~ "drugs and alcohol", question == "worry_gambling" ~ "gambling",
#     question == "worry_future" ~ "thier future", question == 	"worry_treatgirls" ~ "harassment/violence against girls",
#     question == "worry_violencegirls" ~ "harassment/violence against girls", question ==	"worry_racism" ~ "racism",
#     question == "worry_lgbt" ~ "sexual orientation/identity", question == "worry_violenceboys" ~"harassment/violence against boys",
#     question == "worry_incrime" ~ "getting invovled in crime"
#   ))
# 
# # Coping
# cope_list <- c("cope_nothing", "cope_think", "cope_internet", "cope_talkadult", "cope_talkfriend", "cope_counselling", "cope_exercise", "cope_creative", "cope_music", "cope_games", "cope_smoke", "cope_alcohol", "cope_selfharm", "cope_drugs", "cope_eat", "cope_talkfamily", "cope_other")
# 
# cope_ <- stats %>% # count of responses for each answer in descending order
#   dplyr::filter(breakdown == custom_filter & question %in% cope_list & response == "Yes") %>%
#   dplyr::group_by(question) %>%
#   dplyr::arrange(desc(count))
# 
# cope_$question_text <- gsub("Coping with their problems by", "", cope_$question_text)
# 
# cope_ <- cope_ %>%
#   dplyr::mutate(answer_text = case_when(
#     question == "cope_music" ~ "listening to music", question == "cope_talkfriend" ~ "talking to their friends", question == "cope_talkfamily" ~ "talking to their family",
#     question == "cope_games" ~ "playing games", question == "cope_exercise" ~ "exercising", question == "cope_think" ~ "thinking",
#     question == "cope_creative" ~ "being creative", question == "cope_nothing" ~ "doing nothing", question == "cope_eat" ~ "eating",
#     question == "cope_internet" ~ "using the internet", question == "cope_other" ~ "other ways", question == "cope_talkadult" ~ "talking to an adult",
#     question == "cope_selfharm" ~ "selfharming", question == "cope_counselling" ~ "talking to a counsellor",
#     question == "cope_alcohol" ~ "drinking alcohol", question == "cope_smoke" ~ "smoking", question == "cope_drugs" ~ "using drugs"
#   ))
# 
# # School support
# options <- c("Yes", "Somewhat")
# 
# # General groups for plotting function
# 
# plot_custom_grp <- as.character(na.omit(unique(data$breakdown)))
# if(group == "District"){
#   plot_custom_grp <- c("All Responses", "Broxbourne", "Dacorum", "East Hertfordshire", "Hertsmere", "North Hertfordshire", "St Albans", "Stevenage", "Three Rivers", "Watford", "Welwyn Hatfield", "Outside of Hertfordshire", "Prefer not to say/Unknown")
# }
# plot_custom_grp <- plot_custom_grp[plot_custom_grp != "All Responses"]
# 
# 
# low_denom_warning <- "Some metrics in this section are based on small numbers and may be unreliable."
# 
# group_name <- ifelse(!is.na(cat_of_interest), cat_of_interest, "respondents")
# 
# group_breakdown <- ifelse(!is.na(cat_of_interest), cat_of_interest, "All Responses")
# 
# stats_all <- stats %>% 
#   filter(breakdown == "All Responses" & !is.na(question_text))
# 
# mh1 <- ifelse(!is.na(cat_of_interest), paste0("This statistic was ", 
#                                               dplyr::filter(stats_all, question == 'life_satisfied' & response == "low") %>% 
#                                                 .$value, " for all responses."), "")
# 
# # mh2 <- ifelse(!is.na(cat_of_interest), paste0("This statistic was ", 
# #                                               dplyr::filter(stats_all, question == 'life_satisfied_before_covid' & response == "low") %>% .$value, " for all responses."), "")
# 
# mh3 <- ifelse(!is.na(cat_of_interest), paste0("From all respondents,  ", 
#                                               dplyr::filter(stats_all, question =='weight' & response=='Overweight') %>% 
#                                                 .$value, " felt overweight, and ",
#                                               dplyr::filter(stats_all, question =='weight' & response=='Underweight') %>% 
#                                                 .$value, " felt underweight."), "")
# 
# mh4 <- ifelse(!is.na(cat_of_interest), paste0("From all responses, ", 
#                                               dplyr::filter(stats_all, question == 'mental_howaccess' & response == 'Yes') %>% 
#                                                 .$value, " stated 'Yes'."), "")
# 
# 
# ls1 <- ifelse(!is.na(cat_of_interest), paste0("This statistic was ", 
#                                               dplyr::filter(stats_all, question == 'pa_60' & response == "6 to 7") %>% 
#                                                 .$value, " for all responses."), "")
# 
# ls2 <- ifelse(!is.na(cat_of_interest), paste0("For all responses, this was ", 
#                                               sum(dplyr::filter(stats_all, question == 'smoke_ever' & 
#                                                                   response != 'I have never smoked') %>% .$value), " and ",
#                                               sum(dplyr::filter(stats_all, question == 'smoke_ever' & 
#                                                                   response == 'I smoke regularly (once a week or more)') %>% .$value),
#                                               " respectively."), "")
# 
# ls3 <- ifelse(!is.na(cat_of_interest), paste0("For all responses, this was ", 
#                                               sum(dplyr::filter(stats_all, question == 'vaping' & 
#                                                                   response != 'I have never vaped') %>% .$value), " and ",
#                                               sum(dplyr::filter(stats_all, question == 'vaping' & 
#                                                                   response == 'I vape regularly (once a week or more)') %>% .$value),
#                                               " respectively."), "")
# 
# ls4 <- ifelse(!is.na(cat_of_interest), paste0("For all responses, this was ", 
#                                               sum(dplyr::filter(stats_all, question == 'alcohol_ever' & 
#                                                                   response != 'Never') %>% .$value), " and ",
#                                               sum(dplyr::filter(stats_all, question == 'alcohol_ever' & 
#                                                                   response == '4 or more times a week') %>% .$value),
#                                               " respectively."), "")
# 
# ls5 <- ifelse(!is.na(cat_of_interest), paste0("For all responses, this was ", 
#                                               sum(dplyr::filter(stats_all, question == 'drug_ever' & 
#                                                                   response != 'I have never taken drugs') %>% .$value), " and ",
#                                               sum(dplyr::filter(stats_all, question == 'drug_ever' & 
#                                                                   response == 'I take drugs regularly (once a week or more)') %>% .$value),
#                                               " respectively."), "")
# 
# safety <- ifelse(!is.na(cat_of_interest), paste0("For all responses, this was ", 
#                                                  dplyr::filter(stats_all, question == 'safety_day' & 
#                                                                  response == 'Unsafe') %>% .$value, ", ",
#                                                  dplyr::filter(stats_all, question == 'safety_dark' & 
#                                                                  response == 'Unsafe') %>% .$value, ", ",
#                                                  dplyr::filter(stats_all, question == 'safety_school' & 
#                                                                  response == 'Unsafe') %>% .$value, ", and ",
#                                                  dplyr::filter(stats_all, question == 'safety_journey' & 
#                                                                  response == 'Unsafe') %>% .$value, 
#                                                  " respectively."), "")
# 
# sch1 <- ifelse(!is.na(cat_of_interest), paste0("This statistic was ", 
#                                                dplyr::filter(stats_all, question == 'schoolsupp_academic' & response == "Yes") %>% 
#                                                  .$value, " for all responses."), "")
# 
# sch2 <- ifelse(!is.na(cat_of_interest), paste0("This statistic was ", 
#                                                dplyr::filter(stats_all, question == 'schoolsupp_wellbeing' & response == "Yes") %>% 
#                                                  .$value, " for all responses."), "")
# 
# 
# knitr::opts_chunk$set(results = 'asis', comment = NA, prompt = F, cache = F, echo = F, out.extra = 'class="plot"', message = F, warning = F)
# 
# # low_denom_warning <- ifelse(filter(stats, school == sch) %>% 
# #                               select(denominator) %>% max() >= 200, "",
# #                             "Please note that percentages may be up to 3% off due to small numbers. This is due to rounding the response counts to the nearest 5, in line with ONS and NHSD recommendations for disclosure control.")
# 
# key_df <- stats_diffs_sch %>%
#   filter(breakdown.x == "All Responses", breakdown.y == "All Responses", school.y == "All Schools") %>%
#   dplyr::mutate(diff = case_when(question %in% c("schoolsupp_academic", "schoolsupp_wellbeing", "pa_60") & diff == "significantly lower than" ~ "significantly worse than", question %in% c("schoolsupp_academic", "schoolsupp_wellbeing", "pa_60") & diff == "significantly higher than" ~ "significantly better than", question %in% c("hopeful_future", "smoke_ever", "vaping", "alcohol_ever", "drug_ever", "schoolsupp_academic", "schoolsupp_wellbeing", "bullied_currently", "bullied", "life_satisfied", "life_worthwhile", "mental_howaccess") & diff == "significantly higher than" ~ "significantly worse than", question %in% c("hopeful_future", "smoke_ever", "vaping", "alcohol_ever", "drug_ever", "schoolsupp_academic", "schoolsupp_wellbeing", "bullied_currently", "bullied", "life_satisfied", "life_worthwhile", "mental_howaccess") & diff == "significantly lower than" ~ "significantly better than", TRUE ~ diff))
# 
# mh_1 <- ifelse(!is.na(key_df$diff[key_df$question == "life_satisfied" & key_df$response == "low"]), 
#                paste0("This is ", key_df$diff[key_df$question == "life_satisfied" & key_df$response == "low"], " the figure for all schools (", key_df$value.y[key_df$question == "life_satisfied" & key_df$response == "low" ], ")."), "This figure is statistically similar to all schools.")
# 
# # Financial differences
# mh_3 <- ifelse(!is.na(key_df$diff[key_df$question == 'worry_money' & key_df$response == "Yes"]), paste0("This is ", key_df$diff[key_df$question == 'worry_money' & key_df$response == "Yes" ], " all schools."), "This figure is statistically similar to all schools")
# 
# mh_4 <- paste0(worries_$question_text[1], " (", worries_$count[1], ")", ", ", worries_$question_text[2], " (", worries_$count[2], ")", ", ",  worries_$question_text[3], " (", worries_$count[3], ")", ", ",worries_$question_text[4], " (", worries_$count[4], ")", ", and ", worries_$question_text[5], " (", worries_$count[5], ").")
# 
# mh_5 <- paste0(cope_$question_text[1], " (", cope_$count[1], ")", ", ", cope_$question_text[2], " (", cope_$count[2], ")", ", ",  cope_$question_text[3], " (", cope_$count[3], ")", ", ",cope_$question_text[4], " (", cope_$count[4], ")", ", and ", cope_$question_text[5], " (", cope_$count[5], ").")
# 
# mh_6 <- ifelse(!is.na(key_df$diff[key_df$question == 'mental_howaccess' & key_df$response == "No" ]), 
#                paste0("This is ", key_df$diff[key_df$question == 'mental_howaccess' & key_df$response == "No" ], " the figure for all schools (", key_df$value.y[key_df$question == 'mental_howaccess' & key_df$response == "No" ], ")."), "The proportion of respondents saying 'No' is statistically similar to all schools.")
# 
# mh_7 <- ifelse(!is.na(key_df$diff[key_df$question == 'hopeful_future' & key_df$response == "Never" ]), 
#                paste0("This is ", key_df$diff[key_df$question == 'hopeful_future' & key_df$response == "Never" ], " the figure for all schools (", key_df$value.y[key_df$question == 'hopeful_future' & key_df$response == "Never" ], ")."), "The proportion of respondents saying 'Never' is statistically similar to all schools.")
# 
# ls_1 <- ifelse(!is.na(key_df$diff[key_df$question == 'pa_60' & key_df$response == "6 to 7" ]), 
#                paste0("This is ", key_df$diff[key_df$question == 'pa_60' & key_df$response == "6 to 7" ], " the figure for all schools (", key_df$value.y[key_df$question == 'pa_60' & key_df$response == "6 to 7" ], ")."), "This figure is statistically similar to all schools.")
# 
# # ls_2 <- ifelse(!is.na(key_df$diff[key_df$question == 'smoke_ever' & key_df$response == "I smoke regularly (once a week or more)" ]), 
# #                paste0("This is ", key_df$diff[key_df$question == 'smoke_ever' & key_df$response == "I smoke regularly (once a week or more)" ], " the figure for all schools (", key_df$value.y[key_df$question == 'smoke_ever' & key_df$response == "I smoke regularly (once a week or more)" ], ")."), "This figure is statistically similar to all schools.")
# 
# ls_3 <- ifelse(!is.na(key_df$diff[key_df$question == 'vaping' & key_df$response == "I vape regularly (once a week or more)" ]), paste0("This is ", key_df$diff[key_df$question == 'vaping' & key_df$response == "I vape regularly (once a week or more)" ], " the figure for all schools (", key_df$value.y[key_df$question == 'vaping' & key_df$response == "I vape regularly (once a week or more)" ], ")."), "This figure is statistically similar to all schools.")
# 
# ls_4 <- ifelse(!is.na(key_df$diff[key_df$question == 'alcohol_ever' & key_df$response == "4 or more times a week" ]), paste0("This is ", key_df$diff[key_df$question == 'alcohol_ever' & key_df$response == "4 or more times a week" ], " the figure for all schools (", key_df$value.y[key_df$question == 'alcohol_ever' & key_df$response == "4 or more times a week" ], ")."), "This figure is statistically similar to all schools.")
# 
# ls_5 <- ifelse(!is.na(key_df$diff[key_df$question == 'drug_ever' & key_df$response == "I take drugs regularly (once a week or more)" ]), paste0("This is ", key_df$diff[key_df$question == 'drug_ever' & key_df$response == "I take drugs regularly (once a week or more)" ], " the figure for all schools (", key_df$value.y[key_df$question == 'drug_ever' & key_df$response == "I take drugs regularly (once a week or more)" ], ")."), "This figure is statistically similar to all schools.")
# 
# bully_1 <- ifelse(!is.na(key_df$diff[key_df$question == 'bullied' & key_df$response == "Yes" ]), paste0("This is ", key_df$diff[key_df$question == 'bullied' & key_df$response == "Yes" ], " the figure for all schools (", key_df$value.y[key_df$question == 'bullied' & key_df$response == "Yes" ], ")."), "This figure is statistically similar to all schools.")
# 
# bully_2 <- ifelse(!is.na(key_df$diff[key_df$question == 'bullied_currently' & key_df$response == "Yes" ]), paste0("This is ", key_df$diff[key_df$question == 'bullied_currently' & key_df$response == "Yes" ], " the figure for all schools (", key_df$value.y[key_df$question == 'bullied_currently' & key_df$response == "Yes" ], ")."), "This figure is statistically similar to all schools.")
# 
# ss_1 <- ifelse(!is.na(key_df$diff[key_df$question == 'schoolsupp_academic' & key_df$response == 'Yes' ]), paste0("This is ", key_df$diff[key_df$question == 'schoolsupp_academic' & key_df$response == 'Yes' ], " the figure for all schools (", key_df$value.y[key_df$question == 'schoolsupp_academic' & key_df$response == 'Yes' ], ")."), "This figure is statistically similar to all schools.")
# 
# ss_2 <- ifelse(!is.na(key_df$diff[key_df$question == 'schoolsupp_wellbeing' & key_df$response == 'Yes' ]), paste0("This is ", key_df$diff[key_df$question == 'schoolsupp_wellbeing' & key_df$response == 'Yes' ], " the figure for all schools (", key_df$value.y[key_df$question == 'schoolsupp_wellbeing' & key_df$response == 'Yes' ], ")."), "This figure is statistically similar to all schools.")
# 
# vars_df <- data.frame(vars = unique(stats$question))
# 
# vars_df <- left_join(vars_df, 
#                      select(q_coded, order, question_theme, question_raw, question_coded,
#                             heading, question_coded_gen, survey_text, survey_text_gen,
#                             multi_binary, multi_cat, rotation), 
#                      by = c("vars" = "question_coded")) %>% 
#   filter(#!is.na(question_raw), 
#     !question_coded_gen %in% c("dfe", "school"),
#     rotation %in% c(0, rotation_number)) %>% 
#   mutate(multi = case_when(multi_cat == "TRUE"|multi_binary == "TRUE" ~ TRUE, 
#                            TRUE ~ FALSE),
#          order = as.numeric(order),
#          response_filter_top = case_when(question_coded_gen %in% c("buycig", "buyalc",
#                                                                    "buydrug","drugsupp",
#                                                                    "shinfo", "internet",
#                                                                    "mental", "skipschool",
#                                                                    "clubs", "prospects",
#                                                                    "prospects_support", 
#                                                                    "prospects_info",
#                                                                    "environ", "environ_barriers",
#                                                                    "pa_barriers", "sh") ~ 3,
#                                          question_coded_gen %in% c("taken", "offered",
#                                                                    "worry", "cope",
#                                                                    "findiff") ~ 5,
#                                          TRUE ~ NA),
#          value_of_interest = case_when(question_coded_gen %in% c("diet") ~ "On most days",
#                                        question_coded_gen %in% c("std") ~ "I have never heard of it",
#                                        question_coded_gen %in% c("sh") ~ "Agree",
#                                        question_coded_gen %in% c("life") ~ "low",
#                                        question_coded_gen %in% c("safety") ~ "Unsafe",
#                                        question_coded_gen %in% c("clubs") ~ "Currently attending",
#                                        TRUE ~ "Yes")) %>% 
#   arrange(order)
# 
# list <- create_report_chunk(df = vars_df, theme = "Demographics")
# 
# ## CHUNK list
# demographics_title_list <- list$title_list
# demographics_summary_list <- list$summary_list
# demographics_diffs_list <- list$diffs_list
# demographics_plot_list <- list$plot_list
# demographics_plot2_list <- list$plot2_list
# demographics_table_list <- list$table_list

create_multicat_plot_herts(df = chk_stats, plot_title = plot_title, q_coded = q_coded)

create_multicat_plot_herts <- function(df = chk_stats, plot_title = plot_title, q_coded = q_coded) {
  
  multi_bin <- all(df$response %in% c("Yes", "No"))
  
  df <- df %>% dplyr::filter(!is.na(question_text)) %>% 
    droplevels() 
  
  df <- df %>% dplyr::arrange(response) %>% 
    dplyr::left_join(select(q_coded, question_coded, menu_text), by = c("question" = "question_coded"))
  
  df$menu_text <- factor(df$menu_text)
  
  groups <- levels(df$menu_text)
  
  # if (by == "percent") { groups <- unique(df$menu_text) } else { groups <- unique(df$breakdown) }
  
  # button_list <- lapply(1:length(groups), function(x){
  #   list(method = "restyle",
  #        args = list("transforms[0].value", groups[x]),
  #        label = groups[x])
  # })
  
  # type_list <-  list(
  #   type = 'dropdown',
  #   active = 0,
  #   xanchor = 'left',
  #   yanchor = "top",
  #   pad = list('r'= 0, 't'= 0, 'b' = 0),
  #   y = 1.1,
  #   x = 1.02,
  #   buttons = button_list
  # )
  
  if (multi_bin) {
    
    df %>%
      dplyr::filter(response == "Yes") %>%
      dplyr::arrange(response) %>%
      plotly::plot_ly(x = ~value, y = ~question_text, type = "bar", name = ~breakdown, color = ~breakdown,
                      colors = "viridis", legendgroup = ~breakdown, orientation = 'h',
                      hovertemplate = ~paste(stringr::str_wrap(paste0(value, " (", count, ") in the ", breakdown, " breakdown replied ",
                                                                      response, "<br>(CI:", lowercl, " to ",
                                                                      uppercl, ")"), 30), "<extra></extra>")) %>%
      plotly::layout(title = list(text = paste("<b>", plot_title, "</b>"),
                                  yanchor = "bottom", y = 1.3, x = -1, font = list(size= 12)),
                     xaxis = list(title = "Percent", tickformat = ".1%"),
                     yaxis = list(title = "", autorange = "reversed"),
                     margin = list(l = 50, r = 50, b = 50, t = 50)) %>%
      plotly::config(displaylogo = FALSE,
                     modeBarButtons = list(list("toImage", "zoomIn2d", "zoomOut2d", "pan2d", "resetScale2d", "hoverClosestCartesian")))
    
  } else {
    
    groups <- unique(df$menu_text)
    
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
      x = 1.02
    )
    
    df %>%
      dplyr::arrange(response) %>%
      plotly::plot_ly(x = ~value, y = ~breakdown, type = "bar", name = ~response, color = ~response,
                      colors = "viridis", legendgroup = ~response, orientation = 'h',
                      hovertemplate = ~paste(stringr::str_wrap(paste0(value, " (", count, ") in the ", breakdown, " breakdown replied ",
                                                                      response, "<br>(CI:", lowercl, " to ",
                                                                      uppercl, ")"), 30), "<extra></extra>"),
                      transforms = list(list(type = "filter", target = ~menu_text, operator = '=', value = groups[1]))) %>%
      plotly::layout(barmode = "stack",
                     title = list(text = paste("<b>", plot_title, "</b>"), yanchor = "bottom", y = 1.3, x = -1, font = list(size= 12)),
                     xaxis = list(title = "Percent", tickformat = ".1%"),
                     updatemenus = list(type_list),
                     yaxis = list(title = "", autorange = "reversed"),
                     margin = list(l = 50, r = 50, b = 50, t = 50)) %>%
      plotly::config(displaylogo = FALSE,
                     modeBarButtons = list(list("toImage", "zoomIn2d", "zoomOut2d", "pan2d", "resetScale2d", "hoverClosestCartesian")))
    
  }  
  
}
