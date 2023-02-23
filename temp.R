
q_coded1 <- readr::read_csv("data-raw/q_coded.csv")
data1 <- readRDS("data-raw/stats.rds")
meta1 <- readRDS("data-raw/params.rds")

params <- list(var = "schyear",
               cat = "Year 7",
               #year = input$exp_report_year,
               rendered_by_shiny = TRUE,
               q_coded = q_coded1,
               data = data1,
               meta = meta1$meta)

# Parameters --------------------------------------------------------------
year_report <- 2022
#year_report <- params$year#2021 # year of survey
school_number <- params$meta
school_number <- school_number %>%
  dplyr::filter(year %in% year_report)
school_number <- school_number$unique_schools
group <- params$var # Which variable to break down by. Usually ethnicity.
cat_of_interest <- params$cat # Only applicable if group parameter is custom and you'd like to highlight a specific category within the group. Otherwise, state NA

districts <- c(
  "Broxbourne", "Dacorum", "East Hertfordshire", "Hertsmere", "North Hertfordshire",
  "St Albans", "Stevenage", "Three Rivers", "Watford", "Welwyn Hatfield"
)

# Load data ---------------------------------------------------------------

q_coded <- params$q_coded
q_coded <- q_coded %>%
  dplyr::select("question_raw", "question_theme", "question_coded", "question_text", "reworded", "survey_text", "menu_text", "multi_cat", "multi_binary", "question_coded_gen", "survey_text_gen") %>%
  distinct()

data <- params$data
data <- data[[group]]
data <- data %>% dplyr::filter(question != "lsoa_code")

# Stats processing ----------------------------------------------------------

# DIFFERENCES
stats_diffs <- data %>%
  dplyr::mutate(year = as.numeric(year)) %>%
  dplyr::filter(year == year_report)

# get main stats df
stats <- data %>%
  dplyr:: mutate(year = as.numeric(year)) %>%
  dplyr::filter(year == year_report) %>%
  #dplyr::select(- value.y, -breakdown.y, -question_text.y, -year.y, -diff,- image) %>%
  dplyr::distinct() %>%
  dplyr::mutate(value = formattable::percent(value, digits = 1),
                lowercl = formattable::percent(lowercl, digits = 1),
                uppercl = formattable::percent(uppercl, digits = 1),
                lowereb = value - lowercl,
                uppereb = uppercl - value)

# Report processing --------------------------------------------------------
custom_filter <- ifelse(!is.na(cat_of_interest), cat_of_interest, "All Responses")

# Worries
worry_list <- unique(stats$question[grepl("worry_", stats$question)])

worries_<- stats %>% #count of responses for each answer in descending order
  filter(breakdown == 'All Responses' & question %in% worry_list & response == 'Yes') %>% 
  group_by(question) %>% 
  arrange(desc(count))

worries_$question_text <- gsub("Whether they worried about", "", worries_$question_text)

# Coping
cope_list <- unique(stats$question[grepl("cope_", stats$question)])

cope_<- stats %>% #count of responses for each answer in descending order
  filter(breakdown == 'All Responses' & question %in% cope_list & response == 'Yes') %>% 
  group_by(question) %>% 
  arrange(desc(count))

cope_$question_text <- gsub("by ", "", cope_$question_text)

# School support
options <- c("Yes", "Somewhat")

# General groups for plotting function

plot_custom_grp <- as.character(na.omit(unique(data$breakdown)))
plot_custom_grp <- plot_custom_grp[plot_custom_grp != "All Responses"]
