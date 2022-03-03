
## IN DEVELOPMENT. 

library(tidyverse)
library(pins)
library(echarts4r)
library(reactable)
library(shiny)
library(tablerDash)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinybusy)
library(bs4Dash)

year <- "2021"
board_register("rsconnect",
               server = "srv-gcp-ms-connect:3939",
               key = Sys.getenv("CONNECT_API_KEY"))

domains <- c("Living Conditions", "Diet and Lifestyle",     
             "Education", "Demographics",
             "Mental Health and Wellbeing", "Smoking and Vaping",         
             "Alcohol Consumption", "Drug Use",                   
             "Sexual Health", "Safety",                  
             "Sustainability", "COVID-19")
names(domains) <- domains

# WHY???
create_sum_sentence <- function(dataset, 
                                multi = F, 
                                value_of_interest = F, 
                                full_data,
                                diffs,
                                custom_grp,
                                group_of_interest) {
  
  if (nrow(dataset) == 0) { return ("") }
  
  if (multi == F) { # run the following if it's a simple one-choice question
    
    q_binary <- F
    
    data <- dataset
    # generate the values used for the sentences. 
    
    df <- dataset[dataset$breakdown == "All Responses" & dataset$school == "All Schools", ]
    
    most_common <- df$response[df$count == max(df$count) & !is.na(df$question_text)]
    most_v <- df$value[df$count == max(df$count) & !is.na(df$question_text)]
    least_common <- df$response[df$count == min(df$count[df$count >= 0 & !is.na(df$question_text)])]
    least_v <- df$value[df$count == min(df$count[df$count >= 0 & !is.na(df$question_text)])][1]
    
    # If all students responded to this question, skip the sex breakdown. Include if not. 
    
    total_resp <- sum(dataset$count[dataset$breakdown == "All Responses" &
                                      dataset$school == "All Schools"], na.rm = TRUE)
    max_resp <- max(full_data$denominator)
    
    perc <- round(total_resp / max_resp * 100, 1)
    
    total_resp <- ifelse(total_resp == max_resp, "every student", 
                         paste0(total_resp, " students", " (", perc, "%)"))
    
    # generate main sentence for All Responses
    
    if (is.na(total_resp)) { 
      
      return(paste0("Within the school, no students responded to this question."))
      
    } else {
      
      if (!is.na(group_of_interest)[1]) {
        
        grp_df <- data %>% 
          filter(breakdown %in% group_of_interest) %>% 
          group_by(breakdown) %>% 
          filter(denominator == max(denominator)) %>% 
          ungroup() %>% 
          select(breakdown, denominator) %>% 
          distinct()
        
        prop <- paste0(round(grp_df$denominator / max(data$denominator, na.rm = T)[1] * 100, 2), "%")
        add <- paste0(" Among them, ", paste0(
          prop, " were ", grp_df$breakdown, collapse = ", "), ". ")
        
        sentence <- paste0("Within Hertfordshire, ", total_resp, " responded to this question." , add, 
                           "<br> <br> The most common response for all respondents was '", most_common[1], "', which made up ", most_v[1], 
                           " of responses and the least common response was '", least_common[1], "', with ",
                           least_v[1], " of responses.")
        
        df1 <- dataset[dataset$breakdown %in% group_of_interest & dataset$school == "All Schools", ]
        
        most_common <- df1 %>%
          group_by(breakdown) %>% 
          filter(count == max(count), !is.na(question_text)) %>% 
          summarise(most_common = paste(response, collapse = "' or '"),
                    most_v = value) %>% 
          ungroup() %>% 
          distinct() 
        
        least_common <- df1 %>% 
          group_by(breakdown) %>% 
          filter(count == min(count), !is.na(question_text), ) %>% 
          summarise(least_common = paste(response, collapse = "' or '"),
                    least_v = value) %>% 
          ungroup() %>% 
          distinct() 
        
        sentence <- paste(sentence, "<br><br>", paste0("The most common response for ", group_of_interest, " was '", most_common$most_common, 
                                                       "', which made up ", most_common$most_v, " of responses and the least common response for ", 
                                                       group_of_interest, " was '", least_common$least_common, "', with ", least_common$least_v, " of responses.", 
                                                       collapse = "<br><br>"))
        
      } else {
        
        sentence <- paste0("Within Hertfordshire, ", total_resp, " responded to this question. ",
                           "<br> <br> The most common response for all respondents was '", most_common[1], "', which made up ", most_v[1], 
                           " of responses and the least common response was '", least_common[1], "', with ",
                           least_v[1], " of responses.")
      }
      
    }
    
    
  } else { #run the following instead if it's a multi-check question
    
    reps <- unique(dataset$question)
    data <- filter(dataset, !is.na(question_text))
    
    total_resp <- max(data$denominator)[1]
    max_resp <- max(full_data$denominator)[1]
    
    perc <- round(total_resp / max_resp * 100, 1)
    
    total_resp <- ifelse(total_resp == max_resp, "every student", 
                         paste0(total_resp, " students", " (", perc, "%)"))
    
    if (is.na(total_resp)) { 
      
      return(paste0("Within the school, no students responded to this question."))
      
    } else {
      
      # Start main sentence. 
      sentence <- paste0("Within Hertfordshire, ", total_resp, " responded to this question. ")
      
      binary <- ifelse(all(unique(data$response) %in% c("Yes", "No")), TRUE, FALSE) # check if it's a Yes or No
      
      if(!is.na(group_of_interest)) {
        
        grp_df <- data %>% 
          filter(breakdown %in% group_of_interest) %>% 
          group_by(breakdown) %>% 
          filter(denominator == max(denominator)) %>% 
          ungroup() %>% 
          select(breakdown, denominator) %>% 
          distinct()
        
        prop <- paste0(round(grp_df$denominator / max(data$denominator, na.rm = T)[1] * 100, 2), "%")
        sentence <- paste0(sentence, "Among them, ", paste0(
          prop, " were ", grp_df$breakdown, collapse = ", "), ". ")
        
        for(group in 1:length(c("All Responses", group_of_interest))) {
          
          grp_df <- data %>% 
            filter(breakdown == c("All Responses", group_of_interest)[group]) 
          
          group_name <- ifelse(unique(grp_df$breakdown) == "All Responses", "students", grp_df$breakdown)
          
          # generate the values used for the sentences. 
          df <- grp_df %>% 
            filter(question %in% reps, response == value_of_interest) %>% 
            left_join(q_coded, by = c("question" = "question_coded")) %>% 
            drop_na(reworded) %>% 
            arrange(desc(count))
          
          if (binary) {
            temp <- paste0("Out of responses from ", group_name, ", ", 
                           glue_collapse(glue("{df$value} selected '{df$question_text.x}'"), ", ", last = ", and "))
          } else{
            temp <- paste0("The number of ", group_name, " who stated '", df$response[1], "' was ",
                           glue_collapse(glue("{df$value} for '{df$question_text.x}'"), ", ", last = ", and "))
          }
          
          sentence <- paste0(sentence, temp, ".<br><br>")
          
        }
      }
    }
  }
  
  # Add a short prompt(?) sentence.
  sentence <- paste0(sentence, " For more detail, please see the Graph or Table tabs.")
  
  return(sentence)
  
}

# UI ----------------------------------------------------------------

ui <- tablerDashPage(
  title = "Dashboard", 
    navbar = tablerDashNav(
      id = "nav",
      src = "img/yphws_logo.png",
      tablerNavMenu(id = "tabs",
                    pickerInput("comp", label = "Select what to group by",
                                choices = list("Sex" = "sex", 
                                               "Year group" = "schyear", 
                                               "Ethnicity" = "ethnicity",
                                               "IMD Quintile" = "imd_quintile",
                                               "Sexuality" = "sexuality", 
                                               "Child looked after" = "cla",
                                               "Young carer" = "caring", 
                                               "Adopted" = "adopted", 
                                               "Smoker" = "smoke_ever",
                                               "Self-harm" = "selfharm_ever",
                                               "Bullied" = "bullied",
                                               "District" = "District"), 
                                selected = "sex", multiple = FALSE), 
                    tablerNavMenuItem(
                      "Key Points",
                      tabName = "KeyPoints"
                    ),
                    tablerNavMenuItem(
                      "Explore Data",
                      tabName = "ExploreData"
                    ),
                    tablerNavMenuItem(
                      "Inequalities",
                      tabName = "Inequalities"
                    ),
                    tablerNavMenuItem(
                      "Export",
                      tabName = "Export"
                    ),
                    tablerNavMenuItem(
                      "About",
                      tabName = "About"
                    )
                    
                    
      )
    ),
    body = tablerDashBody(
      tablerTabItems(
        tablerTabItem(
          tabName = "KeyPoints",
          tagList(
            fluidRow( 
              uiOutput("infobox1"),
              uiOutput("infobox2"),
              uiOutput("infobox3"),
              uiOutput("infobox4"),
              uiOutput("infobox5"),
              uiOutput("infobox6")
            )
          ),
          tagList(
            fluidRow(
              tablerCard(width = 4, 
                         echarts4rOutput("key_graph")
              ), 
              tablerCard(width = 8, 
                         htmlOutput("key_themes")
              )
            )
          )
          
        ),
        tablerTabItem(
          tabName = "ExploreData",
          fluidRow(
            column(2, 
                   # pick survey topic
                   pickerInput(
                     inputId = "domains", 
                     label = "Select health topic/s:", 
                     choices = c(domains),
                     selected = "Safety", multiple = T
                   ),
                   uiOutput("questions"),
                   HTML("<a href='#anchorid'>Working anchor example</a>")
            ),
            column(10, uiOutput("explore_boxes"))
        )
      ),
      tablerTabItem(
        tabName = "Inequalities",
        tagList(
          fluidRow(
            tablerCard(title = "Demographics Indicators", 
                       width = 6, plotOutput("tartan1")),
            tablerCard(title = "Alcohole Indicators",
                       width = 6, plotOutput("tartan2"))
          ),
          fluidRow(
            tablerCard(title = "Drug Indicators", 
                       width = 6, plotOutput("tartan3")),
            tablerCard(title = "COVID-19 Indicators", 
                       width = 6, plotOutput("tartan4"))
          ),
          fluidRow(
            tablerCard(title = "Safety Indicators", 
                       width = 6, plotOutput("tartan5")),
            tablerCard(title = "Other Indicators",
                       width = 6, plotOutput("tartan6"))
          )
        )
        
      ),
      tablerTabItem(
        tabName = "Export",
        tablerCard(width = 12, title = "Data table",
                   reactableOutput("export")
        )
        
      ),
      tablerTabItem(
        tabName = "About"
      )
    )
  )
)
  



# Server ------------------------------------------------------------------

server <- function(input, output) {
  
  # Uncomment for testing
  # observe({
  #   
  #   if ("ethnicity" %in% input$comp) { browser() }
  #   
  # })

  # --Load all data-----
  rv <- reactiveValues()
  rv$params <- get_params() # params
  rv$data <- get_data(params = isolate(rv$params), data = isolate(params$data_pin)) # Raw data
  rv$data_old <- get_data(params = isolate(rv$params), data = isolate(params$prev_data_pin)) # Raw data, previous. 
  
  # --Reactive UIs----
  output$questions <- renderUI({
    questions <- isolate(rv$data$q_coded) %>% 
      mutate(survey_text = as.character(survey_text)) %>% 
      filter(question_theme %in% input$domains) 
    
    pickerInput("questions", label = "Select/type in a question (multiple can be selected)",
                choices = as.character(unique(questions$survey_text)), multiple = T, 
                selected = as.character(unique(questions$survey_text)), 
                options = list(`live-search` = TRUE))
  })
  
  # --Generate statistics using raw data
  stats <- reactive({
    get_stats(
      data = rv$data$data,
      group = input$comp,
      q_coded = rv$data$q_coded
    )
  })
  
  diffs <- reactive({
    sel_comp <- rv$data$data %>% 
      select(input$comp) 
    sel_comp <- unique(na.omit(unlist(sel_comp)))
    
    get_stats_diffs(stats = stats(), 
                    levels = c("All Responses", sel_comp), 
                    compare_to_all = F) # TODO
  })
  
  observe({
    # vector of selected vars
    single <- rv$data$q_coded %>% 
      filter(survey_text %in% input$questions, !multicat)
    
    #TODO deduplicate multicat questions.
    rv$filtered$chk_var <- rv$data$q_coded %>% 
      filter(question_coded %in% single$question_coded) %>% 
      pull(question_coded)
    # rv$filtered$chk_var <- rv$data$q_coded %>% 
    #   filter(survey_text %in% input$questions, multicat) %>% 
    #   mutate(question_raw = gsub("\\..*", "", question_raw),
    #          question_coded = question_coded_gen) %>% 
    #   distinct(question_raw, .keep_all = TRUE) %>% 
    #   bind_rows(single) %>% 
    #   pull(question_coded)
    
    # filtered datasets
    rv$filtered$chk_stats <- filter(stats(), question %in% rv$filtered$chk_var)
    rv$filtered$chk_diff <- filter(diffs(), question %in% rv$filtered$chk_var)
  })
  

# Key Points --------------------------------------------------------------

  output$infobox1 <- renderUI({
    
    value <- stats() %>% 
      filter(question == "bullied" & response == "Yes",
             breakdown == "All Responses") %>% 
      pull(value)
    
    tablerStatCard(
      value = max(value),
      title = "Been bullied",
      width = 12, 
      trend = -10 #TODO
    )
    
  })
  
  output$infobox2 <- renderUI({
    
    value <- stats() %>% 
      filter(question == "selfharmed_ever" & response == "Yes",
             breakdown == "All Responses") %>% 
      pull(value)
    
    tablerStatCard(
      value = max(value),
      title = "Self-harmed",
      width = 12,
      trend = -10 #TODO
    )
    
  })
  
  output$infobox3 <- renderUI({
    
    value <- stats() %>% 
      filter(question == "smoke_ever" & response == "I smoke regularly (once a week or more)",
             breakdown == "All Responses") %>% 
      pull(value)
    
    tablerStatCard(
      value = max(value),
      title = "Regular smokers",
      width = 12,
      trend = -15 #TODO
    )
    
  })
  
  output$infobox4 <- renderUI({
    
    value <- stats() %>% 
      filter(question == "drug_ever" & response == "I take drugs regularly (once a week or more)",
             breakdown == "All Responses") %>% 
      pull(value)
  
    tablerStatCard(
      value = max(value),
      title = "Regular drug use",
      width = 12,
      trend = +20 #TODO
    )
    
  })
  
  output$infobox5 <- renderUI({
    
    value <- stats() %>% 
      filter(question == "home_violence" & response == "Most days/Every day",
             breakdown == "All Responses") %>% 
      pull(value)
    
    tablerStatCard(
      value = max(value),
      title = "Domestic violence",
      width = 12,
      trend = -90 #TODO
    )
    
  })
  
  output$infobox6 <- renderUI({
    
    value <- stats() %>% 
      filter(question == "weight" & response == "Overweight",
             breakdown == "All Responses") %>% 
      pull(value)
    
    tablerStatCard(
      value = max(value),
      title = "Feel overweight",
      width = 12,
      trend = -10 #TODO
    )
    
  })
  
  output$key_graph <- renderEcharts4r({
    
    data.frame(
      x = LETTERS[1:5],
      y = runif(5, 1, 15)
    ) %>% 
      e_charts(x) %>%  
      e_bar(y, name = "flipped")  %>%  
      e_flip_coords() # flip axis
    
  })

output$key_themes <- renderText({
  
  # --custom groups of interest for each breakdown----
  if (!is.null(input$comp)) { 
    
    if (input$comp == "sex") { 
      group_name <- c("Female respondents") 
      group_breakdown <- c("Female")
      key_data <- stats() %>% 
        filter(breakdown %in% group_breakdown) }
    
    if (input$comp == "schyear") { 
      group_name <- NA 
      group_breakdown <- "All Respondents"
      key_data <- stats() }
    
    if (input$comp == "ethnicity") { 
      group_name <- c("Non-white respondents") 
      group_breakdown <- "Non-white"
      key_data <- stats() %>% 
        mutate(breakdown = case_when(breakdown != "White" ~ "Non-white", TRUE ~ "White")) %>% 
        group_by(breakdown, question, response) %>% 
        mutate(denominator = sum(denominator),
               count = sum(count),
               value = round((count/denominator) * 100, 2)) %>% 
        ungroup() %>% 
        distinct(breakdown, question, response, .keep_all = TRUE)
    }
    
    if (input$comp == "sexuality") { 
      group_name <- c("Gay, lesbian or bisexual respondents") 
      group_breakdown <- c("Gay, lesbian or bisexual")
      key_data <- stats() %>% 
        filter(breakdown %in% group_breakdown) }
    
    if (input$comp == "imd_quintile") { 
      group_name <- "Respondents from the most deprived IMD quintile" 
      group_breakdown <- c("Quintile 1 - Most Deprived")
      key_data <- stats() %>% 
        filter(breakdown %in% group_breakdown) }
    
    if (input$comp == "cla") { 
      group_name <- "Respondents who were children looked after" 
      group_breakdown <- "Yes"
      key_data <- stats() %>% 
        filter(breakdown %in% c("Yes")) }
    
    if (input$comp == "caring") { 
      group_name <- "Young carers" 
      group_breakdown <- "Yes"
      key_data <- stats() %>% 
        filter(breakdown %in% c("Yes")) }
    
    if (input$comp == "adopted") { 
      group_name <- "Respondents who were adopted"
      group_breakdown <- "Yes"
      key_data <- stats() %>% 
        filter(breakdown %in% c("Yes")) }
    if (input$comp == "smoke_ever") { 
      
      group_name <- c("Smokers") 
      group_breakdown <- c("I smoke occasionally (less than 1 cigarette a week)",
                           "I smoke regularly (once a week or more)")
      key_data <- stats() %>% 
        mutate(breakdown = case_when(breakdown %in% group_breakdown ~ "Smoker", 
                                     TRUE ~ breakdown)) %>% 
        group_by(breakdown, question, response) %>% 
        mutate(denominator = sum(denominator),
               count = sum(count),
               value = round((count/denominator) * 100, 2)) %>% 
        ungroup() %>% 
        distinct(breakdown, question, response, .keep_all = TRUE)
      
    }
    
    if (input$comp == "selfharm_ever") { 
      group_name <- "Respondents who had self-harmed" 
      group_breakdown <- "Yes"
      key_data <- stats() %>% 
        filter(breakdown %in% c("Yes")) }
    
    if (input$comp == "bullied") { 
      group_name <- "Respondents who were bullied" 
      group_breakdown <- "Yes"
      key_data <- stats() %>% 
        filter(breakdown %in% c("Yes")) }
    
    if (input$comp == "District") { 
      group_name <- NA
      group_breakdown <- "All Respondents"
      key_data <- stats() }
    
    # --Stats for all respondents ----
    all_data <- stats() %>%
      filter( breakdown == "All Responses" & !is.na(question_text))

    # --Stats for key respondents ----
    mh1 <- ifelse(!is.na(group_name), paste0("This statistic was ",
                                             filter(key_data, question == 'life_satisfied' & response == "low") %>%
                                               .$value, "% for ", group_name), ".")
    
    mh2 <- ifelse(!is.na(group_name), paste0("This statistic was ",
                                             filter(key_data, question == 'life_satisfied_before_covid' & response == "low") %>% .$value, 
                                             "% for ", group_name), ".")
    
    mh3 <- ifelse(!is.na(group_name), paste0("From ", group_name, " ,",
                                             filter(key_data, question =='weight' & response=='Overweight') %>%
                                               .$value, " felt overweight, and ",
                                             filter(key_data, question =='weight' & response=='Underweight') %>%
                                               .$value, " felt underweight."), "")
    
    mh4 <- ifelse(!is.na(group_name), paste0("From ", group_name, " ,",
                                             filter(key_data, question == 'mental_howaccess' & response == 'Yes') %>%
                                               .$value, " stated 'Yes'."), "")
    
    
    ls1 <- ifelse(!is.na(group_name), paste0("This statistic was ",
                                             filter(key_data, question == 'pa_60' & response == "6-7") %>%
                                               .$value, "% for ", group_name), ".")
    
    ls2 <- ifelse(!is.na(group_name), paste0("For ", group_name, " this was ",
                                             sum(filter(key_data, question == 'smoke_ever' &
                                                          response != 'I have never smoked') %>% .$value), " and ",
                                             sum(filter(key_data, question == 'smoke_ever' &
                                                          response == 'I smoke regularly (once a week or more)') %>% .$value),
                                             " respectively."), "")
    
    ls3 <- ifelse(!is.na(group_name), paste0("For ", group_name, " this was ",
                                             sum(filter(key_data, question == 'vaping' &
                                                          response != 'I have never vaped') %>% .$value), " and ",
                                             sum(filter(key_data, question == 'vaping' &
                                                          response == 'I vape regularly (once a week or more)') %>% .$value),
                                             " respectively."), "")
    
    ls4 <- ifelse(!is.na(group_name), paste0("For ", group_name, " this was ",
                                             sum(filter(key_data, question == 'alcohol_ever' &
                                                          response != 'Never') %>% .$value), " and ",
                                             sum(filter(key_data, question == 'alcohol_ever' &
                                                          response == '4 or more times a week') %>% .$value),
                                             " respectively."), "")
    
    ls5 <- ifelse(!is.na(group_name), paste0("For ", group_name, " this was ",
                                             sum(filter(key_data, question == 'drug_ever' &
                                                          response != 'I have never taken drugs') %>% .$value), " and ",
                                             sum(filter(key_data, question == 'drug_ever' &
                                                          response == 'I take drugs regularly (once a week or more)') %>% .$value),
                                             " respectively."), "")
    
    safety <- ifelse(!is.na(group_name), paste0("For ", group_name, " this was ",
                                                filter(key_data, question == 'safety_day' &
                                                         response == 'Unsafe') %>% .$value, ", ",
                                                filter(key_data, question == 'safety_dark' &
                                                         response == 'Unsafe') %>% .$value, ", ",
                                                filter(key_data, question == 'safety_school' &
                                                         response == 'Unsafe') %>% .$value, ", and ",
                                                filter(key_data, question == 'safety_journey' &
                                                         response == 'Unsafe') %>% .$value,
                                                " respectively."), "")
    
    sch1 <- ifelse(!is.na(group_name), paste0("This statistic was ",
                                              filter(key_data, question == 'schoolsupp_academic' & response == "Yes") %>%
                                                .$value, "% for ", group_name), ".")
    
    sch2 <- ifelse(!is.na(group_name), paste0("This statistic was ",
                                              filter(key_data, question == 'schoolsupp_wellbeing' & response == "Yes") %>%
                                                .$value, "% for ", group_name), ".")
    
    cov1 <- ifelse(!is.na(group_name), paste0("This statistic was ",
                                              filter(key_data, question == 'worry_covid19' & response == 'Yes') %>%
                                                .$value, "% for ", group_name), ".")
    
    cov2 <- ifelse(!is.na(group_name), paste0("This statistic was ",
                                              filter(key_data, question == 'any_vacc_taken' & response == 'Yes') %>%
                                                .$value, "% for ", group_name), ".")
    
    # --Text output ----
    output <- HTML(
      paste0(
        "This page summarises the results of", max(stats()$denominator) , 
        "pupils from schools in Hertfordshire who responded to the 2021 Young People’s Health & Wellbeing Survey (YPHWS).<br><br>",
        
        "<h1>Mental health and wellbeing</h1>", 
        filter(all_data, question == 'life_satisfied' & response == "low" & !is.na(question_text)) %>% .$value, 
        "</b> of all respondents rated their life satisfaction as low. ", mh1, "<br><br>",
        
        "<b>", filter(all_data, question == 'life_satisfied_before_covid' & response == "low" & !is.na(question_text)) %>% .$value,
        "</b>", " of all respondents rated  their satisfaction now compared to before COVID-19 as low. ", mh2, "<br><br>",
        
        "<b>", filter(all_data,  question =='weight' & response=='Overweight' & !is.na(question_text)) %>% .$value, 
        "</b> felt they were overweight while <b>", 
        filter(all_data,  question == 'weight' & response == 'Underweight' & !is.na(question_text)) %>% .$value,
        "</b> felt they were underweight. ", mh3, "<br><br>",
        
        # "The top 5 issues ", group_name, " were worried about were: ", worries_$question_text[1], " (", worries_$count[1], ")", ", ", worries_$question_text[2], " (", worries_$count[2], "), ",
        # worries_$question_text[3], " (", worries_$count[3], "), ",worries_$question_text[4], " (", worries_$count[4], "), and ", worries_$question_text[5], " (", worries_$count[5], "). <br><br>",
        # 
        "<b>", sum(filter(all_data,  question == 'mental_howaccess' & response != 'Yes') %>% .$value),
        " of respondents answered 'Not sure' or 'No' when asked if they knew how to access support and services for mental health. <b>",
        sum(filter(all_data,  question == 'mental_howaccess' & response == 'Yes') %>% .$value),
        "</b> answered 'Yes'. ", mh4, "<br><br>",
        
        "<h1>Lifestyle</h1>",
        
        "Out of all responses ", filter(all_data,  question == 'pa_60' & response == "6-7") %>% .$value,
        " had done a total of 60 minutes or more of physical activity 6-7 days of the week (in line with recommended daily physical activity guidance). ", ls1, 
        " The most common response for this question was ", 
        filter(all_data,  question =='pa_60') %>% filter(count == max(count)) %>% .$response,
        " days. <br><br>",
        
        sum(filter(all_data,  question == 'smoke_ever' & response != 'I have never smoked') %>% .$value),
        " of respondents reported having ever smoked and ", 
        sum(filter(all_data,  question == 'smoke_ever' & response == 'I smoke regularly (once a week or more)') %>% .$value),
        " reported smoking regularly (once a week or more). ", ls2, "<br><br>",
        
        sum(filter(all_data,  question == 'vaping' & response != 'I have never vaped') %>% .$value), 
        " of respondents reported having ever vaped and ", 
        sum(filter(all_data,  question == 'vaping' & response == 'I vape regularly (once a week or more)') %>% .$value), 
        " reported vaping regularly (once a week or more). ", ls3, "<br><br>",
        
        sum(filter(all_data,  question == 'alcohol_ever' & response != 'Never') %>% .$value), 
        " of respondents reported having had an alcoholic drink in the past 3 months and ", 
        sum(filter(all_data,  question == 'alcohol_ever' & response == '4 or more times a week') %>% .$value), 
        " reported drinking 4 or more times a week. ", ls4, "<br><br>",
        
        sum(filter(all_data,  question == 'drug_ever' & response != 'I have never taken drugs') %>% .$value), 
        " of respondents reported having ever taken drugs and ", 
        sum(filter(all_data,  question == 'drug_ever' & response == 'I take drugs regularly (once a week or more)') %>% .$value),
        " reported taking drugs regularly (once a week or more).", ls5, "<br><br>",
        
        "<h1>Safety</h1>",
        
        "Regarding safety, ", 
        sum(filter(all_data,  question == 'safety_day' & response == 'Unsafe') %>% .$value), 
        " of respondents felt unsafe going out during the day, ",
        sum(filter(all_data,  question == 'safety_dark' & response == 'Unsafe') %>% .$value), 
        " felt unsafe going out after dark, ",
        filter(all_data,  question == 'safety_school' & response == 'Unsafe' & !is.na(question_text)) %>% .$value, 
        " felt unsafe at school, and ",
        filter(all_data,  question == 'safety_journey' & response == 'Unsafe' & !is.na(question_text)) %>% .$value,
        " felt unsafe on their journey to school. ", safety, "<br><br>",
        
        "<h1>COVID-19</h1>",
        
        filter(all_data, question == 'worry_covid19' & response == 'Yes') %>% .$value, 
        " stated that COVID-19 was one of the issuers they worry about. ", cov1, "<br><br>",
        
        filter(all_data,  question == 'any_vacc_taken' & response == 'Yes') %>% .$value,
        " stated that they have taken any dose of the COVID-19 vaccine. ", cov2
        
      )
    )
    
    return(output)
  } else { return(NULL) }

  })
  
# Explore data --------------------------------------------------------------------

  boxes <- reactive({
    
    l <- list()
    for (i in 1:length(rv$filtered$chk_var)){
      
      # Current question
      current <- filter(rv$filtered$chk_stats, question %in% rv$filtered$chk_var[i])
      
      l[[i]] <- tabItem("name", 
                              tabBox(width = 12, side = "right", status = "success",
                                         collapsible = TRUE, 
                                         title = HTML(paste0("<hr><br><a id='anchorid'></a>", rv$filtered$chk_var[i],"<br>")),
                                         tabPanel("Summary", 
                                           HTML(create_sum_sentence(dataset = current,
                                                               multi = F,
                                                               value_of_interest = F,
                                                               full_data = rv$filtered$chk_stats,
                                                               diffs = rv$filtered$chk_diff,
                                                               custom_grp = unique(current$breakdown),
                                                               group_of_interest = unique(current$breakdown)[2])),
                                           br(),
                                           create_basic_plot(df = current,
                                                             plot_custom_grp = unique(current$breakdown),
                                                             rotate = 60,
                                                             plot_title = current$question_text[1])
                                         ),
                                     tabPanel(
                                       "Table", 
                                       rv$filtered$chk_stats %>% 
                                         mutate(value = paste0(round(as.numeric(value) * 100, 2), "%"),
                                                lowercl = paste0(round(as.numeric(lowercl) * 100, 2), "%"),
                                                uppercl = paste0(round(as.numeric(uppercl) * 100, 2), "%")
                                                ) %>% 
                                         select(breakdown, question = question_text, response, value, count, denominator,
                                                lowercl, uppercl) %>% 
                                       reactable(groupBy = c("breakdown", "question"),
                                                 columns = list(
                                                   value = colDef(maxWidth = 70),
                                                   count = colDef(maxWidth = 65),
                                                   denominator = colDef(maxWidth = 70),
                                                   lowercl = colDef(maxWidth = 70),
                                                   uppercl = colDef(maxWidth = 70)
                                                 ))
                                     )
                                         
                                         
        
        
      ) 
      )
    }
    
    return(l)
    
  })

  output$explore_boxes <- renderUI(boxes())
  
  # Inequalities ------------------------------------------------------------
  
  tartans <- reactive({
    
    df <- diffs() %>% 
      left_join(select(rv$data$q_coded, question_coded, question_theme), by = c("question" = "question_coded"))
    
    categories <- as.character(unique(df$breakdown.x))
    categories <- categories[which(!grepl("All Responses", categories))]
    
    themes <- unique(na.omit(df$question_theme))

    t <- list()
    for (i in 1:length(themes)) {
      
      loop_df <- filter(df, question_theme == themes[i]) %>% 
        mutate(Timeperiod = year,
               TimeperiodSortable = year,
               value.x = as.numeric(gsub("%", "", as.character(value.x))),
               diff = ifelse(is.na(diff), "statistically similar", diff)) %>% 
        filter(response == "Yes") #TODO 
      
      t[[i]] <- tartan(df = loop_df,
                                  indicators = unique(loop_df$question),
                                  comparator_area = "All Responses",
                                  areas = categories,
                                  palette = "rag",
                                  area_col = "breakdown.x",
                                  period_col = "Timeperiod",
                                  period_sort_col = "TimeperiodSortable",
                                  indicator_col = "question",
                                  value_col = "value.x",
                                  upper_ci = "uppercl.x",
                                  lower_ci = "lowercl.x")
    
    } 
    return(t)
    
  })
  
  output$tartan1 <- renderPlot(tartans()[[1]])
  output$tartan2 <- renderPlot(tartans()[[2]])
  output$tartan3 <- renderPlot(tartans()[[3]])
  output$tartan4 <- renderPlot(tartans()[[4]])
  output$tartan5 <- renderPlot(tartans()[[5]])
  output$tartan6 <- renderPlot(tartans()[[6]])
  

# Export ------------------------------------------------------------------

  output$export <- renderReactable({
    
    reactable(stats())
    
  })
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)
