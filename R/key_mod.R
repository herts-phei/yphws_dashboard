# UI ----------------------------------------------------------------------

key_mod <- function(id, 
                    name = "KeyPoints") {
  
  ns <- NS(id)
  
  tablerTabItem(
    tabName = name,
    tagList(
      fluidRow( 
        uiOutput(ns("infobox1")),
        uiOutput(ns("infobox2")),
        uiOutput(ns("infobox3")),
        uiOutput(ns("infobox4")),
        uiOutput(ns("infobox5")),
        uiOutput(ns("infobox6"))
      )
    ),
    tagList(
      fluidRow(
        tablerCard(width = 4, 
                   echarts4rOutput(ns("key_graph"))
        ), 
        tablerCard(width = 8, 
                   htmlOutput(ns("key_themes"))
        )
      )
    )
    
  )
  
}


# Server ------------------------------------------------------------------

key_mod_server <- function(id,
                           stats, 
                           comp) {
  moduleServer(
    id, 
    function(input, output, session) {
      
      # Info boxes --------------------------------------------------------------
      
      output$infobox1 <- renderUI({
        
        stats <- stats()
        comp <- comp()
        
        value <- stats %>% 
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
        
        stats <- stats()
        comp <- comp()
        
        value <- stats %>% 
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
        
        stats <- stats()
        comp <- comp()
        
        value <- stats %>% 
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
        
        stats <- stats()
        comp <- comp()
        
        value <- stats %>% 
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
        
        stats <- stats()
        comp <- comp()
        
        value <- stats %>% 
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
        
        stats <- stats()
        comp <- comp()
        
        value <- stats %>% 
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
      
      
      # Graphs ------------------------------------------------------------------
      
      output$key_graph <- renderEcharts4r({
        
        data.frame(
          x = LETTERS[1:5],
          y = runif(5, 1, 15)
        ) %>% 
          e_charts(x) %>%  
          e_bar(y, name = "flipped")  %>%  
          e_flip_coords() # flip axis
        
      })
      
      
      # Text --------------------------------------------------------------------
      
      output$key_themes <- renderText({
        
        stats <- stats()
        comp <- comp()
        
        # --custom groups of interest for each breakdown--
        if (!is.null(comp)) { 
          
          if (comp == "sex") { 
            group_name <- c("Female respondents") 
            group_breakdown <- c("Female")
            key_data <- stats %>% 
              filter(breakdown %in% group_breakdown) }
          
          if (comp == "schyear") { 
            group_name <- NA 
            group_breakdown <- "All Respondents"
            key_data <- stats }
          
          if (comp == "ethnicity") { 
            group_name <- c("Non-white respondents") 
            group_breakdown <- "Non-white"
            key_data <- stats %>% 
              mutate(breakdown = case_when(breakdown != "White" ~ "Non-white", TRUE ~ "White")) %>% 
              group_by(breakdown, question, response) %>% 
              mutate(denominator = sum(denominator),
                     count = sum(count),
                     value = round((count/denominator) * 100, 2)) %>% 
              ungroup() %>% 
              distinct(breakdown, question, response, .keep_all = TRUE)
          }
          
          if (comp == "sexuality") { 
            group_name <- c("Gay, lesbian or bisexual respondents") 
            group_breakdown <- c("Gay, lesbian or bisexual")
            key_data <- stats %>% 
              filter(breakdown %in% group_breakdown) }
          
          if (comp == "imd_quintile") { 
            group_name <- "Respondents from the most deprived IMD quintile" 
            group_breakdown <- c("Quintile 1 - Most Deprived")
            key_data <- stats %>% 
              filter(breakdown %in% group_breakdown) }
          
          if (comp == "cla") { 
            group_name <- "Respondents who were children looked after" 
            group_breakdown <- "Yes"
            key_data <- stats %>% 
              filter(breakdown %in% c("Yes")) }
          
          if (comp == "caring") { 
            group_name <- "Young carers" 
            group_breakdown <- "Yes"
            key_data <- stats %>% 
              filter(breakdown %in% c("Yes")) }
          
          if (comp == "adopted") { 
            group_name <- "Respondents who were adopted"
            group_breakdown <- "Yes"
            key_data <- stats %>% 
              filter(breakdown %in% c("Yes")) }
          if (comp == "smoke_ever") { 
            
            group_name <- c("Smokers") 
            group_breakdown <- c("I smoke occasionally (less than 1 cigarette a week)",
                                 "I smoke regularly (once a week or more)")
            key_data <- stats %>% 
              mutate(breakdown = case_when(breakdown %in% group_breakdown ~ "Smoker", 
                                           TRUE ~ breakdown)) %>% 
              group_by(breakdown, question, response) %>% 
              mutate(denominator = sum(denominator),
                     count = sum(count),
                     value = round((count/denominator) * 100, 2)) %>% 
              ungroup() %>% 
              distinct(breakdown, question, response, .keep_all = TRUE)
            
          }
          
          if (comp == "selfharm_ever") { 
            group_name <- "Respondents who had self-harmed" 
            group_breakdown <- "Yes"
            key_data <- stats %>% 
              filter(breakdown %in% c("Yes")) }
          
          if (comp == "bullied") { 
            group_name <- "Respondents who were bullied" 
            group_breakdown <- "Yes"
            key_data <- stats %>% 
              filter(breakdown %in% c("Yes")) }
          
          if (comp == "District") { 
            group_name <- NA
            group_breakdown <- "All Respondents"
            key_data <- stats }
          
          # --Stats for all respondents ----
          all_data <- stats %>%
            filter(breakdown == "All Responses" & !is.na(question_text))
          
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
              "This page summarises the results of", max(stats$denominator) , 
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
      
    }
  )
}


