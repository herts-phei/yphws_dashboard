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
                   htmlOutput(ns("key_themes"))),
        tablerCard(width = 4, 
                   echarts4rOutput(ns("smoke_donut"))),
        tablerCard(width = 4, 
                   echarts4rOutput(ns("drugs_donut"))), 

      ),
      fluidRow(
        tablerCard(title = "Trend Summary", width = 8, 
                   reactableOutput(ns("mhw_summary"))),
        tablerCard(width = 4, 
                   echarts4rOutput(ns("worries_graph")),
                   echarts4rOutput(ns("coping_graph")))
        # tablerCard(title = "Lifestyle Summary", width = 6, 
        #            reactableOutput(ns("lifestyle_summary")))
      )
    )
    
  )
  
}


# Server ------------------------------------------------------------------

key_mod_server <- function(id,
                           data,
                           data_old,
                           stats, 
                           stats_old,
                           comp) {
  moduleServer(
    id, 
    function(input, output, session) {
      
      # Info boxes --------------------------------------------------------------
      
      # Total responses
      output$infobox1 <- renderUI({
        
        value <- max(stats()$denominator, na.rm = TRUE) 
        tablerStatCard(
          value = value,
          title = "Total responses this year",
          width = 12
        )
      })
      
      # Total schools particiapted
      output$infobox2 <- renderUI({
        
        value <- length(unique(data()$school)) - 1
        tablerStatCard(
          value = value,
          title = "Schools participated",
          width = 12
        )
      })
      
      # Female 
      output$infobox3 <- renderUI({
        
        value <- stats()$value[stats()$question == "sex" & stats()$response == "Female"][1]
        
        tablerStatCard(
          value = value,
          title = "Female respondents",
          width = 12
        )
      })
      
      # Non-white
      output$infobox4 <- renderUI({
        
        value <- stats() %>% 
          filter(breakdown == "All Responses", 
                 question == "ethnicity",
                 !response %in% c("White", "Prefer not to say")) %>% 
          summarise(value = sum(value)) %>% 
          pull(value)
        
        tablerStatCard(
          value = value,
          title = "Non-white respondents",
          width = 12
        )
      })
      
      # LGBTQ+
      output$infobox5 <- renderUI({

        value <- stats() %>% 
          filter(breakdown == "All Responses", 
                 question == "sexuality",
                 response %in% c("Gay, lesbian or bisexual", 
                                 "Other")) %>% 
          summarise(value = sum(value)) %>% 
          pull(value)
        
        tablerStatCard(
          value = value,
          title = "LGBTQ+ respondents",
          width = 12
        )
      })
      
      # Lowest IMD Quintile 
      output$infobox6 <- renderUI({

        value <- stats() %>% 
          filter(breakdown == "All Responses", 
                 question == "imd_quintile",
                 response == "Quintile 1 - Most Deprived") %>% 
          summarise(value = sum(value)) %>% 
          pull(value)
        
        tablerStatCard(
          value = value,
          title = "From IMD Quintile 1",
          width = 12
        )
      })
      
      # Text --------------------------------------------------------------------
      
      output$key_themes <- renderText({
        
        paste("This dashboard shows the results of", max(stats()$denominator, na.rm = TRUE),
              "pupils from schools in Hertfordshire who responded to the Young People’s Health & Wellbeing Survey (YPHWS).",
              "This is second year of running this survey. This dashboard will primarily display data from the latest year",
              "(2021) with some comparisons to the previous year. To see more of the data, check the Explore Data and Inequalities tabs.<br><br>",
              "The Young People’s Health & Wellbeing Survey (YPHWS) is a youth health and wellbeing survey which gathers self-reported",
              "information annually from those aged 11-19 in Hertfordshire. The survey includes questions about home life, wellbeing, diet, physical",
              "activity, smoking, alcohol use, drug use, sexual health, mental health, bullying, and safety. <br><br>")

      })
      
      # Summary tables ------------------------------------------------------------------

      output$mhw_summary <- renderReactable({

        stats <- stats()

        stats_old <- mutate(stats_old(), `2020` = value)
        names(stats_old) <- paste0("prev_", names(stats_old))
        
        stats_ <- stats %>% 
          filter((question == "life_satisfied" & response == "low") |
                 (question == "life_satisfied_before_covid" & response == "low") |
                 (question == "weight" & response %in% c("Overweight", "Underweight")) |
                 (question == "mental_howaccess" & response == "No"))
        
        create_trend_table(stats = stats_,
                           colors = c("#d9f3ff", "#006cdf"),
                           stats_old = stats_old)
        })
      
      # output$lifestyle_summary <- renderReactable({
      # 
      #   stats <- stats()
      # 
      #   stats_old <- mutate(stats_old(), `2020` = value)
      #   names(stats_old) <- paste0("prev_", names(stats_old))
      # 
      #   stats <- stats %>%
      #     filter((question == "pa_60" & response == "6-7") |
      #              (question == "smoke_ever" & response == "I smoke regularly (once a week or more)") |
      #              (question == "alcohol" & response == "4 or more times a week") |
      #              (question == "drug_ever" & response == "I take drugs regularly (once a week or more)"))
      # 
      #   create_trend_table(stats = stats,
      #                      stats_old = stats_old)
      # 
      # })

      # Graphs -------------------------------------------------------------------

      output$smoke_donut <- renderEcharts4r({
        
        stats <- stats()
        
        stats %>% 
          filter(question == "smoke_ever",
                 response == "I smoke regularly (once a week or more)",
                 breakdown != "All Responses") %>% 
          e_charts(breakdown) %>% 
          e_pie(count, radius = c("50%", "70%")) %>% 
          e_tooltip("item") %>% 
          e_legend(bottom = 0) %>% 
          e_title("Regular smokers") %>% 
          e_theme("westeros")
        
      })
      
      output$drugs_donut <- renderEcharts4r({
        
        stats <- stats()
        
        stats %>% 
          filter(question == "drug_ever",
                 response == "I take drugs regularly (once a week or more)",
                 breakdown != "All Responses") %>% 
          e_charts(breakdown) %>% 
          e_pie(count, radius = c("50%", "70%")) %>% 
          e_tooltip("item") %>% 
          e_legend(bottom = 0) %>% 
          e_title("Regular drug use") %>% 
          e_theme("westeros")
        
      })
      
      output$worries_graph <- renderEcharts4r({
        
        stats <- stats()
        
        stats %>% 
          filter(grepl("worry_", question),
                 breakdown == "All Responses",
                 response == "Yes") %>% 
          arrange(count) %>% 
          slice(tail(row_number(), 5)) %>% 
          e_charts(question_text) %>% 
          e_bar(count) %>% 
          e_legend(show = FALSE) %>% 
          e_flip_coords() %>% 
          e_tooltip("item") %>% 
          e_grid(left = "20%") %>% 
          e_title("Top 5 worries") %>% 
          e_theme("westeros")
        
      })
      
      output$coping_graph <- renderEcharts4r({
        
        stats <- stats()
        
        stats %>% 
          filter(grepl("cope_", question),
                 breakdown == "All Responses",
                 response == "Yes") %>% 
          arrange(count) %>% 
          slice(tail(row_number(), 5)) %>% 
          e_charts(question_text) %>% 
          e_bar(count) %>% 
          e_legend(show = FALSE) %>% 
          e_flip_coords() %>% 
          e_grid(left = "20%") %>% 
          e_tooltip("item") %>% 
          e_title("Top 5 ways to cope") %>% 
          e_theme("westeros")
        
      })
      
    }
  )
}


