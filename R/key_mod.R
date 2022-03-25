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
                   echarts4rOutput(ns("ethn_donut"))),
        tablerCard(width = 4, 
                   echarts4rOutput(ns("imd_donut"))), 

      ),
      fluidRow(
        tabItem("name", 
                bs4TabCard(width = 12, side = "right", status = "success",
                           collapsible = FALSE, 
                           title = "",
                           tabPanel("Mental Health", 
                                    fluidRow(
                                      column(6, echarts4rOutput(ns("life_sat")),
                                             echarts4rOutput(ns("life_worth"))),
                                      column(6, echarts4rOutput(ns("self_harm")),
                                             echarts4rOutput(ns("mh_services")))
                                    ),
                                    fluidRow(
                                      column(12, 
                                             uiOutput(ns("mh_year")),
                                             uiOutput(ns("mh_breakdown")))
                                    ),
                                    fluidRow(
                                      column(6, 
                                             echarts4rOutput(ns("worries_graph"))),
                                      column(6, 
                                             echarts4rOutput(ns("coping_graph")))
                                    )     
                           ),
                           tabPanel(
                             "Lifestyle"
                           ),
                           tabPanel(
                             "Safety"
                           ),
                           tabPanel(
                             "Sexual Health"
                           ),
                           tabPanel(
                             "Other"
                           )) )
      )
    )
    
  )
  
}


# Server ------------------------------------------------------------------

key_mod_server <- function(id,
                           params,
                           data,
                           data_old,
                           stats, 
                           stats_old,
                           q_coded,
                           comp) {
  moduleServer(
    id, 
    function(input, output, session) {
      
      ns <- NS(id)
      
      # for certain plots we want to visualise both years worth of data
      stats_combined <- reactive({
        
        stats_old <- mutate(stats_old(), year = as.character(as.numeric(params()$year) - 1)) 
        
        stats() %>% 
          mutate(year = as.character(as.numeric(params()$year))) %>% 
          bind_rows(stats_old)
        
      })
      
      
      # Info boxes --------------------------------------------------------------
      
      # Total responses
      output$infobox1 <- renderUI({
        
        value <- max(stats()$denominator, na.rm = TRUE) 
        tablerStatCard(
          value = value,
          title = "Responses this year",
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
          title = "IMD Quint. 1(most deprived)",
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
      

      # Group summary -----------------------------------------------------------
      output$ethn_donut <- renderEcharts4r({
        
        stats <- stats()
        
        stats %>% 
          filter(breakdown == "All Responses",
                 question == "ethnicity") %>% 
          mutate(value = round(as.numeric(value), 4) * 100) %>% 
          e_charts(response) %>% 
          e_pie(value, radius = c("50%", "70%")) %>% 
          e_labels(formatter = htmlwidgets::JS("function(params){
           return(`${params.value}`);}")) %>% 
          e_tooltip("item") %>% 
          e_legend(bottom = 0) %>% 
          e_title("Ethnicity breakdown in %") %>% 
          e_theme_custom("phei.json")
        
      })
      
      output$imd_donut <- renderEcharts4r({
        
        stats <- stats()
        
        stats %>% 
          filter(breakdown == "All Responses",
                 question == "imd_quintile") %>% 
          mutate(value = round(as.numeric(value), 4) * 100) %>% 
          e_charts(response) %>% 
          e_pie(value, radius = c("50%", "70%")) %>% 
          e_labels(formatter = htmlwidgets::JS("function(params){
           return(`${params.value}`);}")) %>% 
          e_tooltip("item") %>% 
          e_legend(bottom = 0) %>% 
          e_title("IMD breakdown in %") %>% 
          e_theme_custom("phei.json")
        
      })

      
      # MH & Wellbeing Graphs -------------------------------------------------------------------

      output$life_sat <- renderEcharts4r({
        
        stats_combined() %>% 
          filter(question == "life_satisfied",
                 response == "low",
                 breakdown != "All Responses") %>% 
          mutate(value = as.numeric(value)) %>% 
          group_by(year) %>% 
          e_charts(breakdown) %>% 
          e_bar(value) %>%
          e_tooltip(trigger = "axis") %>% 
          e_y_axis(name = "Percent", nameLocation = "middle", nameGap = 35, max = 1, min = 0) %>% 
          e_x_axis(axisLabel = list(interval = 0)) %>% 
          e_format_y_axis(suffix = "%", formatter = e_axis_formatter("percent")) %>% 
          e_grid(bottom = 100) %>% 
          e_title("Low life satisfaction",
                  "Proportion from each group that responded with a rating of 4 or less out of 10.") %>% 
          e_theme_custom("phei.json") %>% 
          e_group("mh")
        
      })
      
      output$life_worth <- renderEcharts4r({
         
         stats_combined() %>% 
           filter(question == "bullied",
                  response == "Yes",
                  breakdown != "All Responses") %>% 
           mutate(value = as.numeric(value)) %>% 
           group_by(year) %>% 
           e_charts(breakdown) %>% 
           e_bar(value) %>%
           e_tooltip(trigger = "axis") %>% 
           e_y_axis(name = "Percent", nameLocation = "middle", nameGap = 35, max = 1, min = 0) %>% 
           e_x_axis(axisLabel = list(interval = 0)) %>% 
           e_format_y_axis(suffix = "%", formatter = e_axis_formatter("percent")) %>% 
           e_grid(bottom = 100) %>% 
           e_title("Bullying",
                   "Proportion from each group that stated that they have been bullied before.") %>% 
           e_theme_custom("phei.json") %>% 
           e_group("mh")
         
         })
        
      output$self_harm <- renderEcharts4r({
          
          stats_combined() %>% 
            filter(question == "selfharm_ever",
                   response == "Yes",
                   breakdown != "All Responses") %>% 
            mutate(value = as.numeric(value)) %>% 
            group_by(year) %>% 
            e_charts(breakdown) %>% 
            e_bar(value) %>%
            e_tooltip(trigger = "axis") %>% 
            e_y_axis(name = "Percent", nameLocation = "middle", nameGap = 35, max = 1, min = 0) %>% 
            e_x_axis(axisLabel = list(interval = 0)) %>% 
            e_format_y_axis(suffix = "%", formatter = e_axis_formatter("percent")) %>% 
            e_legend(show = F) %>% 
            e_grid(bottom = 100) %>% 
            e_title("Self-harm",
                    "Proportion from each group that stated that they had self-harmed before.") %>% 
            e_theme_custom("phei.json") %>% 
            e_group("mh")
          
          })
        
      output$mh_services <- renderEcharts4r({
          
          stats_combined() %>% 
            filter(question == "mental_howaccess",
                   response == "Yes",
                   breakdown != "All Responses") %>% 
            mutate(value = as.numeric(value)) %>% 
            group_by(year) %>% 
            e_charts(breakdown) %>% 
            e_bar(value) %>%
            e_tooltip(trigger = "axis") %>% 
            e_y_axis(name = "Percent", nameLocation = "middle", nameGap = 35, max = 1, min = 0) %>% 
            e_x_axis(axisLabel = list(interval = 0)) %>% 
            e_legend(show = F) %>% 
            e_format_y_axis(suffix = "%", formatter = e_axis_formatter("percent")) %>% 
            e_grid(bottom = 100) %>% 
            e_title("Accessing mental health services",
                    "Proportion from each group stating that they knew how to access mental health services.") %>% 
            e_theme_custom("phei.json") %>% 
            e_group("mh") %>% 
            e_connect_group("mh")
          
          })

      output$mh_breakdown <- renderUI({
          
          shinyWidgets::prettyRadioButtons(
            inputId = ns("mh_breakdown"),
            label = "", 
            choices = unique(stats()$breakdown),
            inline = TRUE, 
            status = "info",
            fill = TRUE
          )
          
        })
      
      output$mh_year <- renderUI({
        
        shinyWidgets::prettyRadioButtons(
          inputId = ns("mh_year"),
          label = "", 
          choices = unique(stats_combined()$year),
          inline = TRUE, 
          status = "info",
          fill = TRUE
        )
        
      })
        
      output$worries_graph <- renderEcharts4r({
        
        stats <- stats_combined()
        
        stats %>% 
          filter(grepl("worry_", question),
                 breakdown == input$mh_breakdown,
                 year == input$mh_year,
                 response == "Yes") %>% 
          group_by(breakdown) %>% 
          arrange(count) %>% 
          slice(tail(row_number(), 5)) %>% 
          e_charts(question_text) %>% 
          e_bar(count) %>% 
          e_legend(show = FALSE) %>% 
          e_flip_coords() %>% 
          e_tooltip("item") %>% 
          e_grid(left = "20%") %>% 
          e_title("Top 5 worries",
                  paste("For", input$mh_breakdown, "in", input$mh_year)) %>% 
          e_theme("blue")
        
      })
      
      output$coping_graph <- renderEcharts4r({
        
        stats <- stats_combined()
        
        stats %>% 
          filter(grepl("cope_", question),
                 breakdown == input$mh_breakdown,
                 year == input$mh_year,
                 response == "Yes") %>% 
          group_by(breakdown) %>% 
          arrange(count) %>% 
          slice(tail(row_number(), 5)) %>% 
          e_charts(question_text) %>% 
          e_bar(count) %>% 
          e_legend(show = FALSE) %>% 
          e_flip_coords() %>% 
          e_tooltip("item") %>% 
          e_grid(left = "20%") %>% 
          e_title("Top 5 ways to cope",
                  paste("For", input$mh_breakdown, "in", input$mh_year)) %>% 
          e_theme("blue")
        
      })
      
      

    }
  )
}



