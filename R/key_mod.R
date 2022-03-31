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
        tablerCard(width = 5, 
                   uiOutput(ns("mh_year")),
                   uiOutput(ns("mh_breakdown")),
                   br(),
                   echarts4rOutput(ns("worries_graph")),
                   echarts4rOutput(ns("coping_graph"))
                   ),
        tablerCard(width = 7,
                   htmlOutput(ns("key_themes_text"))
                   )
      )
      # fluidRow(
      #   tabItem("name", 
      #           bs4TabCard(width = 12, side = "right", status = "success",
      #                      collapsible = FALSE, 
      #                      title = "",
      #                      tabPanel("Mental Health", 
      #                               fluidRow(
      #                                 column(6, echarts4rOutput(ns("life_sat")),
      #                                        echarts4rOutput(ns("life_worth"))),
      #                                 column(6, echarts4rOutput(ns("self_harm")),
      #                                        echarts4rOutput(ns("mh_services")))
      #                               )    
      #                      ),
      #                      tabPanel(
      #                        "Lifestyle"
      #                      ),
      #                      tabPanel(
      #                        "Safety"
      #                      ),
      #                      tabPanel(
      #                        "Sexual Health"
      #                      ),
      #                      tabPanel(
      #                        "Other"
      #                      )) )
      # )
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
                           stats_combined,
                           q_coded,
                           grp_lookup,
                           comp) {
  moduleServer(
    id, 
    function(input, output, session) {
      
      ns <- NS(id)
      
      # observe({
      #   if ("2020" %in% input$mh_year ) {browser()}
      # })
      
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
        
        value <- params()$unique_schools
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
          summarise(value = sum(value))  %>% 
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
                 question == comp()) %>% 
          mutate(value = round(as.numeric(value), 2) * 100) %>% 
          e_charts(response) %>% 
          e_pie(value, radius = c("50%", "70%"), label = list(position = "inside", 
                                                              formatter = htmlwidgets::JS("function(params){
           return(`${params.value}`+'%');}"))) %>% 
          e_tooltip("item") %>% 
          e_grid(left = "10%", right = "10%") %>%
          e_legend(bottom = 0) %>% 
          e_title("Group breakdown in %") %>% 
          e_theme_custom("phei.json")
        
      })
      
      output$imd_donut <- renderEcharts4r({
        
        stats <- stats()
        
        if (comp() == "imd_quintile") { # if breakdown is by IMD, turn second plot into ethnicity plot.
          
          stats %>% 
            filter(breakdown == "All Responses",
                   question == "ethnicity") %>% 
            mutate(value = round(as.numeric(value), 2) * 100) %>% 
            e_charts(response) %>% 
            e_pie(value, radius = c("50%", "70%"), label = list(position = "inside", 
                                                                formatter = htmlwidgets::JS("function(params){
           return(`${params.value}`+'%');}"))) %>% 
            e_tooltip("item") %>% 
            e_grid(left = "10%", right = "10%") %>%
            e_legend(bottom = 0) %>% 
            e_title("Ethnicity breakdown in %") %>% 
            e_theme_custom("phei.json")
          
        } else {
          
          stats %>% 
            filter(breakdown == "All Responses",
                   question == "imd_quintile") %>% 
            mutate(value = round(as.numeric(value), 2) * 100) %>% 
            e_charts(response) %>% 
            e_pie(value, radius = c("50%", "70%"), label = list(position = "inside", 
                                                                formatter = htmlwidgets::JS("function(params){
           return(`${params.value}`+'%');}"))) %>% 
            e_tooltip("item") %>% 
            e_legend(bottom = 0) %>% 
            e_title("IMD breakdown in %") %>% 
            e_theme_custom("phei.json")
          
        }

        
      })


      # Text summary ------------------------------------------------------------

      output$key_themes_text <- renderText({

        comp <- comp()
        stats <- stats()
        grp_lookup <- grp_lookup()

        group_name <- grp_lookup$group_value[grp_lookup$group == comp]
        group_breakdown <- ifelse(is.na(group_name), "All Responses", group_name)
        key_data <- stats %>%
          filter(breakdown %in% group_breakdown)

        # --Stats for all respondents ----
        all_data <- stats %>%
          filter(breakdown == "All Responses" & !is.na(question_text))

        # --Stats for key respondents ----
        mh1 <- ifelse(!is.na(group_name), paste0("This statistic was <b>",
                                                 filter(key_data, question == 'life_satisfied' & response == "low") %>%
                                                   .$value, "</b> for ", group_name,  " respondents."), "")

        mh2 <- ifelse(!is.na(group_name), paste0("This statistic was <b>",
                                                 filter(key_data, question == 'life_satisfied_before_covid' & response == "low") %>% .$value,
                                                 "</b> for ", group_name, " respondents."), "")

        mh3 <- ifelse(!is.na(group_name), paste0("From ", group_name, " respondents, <b>",
                                                 filter(key_data, question =='weight' & response=='Overweight') %>%
                                                   .$value, "</b> felt overweight, and <b>",
                                                 filter(key_data, question =='weight' & response=='Underweight') %>%
                                                   .$value, "</b> felt underweight."), "")

        mh4 <- ifelse(!is.na(group_name), paste0(" From ", group_name, " respondents, <b>",
                                                 filter(key_data, question == 'mental_howaccess' & response == 'Yes') %>%
                                                   .$value, "</b> stated 'Yes'."), "")


        ls1 <- ifelse(!is.na(group_name), paste0("This statistic was <b>",
                                                 filter(key_data, question == 'pa_60' & response == "6-7") %>%
                                                   .$value, "</b> for ", group_name, " respondents."), "")

        ls2 <- ifelse(!is.na(group_name), paste0("For ", group_name, " respondents this was <b>",
                                                 sum(filter(key_data, question == 'smoke_ever' &
                                                              response != 'I have never smoked') %>% .$value), "</b> and <b>",
                                                 sum(filter(key_data, question == 'smoke_ever' &
                                                              response == 'I smoke regularly (once a week or more)') %>% .$value),
                                                 "</b> respectively."), "")

        ls3 <- ifelse(!is.na(group_name), paste0("For ", group_name, " respondents this was <b>",
                                                 sum(filter(key_data, question == 'vaping' &
                                                              response != 'I have never vaped') %>% .$value), "</b> and <b>",
                                                 sum(filter(key_data, question == 'vaping' &
                                                              response == 'I vape regularly (once a week or more)') %>% .$value),
                                                 "</b> respectively."), "")

        ls4 <- ifelse(!is.na(group_name), paste0("For ", group_name, " respondents this was <b>",
                                                 sum(filter(key_data, question == 'alcohol_ever' &
                                                              response != 'Never') %>% .$value), "</b> and <b>",
                                                 sum(filter(key_data, question == 'alcohol_ever' &
                                                              response == '4 or more times a week') %>% .$value),
                                                 "</b> respectively."), "")

        ls5 <- ifelse(!is.na(group_name), paste0("For ", group_name, " respondents this was <b>",
                                                 sum(filter(key_data, question == 'drug_ever' &
                                                              response != 'I have never taken drugs') %>% .$value), "</b> and <b>",
                                                 sum(filter(key_data, question == 'drug_ever' &
                                                              response == 'I take drugs regularly (once a week or more)') %>% .$value),
                                                 "</b> respectively."), "")

        safety <- ifelse(!is.na(group_name), paste0("For ", group_name, " respondents this was <b>",
                                                    filter(key_data, question == 'safety_day' &
                                                             response == 'Unsafe') %>% .$value, ", ",
                                                    filter(key_data, question == 'safety_dark' &
                                                             response == 'Unsafe') %>% .$value, ", ",
                                                    filter(key_data, question == 'safety_school' &
                                                             response == 'Unsafe') %>% .$value, ", and ",
                                                    filter(key_data, question == 'safety_journey' &
                                                             response == 'Unsafe') %>% .$value,
                                                    "</b> respectively."), "")

        sch1 <- ifelse(!is.na(group_name), paste0("This statistic was <b>",
                                                  filter(key_data, question == 'schoolsupp_academic' & response == "Yes") %>%
                                                    .$value, "%</b> for ", group_name, " respondents."), "")

        sch2 <- ifelse(!is.na(group_name), paste0("This statistic was <b>",
                                                  filter(key_data, question == 'schoolsupp_wellbeing' & response == "Yes") %>%
                                                    .$value, "%</b> for ", group_name, " respondents."), "")

        cov1 <- ifelse(!is.na(group_name), paste0("This statistic was <b>",
                                                  filter(key_data, question == 'worry_covid19' & response == 'Yes') %>%
                                                    .$value, "</b> for ", group_name, " respondents."), "")

        cov2 <- ifelse(!is.na(group_name), paste0("This statistic was <b>",
                                                  filter(key_data, question == 'any_vacc_taken' & response == 'Yes') %>%
                                                    .$value, "</b> for ", group_name, " respondents."), "")


        # --Text output ----
        output <- HTML(
          paste0(
            "This box summarises the results of <b>", max(stats$denominator) ,
            "</b> pupils from schools in Hertfordshire who responded to the 2021 Young People’s Health & Wellbeing Survey (YPHWS).<br><br>",

            "<h1>Mental health and wellbeing</h1>",
            "<b>", filter(all_data, question == 'life_satisfied' & response == "low" & !is.na(question_text)) %>% .$value,
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
            "</b> of respondents answered 'Not sure' or 'No' when asked if they knew how to access support and services for mental health. <b>",
            sum(filter(all_data,  question == 'mental_howaccess' & response == 'Yes') %>% .$value),
            "</b> answered 'Yes'. ", mh4, "<br><br>",

            "<h1>Lifestyle</h1>",

            "Out of all responses <b>", filter(all_data,  question == 'pa_60' & response == "6-7") %>% .$value,
            "</b> had done a total of 60 minutes or more of physical activity 6-7 days of the week (in line with recommended daily physical activity guidance). ", ls1,
            " The most common response for this question was <b>",
            filter(all_data,  question =='pa_60') %>% filter(count == max(count)) %>% .$response,
            " days</b>. <br><br>",

            "<b>", sum(filter(all_data,  question == 'smoke_ever' & response != 'I have never smoked') %>% .$value),
            "</b> of respondents reported having ever smoked and <b>",
            sum(filter(all_data,  question == 'smoke_ever' & response == 'I smoke regularly (once a week or more)') %>% .$value),
            "</b> reported smoking regularly (once a week or more). ", ls2, "<br><br>",

            "<b>", sum(filter(all_data,  question == 'vaping' & response != 'I have never vaped') %>% .$value),
            "</b> of respondents reported having ever vaped and <b>",
            sum(filter(all_data,  question == 'vaping' & response == 'I vape regularly (once a week or more)') %>% .$value),
            "</b> reported vaping regularly (once a week or more). ", ls3, "<br><br>",

            "<b>", sum(filter(all_data,  question == 'alcohol_ever' & response != 'Never') %>% .$value),
            "</b> of respondents reported having had an alcoholic drink in the past 3 months and <b>",
            sum(filter(all_data,  question == 'alcohol_ever' & response == '4 or more times a week') %>% .$value),
            "</b> reported drinking 4 or more times a week. ", ls4, "<br><br>",

            "<b>", sum(filter(all_data,  question == 'drug_ever' & response != 'I have never taken drugs') %>% .$value),
            "</b> of respondents reported having ever taken drugs and <b>",
            sum(filter(all_data,  question == 'drug_ever' & response == 'I take drugs regularly (once a week or more)') %>% .$value),
            "</b> reported taking drugs regularly (once a week or more).", ls5, "<br><br>",

            "<h1>Safety</h1>",

            "Regarding safety, <b>",
            sum(filter(all_data,  question == 'safety_day' & response == 'Unsafe') %>% .$value),
            "</b> of respondents felt unsafe going out during the day, <b>",
            sum(filter(all_data,  question == 'safety_dark' & response == 'Unsafe') %>% .$value),
            "</b> felt unsafe going out after dark, <b>",
            filter(all_data,  question == 'safety_school' & response == 'Unsafe' & !is.na(question_text)) %>% .$value,
            "</b> felt unsafe at school, and <b>",
            filter(all_data,  question == 'safety_journey' & response == 'Unsafe' & !is.na(question_text)) %>% .$value,
            "</b> felt unsafe on their journey to school. ", safety, "<br><br>",

            "<h1>COVID-19</h1>",

            "<b>", filter(all_data, question == 'worry_covid19' & response == 'Yes') %>% .$value,
            "</b> stated that COVID-19 was one of the issuers they worry about. ", cov1, "<br><br>",

            "<b>", filter(all_data,  question == 'any_vacc_taken' & response == 'Yes') %>% .$value,
            "</b> stated that they have taken any dose of the COVID-19 vaccine. ", cov2

          )
        )


      })
        
        
      # MH & Wellbeing Graphs -------------------------------------------------------------------
        
        output$mh_breakdown <- shiny::renderUI({

          shinyWidgets::prettyRadioButtons(
            inputId = ns("mh_breakdown"),
            label = "",
            choices = unique(stats()$breakdown),
            inline = TRUE,
            status = "info",
            fill = TRUE
          )
          

        })

        output$mh_year <- shiny::renderUI({

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
            e_x_axis(splitNumber = 2) %>% 
            e_tooltip("item") %>% 
            e_grid(left = "30%") %>%
            e_title("Top 5 worries",
                    paste("For", input$mh_breakdown, "in", input$mh_year)) %>%
            e_theme("walden")
          
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
            e_x_axis(splitNumber = 2) %>% 
            e_tooltip("item") %>%
            e_grid(left = "44%") %>%
            e_title("Top 5 ways to cope",
                    paste("For", input$mh_breakdown, "in", input$mh_year)) %>%
            e_theme("walden")

        })
        
        # output$life_sat <- renderEcharts4r({
        #   
        #   stats_combined() %>% 
        #     filter(question == "life_satisfied",
        #            response == "low",
        #            breakdown != "All Responses") %>% 
        #     mutate(value = as.numeric(value)) %>% 
        #     group_by(year) %>% 
        #     e_charts(breakdown) %>% 
        #     e_bar(value) %>%
        #     e_tooltip(trigger = "axis") %>% 
        #     e_y_axis(name = "Percent", nameLocation = "middle", nameGap = 35, max = 1, min = 0) %>% 
        #     e_x_axis(axisLabel = list(interval = 0)) %>% 
        #     e_format_y_axis(suffix = "%", formatter = e_axis_formatter("percent")) %>% 
        #     e_grid(bottom = 100) %>% 
        #     e_title("Low life satisfaction",
        #             "Proportion from each group that responded with a rating of 4 or less out of 10.") %>% 
        #     e_theme_custom("phei.json") %>% 
        #     e_group("mh")
        #   
        # })
        # 
        # output$life_worth <- renderEcharts4r({
        #   
        #   stats_combined() %>% 
        #     filter(question == "bullied",
        #            response == "Yes",
        #            breakdown != "All Responses") %>% 
        #     mutate(value = as.numeric(value)) %>% 
        #     group_by(year) %>% 
        #     e_charts(breakdown) %>% 
        #     e_bar(value) %>%
        #     e_tooltip(trigger = "axis") %>% 
        #     e_y_axis(name = "Percent", nameLocation = "middle", nameGap = 35, max = 1, min = 0) %>% 
        #     e_x_axis(axisLabel = list(interval = 0)) %>% 
        #     e_format_y_axis(suffix = "%", formatter = e_axis_formatter("percent")) %>% 
        #     e_grid(bottom = 100) %>% 
        #     e_title("Bullying",
        #             "Proportion from each group that stated that they have been bullied before.") %>% 
        #     e_theme_custom("phei.json") %>% 
        #     e_group("mh")
        #   
        # })
        # 
        # output$self_harm <- renderEcharts4r({
        #   
        #   stats_combined() %>% 
        #     filter(question == "selfharm_ever",
        #            response == "Yes",
        #            breakdown != "All Responses") %>% 
        #     mutate(value = as.numeric(value)) %>% 
        #     group_by(year) %>% 
        #     e_charts(breakdown) %>% 
        #     e_bar(value) %>%
        #     e_tooltip(trigger = "axis") %>% 
        #     e_y_axis(name = "Percent", nameLocation = "middle", nameGap = 35, max = 1, min = 0) %>% 
        #     e_x_axis(axisLabel = list(interval = 0)) %>% 
        #     e_format_y_axis(suffix = "%", formatter = e_axis_formatter("percent")) %>% 
        #     e_legend(show = F) %>% 
        #     e_grid(bottom = 100) %>% 
        #     e_title("Self-harm",
        #             "Proportion from each group that stated that they had self-harmed before.") %>% 
        #     e_theme_custom("phei.json") %>% 
        #     e_group("mh")
        #   
        # })
        # 
        # output$mh_services <- renderEcharts4r({
        #   
        #   stats_combined() %>% 
        #     filter(question == "mental_howaccess",
        #            response == "Yes",
        #            breakdown != "All Responses") %>% 
        #     mutate(value = as.numeric(value)) %>% 
        #     group_by(year) %>% 
        #     e_charts(breakdown) %>% 
        #     e_bar(value) %>%
        #     e_tooltip(trigger = "axis") %>% 
        #     e_y_axis(name = "Percent", nameLocation = "middle", nameGap = 35, max = 1, min = 0) %>% 
        #     e_x_axis(axisLabel = list(interval = 0)) %>% 
        #     e_legend(show = F) %>% 
        #     e_format_y_axis(suffix = "%", formatter = e_axis_formatter("percent")) %>% 
        #     e_grid(bottom = 100) %>% 
        #     e_title("Accessing mental health services",
        #             "Proportion from each group stating that they knew how to access mental health services.") %>% 
        #     e_theme_custom("phei.json") %>% 
        #     e_group("mh") %>% 
        #     e_connect_group("mh")
        #   
        # })
        
      })
      
    }




