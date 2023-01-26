# UI ----------------------------------------------------------------------

key_mod <- function(id, 
                    name = "KeyPoints") {
  ns <- shiny::NS(id)
  
  tablerDash::tablerTabItem(
    tabName = name,
    shiny::fluidRow(
      infobox_mod(ns("infobox1")),
      infobox_mod(ns("infobox2")),
      infobox_mod(ns("infobox3")),
      infobox_mod(ns("infobox4")),
      infobox_mod(ns("infobox5")),
      infobox_mod(ns("infobox6"))
    ),
    shiny::tagList(
      shiny::fluidRow(
        tablerDash::tablerCard(width = 4, 
                               shiny::htmlOutput(ns("key_themes"))),
        tablerDash::tablerCard(width = 4, 
                               echarts4r::echarts4rOutput(ns("ethn_donut"))),
        tablerDash::tablerCard(width = 4, 
                               echarts4r::echarts4rOutput(ns("imd_donut"))), 
        
      ),
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
      # ),
      shiny::fluidRow(
        tablerDash::tablerCard(width = 5, 
                               shiny::uiOutput(ns("mh_year")),
                               shiny::uiOutput(ns("mh_breakdown")),
                               shiny::br(),
                               echarts4r::echarts4rOutput(ns("worries_graph")),
                               echarts4r::echarts4rOutput(ns("coping_graph"))
        ),
        tablerDash::tablerCard(width = 7,
                               shiny::htmlOutput(ns("key_themes_text"))
        )
      )
    )
    
  )
  
}

# Server ------------------------------------------------------------------

key_mod_server <- function(id,
                           params,
                           year, 
                           stats, 
                           stats_old,
                           stats_combined,
                           q_coded,
                           grp_lookup,
                           comp) {
  shiny::moduleServer(
    id, 
    function(input, output, session) {
      
      ns <- shiny::NS(id)
      
      # observe({
      #   if ("2020" %in% input$mh_year ) {browser()}
      # })
      
      # Info boxes --------------------------------------------------------------
      
      # Total responses
      infobox_mod_server("infobox1",
                         value = reactive(max(stats()$denominator, na.rm = TRUE)),
                         title = "Responses this year"
      )
      
      # Total schools participated
      infobox_mod_server("infobox2",
                         value = reactive(params()$meta$unique_schools[params()$meta$year == stats()$year[1]]),
                         title = "Schools participated"
      )
      
      # Female 
      infobox_mod_server("infobox3",
                         value = reactive(stats()$value[stats()$question == "sex" & stats()$response == "Female"][1]),
                         title = "Female respondents"
      )
      
      # Non-white
      infobox_mod_server("infobox4",
                         value = reactive({stats() %>% 
                             dplyr::filter(breakdown == "All Responses",
                                           question == "ethnicity",
                                           !response %in% c("White", "Prefer not to say")) %>%
                             dplyr::summarise(value = sum(value))  %>%
                             dplyr::pull(value)}),
                         title = "Non-white respondents"
      )
      
      # LGBTQ+
      infobox_mod_server("infobox5",
                         value = reactive({stats() %>% 
                             dplyr::filter(breakdown == "All Responses",
                                           question == "sexuality",
                                           !response %in% c("Heterosexual/Straight",
                                                            "Prefer not to say",
                                                            "Undecided/Questioning")) %>%
                             dplyr::summarise(value = sum(value)) %>%
                             dplyr::pull(value)}),
                         title = "LGBTQ+ respondents"
      )
      
      # Lowest IMD Quintile 
      infobox_mod_server("infobox6",
                         value = reactive({stats() %>% 
                             dplyr::filter(breakdown == "All Responses",
                                           question == "imd_quintile",
                                           response == "Quintile 1 - Most Deprived") %>%
                             dplyr::summarise(value = sum(value)) %>%
                             dplyr::pull(value)}),
                         title = "IMD 1- Most Deprived"
      )
      
      # Text --------------------------------------------------------------------
      
      output$key_themes <- shiny::renderText({
        
        paste("This dashboard shows the results of", max(stats()$denominator, na.rm = TRUE),
              "pupils from schools in Hertfordshire who responded to the Young People’s Health & Wellbeing Survey (YPHWS).",
              "This is second year of running this survey. You can change the data by year of survey and breakdown of interest using the", 
              " dropdowns in the navigation bar. To see more of the data, check the Explore Data and Inequalities tabs.<br><br>",
              "The Young People’s Health & Wellbeing Survey (YPHWS) is a youth health and wellbeing survey which gathers self-reported",
              "information annually from those aged 11-19 in Hertfordshire. The survey includes questions about home life, wellbeing, diet, physical",
              "activity, smoking, alcohol use, drug use, sexual health, mental health, bullying, and safety. <br><br>")
        
      })
      
      
      # Group summary -----------------------------------------------------------
      
      output$ethn_donut <- echarts4r::renderEcharts4r({
        
        stats <- stats()
        grp <- q_coded()$heading[q_coded()$question_coded == comp()][1]
        
        #TODO
        if(grp == "Sex" & !year() %in% c("2020", "2021")) { grp <- "Gender" }
        
        if(comp() == "District") { grp <- "District" }
        
        # since schyear question isn't present, visualise age instead.
        if(comp() == "schyear") {
          
          stats %>% 
            dplyr::filter(breakdown == "All Responses",
                          question == "age") %>% 
            dplyr::mutate(value = round(as.numeric(value), 2) * 100) %>% 
            echarts4r::e_charts(response) %>% 
            echarts4r::e_pie(value, radius = c("50%", "70%"), label = list(position = "inside", 
                                                                           formatter = htmlwidgets::JS("function(params){
           return(`${params.value}`+'%');}"))) %>% 
            echarts4r::e_tooltip("item") %>% 
            echarts4r::e_grid(left = "10%", right = "10%") %>%
            echarts4r::e_legend(bottom = 0) %>% 
            echarts4r::e_title("Age breakdown in %") %>% 
            echarts4r::e_theme_custom("phei.json")
          
        } else {
          
          stats %>% 
            dplyr::filter(breakdown == "All Responses",
                          question == comp()) %>% 
            dplyr::mutate(value = round(as.numeric(value), 2) * 100) %>% 
            echarts4r::e_charts(response) %>% 
            echarts4r::e_pie(value, radius = c("50%", "70%"), label = list(position = "inside", 
                                                                           formatter = htmlwidgets::JS("function(params){
           return(`${params.value}`+'%');}"))) %>% 
            echarts4r::e_tooltip("item") %>% 
            echarts4r::e_grid(left = "10%", right = "10%") %>%
            echarts4r::e_legend(bottom = 0) %>% 
            echarts4r::e_title(paste(grp, "breakdown in %")) %>% 
            echarts4r::e_theme_custom("phei.json")
          
        }
        
        
      })
      
      output$imd_donut <- renderEcharts4r({
        
        stats <- stats()
        
        if (comp() == "imd_quintile") { # if breakdown is by IMD, turn second plot into ethnicity plot.
          
          stats %>% 
            dplyr::filter(breakdown == "All Responses",
                          question == "ethnicity") %>% 
            dplyr::mutate(value = round(as.numeric(value), 2) * 100) %>% 
            echarts4r::e_charts(response) %>% 
            echarts4r::e_pie(value, radius = c("50%", "70%"), label = list(position = "inside", 
                                                                           formatter = htmlwidgets::JS("function(params){
           return(`${params.value}`+'%');}"))) %>% 
            echarts4r::e_tooltip("item") %>% 
            echarts4r::e_grid(left = "10%", right = "10%") %>%
            echarts4r::e_legend(bottom = 0) %>% 
            echarts4r::e_title("Ethnicity breakdown in %") %>% 
            echarts4r::e_theme_custom("phei.json")
          
        } else {
          
          stats %>% 
            dplyr::filter(breakdown == "All Responses",
                          question == "imd_quintile") %>% 
            dplyr::mutate(value = round(as.numeric(value), 2) * 100) %>% 
            echarts4r::e_charts(response) %>% 
            echarts4r::e_pie(value, radius = c("50%", "70%"), label = list(position = "inside", 
                                                                           formatter = htmlwidgets::JS("function(params){
           return(`${params.value}`+'%');}"))) %>% 
            echarts4r::e_tooltip("item") %>% 
            echarts4r::e_legend(bottom = 0) %>% 
            echarts4r::e_title("IMD breakdown in %") %>% 
            echarts4r::e_theme_custom("phei.json")
          
        }
        
        
      })
      
      # Text summary ------------------------------------------------------------
      
      output$key_themes_text <- shiny::renderText({
        
        comp <- comp()
        stats <- stats()
        grp_lookup <- grp_lookup()
        
        #TODO clean this.
        group_name <- unique(stats$breakdown)[grepl(paste0(unique(c(grp_lookup$value_reworded, grp_lookup$value_reworded2)), collapse = "|"), 
                                                    unique(stats$breakdown))]
        
        group_name <- ifelse(length(group_name) == 0, NA, group_name)
        group_breakdown <- ifelse(is.na(group_name), "All Responses", group_name)
        key_data <- stats %>%
          dplyr::filter(breakdown %in% group_breakdown)
        
        # --Stats for all respondents ----
        all_data <- stats %>%
          dplyr::filter(breakdown == "All Responses" & !is.na(question_text))
        
        # --Stats for key respondents ----
        mh1 <- ifelse(!is.na(group_name), paste0("This statistic was <b>",
                                                 dplyr::filter(key_data, question == 'life_satisfied' & response == "low") %>%
                                                   .$value, "</b> for ", group_name,  " respondents."), "")
        
        # mh2 <- ifelse(!is.na(group_name), paste0(" This statistic was <b>",
        #                                          dplyr::filter(key_data, question == 'hopeful_future' & response == "Never") %>% .$value,
        #                                          "</b> for ", group_name, " respondents."), "")
        
        mh3 <- ifelse(!is.na(group_name), paste0("From ", group_name, " respondents, <b>",
                                                 dplyr::filter(key_data, question =='weight' & response=='Overweight') %>%
                                                   .$value, "</b> felt overweight, and <b>",
                                                 dplyr::filter(key_data, question =='weight' & response=='Underweight') %>%
                                                   .$value, "</b> felt underweight."), "")
        
        mh4 <- ifelse(!is.na(group_name), paste0(" From ", group_name, " respondents, <b>",
                                                 dplyr::filter(key_data, question == 'mental_howaccess' & response == 'Yes') %>%
                                                   .$value, "</b> stated 'Yes'."), "")
        
        mh5 <- ifelse(!is.na(group_name), paste0(" From ", group_name, " respondents, <b>",
                                                 dplyr::filter(key_data, question == 'selfharm_ever' & response == 'Yes') %>%
                                                   .$value, "</b> stated 'Yes'."), "")
        
        mh6 <- ifelse(!is.na(group_name), paste0(" From ", group_name, " respondents, <b>",
                                                 dplyr::filter(key_data, question == 'bullied' & response == 'Yes') %>%
                                                   .$value, "</b> stated 'Yes'."), "")
        
        mh7 <- ifelse(!is.na(group_name), paste0(" From ", group_name, " respondents, <b>",
                                                 dplyr::filter(key_data, question == 'bullied_currently' & response == 'Yes') %>%
                                                   .$value, "</b> stated 'Yes'."), "")
        
        ls1 <- ifelse(!is.na(group_name), paste0("This statistic was <b>",
                                                 dplyr::filter(key_data, question == 'pa_60' & response == "6 to 7") %>%
                                                   .$value, "</b> for ", group_name, " respondents."), "")
        
        ls2 <- ifelse(!is.na(group_name), paste0("For ", group_name, " respondents this was <b>",
                                                 sum(dplyr::filter(key_data, question == 'smoke_ever' &
                                                                     response != 'I have never smoked') %>% .$value), "</b> and <b>",
                                                 sum(dplyr::filter(key_data, question == 'smoke_ever' &
                                                                     response == 'I smoke regularly (once a week or more)') %>% .$value),
                                                 "</b> respectively."), "")
        
        ls3 <- ifelse(!is.na(group_name), paste0("For ", group_name, " respondents this was <b>",
                                                 sum(filter(key_data, question == 'vaping' &
                                                              response != 'I have never vaped') %>% .$value), "</b> and <b>",
                                                 sum(filter(key_data, question == 'vaping' &
                                                              response == 'I vape regularly (once a week or more)') %>% .$value),
                                                 "</b> respectively."), "")
        
        ls4 <- ifelse(!is.na(group_name), paste0("For ", group_name, " respondents this was <b>",
                                                 sum(dplyr::filter(key_data, question == 'alcohol_ever' &
                                                                     response != 'Never') %>% .$value), "</b> and <b>",
                                                 sum(dplyr::filter(key_data, question == 'alcohol_ever' &
                                                                     response == '4 or more times a week') %>% .$value),
                                                 "</b> respectively."), "")
        
        ls5 <- ifelse(!is.na(group_name), paste0("For ", group_name, " respondents this was <b>",
                                                 sum(dplyr::filter(key_data, question == 'drug_ever' &
                                                                     response != 'I have never taken drugs') %>% .$value), "</b> and <b>",
                                                 sum(dplyr::filter(key_data, question == 'drug_ever' &
                                                                     response == 'I take drugs regularly (once a week or more)') %>% .$value),
                                                 "</b> respectively."), "")
        
        safety <- ifelse(!is.na(group_name), paste0("For ", group_name, " respondents this was <b>",
                                                    dplyr::filter(key_data, question == 'safety_day' &
                                                                    response == 'Unsafe') %>% .$value, ", ",
                                                    dplyr::filter(key_data, question == 'safety_dark' &
                                                                    response == 'Unsafe') %>% .$value, ", ",
                                                    dplyr::filter(key_data, question == 'safety_school' &
                                                                    response == 'Unsafe') %>% .$value, ", and ",
                                                    dplyr::filter(key_data, question == 'safety_journey' &
                                                                    response == 'Unsafe') %>% .$value,
                                                    "</b> respectively."), "")
        
        bull1 <- ifelse(!is.na(group_name), paste0("This statistic was <b>",
                                                 dplyr::filter(key_data, question == 'bullied' & response == "Yes") %>%
                                                   .$value, "</b> for ", group_name, " respondents."), "")
        
        bull2 <- ifelse(!is.na(group_name), paste0("This statistic was <b>",
                                                   dplyr::filter(key_data, question == 'bullied_currently' & response == "Yes") %>%
                                                     .$value, "</b> for ", group_name, " respondents."), "")
        
        sch1 <- ifelse(!is.na(group_name), paste0("This statistic was <b>",
                                                  dplyr::filter(key_data, question == 'schoolsupp_academic' & response == "Yes") %>%
                                                    .$value, "%</b> for ", group_name, " respondents."), "")
        
        sch2 <- ifelse(!is.na(group_name), paste0("This statistic was <b>",
                                                  dplyr::filter(key_data, question == 'schoolsupp_wellbeing' & response == "Yes") %>%
                                                    .$value, "%</b> for ", group_name, " respondents."), "")
        
        # cov1 <- ifelse(!is.na(group_name), paste0("This statistic was <b>",
        #                                           dplyr::filter(key_data, question == 'worry_covid19' & response == 'Yes') %>%
        #                                             .$value, "</b> for ", group_name, " respondents."), "")
        # 
        # cov2 <- ifelse(!is.na(group_name), paste0("This statistic was <b>",
        #                                           dplyr::filter(key_data, question == 'any_vacc_taken' & response == 'Yes') %>%
        #                                             .$value, "</b> for ", group_name, " respondents."), "")
        
        
        # --Text output ----
        output <- shiny::HTML(
          paste0(
            "This box summarises the results of <b>", max(stats$denominator) ,
            "</b> pupils from schools in Hertfordshire who responded to the ", stats$year[1], " Young People’s Health & Wellbeing Survey (YPHWS).<br><br>",
            
            "<h1>Mental health and wellbeing</h1>",
            "<b>", dplyr::filter(all_data, question == 'life_satisfied' & response == "low" & !is.na(question_text)) %>% .$value,
            "</b> of all respondents rated their life satisfaction as low. ", mh1, "<br><br>",
            
            # "<b>", dplyr::filter(all_data, question == 'hopeful_future' & response == "Never" & !is.na(question_text)) %>% .$value,
            # "</b>", " of all respondents stated that they never feel hopeful about their future.", mh2, "<br><br>",
            
            "<b>", dplyr::filter(all_data,  question =='weight' & response =='Overweight' & !is.na(question_text)) %>% .$value,
            "</b> felt they were overweight while <b>",
            dplyr::filter(all_data,  question == 'weight' & response == 'Underweight' & !is.na(question_text)) %>% .$value,
            "</b> felt they were underweight. ", mh3, "<br><br>",
            
            "<b>", dplyr::filter(all_data, question == 'selfharm_ever' & response == "Yes" & !is.na(question_text)) %>% .$value,
            "</b>", " of all respondents stated that they have self-harmed before.", mh5, "<br><br>",
          
            "<b>", sum(dplyr::filter(all_data,  question == 'mental_howaccess' & response != 'Yes') %>% .$value),
            "</b> of respondents answered 'Not sure' or 'No' when asked if they knew how to access support and services for mental health. <b>",
            sum(dplyr::filter(all_data,  question == 'mental_howaccess' & response == 'Yes') %>% .$value),
            "</b> answered 'Yes'. ", mh4, "<br><br>",
          
            "<h1>Lifestyle</h1>",
            
            "Out of all responses <b>", dplyr::filter(all_data,  question == 'pa_60' & response == "6 to 7") %>% .$value,
            "</b> had done a total of 60 minutes or more of physical activity 6 to 7 days of the week (in line with recommended daily physical activity guidance). ", ls1,
            " The most common response for this question was <b>",
            dplyr::filter(all_data,  question =='pa_60') %>% dplyr::filter(count == max(count)) %>% .$response,
            " days</b>. <br><br>",
            
            "<b>", sum(dplyr::filter(all_data,  question == 'smoke_ever' & response != 'I have never smoked') %>% .$value),
            "</b> of respondents reported having ever smoked and <b>",
            sum(dplyr::filter(all_data,  question == 'smoke_ever' & response == 'I smoke regularly (once a week or more)') %>% .$value),
            "</b> reported smoking regularly (once a week or more). ", ls2, "<br><br>",
            
            "<b>", sum(dplyr::filter(all_data,  question == 'vaping' & response != 'I have never vaped') %>% .$value),
            "</b> of respondents reported having ever vaped and <b>",
            sum(dplyr::filter(all_data,  question == 'vaping' & response == 'I vape regularly (once a week or more)') %>% .$value),
            "</b> reported vaping regularly (once a week or more). ", ls3, "<br><br>",
            
            "<b>", sum(dplyr::filter(all_data,  question == 'alcohol_ever' & response != 'Never') %>% .$value),
            "</b> of respondents reported having had an alcoholic drink in the past 3 months and <b>",
            sum(dplyr::filter(all_data,  question == 'alcohol_ever' & response == '4 or more times a week') %>% .$value),
            "</b> reported drinking 4 or more times a week. ", ls4, "<br><br>",
            
            "<b>", sum(dplyr::filter(all_data,  question == 'drug_ever' & response != 'I have never taken drugs') %>% .$value),
            "</b> of respondents reported having ever taken drugs and <b>",
            sum(dplyr::filter(all_data,  question == 'drug_ever' & response == 'I take drugs regularly (once a week or more)') %>% .$value),
            "</b> reported taking drugs regularly (once a week or more).", ls5, "<br><br>",
            
            "<h1>Bullying</h1>",
            
            "<b>", dplyr::filter(all_data, question == 'bullied' & response == "Yes" & !is.na(question_text)) %>% .$value,
            "</b>", " of all respondents stated that they have been bullied before.", mh6, "<br><br>",
            
            "<b>", dplyr::filter(all_data, question == 'bullied_currently' & response == "Yes" & !is.na(question_text)) %>% .$value,
            "</b>", " of all respondents stated that they have self-harmed before.", mh7, "<br><br>",
            
            "<h1>Safety</h1>",
            
            "Regarding safety, <b>",
            sum(dplyr::filter(all_data,  question == 'safety_day' & response == 'Unsafe') %>% .$value),
            "</b> of respondents felt unsafe going out during the day, <b>",
            sum(dplyr::filter(all_data,  question == 'safety_dark' & response == 'Unsafe') %>% .$value),
            "</b> felt unsafe going out after dark, <b>",
            dplyr::filter(all_data,  question == 'safety_school' & response == 'Unsafe' & !is.na(question_text)) %>% .$value,
            "</b> felt unsafe at school, and <b>",
            dplyr::filter(all_data,  question == 'safety_journey' & response == 'Unsafe' & !is.na(question_text)) %>% .$value,
            "</b> felt unsafe on their journey to school. ", safety, "<br><br>"
            
          )
        )
        
        
      })
      
      
      # MH & Wellbeing Graphs -------------------------------------------------------------------
      
      output$mh_breakdown <- shiny::renderUI({
        
        if(comp() == "schyear"){
          
          shinyWidgets::prettyRadioButtons(
            inputId = ns("mh_breakdown"),
            label = "",
            choices = unique(c("Year 7", "Year 8", "Year 9", "Year 10", "Year 11", "Year 12", "Year 13", "All Responses", "Not at school/other")),
            inline = TRUE,
            status = "info",
            fill = TRUE
          )
          
        }else{
          
          shinyWidgets::prettyRadioButtons(
            inputId = ns("mh_breakdown"),
            label = "",
            choices = unique(stats()$breakdown),
            inline = TRUE,
            status = "info",
            fill = TRUE
          )}
        
        
      })
      
      output$mh_year <- shiny::renderUI({
        stats <- stats_combined() %>%
          dplyr::arrange(desc(year))
        
        shinyWidgets::prettyRadioButtons(
          inputId = ns("mh_year"),
          label = "",
          choices = unique(stats$year),
          inline = TRUE,
          status = "info",
          fill = TRUE
        )
        
      })
      
      output$worries_graph <- echarts4r::renderEcharts4r({
        
        stats <- stats_combined()
        
        stats %>%
          dplyr::filter(grepl("worry_", question),
                        breakdown == input$mh_breakdown,
                        year == input$mh_year,
                        response == "Yes") %>%
          dplyr::group_by(breakdown) %>%
          dplyr::arrange(count) %>%
          dplyr::slice(tail(dplyr::row_number(), 5)) %>%
          echarts4r::e_charts(question_text) %>%
          echarts4r::e_bar(count) %>%
          echarts4r::e_legend(show = FALSE) %>%
          echarts4r::e_flip_coords() %>%
          echarts4r::e_x_axis(splitNumber = 2) %>%
          echarts4r::e_tooltip("item") %>%
          echarts4r::e_grid(left = "30%") %>%
          echarts4r::e_title("Top 5 worries",
                             paste("For", input$mh_breakdown, "in", input$mh_year)) %>%
          echarts4r::e_theme("walden")
        
      })
      
      output$coping_graph <- echarts4r::renderEcharts4r({
        
        stats <- stats_combined()
        
        stats %>%
          dplyr::filter(grepl("cope_", question),
                        breakdown == input$mh_breakdown,
                        year == input$mh_year,
                        response == "Yes") %>%
          dplyr::group_by(breakdown) %>%
          dplyr::arrange(count) %>%
          slice(tail(dplyr::row_number(), 5)) %>%
          echarts4r::e_charts(question_text) %>%
          echarts4r::e_bar(count) %>%
          echarts4r::e_legend(show = FALSE) %>%
          echarts4r::e_flip_coords() %>%
          echarts4r::e_x_axis(splitNumber = 2) %>%
          echarts4r::e_tooltip("item") %>%
          echarts4r::e_grid(left = "44%") %>%
          echarts4r::e_title("Top 5 ways to cope",
                             paste("For", input$mh_breakdown, "in", input$mh_year)) %>%
          echarts4r::e_theme("walden")
        
      })
      
      
      # Extras ------------------------------------------------------------------
      
      
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
      
    })
  
}
