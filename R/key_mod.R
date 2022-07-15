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
      fluidRow(
        bs4Dash::tabItem("name",
                         bs4TabCard(width = 12, side = "right", status = "success",
                                    collapsible = FALSE,
                                    title = "", 
                                    tabPanel("Mental Health",
                                             shiny::fluidRow(
                                               shiny::htmlOutput(ns("mh_summary"))
                                             ),
                                             br(),
                                             fluidRow(
                                               column(6, echarts4rOutput(ns("mh_life_sat")),
                                                      echarts4rOutput(ns("mh_bullied")),
                                                      echarts4r::echarts4rOutput(ns("mh_worries"))),
                                               column(6, echarts4rOutput(ns("mh_self_harm")),
                                                      echarts4rOutput(ns("mh_services")),
                                                      echarts4r::echarts4rOutput(ns("mh_coping")))
                                             ),
                                             fluidRow(
                                               column(
                                                 12,
                                                 shiny::br(),
                                                 shiny::uiOutput(ns("mh_ui_breakdown")),
                                                 shiny::uiOutput(ns("mh_ui_year"))
                                               )
                                             )
                                    ),
                                    tabPanel(
                                      "Lifestyle",
                                      shiny::fluidRow(
                                        shiny::htmlOutput(ns("ls_summary"))
                                      ),
                                      shiny::fluidRow(
                                        shiny::column(6, 
                                                      echarts4r::echarts4rOutput(ns("ls_pa")),
                                                      echarts4r::echarts4rOutput(ns("ls_alcohol"))
                                                      ),
                                        shiny::column(6, 
                                                      echarts4r::echarts4rOutput(ns("ls_smoking")),
                                                      echarts4r::echarts4rOutput(ns("ls_drugs")))
                                      )
                                    ),
                                    tabPanel(
                                      "Safety",
                                      shiny::fluidRow(
                                        shiny::htmlOutput(ns("s_summary"))
                                      ),
                                      shiny::fluidRow(
                                        shiny::column(6, 
                                                      echarts4r::echarts4rOutput(ns("s_safe_dark")),
                                                      echarts4r::echarts4rOutput(ns("s_incident"))
                                                      ),
                                        shiny::column(6, 
                                                      echarts4r::echarts4rOutput(ns("s_safe_day")),
                                                      echarts4r::echarts4rOutput(ns("s_domestic")))
                                      )
                                    ),
                                    tabPanel(
                                      "Sexual Health",
                                      shiny::fluidRow(
                                        shiny::htmlOutput(ns("sh_summary"))
                                      ),
                                      shiny::fluidRow(
                                        shiny::column(6, 
                                                      echarts4r::echarts4rOutput(ns("sh_services")),
                                                      echarts4r::echarts4rOutput(ns("sh_info"))
                                        ),
                                        shiny::column(6, 
                                                      echarts4r::echarts4rOutput(ns("sh_condoms")),
                                                      echarts4r::echarts4rOutput(ns("sh_pressure"))
                                        )
                                      ),
                                      fluidRow(
                                        column(
                                          12,
                                          shiny::br(),
                                          shiny::uiOutput(ns("sh_ui_breakdown")),
                                          shiny::uiOutput(ns("sh_ui_year"))
                                        )
                                      ),
                                    )) )
      )
    )
  )
}

# Server ------------------------------------------------------------------

key_mod_server <- function(id,
                           params,
                           year,
                           data,
                           data_old,
                           stats, 
                           stats_old,
                           stats_combined,
                           diffs,
                           q_coded,
                           grp_lookup,
                           comp) {
  shiny::moduleServer(
    id, 
    function(input, output, session) {
      
      ns <- shiny::NS(id)
      
      observe({
        if ("Broxbourne" %in% input$mh_breakdown ) {browser()}
      })
      
      stats_w_diffs <- reactive({ add_year_diff(diffs(), stats_combined()) })
      
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
                                           response %in% c("Gay, lesbian or bisexual",
                                                           "Other")) %>%
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
                         title = "IMD Quint. 1 (most deprived)"
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
      
      # MH & Wellbeing Graphs -------------------------------------------------------------------
      
      output$mh_summary <- shiny::renderText({
        
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
        
        mh2 <- ifelse(!is.na(group_name), paste0("This statistic was <b>",
                                                 dplyr::filter(key_data, question == 'selfharm_ever' & response == "Yes") %>% .$value,
                                                 "</b> for ", group_name, " respondents."), "")
        
        mh3 <- ifelse(!is.na(group_name), paste0("This statistic was <b>",
                                                 dplyr::filter(key_data, question == 'bullied' & response == "Yes") %>% .$value,
                                                 "</b> for ", group_name, " respondents."), "")
        
        mh4 <- ifelse(!is.na(group_name), paste0(" From ", group_name, " respondents, this statistic was <b>",
                                                 dplyr::filter(key_data, question == 'mental_howaccess' & response == 'Yes') %>%
                                                   .$value, "</b>."), "")
        
        # --Text output ----
        shiny::HTML(
          paste0(
            "<h2>Mental Health & Wellbeing</h2>",
            "In ", year(), " <b>", dplyr::filter(all_data, question == 'life_satisfied' & response == "low" & !is.na(question_text)) %>% .$value,
            "</b> of all respondents rated their life satisfaction as low. ", mh1, "<br>",
            
            "In ", year(), " <b>", dplyr::filter(all_data, question == 'selfharm_ever' & response == "Yes" & !is.na(question_text)) %>% .$value,
            "</b>", " of all respondents stated that they had self-harmed before. ", mh2, "<br>",
            
            "In ", year(), " <b>", dplyr::filter(all_data, question == 'bullied' & response == "Yes" & !is.na(question_text)) %>% .$value,
            "</b>", " of all respondents stated that they had been bullied before. ", mh3, "<br>",
            
            "In ", year(), " <b>", sum(dplyr::filter(all_data,  question == 'mental_howaccess' & response == 'Yes') %>% .$value),
            "</b> of respondents knew how to access support and services for mental health. ", mh4, "<br><br>",
            "* Green or red arrows in the plots indicate a <b>statistically significant difference</b> found between years in that particular group.<br><br>"
          )
        )
        
      })
      
      output$mh_life_sat <- renderEcharts4r({
        
        create_yearly_plot(stats = stats_w_diffs(),
                           q_coded = q_coded(),
                           question_p = "life_satisfied",
                           response_p = "low",
                           title = "Low life satisfaction", 
                           subtitle = "Proportion from each group that responded with a rating of 4 or less out of 10.",
                           group_id = "mh",
                           legend = T, 
                           connect = F)
        
      })
      
      output$mh_bullied <- renderEcharts4r({
        
        create_yearly_plot(stats = stats_w_diffs(),
                           q_coded = q_coded(), 
                           question_p = "bullied",
                           response_p = "Yes",
                           title = "Bullying", 
                           subtitle = "Proportion from each group stating that they have been bullied before",
                           group_id = "mh",
                           connect = F)
        
      })
      
      output$mh_self_harm <- renderEcharts4r({
        
        create_yearly_plot(stats = stats_w_diffs(),
                           q_coded = q_coded(),
                           question_p = "selfharm_ever",
                           response_p = "Yes",
                           title = "Self-harm", 
                           subtitle = "Proportion from each group that stated that they had self-harmed before.",
                           group_id = "mh",
                           connect = F)
        
      })
      
      output$mh_services <- renderEcharts4r({
        
        create_yearly_plot(stats = stats_w_diffs(),
                           q_coded = q_coded(),
                           question_p = "mental_howaccess",
                           response_p = "Yes",
                           title = "Accessing mental health services", 
                           subtitle = "Proportion from each group stating that they knew how to access mental health services.",
                           group_id = "mh",
                           connect = T)
        
      })
      
      output$mh_ui_breakdown <- shiny::renderUI({
        
        shinyWidgets::prettyRadioButtons(
          inputId = ns("mh_breakdown"),
          label = "",
          choices = unique(stats()$breakdown),
          status = "info",
          inline = TRUE
        )
        
      })
      
      output$mh_ui_year <- shiny::renderUI({
        
        shinyWidgets::prettyRadioButtons(
          inputId = ns("mh_year"),
          label = "",
          choices = unique(stats_combined()$year),
          status = "info",
          inline= TRUE
        )
        
      })
      
      output$mh_worries <- echarts4r::renderEcharts4r({
        
        stats <- stats_combined()
        
        stats %>%
          dplyr::filter(grepl("worry_", question),
                        breakdown == input$mh_breakdown,
                        year == input$mh_year,
                        response == "Yes")  %>%
          dplyr::group_by(breakdown) %>%
          dplyr::arrange(count) %>%
          dplyr::slice(tail(dplyr::row_number(), 5)) %>%
          echarts4r::e_charts(question_text) %>%
          echarts4r::e_bar(count, stack = "grp") %>%
          echarts4r::e_flip_coords() %>%
          echarts4r::e_x_axis(splitNumber = 2) %>%
          echarts4r::e_tooltip("item") %>%
          echarts4r::e_grid(left = "30%") %>%
          echarts4r::e_title("Top 5 worries",
                             paste("For", input$mh_breakdown, "in", input$mh_year)) %>%
          echarts4r::e_theme("walden")
        
      })
      
      output$mh_coping <- echarts4r::renderEcharts4r({
        
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
      
      
      # Lifestyle Graphs --------------------------------------------------------
      
      output$ls_summary <- shiny::renderText({
        
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
                                                 sum(dplyr::filter(key_data, question == 'alcohol_ever' &
                                                                     response != 'Never') %>% .$value), "</b> and <b>",
                                                 sum(dplyr::filter(key_data, question == 'alcohol_ever' &
                                                                     response == '4 or more times a week') %>% .$value),
                                                 "</b> respectively."), "")
        
        ls4 <- ifelse(!is.na(group_name), paste0("For ", group_name, " respondents this was <b>",
                                                 sum(dplyr::filter(key_data, question == 'drug_ever' &
                                                                     response != 'I have never taken drugs') %>% .$value), "</b> and <b>",
                                                 sum(dplyr::filter(key_data, question == 'drug_ever' &
                                                                     response == 'I take drugs regularly (once a week or more)') %>% .$value),
                                                 "</b> respectively."), "")
        
        # --Text output ----
        shiny::HTML(
          paste0(
            "<h2>Lifestyle</h2>",
            "Out of all responses <b>", dplyr::filter(all_data,  question == 'pa_60' & response == "6 to 7") %>% .$value,
            "</b> had done a total of 60 minutes or more of physical activity 6 to 7 days of the week (in line with recommended daily physical activity guidance) in ", year(), 
            " ", ls1, "<br>",
            
            "In ", year(), " <b>", sum(dplyr::filter(all_data,  question == 'smoke_ever' & response != 'I have never smoked') %>% .$value),
            "</b> of respondents reported having ever smoked and <b>",
            sum(dplyr::filter(all_data,  question == 'smoke_ever' & response == 'I smoke regularly (once a week or more)') %>% .$value),
            "</b> reported smoking regularly (once a week or more). ", ls2, "<br>",
            
            "In ", year(), " <b>", sum(dplyr::filter(all_data,  question == 'alcohol_ever' & response != 'Never') %>% .$value),
            "</b> of respondents reported having had an alcoholic drink in the past 3 months and <b>",
            sum(dplyr::filter(all_data,  question == 'alcohol_ever' & response == '4 or more times a week') %>% .$value),
            "</b> reported drinking 4 or more times a week. ", ls3, "<br>",
            
            "In ", year(), " <b>", sum(dplyr::filter(all_data,  question == 'drug_ever' & response != 'I have never taken drugs') %>% .$value),
            "</b> of respondents reported having ever taken drugs and <b>",
            sum(dplyr::filter(all_data,  question == 'drug_ever' & response == 'I take drugs regularly (once a week or more)') %>% .$value),
            "</b> reported taking drugs regularly (once a week or more). ", ls4, "<br><br>",
            "* Green or red arrows in the plots indicate a <b>statistically significant difference</b> found between years in that particular group.<br><br>"
            )
        )
        
      })
      
      output$ls_pa <- renderEcharts4r({
        
        create_yearly_plot(stats = stats_w_diffs(),
                           q_coded = q_coded(),
                           question_p = "pa_60",
                           response_p = "6 to 7",
                           title = "Physical activity", 
                           subtitle = "Proportion from each group doing >=60 minutes of physical activity 6-7 days a week",
                           group_id = "ls",
                           legend = T, 
                           connect = F)
        
      })
      
      output$ls_smoking <- renderEcharts4r({
        
        create_yearly_plot(stats = stats_w_diffs(),
                           q_coded = q_coded(),
                           question_p = "smoke_ever",
                           response_p = "I smoke regularly (once a week or more)",
                           title = "Regular smokers", 
                           subtitle = "Proportion from each group that smoked often (once a week or more)",
                           group_id = "ls",
                           connect = F)
        
      })
      
      output$ls_alcohol <- renderEcharts4r({
        
        create_yearly_plot(stats = stats_w_diffs(),
                           q_coded = q_coded(),
                           question_p = "alcohol_ever",
                           response_p = "4 or more times a week",
                           title = "Alcohol consumption", 
                           subtitle = "Proportion from each group that drank alcohol more than 4 times a week",
                           group_id = "ls",
                           connect = F)
        
      })
      
      output$ls_drugs <- renderEcharts4r({
        
        create_yearly_plot(stats = stats_w_diffs(),
                           q_coded = q_coded(),
                           question_p = "drug_ever",
                           response_p = "I take drugs regularly (once a week or more)",
                           title = "Regular drug use", 
                           subtitle = "Proportion from each group that take drugs regularly (once a week or more)",
                           group_id = "ls",
                           connect = T)
        
      })
      
      
      # Safety Graphs -----------------------------------------------------------
      
      output$s_summary <- shiny::renderText({
        
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
        s1 <- ifelse(!is.na(group_name), paste0("This statistic was <b>",
                                                 dplyr::filter(key_data, question == 'safety_day' & response == "Unsafe") %>%
                                                   .$value, "</b> and <b>", 
                                                dplyr::filter(key_data, question == 'safety_dark' & response == "Unsafe") %>%
                                                  .$value,"</b> for ", group_name, " respondents, respectively."), "")
        
        s3 <- ifelse(!is.na(group_name), paste0("This statistic was <b>",
                                                dplyr::filter(key_data, question == 'violence_involved' & response == "Yes, I was the victim") %>%
                                                  .$value, "</b> for ", group_name, " respondents."), "")
        
        s4 <- ifelse(!is.na(group_name), paste0("This statistic was <b>",
                                                dplyr::filter(key_data, question == 'home_violence' & response == "Most days/Every day") %>%
                                                  .$value, "</b> for ", group_name, " respondents."), "")
        
        # --Text output ----
        shiny::HTML(
          paste0(
            "<h2>Safety</h2>",
            "Regarding safety, <b>",
            sum(dplyr::filter(all_data,  question == 'safety_day' & response == 'Unsafe') %>% .$value),
            "</b> of respondents felt unsafe going out during the day and <b>",
            sum(dplyr::filter(all_data,  question == 'safety_dark' & response == 'Unsafe') %>% .$value),
            "</b> felt unsafe going out after dark in ", year(), ". ", s1, "<br>",
            
            "In ", year(), " <b>", sum(dplyr::filter(all_data,  question == 'violence_involved' & response == 'Yes, I was the victim') %>% .$value),
            "</b> of respondents reported having been a victim of a violent incident in the past year. ", s3, "<br>",
            
            "In ", year(), " <b>", sum(dplyr::filter(all_data,  question == 'home_violence' & response == 'Most days/Every day') %>% .$value),
            "</b> of respondents reported violence at home (e.g. hitting, punching, slapping) between adults or older siblings at home most days or every day. ", s4, " <br><br>",
            
            "* Green or red arrows in the plots indicate a <b>statistically significant difference</b> found between years in that particular group.<br><br>"
          )
        )
        
      })
      
      output$s_safe_dark <- renderEcharts4r({
        
        create_yearly_plot(stats = stats_w_diffs(),
                           q_coded = q_coded(),
                           question_p = "safety_dark",
                           response_p = "Unsafe",
                           title = "Safety after dark", 
                           subtitle = "Proportion from each group that feel unsafe going out after dark",
                           group_id = "s",
                           legend = T, 
                           connect = F)
        
      })
      
      output$s_safe_day <- renderEcharts4r({
        
        create_yearly_plot(stats = stats_w_diffs(),
                           q_coded = q_coded(),
                           question_p = "safety_day",
                           response_p = "Unsafe",
                           title = "Safety during the day", 
                           subtitle = "Proportion from each group that feel unsafe going out during the day",
                           group_id = "s",
                           connect = F)
        
      })
      
      output$s_incident <- renderEcharts4r({
        
        create_yearly_plot(stats = stats_w_diffs(),
                           q_coded = q_coded(),
                           question_p = "violence_involved",
                           response_p = "Yes, I was the victim",
                           title = "Victim of a violent incident", 
                           subtitle = "Proportion from each group that were the victim of a violent incident in the past year",
                           group_id = "s",
                           connect = F)
        
      })
      
      output$s_domestic <- renderEcharts4r({
        
        create_yearly_plot(stats = filter(stats_w_diffs(), response != "Prefer not to say"),
                           q_coded = q_coded(),
                           question_p = "home_violence",
                           response_p = "Most days/Every day",
                           title = "Domestic violence", 
                           subtitle = "Proportion from each group that reported violence at home most days in the past month",
                           group_id = "s",
                           connect = T)
        
      })
      
      
      
      # Sexual Health Graphs ----------------------------------------------------
      
      output$sh_summary <- shiny::renderText({
        
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
        
        sh1 <- ifelse(!is.na(group_name), paste0("This statistic was <b>",
                                                dplyr::filter(key_data, question == 'sh_access' & response == "Yes") %>%
                                                  .$value, "</b> for ", group_name, " respondents."), "")
        
        sh2 <- ifelse(!is.na(group_name), paste0("This statistic was <b>",
                                                dplyr::filter(key_data, question == 'condoms_free' & response == "Yes") %>%
                                                  .$value, "</b> for ", group_name, " respondents."), "")
        
        sh3 <- ifelse(!is.na(group_name), paste0("This statistic was <b>",
                                                 dplyr::filter(key_data, question == 'sh_pressure' & response == "Agree") %>%
                                                   .$value, "</b> for ", group_name, " respondents."), "")
        
        # --Text output ----
        shiny::HTML(
          paste0(
            "<h2>Sexual Health</h2>",
            "In ", year(), " <b>", sum(dplyr::filter(all_data,  question == 'sh_access' & response == 'Yes') %>% .$value),
            "</b> of respondents stated that they know how to access sexual health services. ", sh1, "<br>",
            
            "In ", year(), " <b>", sum(dplyr::filter(all_data,  question == 'condoms_free' & response == 'Yes') %>% .$value),
            "</b> of respondents stated they knew where to get free condoms. ", sh2, " <br>",
            
            "In ", year(), " <b>", sum(dplyr::filter(all_data,  question == 'sh_pressure' & response == 'Agree') %>% .$value),
            "</b> of respondents agree with the statement that there is pressure on young people to have sex. ", sh3, " <br><br>",
            
            "* Green or red arrows in the plots indicate a <b>statistically significant difference</b> found between years in that particular group.<br><br>"
          )
        )
        
      })
      
      output$sh_services <- renderEcharts4r({
        
        create_yearly_plot(stats = stats_w_diffs(),
                           q_coded = q_coded(),
                           question_p = "sh_access",
                           response_p = "Yes",
                           title = "Access services", 
                           subtitle = "Proportion from each group that said they know how to access sexual health services",
                           group_id = "sh",
                           legend = T, 
                           connect = F)
        
      })
      
      output$sh_condoms <- renderEcharts4r({
        
        create_yearly_plot(stats = stats_w_diffs(),
                           q_coded = q_coded(),
                           question_p = "condoms_free",
                           response_p = "Yes",
                           title = "Knowing where to get free condoms", 
                           subtitle = "Proportion from each group that said they know where to get free condoms",
                           group_id = "sh",
                           connect = F)
        
      })
      
      output$sh_pressure <- renderEcharts4r({
        
        create_yearly_plot(stats = stats_w_diffs(),
                           q_coded = q_coded(),
                           question_p = "sh_pressure",
                           response_p = "Agree",
                           title = "Pressure to have sex", 
                           subtitle = "Proportion from each group that agreed that there is pressure on young people to have sex",
                           group_id = "sh",
                           connect = T)
        
      })
      
      output$sh_info <- echarts4r::renderEcharts4r({
        
        stats <- stats_combined()
        
        stats %>%
          dplyr::filter(grepl("shinfo_", question),
                        breakdown == input$sh_breakdown,
                        year == input$sh_year,
                        response == "Yes") %>%
          dplyr::group_by(breakdown) %>%
          dplyr::arrange(count) %>%
          dplyr::slice(tail(dplyr::row_number(), 5)) %>%
          echarts4r::e_charts(question_text) %>%
          echarts4r::e_bar(count, stack = "grp") %>%
          echarts4r::e_flip_coords() %>%
          echarts4r::e_x_axis(splitNumber = 2) %>%
          echarts4r::e_tooltip("item") %>%
          echarts4r::e_grid(left = "30%") %>%
          echarts4r::e_title("Top 5 reported ways to get information about sex/relationships",
                             paste("For", input$sh_breakdown, "in", input$sh_year)) %>%
          echarts4r::e_theme("walden") %>% 
          echarts4r::e_legend(show = FALSE)
        
      })
      
      output$sh_ui_breakdown <- shiny::renderUI({
        
        shinyWidgets::prettyRadioButtons(
          inputId = ns("sh_breakdown"),
          label = "",
          choices = unique(stats()$breakdown),
          status = "info",
          inline = TRUE
        )
        
      })
      
      output$sh_ui_year <- shiny::renderUI({
        
        shinyWidgets::prettyRadioButtons(
          inputId = ns("sh_year"),
          label = "",
          choices = unique(stats_combined()$year),
          status = "info",
          inline = TRUE
        )
        
      })
      
      # Other Graphs ------------------------------------------------------------
      
      
      
    })
}
