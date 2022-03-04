
# UI ----------------------------------------------------------------------

explore_mod <- function(id,
                        name = "ExploreData") {

  ns <- NS(id)

  domains <- c("Living Conditions", "Diet and Lifestyle",
               "Education", "Demographics",
               "Mental Health and Wellbeing", "Smoking and Vaping",
               "Alcohol Consumption", "Drug Use",
               "Sexual Health", "Safety",
               "Sustainability", "COVID-19")
  names(domains) <- domains

  tablerTabItem(
    tabName = name,
    fluidRow(
      column(2,
             # pick survey topic
             pickerInput(
               inputId = ns("domains"),
               label = "Select health topic/s:",
               choices = c(domains),
               selected = "Safety", multiple = T
             ),
             uiOutput(ns("questions")),
             HTML("<a href='#anchorid'>Working anchor example</a>")
      ),
      column(10, uiOutput(ns("explore_boxes")))
    )
  )

}



# Server ------------------------------------------------------------------

explore_mod_server <- function(id,
                               stats,
                               diffs,
                               comp,
                               q_coded) {

  moduleServer(
    id,
    function(input, output, session) {

      ns <- NS(id)

# Reactive UIs ------------------------------------------------------------

      output$questions <- renderUI({
        questions <- isolate(q_coded) %>%
          mutate(survey_text = as.character(survey_text)) %>%
          filter(question_theme %in% input$domains)

        pickerInput(ns("questions"), label = "Select/type in a question (multiple can be selected)",
                    choices = as.character(unique(questions$survey_text)), multiple = T,
                    selected = as.character(unique(questions$survey_text)),
                    options = list(`live-search` = TRUE))
      })


# Data --------------------------------------------------------------------

      chk_var <- reactive({
        # vector of selected vars
        single <- q_coded %>%
          filter(survey_text %in% input$questions, !multicat)

        #TODO deduplicate multicat questions.
        chk_var <- q_coded %>%
          filter(question_coded %in% single$question_coded) %>%
          pull(question_coded)

        return(chk_var)

      })

        # rv$filtered$chk_var() <- rv$data$q_coded %>%
        #   filter(survey_text %in% input$questions, multicat) %>%
        #   mutate(question_raw = gsub("\\..*", "", question_raw),
        #          question_coded = question_coded_gen) %>%
        #   distinct(question_raw, .keep_all = TRUE) %>%
        #   bind_rows(single) %>%
        #   pull(question_coded)

        # filtered datasets
      chk_stats <- reactive( filter(stats, question %in% chk_var()) )
      chk_diff <- reactive( filter(diffs, question %in% chk_var()) )

# Boxes -------------------------------------------------------------------

      boxes <- reactive({

        l <- list()
        for (i in 1:length(chk_var())){

          # Current question
          current <- filter(chk_stats(), question %in% chk_var()[i])

          l[[i]] <- tabItem("boxes",
                            bs4TabCard(width = 12, side = "right", status = "success", collapsible = FALSE, 
                                                   title = HTML(paste0("<hr><br><a id='anchorid'></a>", chk_var()[i],"<br>")),
                                                   tabPanel("Summary",
                                                            HTML(create_sum_sentence(dataset = current,
                                                                                     multi = F,
                                                                                     value_of_interest = F,
                                                                                     full_data = chk_stats(),
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
                                                     chk_stats() %>%
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

    }
  )
}
