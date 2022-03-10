
## IN DEVELOPMENT. 

library(tidyverse)
library(rmarkdown)
library(pins)
library(shiny)
library(tablerDash)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinybusy)
library(bs4Dash)
library(plotly)
library(echarts4r)
library(reactable)

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
      key_mod("key"),
      tablerTabItem(
        tabName = "ExploreData",
        fluidRow(
          # column(2, tags$style(HTML(".col-sm-2{position:fixed; z-index:1; height: 75%; overflow-y:auto;}")),
          #        tagList(
          #          fluidRow(
          #            tablerCard(width = 2,
          #                       htmlOutput(ns("explore_links")))
          #          )
          #          )
          # ),
          column(12, 
                 # pick survey topic
                 pickerInput(
                   inputId = "domains", 
                   label = "Select health topic/s:", 
                   choices = c(domains),
                   selected = "Safety", multiple = T
                 ),
                 uiOutput("explore_boxes"))
        )
      ),
      inequalities_mod("ineq"),
      tablerTabItem(
        tabName = "Export",
        tagList(
          fluidRow(
            tablerCard(title = "Export full report",
                       width = 12, 
                       uiOutput("exp_report_comp"),
                       uiOutput("exp_report_cat"),
                       downloadButton("exp_report", "Export report"))
          )
        ),
        tablerCard(width = 12, title = "Data table",
                   reactableOutput("export")
        )
        
      )
      ,
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
  rv$data <- get_data(data = isolate(rv$params$data_pin), params = isolate(rv$params)) # Raw data
  rv$data_old <- get_data(isolate(rv$params$prev_data_pin), params = isolate(rv$params)) # Raw data, previous. 
  
  # --Generate statistics using raw data
  observe({ 
    rv$stats <- get_stats(
      data = rv$data$data,
      group = input$comp,
      q_coded = rv$data$q_coded
    ) 
    
    # Last year's data
    if (input$comp %in% rv$data_old$q_coded_prev$question_coded){
      group_old <- input$comp 
    } else { 
      group_old <- "sex"
    }
    
    rv$stats_old <- get_stats(
      data = rv$data_old$data,
      group = group_old,
      q_coded = rv$data_old$q_coded_prev
    )
    
  })
  
  observe({
    sel_comp <- rv$data$data %>% 
      select(input$comp) 
    sel_comp <- as.character(unique(na.omit(unlist(sel_comp))))
    
    rv$diffs <- get_stats_diffs(stats = rv$stats, 
                                levels = c("All Responses", sel_comp), 
                                compare_to_all = F) # TODO
    
    # comparisons with last year
    old <- rv$stats_old %>% 
      mutate(breakdown = paste("Previous", breakdown)) %>% 
      filter(!is.na(breakdown))
    
    rv$diffs_w_old <- get_stats_diffs(stats = bind_rows(old, rv$stats), 
                                      levels = c("All Responses", unique(old$breakdown), sel_comp),
                                      compare_to_all = T) %>% 
      mutate(question_text.x = case_when(is.na(question_text.x) ~ question_text.y, 
                                         TRUE ~ question_text.x))
    
    
  })
  
  # Key Points --------------------------------------------------------------
  
  key_mod_server("key",
                 stats = reactive(rv$stats),
                 comp = reactive(input$comp))
  
  # Explore data --------------------------------------------------------------------
  
  # Data --------------------------------------------------------------------
  
  chk_var <- reactive({
    
    q_coded <- rv$data$q_coded
    # vector of selected vars
    single <- q_coded %>% 
      filter(question_theme %in% input$domains)
    
    #TODO deduplicate multicat questions.
    chk_var <- q_coded %>%
      filter(question_coded %in% single$question_coded) %>%
      pull(question_coded_gen)
    
    return(unique(chk_var))
    
  })
  
  # filtered datasets
  chk_stats <- reactive({
    stats <- rv$stats
    stats %>% 
      left_join(select(rv$data$q_coded, -question_text), by = c("question" = "question_coded")) %>% 
      filter(question_coded_gen %in% chk_var())
    
  })
  
  chk_stats_old <- reactive({
    stats_old <- rv$stats_old
    stats_old %>%
      left_join(select(rv$data_old$q_coded_prev, -question_text), by = c("question" = "question_coded")) %>%
      filter(question_coded_gen %in% chk_var())
  })
  
  chk_diff <- reactive({
    diffs <- rv$diffs
    diffs %>% 
      left_join(select(rv$data$q_coded, -question_text), by = c("question" = "question_coded")) %>% 
      filter(question_coded_gen %in% chk_var())
  })
  
  # Boxes -------------------------------------------------------------------
  boxes <- reactive({
    
    stats <- rv$stats
    stats_old <- rv$stats_old
    diffs <- rv$diffs
    comp <- input$comp
    q_coded <- rv$data$q_coded
    
    l <- list()
    for (i in 1:length(chk_var())){
      
      # Current question
      current <- filter(chk_stats(), question_coded_gen %in% chk_var()[i])
      current_old <- filter(chk_stats_old(), question_coded_gen %in% chk_var()[i]) %>% 
        mutate(multi_cat = as.logical(multi_cat),
               multi_binary = as.logical(multi_binary),
               year = "2021") %>% 
        bind_rows(mutate(current, year = "2020"))  
      
      multi <- ifelse(any(current$multi_cat, current$multi_binary), TRUE, FALSE) # check if multicat question
      multi_bin <- ifelse(all(current$multi_cat), FALSE, TRUE) # check if its multicat binary (yes/no)
      
      # --Create text and plots based on type of question--
      if (multi) { 
        int_plot <- create_multi_plot(df = current,
                                      plot_title = "",
                                      binary = multi_bin)
        
        trend_plot <- ""
        
        if(!multi_bin) {
          
          resp_interest <- paste(c("On most days", "I have never heard of it", "Agree", "Unsafe", "Yes"), 
                                 collapse = "|")
          resp_interest <- unique(current$response)[grepl(resp_interest, unique(current$response))]
          text <- create_sum_sentence(dataset = current,
                                      multi = multi,
                                      value_of_interest = resp_interest,
                                      full_data = chk_stats(),
                                      diffs = chk_diff(),
                                      custom_grp = unique(current$breakdown),
                                      group_of_interest = unique(current$breakdown)[2],
                                      q_coded = q_coded)
          
        } else {
          
          text <- create_sum_sentence(dataset = current,
                                      multi = multi,
                                      value_of_interest = "Yes",
                                      full_data = chk_stats(),
                                      diffs = chk_diff(),
                                      custom_grp = unique(current$breakdown),
                                      group_of_interest = unique(current$breakdown)[2],
                                      q_coded = q_coded)
          
        }
        
      } else {
        
        text <- create_sum_sentence(dataset = current,
                                    multi = multi,
                                    value_of_interest = "Yes",
                                    full_data = chk_stats(),
                                    diffs = chk_diff(),
                                    custom_grp = unique(current$breakdown),
                                    group_of_interest = unique(current$breakdown)[2],
                                    q_coded = q_coded)
        
        int_plot <- create_basic_plot(df = current,
                                      plot_custom_grp = unique(current$breakdown),
                                      plot_title = "")
        
        trend_plot <- current_old %>% 
          ggplot(aes(x = response, y = value, group = year)) +
          geom_bar(
            aes(color = year, fill = year),
            stat = "identity", position = position_dodge(0.8),
            width = 0.7
          ) +
          geom_errorbar(aes(ymin = lowercl, ymax = uppercl, group = year), 
                        width = 0.2, colour = "black", alpha = 0.5,
                        position = position_dodge(0.95)) +
          facet_wrap(~breakdown) +
          theme_minimal()
        
        trend_plot <- ggplotly(trend_plot)
        
        
      }
      
      
      
      # --Create boxes --
      l[[i]] <- tabItem("name", 
                        tabBox(width = 12, side = "right", status = "success",
                               collapsible = FALSE, 
                               title = "",
                               #HTML(paste0("<hr><br><a id='anchor-", current$question_coded_gen[1], "'></a>", chk_var()[i],"<br>")),
                               tabPanel("Summary", 
                                        HTML(
                                          text
                                        ),
                                        br(),
                                        int_plot
                               ),
                               tabPanel(
                                 "Trend",
                                 br(),
                                 trend_plot
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
                        ) )
    }
    
    return(l)
    
    
  })
  
  
  # TOC Links ---------------------------------------------------------------
  # links <- reactive({
  #   
  #   stats <- stats()
  #   diffs <- diffs()
  #   comp <- comp()
  #   q_coded <- q_coded()
  #   
  #   l <- list()
  #   for (i in 1:length(chk_var())){
  #     
  #     # Current question
  #     current <- filter(chk_stats(), question_coded_gen %in% chk_var()[i])
  #     
  #     q_coded <- q_coded
  #     text <- q_coded$question_coded_gen[q_coded$question_coded_gen %in% current$question_coded_gen] # for TOC
  #     
  #     l[[i]] <- paste0("<a href='#anchor-", current$question_coded_gen[i], "'>", text, "</a><br><br>")
  #     
  #   }
  #   
  #   output <- paste(unlist(l), collapse = "")
  #   
  #   return(output)
  #   
  # })
  
  output$explore_boxes <- renderUI(boxes())
  #output$explore_links <- renderText(links())
  
  # explore_mod_server("explore",
  #                    stats = reactive(rv$stats),
  #                    stats_old = reactive(rv$stats_old),
  #                    diffs = reactive(rv$diffs),
  #                    comp = reactive(input$comp),
  #                    q_coded = reactive(rv$data$q_coded),
  #                    q_coded_old = reactive(rv$data_old$q_coded_prev))
  
  # Inequalities ------------------------------------------------------------
  
  inequalities_mod_server("ineq",
                          params = reactive(rv$params),
                          q_coded = reactive(rv$data$q_coded),
                          stats = reactive(rv$stats),
                          diffs = reactive(rv$diffs))
  
  # Export ------------------------------------------------------------------
  
  output$exp_report_comp <- renderUI({
    
    pickerInput("exp_report_comp", label = "Select what to group by in your report",
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
                selected = input$comp, 
                multiple = FALSE)
    
  })
  
  output$exp_report_cat <- renderUI({
    
    choices <- rv$data$data %>% 
      select(input$exp_report_comp) %>% 
      distinct() %>% 
      pull(input$exp_report_comp)
    
    pickerInput("exp_report_cat", "Select the category from the selected group you are most interested in:",
                choices = as.character(na.omit(choices)), multiple = FALSE,
                selected = as.character(na.omit(choices)[1]))
    
  })
  
  output$exp_report <- downloadHandler(
    filename = "report.html",
    content = function(file) {
      tempReport <- file.path(tempdir(), "test.Rmd")
      file.copy("test.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(var = input$comp,
                     cat = input$exp_report_cat)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      show_modal_spinner(text = "Rendering report. Please wait, this should take 1-2 minutes.")
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      remove_modal_spinner() # remove it when done
      
    }
  )
  
  output$export <- renderReactable({
    
    reactable(rv$stats)
    
    
  })
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)
