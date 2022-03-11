
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
library(glue)
library(plyr)

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
    src = "img/yphws_logo_horizontal.png",
    tablerNavMenu(id = "tabs",
                  pickerInput("comp", label = "Select what to group by",
                              choices = list("Sex" = "sex", 
                                             "Year group" = "schyear", 
                                             "Ethnicity" = "ethnicity",
                                             "IMD Quintile" = "imd_quintile",
                                             "Sexuality" = "sexuality", 
                                             "Young carer" = "caring", 
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
      explore_mod("explore"),
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
                   downloadButton("exp_table", "Export table"), 
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
  observe({
    
    if ("ethnicity" %in% input$comp) { browser() }
    
  })
  
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
                 data = reactive(rv$data$data),
                 data_old = reactive(rv$data_old$data),
                 stats = reactive(rv$stats),
                 stats_old = reactive(rv$stats_old),
                 comp = reactive(input$comp)
                 )
  
  # Explore data --------------------------------------------------------------------
  
  explore_mod_server("explore",
                     stats = reactive(rv$stats),
                     stats_old = reactive(rv$stats_old),
                     diffs = reactive(rv$diffs),
                     comp = reactive(input$comp),
                     q_coded = reactive(rv$data$q_coded),
                     q_coded_old = reactive(rv$data_old$q_coded_prev))
  
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
    
    rv$stats %>% 
      select(breakdown, question, question_text, response, count, denominator, value, lowercl, uppercl) %>% 
      mutate(value = round(value, 2), 
             lowercl = round(lowercl, 2), 
             uppercl = round(uppercl, 2)) %>% 
    reactable(filterable = TRUE)
    
  })
  
  output$exp_table <- downloadHandler(
    
    filename = "data_table.csv",
    content = function(con) { 
      
      data <- rv$stats %>% 
        select(breakdown, question, question_text, response, count, denominator, value, lowercl, uppercl) %>% 
        mutate(value = round(value, 2), 
               lowercl = round(lowercl, 2), 
               uppercl = round(uppercl, 2)) 
      
      write.csv(data, con)
      
      }
    
  )
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)
