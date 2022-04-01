# UI ----------------------------------------------------------------------

about_mod <- function(id,
                      name = "About") {
  
  ns <- NS(id)
  
  tablerTabItem(
    tabName = name,
    fluidRow(
      column(12, 
             tablerCard(width = 12, title = "About the survey",
                        htmlOutput(ns("info"))),
             tablerCard(width = 12, title = "Using the dashboard", 
                        htmlOutput(ns("using"))),
             tablerCard(width = 12, "Survey question",
                        "To download the full set of questions asked this year, please click the button below.",
                        br(),
                        downloadButton(ns("download_q"), label = "Download file"))
      )
    )
  )
  
}


# Server ------------------------------------------------------------------

about_mod_server <- function(id) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- NS(id)

      output$info <- renderText({
        
        HTML(
          paste0("This dashboard shows data from the annual Young People’s Health & Wellbeing Survey (YPHWS) at Hertfordshire level.",
                 " The Young People’s Health & Wellbeing Survey (YPHWS) is an anonymous online survey which gathers self-reported",
                 " information annually from those aged 11-19 in Hertfordshire. The survey includes questions about home life, wellbeing,",
                 " diet, physical activity, smoking, alcohol use, drug use, sexual health, mental health, bullying, and safety.",
                 " The project is funded by Public Health and YC Hertfordshire and is run by the Public Health Evidence & Intelligence Team", 
                 " and provides an opportunity for partnership working between organisations providing services to young people around the county.<br><br>",
                 
                 "The survey has been ongoing for two years with good uptake: 12,923 responses in the first year and 11,681 responses in the second.",
                 " The data in this dashboard is expected to update annually during spring with the latest survey data, collected during November - December",
                 " in the previous year. Please note that additional questions/indicators may be added in response to health concerns during the time of the survey (e.g. COVID-19).",
                 " For more information about the survey and supporting reports, please visit the <a href='https://www.hertshealthevidence.org/yphws/what-is-the-yphws.aspx'>YPHWS page</a>",
                 " on the Herts Health Evidence website. For information on support for young people's health, please visit <a href='www.healthforteens.co.uk/hertfordshire'>Health for Teens</a>",
                 " or <a href='www.justtalkherts.org'>Just Talk</a>")
        )
      })
      
      output$using <- renderText({
        
        HTML(
          paste0(
            "To get the most out of the dashboard, we recommend starting by selecting how you want the data to be broken down by using the selection in the navigation bar.",
            " This will affect your data views in all tabs. If you select Sex, for example, most graphs and tables will be broken down by All, Female, Male, and Other. Selecting",
            " Year group will break the graphs and tables will be broken down by Years 7-12, etc. You can then explore the data in the Explore Data tab, where a graph, trend table,",
            " and data table will be generated for each question asked in the survey. You can navigate through health topics by using the checkbox on top. If you want to see inequalities", 
            " between the groups in your selected breakdown (e.g. 'Do female respondents have significantly higher proportions of self-harm this year?'), you can use the Inequalities tab", 
            " to select the health topic/s and question/s you are interested in and produce a 'tartan rug' visualisation showing if there are any statistically significant differences.",
            " The methodology is the same as the one used for the Public Health England Fingertips. <br><br>"
          )
        )
        
      })
      
      output$download_q <- downloadHandler(
        filename <- function() {
          paste("output.docx", sep=".")
        },
        
        content <- function(file) {
          file.copy("data-raw/questions.docx", file)
        },
        contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
      )
      
    }
  )
}
