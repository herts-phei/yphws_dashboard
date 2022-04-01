# UI ----------------------------------------------------------------------

about_mod <- function(id,
                      name = "About") {
  
  ns <- NS(id)
  
  tablerTabItem(
    tabName = name,
    fluidRow(
      column(12, 
             tablerCard(width = 12,
                        htmlOutput(ns("info"))),
             tablerCard(width = 12, 
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
                 " or <a href='www.justtalkherts.org'>Just Talk</a>.")
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
