# UI ----------------------------------------------------------------------

infobox_mod <- function(id, 
                        name = "infobox") {
  
  ns <- NS(id)
  
  uiOutput(ns("infobox"))
  
}

# Server ------------------------------------------------------------------

infobox_mod_server <- function(id,
                               value,
                               title) {
  
  moduleServer(
    id, 
    function(input, output, session) {
      
      ns <- NS(id)
      
      output$infobox <- renderUI({
        
        tablerStatCard(
          value = value,
          title = title,
          width = 12
        )
        
      })
      
    })
}