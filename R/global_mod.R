# UI ----------------------------------------------------------------------

infobox_mod <- function(id, 
                        name = "infobox") {
  
  ns <- shiny::NS(id)
  
  shiny::uiOutput(ns("infobox"))
  
}

# Server ------------------------------------------------------------------

infobox_mod_server <- function(id,
                               value,
                               title) {
  
  shiny::moduleServer(
    id, 
    function(input, output, session) {
      
      ns <- shiny::NS(id)
      
      output$infobox <- shiny::renderUI({
        
        tablerDash::tablerStatCard(
          value = value(),
          title = title,
          width = 12
        )
        
      })
      
    })
}