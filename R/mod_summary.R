mod_summary_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::uiOutput(ns("filters_ui")),
    shiny::plotOutput(ns("plot"))
  )
}

mod_summary_server <- function(id, df){
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    
  })
}





