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
    
    output$plot <- shiny::renderPlot({
      shiny::req(!is.null(df))
      
      create_bar_plot(df, 
                      "Inpatient",
                      "Admissions",
                      "Inpatient admissions summary comparison")
    })
    
    
  })
}





