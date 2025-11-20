mod_processing_server <- function(id, scenario_selections, trigger){
  shiny::moduleServer(id, function(input, output, session){
    
    processed <- shiny::eventReactive(trigger()). {
      req(scenario_selections())
      
    }
    
    
  })
}
