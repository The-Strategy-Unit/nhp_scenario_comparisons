mod_ci_bar_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::verbatimTextOutput(ns("debug")),
    shiny::uiOutput(ns("filters_ui")),
    shiny::plotOutput(ns("plot"))
  )
}

mod_ci_bar_server <- function(id, processed){
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    df <- shiny::reactive(processed()$data_distribution_summary) #takes data_distribution_summary from processed
    
    # could dynamically create UI here, based on the variables found within df?
    
    output$filters_ui <- shiny::renderUI({
      shiny::req(df())
      
      shiny::tagList(
        shiny::tags$div(style = "display: flex; gap: 15px;",
                        shiny::selectInput(ns("filter1"), 
                                           "filter 1", 
                                           choices = unique(df()$pod))
        )
      )
    })
    
    output$plot <- shiny::renderPlot({
      shiny::req(df(), input$filter1)
      
      create_bar_plot_distribution(df(), 
                      input$filter1,
                      "Inpatient admissions summary comparison")
    },
    res = 100,
    )
    
    
  })
}





