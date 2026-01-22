mod_p10_p90_bar_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::verbatimTextOutput(ns("debug")),
    shiny::includeMarkdown("inst/app/p10-p90-text.md"),
    shiny::uiOutput(ns("filters_ui")),
    shiny::plotOutput(ns("plot"))
  )
}

mod_p10_p90_bar_server <- function(id, processed){
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    df <- shiny::reactive(processed()$data_distribution_summary) #takes data_distribution_summary from processed
    
    # could dynamically create UI here, based on the variables found within df?
    
    output$filters_ui <- shiny::renderUI({
      shiny::req(df())
      
      shiny::tagList(
        shiny::tags$div(style = "display: flex; gap: 15px;",
                        shiny::selectInput(ns("filter1"), 
                                           "Point of Delivery", 
                                           choices = pod_pretty_names[pod_pretty_names %in% unique(df()$pod)])
        )
      )
    })
    
    output$plot <- shiny::renderPlot({
      shiny::req(df(), input$filter1)
      
      create_bar_plot_distribution(df(), 
                                   input$filter1,
                                   glue::glue(
                                     "{get_label(input$filter1, pod_pretty_names)}", 
                                     "- Principal projection (with p10 and p90 indicator)", 
                                     .sep = " ")
                                   )
    },
    res = 100,
    )
    
    
  })
}





