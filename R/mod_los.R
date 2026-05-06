mod_los_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::verbatimTextOutput(ns("debug")),
    shiny::uiOutput(ns("filters_ui")),
    shiny::plotOutput(ns("plot"))
  )
}

mod_los_server <- function(id, processed){ 
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    df <- shiny::reactive(processed()$data_combine) #takes data_combine from processed
    
    # could dynamically create UI here, based on the variables found within df?
    
    output$filters_ui <- shiny::renderUI({
      shiny::req(df())
      
      shiny::tagList(
        shiny::tags$div(style = "display: flex; gap: 15px;",
                        shiny::selectInput(ns("filter1"), 
                                           "Point of Delivery", 
                                           choices = unique(df()$pod_name)),
                        shiny::selectInput(ns("filter2"), 
                                           "Measure", 
                                           choices = NULL)
        )
      )
    })
    
    shiny::observe({
      shiny::req(df(), input$filter1)
      
      filter2_choices <- df() |> 
        dplyr::filter(pod_name == input$filter1) |> 
        dplyr::pull(measure) |> 
        unique() |> 
        stringr::str_to_title()
      
      shiny::updateSelectInput(inputId = "filter2",
                               choices = filter2_choices)
      
    })
    
    output$plot <- shiny::renderPlot({
      shiny::req(df(), input$filter1, input$filter2)
      
      create_bar_plot_los(df(), 
                          input$filter1,
                          input$filter2,
                          glue::glue(input$filter1, input$filter2, "- Length of Stay Comparison", .sep = " "))
    },
    res = 100,
    )
    
    
  })
}





