mod_waterfall_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::verbatimTextOutput(ns("debug")),
    shiny::uiOutput(ns("filters_ui")),
    shiny::plotOutput(ns("plot"))
  )
}

mod_waterfall_server <- function(id, processed){ 
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    df <- shiny::reactive(processed()$waterfall_data$pcfs_1) #takes data_combine from processed
    df2 <- shiny::reactive(processed()$waterfall_data$pcfs_2)
    # could dynamically create UI here, based on the variables found within df?
    
    output$filters_ui <- shiny::renderUI({
      shiny::req(df())
      
      shiny::tagList(
        shiny::tags$div(style = "display: flex; gap: 15px;",
                        shiny::selectInput(ns("filter1"), 
                                           "filter 1", 
                                           choices = names(df())),
                        shiny::selectInput(ns("filter2"), 
                                           "filter 2", 
                                           choices = NULL)
        )
      )
    })
    
    shiny::observe({
      shiny::req(df(), input$filter1)
      
      filter2_choices <- df()[[input$filter1]] |> 
        #dplyr::filter(pod_name == input$filter1) |> 
        dplyr::pull(measure) |> 
        unique()
      
      shiny::updateSelectInput(inputId = "filter2",
                               choices = filter2_choices)
      
    })
    
    output$plot <- shiny::renderPlot({
      shiny::req(df(), input$filter1, input$filter2)
      
      generate_waterfall_plot(df(),
                              df2(),
                              activity_type = input$filter1,
                              measure = input$filter2,
                              "Inpatient admissions summary comparison")
    },
    res = 100,
    )
    
    
  })
}





