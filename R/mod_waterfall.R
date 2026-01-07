mod_waterfall_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::verbatimTextOutput(ns("debug")),
    shiny::tags$p(
      "Regard these results as rough, high-level estimates of the number of rows added/removed due to each parameter.",
      style = "margin-top: 15px; margin-bottom: 15px;"),
    shiny::uiOutput(ns("filters_ui")),
    shiny::plotOutput(ns("plot"))
  )
}



mod_waterfall_server <- function(id, processed){ 
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    df <- shiny::reactive(processed()$waterfall_data$pcfs_1) #takes waterfall_data$pcfs_1 from processed
    df2 <- shiny::reactive(processed()$waterfall_data$pcfs_2)
    scenario_1_name <- shiny::reactive(processed()$waterfall_data$scenario_1_name)
    scenario_2_name <- shiny::reactive(processed()$waterfall_data$scenario_2_name)
    # could dynamically create UI here, based on the variables found within df?
    
    
    output$filters_ui <- shiny::renderUI({
      shiny::req(df())
      
      shiny::tagList(
        shiny::tags$div(style = "display: flex; gap: 15px;",
                        shiny::selectInput(ns("filter1"), 
                                           "Activity type", 
                                           choices = activity_type_pretty_names),
                        shiny::selectInput(ns("filter2"), 
                                           "Measure", 
                                           choices = NULL)
        )
      )
    })
    
    shiny::observe({
      shiny::req(df(), input$filter1)
      
      filter2_values <- df()[[input$filter1]] |>
        dplyr::pull(measure) |>
        unique()
      
      filter2_choices <- measure_pretty_names[measure_pretty_names %in% filter2_values]
      
      shiny::updateSelectInput(inputId = "filter2",
                               choices = filter2_choices)
      
    })
    
    output$plot <- shiny::renderPlot({
      shiny::req(df(), input$filter1, input$filter2)
      
      generate_waterfall_plot(df(),
                              df2(),
                              scenario_1_name(),
                              scenario_2_name(),
                              activity_type = input$filter1,
                              measure = input$filter2,
                              x_label = input$filter2,
                              y_label = "Change Factor",
                              title = glue::glue(
                                "{get_label(input$filter1, activity_type_pretty_names)}",
                                "{get_label(input$filter2, measure_pretty_names)}", 
                                "- Waterfall of Change Factors", 
                                .sep = " ")
                              )
    },
    res = 100,
    )
    
    
  })
}





