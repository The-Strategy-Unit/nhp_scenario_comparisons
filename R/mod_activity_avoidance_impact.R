mod_activity_avoidance_impact_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::verbatimTextOutput(ns("debug")),
    shiny::tags$p(
      "Regard these results as rough, high-level estimates of the number of rows added/removed due to each parameter.",
      style = "margin-top: 15px; margin-bottom: 15px;"),
    shiny::uiOutput(ns("filters_ui")),
    shiny::plotOutput(ns("plot"), height = "800px")
  )
}

mod_activity_avoidance_impact_server <- function(id, processed){ 
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    df <- shiny::reactive(processed()$ndg_variants_sc_comparison) #takes ndg_variants_sc_comparison from processed
    
    # could dynamically create UI here, based on the variables found within df?
    
    output$filters_ui <- shiny::renderUI({
      shiny::req(df())
      
      shiny::tagList(
        shiny::tags$div(style = "display: flex; gap: 15px;",
                        shiny::selectInput(ns("filter1"), 
                                           "Activity Type", 
                                           choices = activity_type_pretty_names),
                        shiny::selectInput(ns("filter2"), 
                                           "Measure", 
                                           choices = NULL)
        )
      )
    })
    
    shiny::observe({
      shiny::req(df(), input$filter1)

      filter2_values <- df() |> 
        dplyr::filter(activity_type == input$filter1) |>
        dplyr::pull(measure) |>
        unique()

      filter2_choices <- measure_pretty_names[measure_pretty_names %in% filter2_values]

      shiny::updateSelectInput(session,
                               inputId = "filter2",
                               choices = filter2_choices)

    })
    
    output$plot <- shiny::renderPlot({
      #shiny::req(df())
      shiny::validate(
        shiny::need(!is.null(df()), message = "No data available"),
        shiny::need(nrow(df()) > 0,
                    message = "No data available")
      )
      shiny::req(input$filter1, input$filter2)
      
      impact_bar_plot(df(),
                      chosen_change_factor = "activity_avoidance",
                      input$filter1,
                      input$filter2,
                      title = glue::glue(
                        "{get_label(input$filter1, activity_type_pretty_names)}",
                        "{get_label(input$filter2, measure_pretty_names)}", 
                        "- Impact of Individual Activity Avoidance TPMA Assumptions", 
                        .sep = " ")
                      ) 
      
    },
    res = 100,
    )
    
    
  })
}





