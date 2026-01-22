mod_efficiencies_impact_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::verbatimTextOutput(ns("debug")),
    shiny::includeMarkdown("inst/app/efficiencies-impact-text.md"),
    shiny::uiOutput(ns("filters_ui")),
    shiny::plotOutput(ns("plot"))
  )
}

mod_efficiencies_impact_server <- function(id, processed){ 
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
        dplyr::filter(activity_type == input$filter1,
                      measure != "admissions") |>
        dplyr::pull(measure) |>
        unique()
      
      filter2_choices <- measure_pretty_names[measure_pretty_names %in% filter2_values]
      
      shiny::updateSelectInput(inputId = "filter2",
                               choices = filter2_choices)
      
    })
    
    output$plot <- shiny::renderPlot({
      shiny::req(df(), input$filter1, input$filter2)
      
      impact_bar_plot(df(),
                      chosen_change_factor = "efficiencies",
                      input$filter1,
                      input$filter2,
                      title = glue::glue(
                        "{get_label(input$filter1, activity_type_pretty_names)}",
                        "{get_label(input$filter2, measure_pretty_names)}", 
                        "- Impact of Individual Efficiencies TPMA Assumptions", 
                        .sep = " ")
                      )
    },
    res = 100,
    )
    
    
  })
}





