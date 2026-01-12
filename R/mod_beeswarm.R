mod_beeswarm_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::verbatimTextOutput(ns("debug")),
    shiny::tags$p(
      "Each dot represents an individual model run from the scenario.
    The thickness of the swarm indicates the density of runs in certain ranges.
    The dashed vertical line indicates the principal projection for each scenario; the solid vertical line indicates the baseline value",
      style = "margin-top: 15px; margin-bottom: 15px;"),
    shiny::uiOutput(ns("filters_ui")),
    shiny::plotOutput(ns("plot"))
  )
}

mod_beeswarm_server <- function(id, processed){
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    #will result_1 and result_2 always have the same activity_type and measures 
    #available? or will using result_1 to make the filters leave out some 
    #combinations sometimes?
    df <- shiny::reactive(processed()$distribution_data$result_1) #takes result_1 from processed
    df2 <- shiny::reactive(processed()$distribution_data$result_2) 
    scn1 <- shiny::reactive(processed()$distribution_data$scenario_1_name)
    scn2 <- shiny::reactive(processed()$distribution_data$scenario_2_name)
    
    pods <- shiny::reactive({
      get_activity_type_pod_measure_options() |> 
        dplyr::filter(pod %in% unique(df()$result$default$pod),
                      pod %in% unique(df2()$result$default$pod))
    })
    
    # could dynamically create UI here, based on the variables found within df?
    
    output$filters_ui <- shiny::renderUI({
      shiny::req(df())
      
      shiny::tagList(
        shiny::tags$div(style = "display: flex; gap: 15px;",
                        shiny::selectInput(ns("filter1"), 
                                           "Activity Type", 
                                           choices = unique(pods()$activity_type_name)),
                        shiny::selectInput(ns("filter2"), 
                                           "Measure", 
                                           choices = NULL)
        )
      )
    })
    
    shiny::observe({
      shiny::req(df(), input$filter1)
      
      filter2_values <- pods() |> 
        dplyr::filter(activity_type_name == input$filter1) |> 
        dplyr::pull(measures) |> 
        unique()
      
      filter2_choices <- measure_pretty_names[measure_pretty_names %in% filter2_values]
      
      shiny::updateSelectInput(inputId = "filter2",
                               choices = filter2_choices)
      
    })
    
    output$plot <- shiny::renderPlot({
      shiny::req(df(), input$filter1, input$filter2)
      
      selected_pods <- pods() |> 
        dplyr::filter(activity_type_name == input$filter1) |> 
        dplyr::pull(pod)
      
      combined_dist <- dplyr::bind_rows(scenario_1 = get_model_run_distribution(df(),
                                                                                pod = selected_pods,
                                                                                measure = input$filter2,
                                                                                site_codes = NULL) |> 
                                          dplyr::mutate(scenario = scn1())
                                        ,
                                        scenario_2 = get_model_run_distribution(df2(),
                                                                                pod = selected_pods,
                                                                                measure = input$filter2,
                                                                                site_codes = NULL) |> 
                                          dplyr::mutate(scenario = scn2())
                                        #, .id = "scenario"
      )
      
      
      mod_model_results_distribution_beeswarm_plot_scenario(combined_dist,
                                                            scenario_1_name = scn1(),
                                                            scenario_2_name = scn2(),
                                                            FALSE) +
        ggplot2::labs(y = get_label(input$filter2, measure_pretty_names),
                      title = glue::glue(input$filter1, 
                                         input$filter2,
                                         "- Distribution of Model Runs",
                                         .sep = " "))
    },
    res = 100,
    )
    
    
  })
}





