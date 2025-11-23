mod_beeswarm_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::verbatimTextOutput(ns("debug")),
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
    df <- shiny::reactive(processed()$beeswarm_data$result_1) #takes result_1 from processed
    df2 <- shiny::reactive(processed()$beeswarm_data$result_2) 
    scn1 <- shiny::reactive(processed()$beeswarm_data$scenario_1_name)
    scn2 <- shiny::reactive(processed()$beeswarm_data$scenario_2_name)
    
    pods <- shiny::reactive({
      get_activity_type_pod_measure_options() |> 
        dplyr::filter(pod %in% unique(df()$result$default$pod))
    })
    
    # could dynamically create UI here, based on the variables found within df?
    
    output$filters_ui <- shiny::renderUI({
      shiny::req(df())
      
      shiny::tagList(
        shiny::tags$div(style = "display: flex; gap: 15px;",
                        shiny::selectInput(ns("filter1"), 
                                           "filter 1", 
                                           choices = unique(pods()$activity_type_name)),
                        shiny::selectInput(ns("filter2"), 
                                           "filter 2", 
                                           choices = NULL)
        )
      )
    })
    
    shiny::observe({
      shiny::req(df(), input$filter1)
      
      filter2_choices <- pods() |> 
        dplyr::filter(activity_type_name == input$filter1) |> 
        dplyr::pull(measures) |> 
        unique()
      
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
                                                                   site_codes = NULL)# |> 
                                         # dplyr::mutate(scenario = scn1())
                                          ,
                                        scenario_2 = get_model_run_distribution(df2(),
                                                                   pod = selected_pods,
                                                                   measure = input$filter2,
                                                                   site_codes = NULL) #|> 
                                          #dplyr::mutate(scenario = scn2())
                                        , .id = "scenario"
      )
      
      
      mod_model_results_distribution_beeswarm_plot_scenario(combined_dist, 
                                                            FALSE) +
        ggplot2::ggtitle("TEST")
    },
    res = 100,
    )
    
    
  })
}





