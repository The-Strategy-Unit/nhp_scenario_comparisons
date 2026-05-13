mod_p10_p90_bar_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::verbatimTextOutput(ns("debug")),
    shiny::includeMarkdown("inst/app/p10-p90-text.md"),
    shiny::uiOutput(ns("filters_ui")),
    shiny::plotOutput(ns("plot"))
  )
}

mod_p10_p90_bar_server <- function(id, processed) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    df <- shiny::reactive(processed()$data_distribution_summary) #takes data_distribution_summary from processed

    # could dynamically create UI here, based on the variables found within df?

    output$filters_ui <- shiny::renderUI({
      shiny::req(df())

      shiny::tagList(
        shiny::tags$div(
          style = "display: flex; gap: 15px;",
          shiny::selectInput(
            ns("category"),
            "Point of Delivery Category",
            choices = names(pod_categories)
          ),
          shiny::selectInput(
            ns("filter1"),
            "Point of Delivery",
            choices = NULL # will be filled dynamically
          )
        )
      )
    })

    observeEvent(input$category, {
      req(input$category, df())

      # All possible PODs for this category
      all_choices <- pod_categories[[input$category]]

      # Only keep PODs that actually appear in the data
      available <- all_choices[all_choices %in% unique(df()$pod)]

      updateSelectInput(
        session,
        "filter1",
        choices = available
      )
    })

    output$plot <- shiny::renderPlot(
      {
        shiny::req(df(), input$filter1)

        create_bar_plot_distribution(
          df(),
          input$filter1,
          glue::glue(
            "{get_label(input$filter1, unlist(pod_categories))}",
            "- Principal projection (with p10 and p90 indicator)",
            .sep = " "
          )
        )
      },
      res = 100,
    )
  })
}
