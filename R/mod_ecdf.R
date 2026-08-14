mod_ecdf_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::verbatimTextOutput(ns("debug")),
    htmltools::includeMarkdown("inst/app/probabilistic-model-note.md"),
    htmltools::includeMarkdown("inst/app/s-curve-note.md"),
    shiny::uiOutput(ns("filters_ui")),
    shiny::plotOutput(ns("plot"))
  )
}

mod_ecdf_server <- function(id, processed_data) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # `beeswarm data` is valid for the ecdf plot as well
    df <- shiny::reactive(processed_data()$beeswarm_data)

    output$filters_ui <- shiny::renderUI({
      shiny::req(df())

      shiny::tagList(
        shiny::tags$div(
          style = "display: flex; gap: 15px;",
          shiny::selectInput(
            ns("filter1"),
            "Activity Type",
            choices = pull_unique(df(), "activity_type_label")
          ),
          shiny::selectInput(ns("filter2"), "Measure", choices = NULL)
        )
      )
    })

    shiny::observe({
      shiny::req(df(), input$filter1)
      filter2_choices <- df() |>
        dplyr::filter(.data[["activity_type_label"]] == input$filter1) |>
        pull_unique("measure_label")
      shiny::freezeReactiveValue(input, "filter2")
      shiny::updateSelectInput(inputId = "filter2", choices = filter2_choices)
    })

    output$plot <- shiny::renderPlot(
      {
        shiny::req(df(), input$filter1, input$filter2)
        create_ecdf_chart(df(), input$filter1, input$filter2)
      },
      res = 100
    )
  })
}
