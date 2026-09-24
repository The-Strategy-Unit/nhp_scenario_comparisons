mod_beeswarm_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    htmltools::includeMarkdown(appfile("probabilistic-model-note.md")),
    htmltools::includeMarkdown(appfile("beeswarm-note.md")),
    filter_row(
      shiny::selectInput(ns("filter1"), "Activity Type", choices = NULL),
      shiny::selectInput(ns("filter2"), "Measure", choices = NULL)
    ),
    shiny::checkboxInput(
      ns("show_zero"),
      "Extend x-axis to zero?",
      value = FALSE
    ),
    shiny::plotOutput(ns("plot"))
  )
}

mod_beeswarm_server <- function(id, processed_data) {
  shiny::moduleServer(id, function(input, output, session) {
    df <- shiny::reactive(
      validate_rows(
        processed_data()$beeswarm_data,
        "No model run data is available for these two scenarios."
      )
    )

    filter1_choices <- shiny::reactive(
      pull_unique(df(), "activity_type_label")
    )
    filter1 <- shiny::reactive(
      resolve_selection(input$filter1, filter1_choices(), auto_max = Inf)
    )

    filter2_choices <- shiny::reactive({
      shiny::req(filter1())
      df() |>
        dplyr::filter(.data[["activity_type_label"]] == filter1()) |>
        pull_unique("measure_label")
    })
    filter2 <- shiny::reactive(
      resolve_selection(input$filter2, filter2_choices(), auto_max = Inf)
    )

    shiny::observe(sync_select_input(session, "filter1", filter1_choices()))
    shiny::observe(sync_select_input(session, "filter2", filter2_choices()))

    output$plot <- shiny::renderPlot(
      {
        shiny::req(filter1(), filter2())
        create_beeswarm_chart(df(), filter1(), filter2(), input$show_zero)
      },
      res = 100
    )
  })
}
