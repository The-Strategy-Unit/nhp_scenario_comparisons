mod_p10p90_bar_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    htmltools::includeMarkdown(appfile("p10-p90-text.md")),
    filter_row(
      shiny::selectInput(ns("filter1"), "Activity type", choices = NULL),
      shiny::selectInput(ns("filter2"), "Point of Delivery", choices = NULL)
    ),
    shiny::plotOutput(ns("plot"))
  )
}

mod_p10p90_bar_server <- function(id, processed_data) {
  shiny::moduleServer(id, function(input, output, session) {
    df <- shiny::reactive(
      validate_rows(
        processed_data()$principal_pi_data,
        "No prediction interval data is available for these two scenarios."
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
        pull_unique("pod_label")
    })
    filter2 <- shiny::reactive(
      resolve_selection(input$filter2, filter2_choices(), auto_max = Inf)
    )

    shiny::observe(sync_select_input(session, "filter1", filter1_choices()))
    shiny::observe(sync_select_input(session, "filter2", filter2_choices()))

    output$plot <- shiny::renderPlot(
      {
        shiny::req(filter1(), filter2())
        create_principal_pi_chart(df(), filter1(), filter2())
      },
      res = 100
    )
  })
}
