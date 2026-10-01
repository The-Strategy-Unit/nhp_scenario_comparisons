mod_los_bar_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    filter_row(
      shiny::selectInput(ns("filter1"), "Point of Delivery", choices = NULL)
    ),
    shiny::plotOutput(ns("plot"))
  )
}

mod_los_bar_server <- function(id, processed_data) {
  shiny::moduleServer(id, function(input, output, session) {
    df <- shiny::reactive(
      validate_rows(
        processed_data()$los_data,
        "No length of stay data is available for these two scenarios."
      )
    )

    filter1_choices <- shiny::reactive(pull_unique(df(), "pod_label"))
    filter1 <- shiny::reactive(
      resolve_selection(input$filter1, filter1_choices(), auto_max = Inf)
    )

    shiny::observe(sync_select_input(session, "filter1", filter1_choices()))

    output$plot <- shiny::renderPlot(
      {
        shiny::req(filter1())
        create_los_chart(df(), filter1())
      },
      res = 100
    )
  })
}
