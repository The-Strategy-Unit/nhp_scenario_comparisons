mod_activity_avoidance_impact_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    htmltools::includeMarkdown(appfile("aa-impact-text.md")),
    filter_row(
      shiny::selectInput(ns("filter1"), "Activity Type", choices = NULL),
      shiny::selectInput(ns("filter2"), "Measure", choices = NULL)
    ),
    shiny::plotOutput(ns("plot"), height = "800px")
  )
}

mod_activity_avoidance_impact_server <- function(id, processed_data) {
  shiny::moduleServer(id, function(input, output, session) {
    df <- shiny::reactive({
      processed_data()$tpma_impact_data |>
        dplyr::filter(.data[["change_factor"]] == "activity_avoidance") |>
        validate_rows(
          "No activity avoidance TPMAs are present in these two scenarios."
        )
    })

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

        filtered_data <- df() |>
          dplyr::filter(
            .data[["activity_type_label"]] == filter1(),
            .data[["measure_label"]] == filter2(),
            .data[["value"]] < 0
          )

        shiny::validate(
          shiny::need(
            nrow(filtered_data) > 0,
            message = paste0(
              "No activity avoidance TPMAs affect this combination of ",
              "activity type and measure"
            )
          )
        )

        create_tpma_impact_chart(
          filtered_data,
          "activity_avoidance",
          filter1(),
          filter2()
        )
      },
      res = 100
    )
  })
}
