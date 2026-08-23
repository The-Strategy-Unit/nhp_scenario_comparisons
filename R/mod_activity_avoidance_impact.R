mod_activity_avoidance_impact_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::verbatimTextOutput(ns("debug")),
    htmltools::includeMarkdown("inst/app/aa-impact-text.md"),
    shiny::uiOutput(ns("filters_ui")),
    shiny::plotOutput(ns("plot"), height = "800px")
  )
}

mod_activity_avoidance_impact_server <- function(id, processed_data) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    df <- shiny::reactive(processed_data()$icf_impact_data)
    filt_df <- shiny::reactive({
      dplyr::filter(df(), .data[["change_factor"]] == "activity_avoidance")
    })

    output$filters_ui <- shiny::renderUI({
      shiny::req(filt_df())

      shiny::tagList(
        shiny::tags$div(
          style = "display: flex; gap: 15px;",
          shiny::selectInput(
            ns("filter1"),
            "Activity Type",
            choices = pull_unique(filt_df(), "activity_type_label")
          ),
          shiny::selectInput(ns("filter2"), "Measure", choices = NULL)
        )
      )
    })

    shiny::observe({
      shiny::req(filt_df(), input$filter1)
      filter2_choices <- filt_df() |>
        dplyr::filter(.data[["activity_type_label"]] == input$filter1) |>
        pull_unique("measure_label")
      shiny::freezeReactiveValue(input, "filter2")
      shiny::updateSelectInput(
        session,
        inputId = "filter2",
        choices = filter2_choices,
        selected = filter2_choices[[1]]
      )
    })

    output$plot <- shiny::renderPlot(
      {
        shiny::req(filt_df(), input$filter1, input$filter2)
        # Add validation for filtered data
        filtered_data <- filt_df() |>
          dplyr::filter(
            .data[["activity_type_label"]] == input$filter1,
            .data[["measure_label"]] == input$filter2,
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

        create_impact_chart(
          filtered_data,
          "activity_avoidance",
          input$filter1,
          input$filter2
        )
      },
      res = 100
    )
  })
}
