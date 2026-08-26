mod_p10p90_bar_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    htmltools::includeMarkdown(appfile("p10-p90-text.md")),
    shiny::plotOutput(ns("plot"))
  )
}

mod_p10p90_bar_server <- function(id, processed_data) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    df <- shiny::reactive(processed_data()$principal_pi_data)

    output$filters_ui <- shiny::renderUI({
      shiny::req(df())

      shiny::tagList(
        shiny::tags$div(
          style = "display: flex; gap: 15px;",
          shiny::selectInput(
            ns("filter1"),
            "Activity type",
            choices = pull_unique(df(), "activity_type_label")
          ),
          shiny::selectInput(ns("filter2"), "Point of Delivery", choices = NULL)
        )
      )
    })

    shiny::observe({
      shiny::req(df(), input$filter1)

      filter2_choices <- df() |>
        dplyr::filter(.data[["activity_type_label"]] == input$filter1) |>
        pull_unique("pod_label")
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
        shiny::req(df(), input$filter1, input$filter2)
        create_principal_pi_chart(df(), input$filter1, input$filter2)
      },
      res = 100
    )
  })
}
