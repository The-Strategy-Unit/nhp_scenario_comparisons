app_ui <- function(request) {
  bslib::page_sidebar(
    shinyjs::useShinyjs(),
    shinybusy::add_busy_spinner(position = "bottom-right"),

    title = shiny::div(
      style = "display:flex; justify-content:space-between; align-items:center; width:100%;",
      shiny::h1("Scenario Comparison App"),
      shiny::actionButton(
        inputId = "feedback",
        label = "Give feedback",
        onClick = glue::glue(
          "window.open('{Sys.getenv(\"FEEDBACK_FORM_URL\")}', '_blank')"
        )
      )
    ),
    sidebar = bslib::sidebar(
      shiny::uiOutput("warning_text"),
      title = "Scenario selection",
      shinyWidgets::pickerInput(
        "selected_scheme",
        "Select scheme",
        choices = NULL
      ),
      shinyWidgets::pickerInput(
        "scenario1",
        "Select Scenario 1",
        choices = NULL
      ),
      shinyWidgets::pickerInput(
        "scenario1_rt",
        "Scenario 1 Runtime",
        choices = NULL
      ),
      shinyWidgets::pickerInput(
        "scenario2",
        "Select Scenario 2",
        choices = NULL
      ),
      shinyWidgets::pickerInput(
        "scenario2_rt",
        "Scenario 2 Runtime",
        choices = NULL
      ),
      shiny::actionButton("render_plot", "Render Plots", disabled = TRUE)
    ),
    shiny::verbatimTextOutput("debug"),
    shiny::tabsetPanel(
      shiny::tabPanel(
        "Introduction",
        bslib::card(
          id = "intro",
          bslib::card_body(htmltools::includeMarkdown("inst/app/intro_text.md"))
        )
      ),
      shiny::tabPanel(
        "Guidance",
        bslib::card(
          id = "card_guidance",
          htmltools::includeMarkdown("inst/app/model-version-warning.md"),
          htmltools::includeMarkdown("inst/app/scenario-timespan-warning.md"),
          htmltools::includeMarkdown("inst/app/model-naming-reminder.md"),
          htmltools::includeMarkdown("inst/app/bed-days-note.md"),
          shiny::tagList(
            shiny::tags$h3("Scenarios metadata"),
            DT::dataTableOutput("metadata")
          )
        )
      ),
      shiny::tabPanel(
        "View comparisons",
        bslib::card(
          shiny::uiOutput("result_text"),
          shiny::tabsetPanel(
            shiny::tabPanel("Summary", mod_summary_bar_ui("summary")),
            shiny::tabPanel("Length of Stay", mod_los_bar_ui("los")),
            shiny::tabPanel("Waterfall", mod_waterfall_ui("waterfall")),
            shiny::tabPanel(
              "Activity Avoidance Impact",
              mod_activity_avoidance_impact_ui("activity_avoidance")
            ),
            shiny::tabPanel(
              "Efficiencies Impact",
              mod_efficiencies_impact_ui("efficiencies")
            ),
            shiny::tabPanel(
              "P10-P90 Intervals",
              mod_p10p90_bar_ui("p10p90_bar")
            ),
            shiny::tabPanel("Beeswarm", mod_beeswarm_ui("beeswarm")),
            shiny::tabPanel("S-curve", mod_ecdf_ui("ecdf"))
          )
        )
      )
    )
  )
}
