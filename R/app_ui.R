#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @noRd
#' 
app_ui = function(request) {
  
  bslib::page_sidebar(
    shinyjs::useShinyjs(),
    shinybusy::add_busy_spinner(position = "bottom-right"),
    title = "Scenario comparison app (proto)",
    sidebar = bslib::sidebar(
      title = "Scenario selection",
      shiny::selectInput("selected_scheme", "Select scheme", choices = NULL),
      shiny::selectInput("scenario_1", "Select Scenario 1", choices = NULL),
      shiny::selectInput("scenario_1_runtime", "Scenario 1 runtime", choices = NULL),
      shiny::selectInput("scenario_2", "Select Scenario 2", choices = NULL),
      shiny::selectInput("scenario_2_runtime", "Scenario 2 runtime", choices = NULL),
      shiny::actionButton("render_plot", "Render Plots"),
      shiny::uiOutput("warning_text")
    ),
    bslib::card(
      bslib::card_header("Result"),
      shiny::uiOutput("errors"),
      shiny::textOutput("result_text"),
      shiny::tabsetPanel(
        shiny::tabPanel("Summary", mod_summary_ui("summary1")),
        shiny::tabPanel("Length of Stay", mod_los_ui("los1")),
        shiny::tabPanel("Waterfall", mod_waterfall_ui("waterfall1"))
      )
    )
  )
}