#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @noRd
#' 

nhp_model_runs <- get_nhp_result_sets() |> 
  dplyr::filter(!app_version == "dev")

app_ui = function(request) {
  
  bslib::page_sidebar(
    shinybusy::add_busy_spinner(position = "bottom-right"),
    title = "Scenario comparison app (proto)",
    sidebar = bslib::sidebar(
      title = "Scenario selection",
      shiny::selectInput("selected_scheme", "Select scheme", choices = unique(nhp_model_runs$dataset)),
      shiny::selectInput("scenario_1", "Select Scenario 1", choices = NULL),
      shiny::selectInput("scenario_1_runtime", "Scenario 1 runtime", choices = NULL),
      shiny::selectInput("scenario_2", "Select Scenario 2", choices = NULL),
      shiny::selectInput("scenario_2_runtime", "Scenario 2 runtime", choices = NULL),
      shiny::actionButton("render_quarto", "Render Quarto Summary"),
      shiny::uiOutput("warning_text")
    ),
    bslib::card(
      bslib::card_header("Result"),
      shiny::textOutput("result_text"),
      shiny::uiOutput("quarto_summary")
    )
  )
}