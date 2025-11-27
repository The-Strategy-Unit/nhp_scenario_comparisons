#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @noRd
#' 
#' @import shinyjs
#' @import htmltools
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
    shiny::tabsetPanel(
      shiny::tabPanel(
        "Guidance on scenario selections",
        bslib::card(
          id = "card_guidance",
          #bslib::card_header("Guidance on scenario selections"),
          shiny::HTML(markdown::mark_html("inst/app/model-version-warning.md",
                                          output = FALSE, template = FALSE)),
          shiny::HTML("<br>"),
          shiny::HTML(markdown::mark_html("inst/app/scenario-timespan-warning.md",
                                          output = FALSE, template = FALSE)),
          shiny::HTML("<br>"),
          shiny::HTML(markdown::mark_html("inst/app/model-naming-reminder.md",
                                          output = FALSE, template = FALSE)),
          DT::dataTableOutput("metadata")
        )
      ),
      shiny::tabPanel(
        "Result",
        bslib::card(
          #bslib::card_header("Result"),
          shiny::uiOutput("errors"),
          shiny::textOutput("result_text"),
          shiny::tabsetPanel(
            shiny::tabPanel("Summary", mod_summary_ui("summary1")),
            shiny::tabPanel("Length of Stay", mod_los_ui("los1")),
            shiny::tabPanel("Waterfall", mod_waterfall_ui("waterfall1")),
            shiny::tabPanel("Activity Avoidance Impact", mod_activity_avoidance_impact_ui("activity_avoidance1")),
            shiny::tabPanel("Efficiencies Impact", mod_efficiencies_impact_ui("efficiencies1")),
            shiny::tabPanel("CI Bar", mod_ci_bar_ui("ci_bar1")),
            shiny::tabPanel("Beeswarm", mod_beeswarm_ui("beeswarm1")),
            shiny::tabPanel("ECDF", mod_ecdf_ui("ecdf1"))
          )
        )
      )
    )
  )
}