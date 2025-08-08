# Load required packages
library(shiny)
library(bslib)
library(dplyr)
library(quarto)

# Source the Azure function
source("R/azure.R")

# Get model runs data
nhp_model_runs <- get_nhp_result_sets()

# Define UI
ui <- bslib::page_sidebar(
  title = "Scenario comparison app (proto)",
  sidebar = bslib::sidebar(
    title = "Scenario selection",
    shiny::selectInput("selected_scheme", "Select scheme", choices = unique(nhp_model_runs$dataset)),
    shiny::selectInput("scenario_1", "Select Scenario 1", choices = NULL),
    shiny::selectInput("scenario_2", "Select Scenario 2", choices = NULL),
    shiny::actionButton("render_quarto", "Render Quarto Summary"),
    shiny::textOutput("warning_text")
  ),
  bslib::card(
    bslib::card_header("Result"),
    shiny::textOutput("result_text"),
    shiny::uiOutput("quarto_summary")
  )
)

# Define server
server <- function(input, output, session) {
  # Dynamically update scenarios when scheme is selected
  observe({
    selected_scheme <- input$selected_scheme
    filtered_scenarios <- nhp_model_runs |> 
      dplyr::filter(dataset == selected_scheme) |> 
      pull(scenario) |> 
      unique()
    
    updateSelectInput(session, "scenario_1", choices = filtered_scenarios)
    updateSelectInput(session, "scenario_2", choices = filtered_scenarios)
  })
  
  output$result_text <- renderText({
    paste("You have selected",
          input$scenario_1,
          "and",
          input$scenario_2,
          "from the scheme",
          input$selected_scheme)
  })
  
  output$warning_text <- renderText({
    s1 <- nhp_model_runs |> filter(scenario == input$scenario_1, dataset == input$selected_scheme)
    s2 <- nhp_model_runs |> filter(scenario == input$scenario_2, dataset == input$selected_scheme)
    
    if (input$scenario_1 == input$scenario_2) {
      "Warning: Scenario 1 and Scenario 2 must be different."
    } else if (nrow(s1) > 0 && nrow(s2) > 0 &&
               (s1$start_year != s2$start_year || s1$end_year != s2$end_year)) {
      "Warning: The start and end years of the selected scenarios must match."
    } else {
      ""
    }
  })
  
  observeEvent(input$render_quarto, {
    if (input$scenario_1 != input$scenario_2) {
      quarto::quarto_render(
        "scenario_analysis_summary.qmd", 
        output_file = "scenario_analysis_summary.html", 
        execute_params = list(
          scenario_1 = input$scenario_1,
          scenario_2 = input$scenario_2
        ))
      output$quarto_summary <- renderUI({
        includeHTML("scenario_analysis_summary.html")
      })
    } else {
      output$quarto_summary <- renderUI({
        HTML("<p style='color:red;'>Scenarios must be different to render the summary.</p>")
      })
    }
  })
}

# Run the app
shinyApp(ui, server)
