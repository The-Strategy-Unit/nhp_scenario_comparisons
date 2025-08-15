rsconnect::deployApp(
  appFiles = c("R/", "scenario_analysis_summary.qmd", "app.R", "load-data.R", "R/nhp_outputs/", "supporting_data/"),
  appName = "scenario-comparison-app",
  appTitle = "Scenario comparisons app prototype",
  server = "connect.strategyunitwm.nhs.uk",
  appId = 185,
  lint = FALSE,
  forceUpdate = TRUE
)
