rsconnect::deployApp(
  appFiles = c("R/", "scenario_analysis_summary.qmd", 
               "app.R", "load-data.R", "R/nhp_outputs/", "supporting_data/"),
  appName = "scenario-comparison-app",
  appTitle = "Scenario comparisons app prototype",
  appId = 213,
  server = "connect.strategyunitwm.nhs.uk",
  envVars = c(
    "AZ_STORAGE_EP",
    "AZ_STORAGE_CONTAINER_INPUTS",
    "AZ_STORAGE_CONTAINER_SUPPORT"
  ),
  lint = FALSE,
  forceUpdate = TRUE
)
