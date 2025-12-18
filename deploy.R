rsconnect::deployApp(
  appFiles = c("app.R",
               "R/", 
               "inst/app",
               "supporting_data/",
               "DESCRIPTION"),
  appName = "scenario-comparison-app-dev",
  appTitle = "Scenario Comparison App (dev)",
  appId = 213,
  server = "connect.strategyunitwm.nhs.uk",
  envVars = c(
    "AZ_STORAGE_EP",
    "AZ_STORAGE_CONTAINER_RESULTS",
    "AZ_STORAGE_CONTAINER_SUPPORT",
    "NHP_ENCRYPT_KEY",
    "FEEDBACK_FORM_URL"
  ),
  lint = FALSE,
  forceUpdate = TRUE
)
