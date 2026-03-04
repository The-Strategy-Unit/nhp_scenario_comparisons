deploy <- function(type = c("dev", "prod")) {
  type <- match.arg(type)
  
  # prod details
  app_id <- 303
  app_name <- "scenario-comparison-app"
  app_title <- "Scenario Comparison App"
  
  if (type == "dev") {
    app_id <- 213  # 
    app_name <- paste0(app_name, "-dev")
    app_title <- paste(app_title, "(dev)")
  }
  
  rsconnect::deployApp(
    appName = app_name,
    appTitle = app_title,
    server = "connect.strategyunitwm.nhs.uk",
    appId = app_id,
    appFiles = c(
      "app.R",
      "R/", 
      "inst/app",
      "supporting_data/",
      "DESCRIPTION"),
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
}

# Deploy development version between releases
deploy(type = "dev")

# Deploy on release
deploy(type = "prod")


