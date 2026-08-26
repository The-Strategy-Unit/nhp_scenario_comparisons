read_azure_results <- function(results_dir) {
  token <- azkit::get_auth_token()
  if (!token$validate()) {
    token$refresh()
  }
  results_container_name <- Sys.getenv("AZ_STORAGE_CONTAINER_RESULTS")
  results_cont <- azkit::get_container(results_container_name, token = token)

  app_version <- sub("^aggregated-model-results/([^/]+).*$", "\\1", results_dir)
  tables <- c("default", "step_counts", get_tx_table_name(app_version))
  reskit::read_results_parquet_files(results_cont, results_dir, tables) |>
    shim_results(app_version)
}


get_tx_table_name <- function(app_version) {
  if (grepl("^v3\\.", app_version)) {
    "tretspef_raw+los_group"
  } else {
    "tretspef+los_group"
  }
}

shim_results <- function(results, app_version) {
  if (grepl("^v3\\.", app_version)) {
    results |>
      rlang::set_names(\(x) sub("^tretspef_raw", "tretspef", x)) |>
      purrr::modify_in("tretspef+los_group", \(x) {
        dplyr::rename(x, tretspef = "tretspef_raw")
      })
  } else {
    results
  }
}


get_results_metadata <- function(allowed_datasets) {
  token <- azkit::get_auth_token()
  if (!token$validate()) {
    token$refresh()
  }
  # fmt: skip
  table_cols <- c(
    "dataset", "scenario", "seed", "model_runs", "start_year", "end_year",
    "app_version", "create_datetime",
    "viewable", "run_stage", "aggregated_results_path", "outputs_app_uri"
  )
  select_cols <- paste0(table_cols, collapse = ",")

  azkit::read_azure_table(
    Sys.getenv("AZ_TABLE_NAME"),
    token = token,
    filter = "status eq 'complete' and aggregated_results_path ne ''",
    select = select_cols
  ) |>
    dplyr::filter(
      dplyr::if_any("dataset", \(x) x %in% allowed_datasets),
      # version comparison only valid until v9!
      dplyr::if_any("app_version", \(x) x >= "v3.1" | x == "dev")
    ) |>
    error_on_zero_rows() |>
    dplyr::select(tidyselect::all_of(table_cols)) |>
    dplyr::mutate(dplyr::across("create_datetime", tidy_dttm))
}


get_user_allowed_datasets <- function(groups = NULL) {
  groups <- groups %||% "nhp_devs"
  codes <- names(yyjsonr::read_json_file(appfile("datasets.json")))
  if (any(c("nhp_devs", "nhp_power_users") %in% groups)) {
    codes
  } else {
    nhp_stub <- "^nhp_(national|icb|provider)_"
    allowed <- sub(nhp_stub, "", grepv(nhp_stub, groups))
    c("synthetic", intersect(codes, allowed))
  }
}


add_outputs_app_link <- function(results_metadata_tbl) {
  connect_url <- "https://connect.strategyunitwm.nhs.uk"
  t <- "target='_blank'"
  # fmt: skip
  remove_cols <- c(
    "url_app_version", "outputs_url", "outputs_app_uri", "viewable", "run_stage", "aggregated_results_path"
  )
  results_metadata_tbl |>
    dplyr::mutate(
      url_app_version = gsub("\\.", "-", .data[["app_version"]]),
      outputs_url = glue::glue("{connect_url}/nhp/{url_app_version}"),
      outputs_url = glue::glue("{outputs_url}/outputs/?{outputs_app_uri}"),
      outputs_app = glue::glue("<a href='{outputs_url}' {t}>Launch</a> \U1F517")
    ) |>
    dplyr::select(!tidyselect::all_of(remove_cols))
}
