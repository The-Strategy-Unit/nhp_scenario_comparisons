get_metadata <- function(result_sets, scenario) {
  metadata <- scenario

  metadata |>
    dplyr::mutate(
      create_datetime = create_datetime |>
        lubridate::as_datetime() |>
        format("%Y-%m-%d %H:%M:%S") |>
        as.character(),
      url_app_version = stringr::str_replace(app_version, "\\.", "-"),
      url_stub = glue::glue(
        "https://connect.strategyunitwm.nhs.uk/nhp/{url_app_version}/outputs/?"
      ),
      outputs_link = glue::glue("{url_stub}{outputs_app_uri}")
    ) |>
    dplyr::mutate(
      outputs_app = glue::glue(
        "<a href='{outputs_link}' target='_blank'>Launch</a> \U1F517"
      )
    ) |>
    dplyr::select(
      user,
      dataset,
      scenario,
      seed,
      model_runs,
      start_year,
      end_year,
      app_version,
      create_datetime,
      outputs_app
    )
}
