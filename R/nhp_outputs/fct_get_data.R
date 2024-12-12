parse_results <- function(r) {

  r$population_variants <- as.character(r$population_variants)

  r$results <- purrr::map(
    r$results,
    purrr::map_dfr,
    purrr::modify_at,
    c("model_runs", "time_profiles"),
    purrr::compose(list, as.numeric)
  )

  # Various patches need to happen based on the model version (this logic is
  # required in the nhp_final_reports repo because we need to handle results
  # from all possible model versions, whereas the main branch of nhp_outputs
  # needs only to handle the latest model version)
  model_version <- r$params$app_version

  # If model >v1.2 then results should be fully patched
  needs_patch <- !(model_version %in% c("v1.0", "v1.1", "v1.2"))
  if (needs_patch) r <- patch_results(r)

  # If v1.2, then we only need to patch the tretspef_raw and tretspef+los_group
  needs_tretspef_patch <- model_version == "v1.2"
  if (needs_tretspef_patch) r$results <- patch_tretspef(r$results, "v1.2")

  r

}

patch_results <- function(r) {
  r$results <- purrr::imap(r$results, patch_principal)
  r$results <- patch_step_counts(r$results)
  r$results <- patch_tretspef(r$results, r$params$app_version)
  r
}

patch_tretspef <- function(results, model_version) {

  results[["tretspef_raw"]] <- dplyr::bind_rows(
    results[["tretspef_raw"]],
    results[["tretspef_raw+los_group"]] |>
      dplyr::summarise(
        .by = c("measure", "pod", "tretspef_raw", "sitetret"),
        dplyr::across(
          c("baseline", "principal", "lwr_ci", "median", "upr_ci"),
          sum
        ),
        dplyr::across("time_profiles", \(.x) list(purrr::reduce(.x, `+`)))
      )
  )

  # More granular LoS groups were introduced with model version v3.0
  los_groups <- c(
    "0 days",
    "1 day",
    "2 days",
    "3 days",
    "4-7 days",
    "8-14 days",
    "15-21 days",
    "22+ days"
  )

  # Use less granular groupings for scenarios prior to  model v3.0
  if (model_version %in% c("v1.0", "v1.1", "v1.2", "v2.0", "v2.1", "v2.2")) {
    los_groups <- c("0-day", "1-7 days", "8-14 days", "15-21 days", "22+ days")
  }

  results[["tretspef_raw+los_group"]] <- results[["tretspef_raw+los_group"]] |>
    dplyr::mutate(
      dplyr::across(
        "los_group",
        \(.x) forcats::fct_relevel(.x, los_groups)
      )
    ) |>
    dplyr::arrange(.data$pod, .data$measure, .data$sitetret, .data$los_group)

  results

}

patch_principal <- function(results, name) {
  if (name == "step_counts") {
    return(patch_principal_step_counts(results))
  }

  dplyr::mutate(
    results,
    principal = purrr::map_dbl(.data[["model_runs"]], mean),
    median = purrr::map_dbl(.data[["model_runs"]], quantile, 0.5),
    lwr_ci = purrr::map_dbl(.data[["model_runs"]], quantile, 0.1),
    upr_ci = purrr::map_dbl(.data[["model_runs"]], quantile, 0.9)
  )
}

patch_principal_step_counts <- function(results) {
  dplyr::mutate(
    results,
    value = purrr::map_dbl(.data[["model_runs"]], mean)
  )
}

patch_step_counts <- function(results) {
  if (!"strategy" %in% colnames(results$step_counts)) {
    results$step_counts <- dplyr::mutate(
      results$step_counts,
      strategy = NA_character_,
      .after = "change_factor"
    )
  }
  results
}

get_principal_high_level <- function(r, measures, sites) {
  r$results$default |>
    dplyr::filter(.data$measure %in% measures) |>
    dplyr::select("pod", "sitetret", "baseline", "principal") |>
    dplyr::mutate(dplyr::across("pod", ~ ifelse(
      stringr::str_starts(.x, "aae"), "aae", .x
    ))) |>
    dplyr::group_by(.data$pod, .data$sitetret) |>
    dplyr::summarise(dplyr::across(where(is.numeric), sum), .groups = "drop") |>
    trust_site_aggregation(sites)
}

get_variants <- function(r) {
  r$population_variants |>
    utils::tail(-1) |>
    tibble::enframe("model_run", "variant")
}

get_model_run_distribution <- function(r, pod, measure, site_codes) {
  filtered_results <- r$results$default |>
    dplyr::filter(
      .data$pod %in% .env$pod,
      .data$measure %in% .env$measure
    ) |>
    dplyr::select("sitetret", "baseline", "principal", "model_runs")

  if (nrow(filtered_results) == 0) {
    return(NULL)
  }

  filtered_results |>
    dplyr::mutate(
      dplyr::across(
        "model_runs",
        \(.x) purrr::map(.x, tibble::enframe, name = "model_run")
      )
    ) |>
    tidyr::unnest("model_runs") |>
    dplyr::inner_join(get_variants(r), by = "model_run") |>
    trust_site_aggregation(site_codes)
}

get_principal_change_factors <- function(r, activity_type, sites) {
  stopifnot(
    "Invalid activity_type" = activity_type %in% c("aae", "ip", "op")
  )

  r$results$step_counts |>
    dplyr::filter(.data$activity_type == .env$activity_type) |>
    dplyr::select(-where(is.list)) |>
    dplyr::mutate(dplyr::across("strategy", \(.x) tidyr::replace_na(.x, "-"))) |>
    trust_site_aggregation(sites)
}

get_bed_occupancy <- function(r) {
  r$results$bed_occupancy |>
    dplyr::select(
      "measure",
      "quarter",
      "ward_type",
      "ward_group",
      "baseline",
      "principal",
      "median",
      "lwr_ci",
      "upr_ci",
      "model_runs"
    ) |>
    dplyr::mutate(
      dplyr::across(
        "model_runs",
        \(.x) purrr::map(.x, tibble::enframe, name = "model_run")
      )
    ) |>
    tidyr::unnest("model_runs") |>
    dplyr::inner_join(get_variants(r), by = "model_run")
}

trust_site_aggregation <- function(data, sites) {
  data_filtered <- if (length(sites) == 0) {
    data
  } else {
    dplyr::filter(data, .data$sitetret %in% sites)
  }

  data_filtered |>
    dplyr::group_by(
      dplyr::across(
        c(
          tidyselect::where(is.character),
          tidyselect::where(is.factor),
          tidyselect::any_of(c("model_run", "year")),
          -"sitetret"
        )
      )
    ) |>
    dplyr::summarise(
      dplyr::across(where(is.numeric), \(.x) sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )
}
