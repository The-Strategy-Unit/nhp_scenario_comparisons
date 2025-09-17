parse_results <- function(r) {
  
  r$population_variants <- as.character(r$population_variants)
  
  r$results <- purrr::map(
    r$results,
    purrr::map_dfr,
    purrr::modify_at,
    c("model_runs", "time_profiles"),  # ignores time_profiles if non-existent
    purrr::compose(list, as.numeric)
  )
  
  # Various patches need to happen based on the model version (this logic is
  # required in the nhp_final_reports repo because we need to handle results
  # from all possible model versions, whereas the main branch of nhp_outputs
  # needs only to handle the latest model version).
  model_version <- r$params$app_version
  major_version <- extract_major_version(model_version)
  
  # The tretspef elements of the results object were renamed in v4.0, along with
  # some column names. It's easiest to just rename these in the results object,
  # regardless of app_version, and perform all wrangling on the basis of the new
  # names.
  if (major_version < 4) r <- rename_tretspef(r)
  
  # If model version is after v1.2 then results should be fully patched
  if (major_version >= 2) r <- patch_results(r)
  
  # If model version is v1.2, then we only need to patch the tretspef and
  # tretspef+los_group.
  if (model_version == "v1.2") r$results <- patch_tretspef(r$results, "v1.2")
  
  r
}

rename_tretspef <- function(r) {
  
  # Model v4.0 introduced renamed tretspef-related list-element and column names
  # to the results. Rename them.
  results <- r[["results"]]
  
  # Rename relevant list-elements
  names(results)[names(results) == "tretspef_raw+los_group"] <- "tretspef+los_group"
  names(results)[names(results) == "sex+tretspef"] <- "sex+tretspef_grouped"
  names(results)[names(results) == "tretspef_raw"] <- "tretspef"
  
  # Rename columns within renamed list elements (conditionally, because
  # tretspef_raw+los_group didn't exist in some earlier versions).
  
  has_tretspef_los <- !is.null(results[["tretspef+los_group"]])
  if (has_tretspef_los) {
    results[["tretspef+los_group"]] <- results[["tretspef+los_group"]] |>
      dplyr::rename("tretspef" = "tretspef_raw")
  }
  
  results[["sex+tretspef_grouped"]] <- results[["sex+tretspef_grouped"]] |>
    dplyr::rename("tretspef_grouped" = "tretspef")
  
  results[["tretspef"]] <- results[["tretspef"]] |>
    dplyr::rename("tretspef" = "tretspef_raw")
  
  # Overwrite results with new tretspef names
  r[["results"]] <- results
  r
  
}

patch_results <- function(r) {
  r$results <- purrr::imap(r$results, patch_principal)
  r$results <- patch_step_counts(r$results)
  r$results <- patch_tretspef(r$results, r$params$app_version)
  r
}

patch_tretspef <- function(results, model_version) {
  
  # Assumes tretspef elements have been renamed given changes made in model
  # v4.0, i.e. rename_tretspef() has been applied to incoming results.
  
  results[["tretspef"]] <- dplyr::bind_rows(
    results[["tretspef"]],
    results[["tretspef+los_group"]] |>
      dplyr::summarise(
        .by = c("measure", "pod", "tretspef", "sitetret"),
        dplyr::across(
          c("baseline", "principal", "lwr_ci", "median", "upr_ci"),
          sum
        ),
        dplyr::across(
          tidyselect::any_of("time_profiles"),  # ignored if non-existent
          \(.x) list(purrr::reduce(.x, `+`))
        )
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
  if (extract_major_version(model_version) < 3) {
    los_groups <- c("0-day", "1-7 days", "8-14 days", "15-21 days", "22+ days")
  }
  
  results[["tretspef+los_group"]] <- results[["tretspef+los_group"]] |>
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

extract_major_version <- function(version_string) {
  
  is_correct_format <- stringr::str_detect(
    version_string,
    "^v\\d{1,}\\.\\d{1,}$"  # e.g. 'v3.6'
  )
  
  if (!is_correct_format) {
    stop(
      glue::glue(
        "App version '{version_string}' seems to be in the wrong format."
      )
    )
  }
  
  version_string |>
    stringr::str_remove("v") |>
    as.numeric() |>
    floor()
  
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
    trust_site_aggregation(site_codes) |> 
    dplyr::mutate(scenario = as.factor(r$params$model_runs),
                  pod_measure = paste(.env$pod, .env$measure, sep = "_"))
}

get_aggregation <- function(r, pod, measure, agg_col, sites) {
  agg_type <- agg_col
  
  
  if (agg_col != "tretspef") {
    agg_type <- glue::glue("sex+{agg_col}")
  }
  
  
  filtered_results <- r$results[[agg_type]] |>
    dplyr::filter(
      .data$pod %in% .env$pod,
      .data$measure == .env$measure
    ) |>
    dplyr::select(-"pod", -"measure")
  
  
  if (nrow(filtered_results) == 0) {
    return(NULL)
  }
  
  
  filtered_results |>
    dplyr::mutate(
      dplyr::across(dplyr::matches("sex|tretspef"), as.character)
    ) |>
    trust_site_aggregation(sites)
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