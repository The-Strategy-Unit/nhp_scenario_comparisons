write_all_figures <- function(r_ndg1, r_ndg2, site_codes, fig_dir) {

  # Set up image dimensions (inches, because that's what {officer} deals with)
  w <- 6.5
  h <- 4
  h_dist <- 2.5  # distribution plots (beeswarm and s-curves)

  # Expat/repat figures (unnumbered)

  possibly_info_params_table_expat_repat_adjustment <-
    purrr::possibly(info_params_table_expat_repat_adjustment)

  types <- list("expat", "repat_local", "repat_nonlocal")
  params <- r_ndg2[["params"]]

  expat_repat_tables <- purrr::map(
    types,
    \(type) possibly_info_params_table_expat_repat_adjustment(params, type)
  ) |>
    purrr::set_names(types)

  purrr::walk2(
    expat_repat_tables,
    names(expat_repat_tables),
    \(table, type) {
      path <- glue::glue(fig_dir, "/figure_{type}.png")
      gt::gtsave(table, path)
      cli::cli_text("- Wrote to {path}")
    }
  )

  # Figures 8.2, 8.3, 8.4, 8.5, 8.6

  plots_pcf <- prepare_all_principal_change_factors_plots(r_ndg2, site_codes)

  purrr::walk2(
    paste0(fig_dir, "/figure_8.", 2:6, ".png"),
    plots_pcf,
    \(filename, plot) {
      ggplot2::ggsave(filename, plot, width = w, height = h)
      cli::cli_text("- Wrote to {filename}")
    }
  )

  # Figures 9.1, 9.2, 9.6, 9.7, 9.8, 9.9

  plots_activity_distribution <-
    prepare_all_activity_distribution_plots(r_ndg2, site_codes)

  purrr::walk2(
    paste0(fig_dir, "/figure_9.", c(1:2, 6:9), ".png"),
    plots_activity_distribution,
    \(filename, plot) {
      ggplot2::ggsave(filename, plot, width = w, height = h_dist)
      cli::cli_text("- Wrote to {filename}")
    }
  )

  # Figure 9.3
  path_9.3 <- paste0(fig_dir, "/figure_9.3.png")
  mod_principal_summary_data(
    r_ndg2,
    site_codes$ip  # fig is IP only; we can ignore other site-code groupings
  ) |>
    dplyr::filter(activity_type == "Inpatient") |>
    mod_principal_summary_table() |>
    gt::tab_options(table.align = "left") |>
    gt::gtsave(path_9.3)
  cli::cli_text("- Wrote to {path_9.3}")

  # Figure 9.4
  path_9.4 <- paste0(fig_dir, "/figure_9.4.png")
  prepare_all_principal_change_factors(r_ndg2, site_codes) |>
    _$ip |>  # only need inpatients
    mod_principal_change_factor_effects_summarised("beddays", TRUE) |>
    mod_principal_change_factor_effects_cf_plot() |>
    ggplot2::ggsave(
      filename = path_9.4,
      width = w,
      height = h
    )
  cli::cli_text("- Wrote to {path_9.4}")

  # Figure 9.5
  path_9.5 <- paste0(fig_dir, "/figure_9.5.png")
  mod_principal_summary_los_data(r_ndg2, site_codes[["ip"]], "beddays") |>
    dplyr::mutate(pod_name = stringr::str_replace(pod_name, "Admission", "Bed Days")) |>
    dplyr::arrange(pod_name, los_group) |>
    mod_principal_summary_los_table() |>
    gt::tab_options(table.align = "left") |>
    gt::gtsave(path_9.5)
  cli::cli_text("- Wrote to {path_9.5}")

  # Figures 9.10, 9.11 (NDG variant 1)

  plots_activity_distribution <-
    prepare_all_activity_distribution_plots(r_ndg1, site_codes)  # Note NDG variant 1

  purrr::walk2(
    paste0(fig_dir, "/figure_9.", 10:11, ".png"),
    plots_activity_distribution[1:2],  # only need first two
    \(filename, plot) {
      ggplot2::ggsave(
        filename,
        plot,
        width = w,
        height = h_dist
      )
      cli::cli_text("- Wrote to {filename}")
    }
  )

}

prepare_all_principal_change_factors <- function(
    r,
    site_codes = list(ip = NULL,  op = NULL, aae = NULL)  # character vectors
) {

  mitigators_lookup <- read_mitigators()
  atmpo_lookup <- read_atmpo()

  activity_types_long <- list("inpatients", "outpatients", "aae")
  activity_types_short <- list("ip", "op", "aae")

  pods <- purrr::map(
    activity_types_long,
    \(x) {
      atmpo_lookup |>
        dplyr::filter(activity_type == x) |>
        dplyr::pull(pod) |>
        unique()
    }
  ) |>
    purrr::set_names(activity_types_short)

  possibly_prep_principal_change_factors <-
    purrr::possibly(prep_principal_change_factors)

  principal_change_data <- purrr::map2(
    activity_types_short,
    pods,
    \(activity_type, pod) {
      possibly_prep_principal_change_factors(
        data = r,
        site_codes = site_codes,
        mitigators = mitigators_lookup,
        at = activity_type,
        pods = pod
      )
    }
  ) |>
    purrr::set_names(activity_types_short)

  # Scenarios run under v1.0 don't have A&E data when filtering by site, so
  # provide results for whole-scheme level.
  if (r$params$app_version == "v1.0" & !is.null(site_codes[["aae"]])) {
    principal_change_data[["aae"]] <-
      possibly_prep_principal_change_factors(
        data = r,
        site_codes = list(aae = NULL),  # provide for whole scheme, not site
        mitigators = mitigators_lookup,
        at = "aae",
        pods = pods[["aae"]]
      )
  }

  principal_change_data

}

prepare_all_principal_change_factors_plots <- function(
    r,
    site_codes = list(ip = NULL,  op = NULL, aae = NULL),  # character vectors
    pcf
) {

  pcf_data <- prepare_all_principal_change_factors(r, site_codes)

  dats <- list(pcf_data$ip, pcf_data$ip, pcf_data$ip, pcf_data$op, pcf_data$aae)
  measures <- list("admissions", "beddays", "beddays", "attendances", "arrivals")
  change_factors <- list(
    "activity_avoidance",
    "activity_avoidance",
    "efficiencies",
    "activity_avoidance",
    "activity_avoidance"
  )

  possibly_plot_individual_change_factors <-
    purrr::possibly(plot_individual_change_factors)

  purrr::pmap(
    list(dats, measures, change_factors),
    \(dat, measure, change_factor) {
      dat |>
        possibly_plot_individual_change_factors(
          measure = measure,
          change_factor = change_factor
        )
    }
  )

}

prepare_all_activity_distribution_plots <- function(r, site_codes) {

  atpmo_lookup <- read_atmpo()

  activity_types <- list("inpatients", "outpatients", "aae")

  pods <- purrr::map(
    activity_types,
    \(x) {
      atpmo_lookup |>
        dplyr::filter(activity_type == x) |>
        dplyr::pull(pod) |>
        unique()
    }
  ) |>
    purrr::map(list) |>
    purrr::set_names(activity_types)

  measures <- purrr::map(
    activity_types,
    \(x) {
      atpmo_lookup |>
        dplyr::filter(activity_type == x) |>
        dplyr::pull(measure) |>
        unique()
    }
  ) |>
    purrr::map(list) |>
    purrr::set_names(activity_types)

  measures[["inpatients"]] <- "beddays"  # admissions not shown in report

  possibly_plot_activity_distributions <-
    purrr::possibly(plot_activity_distributions)

  activity_distribution_plots <- purrr::pmap(
    list(activity_types, pods, measures),
    \(activity_type, pod, measure) {
      possibly_plot_activity_distributions(
        data = r,
        site_codes = site_codes,
        activity_type = activity_type,
        pod = pod,
        measure = measure
      )
    }
  ) |>
    purrr::set_names(activity_types)

  # Earlier model runs don't have A&E data when filtering by site, so provide
  # data for whole-scheme level.
  if (r$params$app_version == "v1.0" & !is.null(site_codes[["aae"]])) {
    activity_distribution_plots[["aae"]] <-
      possibly_plot_activity_distributions(
        data = r,
        site_codes = list(aae = NULL),  # provide for whole scheme, not site
        activity_type = "aae",
        pod = pods[["aae"]],
        measure = measures[["aae"]]
      )
  }

  activity_distribution_plots |> purrr::list_flatten()

}

read_mitigators <- function(remove_codes = TRUE) {

  mitigators <- "data/mitigators.json" |>
    jsonlite::read_json(simplifyVector = TRUE) |>
    purrr::simplify() |>
    tibble::enframe("strategy", "mitigator_name")

  if (remove_codes) {
    mitigators = mitigators |>
      dplyr::mutate(
        mitigator_name = stringr::str_remove(
          mitigator_name,
          " \\(\\w{2}-\\w{2}-\\d{3}\\)"
        )
      )
  }

  mitigators

}

read_atmpo <- function() {
  get_activity_type_pod_measure_options() |>
    dplyr::select("activity_type", "pod", "measure" = "measures") |>
    dplyr::mutate(
      activity_type = dplyr::case_match(
        activity_type,
        "ip"  ~ "inpatients",
        "op"  ~ "outpatients",
        "aae" ~ "aae"
      )
    )
}


# new function to get pod into the dataset
prepare_all_principal_change_factors_get_pod <- function(
    r,
    site_codes = list(ip = NULL,  op = NULL, aae = NULL)  # character vectors
) {
  
  mitigators_lookup <- read_mitigators()
  atmpo_lookup <- read_atmpo()
  
  activity_types_long <- list("inpatients", "outpatients", "aae")
  activity_types_short <- list("ip", "op", "aae")
  
  pods <- purrr::map(
    activity_types_long,
    \(x) {
      atmpo_lookup |>
        dplyr::filter(activity_type == x) |>
        dplyr::pull(pod) |>
        unique()
    }
  ) |>
    purrr::set_names(activity_types_short)
  
  possibly_prep_principal_change_factors <-
    purrr::possibly(prep_principal_change_factors)
  
  principal_change_data <- purrr::map2(
    activity_types_short,
    pods,
    \(activity_type, pod) {
      add_pod <- possibly_prep_principal_change_factors(
        data = r,
        site_codes = site_codes,
        mitigators = mitigators_lookup,
        at = activity_type,
        pods = pod
      )
      
      # Ensure 'pod' is assigned correctly for each row based on the 'activity_type'
      if (!is.null(add_pod)) {
        # If there are multiple pods for the activity type, assign each row a corresponding pod
        pod <- rep(pod, length.out = nrow(add_pod))  # Repeating pod value for rows in result
        add_pod$pod <- pod  # Add the 'pod' column
      }
      
      add_pod
    }
  ) |>
    purrr::set_names(activity_types_short)
  
  # Scenarios run under v1.0 don't have A&E data when filtering by site, so
  # provide results for whole-scheme level.
  if (r$params$app_version == "v1.0" & !is.null(site_codes[["aae"]])) {
    principal_change_data[["aae"]] <-
      possibly_prep_principal_change_factors(
        data = r,
        site_codes = list(aae = NULL),  # provide for whole scheme, not site
        mitigators = mitigators_lookup,
        at = "aae",
        pods = pods[["aae"]]
      )
    # Add the pod column for the "aae" case
    if (!is.null(principal_change_data[["aae"]])) {
      pod_col <- rep(pods[["aae"]], length.out = nrow(principal_change_data[["aae"]]))
      principal_change_data[["aae"]]$pod <- pod_col
    }
  }
  
  principal_change_data
  
}

