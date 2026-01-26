
# we create an alternative version of `mod_principal_change_factor_effects_summarised` 
# from `nhp_outputs` where we have an additional grouping on scenario (diffs highlighted)
mod_principal_change_factor_effects_summarised_grouped <- function(data, measure, include_baseline) {
  data <- data |>
    dplyr::filter(
      .data$measure == .env$measure,
      include_baseline | .data$change_factor != "baseline",
      .data$value != 0
    ) |>
    tidyr::drop_na("value") |>
    dplyr::mutate(
      dplyr::across(
        "change_factor",
        \(.x) forcats::fct_reorder(.x, -.data$value)
      ),
      # baseline may now not be the first item, move it back to start
      dplyr::across(
        "change_factor",
        \(.x) forcats::fct_relevel(.x, "baseline")
      )
    )
  
  
  cfs <- data |>
    # DIFF: have to group by scenario as well as change factor
    dplyr::group_by(.data$scenario, .data$change_factor) |>
    dplyr::summarise(dplyr::across("value", \(.x) sum(.x, na.rm = TRUE))) |>
    dplyr::mutate(cuvalue = cumsum(.data$value)) |>
    dplyr::mutate(
      hidden = tidyr::replace_na(dplyr::lag(.data$cuvalue) + pmin(.data$value, 0), 0),
      colour = dplyr::case_when(
        .data$change_factor == "Baseline" ~ "#686f73",
        .data$value >= 0 ~ "#f9bf07",
        TRUE ~ "#2c2825"
      ),
      dplyr::across("value", abs)
    ) |>
    dplyr::select(-"cuvalue")
  
  
  levels <- unique(c("baseline", levels(forcats::fct_drop(cfs$change_factor)), "Estimate"))
  if (!include_baseline) {
    levels <- levels[-1]
  }
  
  
  cfs |>
    # DIFF: have to calculate the estimates for both scenarios
    dplyr::bind_rows(
      dplyr::tibble(
        scenario = "scenario_1",
        change_factor = "Estimate",
        value = sum(data$value[data$scenario=="scenario_1"]),
        hidden = 0,
        colour = "#ec6555"
      ),
      dplyr::tibble(
        scenario = "scenario_2",
        change_factor = "Estimate",
        value = sum(data$value[data$scenario=="scenario_2"]),
        hidden = 0,
        colour = "#ec6555"
      )
    ) |>
    tidyr::pivot_longer(c("value", "hidden")) |>
    dplyr::mutate(
      dplyr::across("colour", \(.x) ifelse(.data$name == "hidden", NA, .x)),
      dplyr::across("name", \(.x) forcats::fct_relevel(.x, "hidden", "value")),
      dplyr::across("change_factor", \(.x) factor(.x, rev(levels)))
    ) |> 
    #diff: ungroup
    dplyr::ungroup()
}

generate_waterfall_plot <- function(
    pcfs_1, 
    pcfs_2,
    scn1_name = NULL,
    scn2_name = NULL,
    activity_type,
    measure = "measure",
    title = "Title", 
    x_label = "X-axis", 
    y_label = "Y-axis") {
  
  # Combine the specified IP columns from both data frames
  pcfs <- dplyr::bind_rows(
    scenario_1 = pcfs_1[[activity_type]],
    scenario_2 = pcfs_2[[activity_type]],
    .id = "scenario"
  )
  
  # Apply the summarization function to the combined data
  activity <- mod_principal_change_factor_effects_summarised_grouped(
    data = pcfs,
    measure = measure,
    include_baseline = TRUE
  ) |> 
    dplyr::mutate(scenario = dplyr::case_when(scenario == "scenario_1" ~ scn1_name,
                                              scenario == "scenario_2" ~ scn2_name,
                                              T ~ "Missing Scenario Name"))
  
  # Generate the plot and customize it for better aesthetics
  plot <- mod_principal_change_factor_effects_cf_plot(activity) +
    ggplot2::ggtitle(title) +
    ggplot2::xlab(x_label) +
    ggplot2::ylab(y_label)
  
  return(plot)
}

impact_bar_plot <- function(data, chosen_change_factor,chosen_activity_type, chosen_measure, title_text = "Example") {
  ggplot2::ggplot(data |>
           dplyr::filter(change_factor==chosen_change_factor,
                         activity_type==chosen_activity_type, 
                         chosen_measure==measure,
                         value != 0.00) |> 
           dplyr::mutate(mitigator_name = dplyr::case_when(strategy == "convert_to_tele" ~ strategy,
                                                           T ~ mitigator_name)) |> 
           dplyr::filter(strategy != "activity_avoidance_interaction_term"),
         ggplot2::aes(x=value, y=stats::reorder(mitigator_name,dplyr::desc(value)), fill = id)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::scale_x_continuous(labels = scales::comma) +
    ggplot2::ggtitle(title_text) +
    ggplot2::ylab("TPMA") +
    ggplot2::xlab(get_label(chosen_measure, measure_pretty_names)) +
    ggplot2::scale_fill_manual(values = c("#f9bf07","#686f73"), 
                               name="Scenario",
                               labels = get_label_map(data)
    ) +
    ggeasy::easy_center_title() + ggplot2::theme(text = ggplot2::element_text(family = "Segoe UI")) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(family = "Segoe UI", size = 12, color="black")) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(family = "Segoe UI", size = 12, color="black")) +
    ggplot2::theme(axis.title.x = ggplot2::element_text(family = "Segoe UI", size = 12, color="black")) +
    ggplot2::theme(axis.title.y = ggplot2::element_text(family = "Segoe UI", size = 12, color="black")) +
    ggplot2::theme(legend.title = ggplot2::element_text(family = "Segoe UI", size = 12, color="black")) +
    ggplot2::theme(legend.text = ggtext::element_markdown(family = "Segoe UI",
                                                          size = 12,
                                                          color = "black",
                                                          hjust = 0.5,
                                                          lineheight = 1.5),
                   legend.position = "bottom",
                   legend.box = "vertical",
                   legend.box.just = "center",
                   plot.title.position = "plot" 
                   )#+
    #ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2, byrow = TRUE))
}

#
