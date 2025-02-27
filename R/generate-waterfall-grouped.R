
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
    activity_type,
    measure = "measure",
    title = "Title", 
    x_label = "X-axis", 
    y_label = "Y-axis") {
  
  # Combine the specified IP columns from both data frames
  pcfs <- bind_rows(
    scenario_1 = pcfs_1[[activity_type]],
    scenario_2 = pcfs_2[[activity_type]],
    .id = "scenario"
  )
  
  # Apply the summarization function to the combined data
  activity <- mod_principal_change_factor_effects_summarised_grouped(
    data = pcfs,
    measure = measure,
    include_baseline = TRUE
  )
  
  # Generate the plot and customize it for better aesthetics
  plot <- mod_principal_change_factor_effects_cf_plot(activity) +
    ggtitle(title) +
    xlab(x_label) +
    ylab(y_label)
  
  return(plot)
}

impact_bar_plot <- function(data, chosen_change_factor,chosen_activity_type, chosen_measure, title_text = "Example") {
  ggplot(filter(data, change_factor==chosen_change_factor,activity_type==chosen_activity_type, chosen_measure==measure,value != 0.00),
         aes(x=value, y=reorder(mitigator_name,desc(value)), fill = scenario)) +
    geom_col(position = "dodge") +
    scale_x_continuous(labels = scales::comma) +
    ggtitle(title_text) +
    ylab("Point of delivery") +
    xlab(chosen_measure) +
    scale_fill_manual(values = c("#f9bf07","#686f73"), name="Scenario", labels = c(scenario_1_name, scenario_2_name)) +
    easy_center_title() + theme(text = element_text(family = "Segoe UI")) +
    theme(axis.text.x = element_text(family = "Segoe UI", size = 12, color="black")) +
    theme(axis.text.y = element_text(family = "Segoe UI", size = 12, color="black")) +
    theme(axis.title.x = element_text(family = "Segoe UI", size = 12, color="black")) +
    theme(axis.title.y = element_text(family = "Segoe UI", size = 12, color="black")) +
    theme(legend.title = element_text(family = "Segoe UI", size = 12, color="black")) +
    theme(legend.text = element_text(family = "Segoe UI", size = 12, color="black"))
}

#
