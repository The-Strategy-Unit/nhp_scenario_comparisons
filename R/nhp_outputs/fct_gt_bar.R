gt_bar <- function(value, display_value_format = NULL, negative_colour = "#ec6555", positive_colour = "#f9bf07") {
  # find the range of value
  r <- range(value, na.rm = TRUE)
  # rescale the values:
  if (r[[1]] >= 0) {
    # if all of the values are positive, then rescale to be constrained to [0, max(value)]
    rvalue <- value / r[[2]]
    zero <- 0
  } else if (r[[2]] <= 0) {
    # if all of the values are negative, then rescale to be constrained to [min(value), 0]
    rvalue <- value / r[[1]]
    zero <- 0
  } else {
    # if we have both negative and positive values, rescale to be contrained to [min(value), max(value)]
    rvalue <- (value - r[[1]]) / diff(r)
    # work out zero in the rescaled domain
    zero <- -r[[1]] / diff(r)
  }
  # our bar consists of 2 parts: an empty bar, and the value bar
  # the empty bar will go from the far left to either then "end" of the negative bar, or the "start" of the positive bar
  bar_data <- tibble::tibble(
    ebar = ifelse(value > 0, zero, rvalue) * 50,
    vbar = ifelse(value <= 0, zero - rvalue, rvalue - zero) * 50,
    colour = ifelse(value <= 0, negative_colour, positive_colour)
  )

  # Choose to display percent of total
  # Begin Exclude Linting
  if (is.null(display_value_format)) {
    display_value <- "&nbsp;"
  } else {
    display_value <- display_value_format(value)
  }
  # End Exclude Linting

  # paste color and value into the html string
  glue::glue_data(
    bar_data,
    .sep = "\n",
    "<div>",
    "  <span style=\"display: inline-block; direction: ltr; border: 0; background-color: transparent; width: {ebar}%\">&nbsp;</span>", # nolint
    "  <span style=\"display: inline-block; direction: ltr; border: 0; background-color: {colour}; width: {vbar}%\">&nbsp;</span>", # nolint
    "  <span style=\"width: 50%\" align=\"right\">{display_value}</span>",
    "</div>"
  ) |>
    purrr::map(purrr::compose(gt::html, as.character))
}
