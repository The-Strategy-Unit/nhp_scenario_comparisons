# This code originated from nhp_outputs
#' @importFrom zeallot %<-%
#' @importFrom rlang .data .env
#' @importFrom stats quantile
NULL

require_rows <- function(x) {
  shiny::req(x)
  shiny::req(nrow(x) > 0)
  x
}

utils::globalVariables(c(
  "where", # source: https://github.com/r-lib/tidyselect/issues/201#issuecomment-650547846
  "ds",
  "sc",
  "cd" # because of the use of %<-%
))
