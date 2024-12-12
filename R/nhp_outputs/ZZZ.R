#' @importFrom zeallot %<-%
NULL

require_rows <- function(x) {
  shiny::req(x)
  shiny::req(nrow(x) > 0)
  x
}
