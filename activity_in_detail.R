library(jsonlite)
library(tidyverse)
library(dplyr)
library(gt)
library(here)
library(ggplot2)
library(ggeasy)

file_names <- list.files(path = 'R/nhp_outputs', pattern = "\\.R$")
lapply(paste0('R/nhp_outputs/',file_names), source)

# read Json ndg1
path_ndg1 <- "jsons/Imperialv1-ndg1-20241022_100916.json.gz"

result_ndg1 <- path_ndg1 |>
  jsonlite::read_json() |>
  parse_results()  # will apply necessary patches to the data

# load the tretspef lookup
tretspef_lookup <- jsonlite::read_json("data/tretspef_lookup.json",
                                       simplifyVector = TRUE
) |>
  dplyr::mutate(
    dplyr::across("Description", \(x) stringr::str_remove(x, " Service$")),
    dplyr::across("Description", \(x) paste0(.data$Code, ": ", .data$Description)),
  ) |>
  dplyr::select(-"Group") |>
  dplyr::add_row(Code = "&", Description = "Not known")  # as per HES dictionary 


# generating ip elective admissions by treatment specialty
generate_activity_in_detail_table(
  data = result_ndg1,
  sites = NULL,
  tretspefs = tretspef_lookup,
  activity_type = "ip",
  pod = "ip_elective_admission",
  measure = "admissions",
  agg_col ="tretspef")

# generating outpatient appointments by age group[]
generate_activity_in_detail_table(
  data = result_ndg1,
  sites = NULL,
  #tretspefs = "tretspefs",
  activity_type = "op",
  pod = "op_first",
  measure = "attendances",
  agg_col ="age_group")


