#' Generate a Report Folder and Populate with Content
#'
#' @param scheme_code Character. Three-digit ODS code.
#' @param site_codes List of three character vectors. To supply your own site
#'   codes, the three list elements should be named `ip`, `op` and `aae` and
#'   contain a vector of site codes (in the form `"XYZ01"`) or `NULL` to mean
#'   'all sites'. Defaults to `NULL`, which fetches the sites automatically from
#'   Azure.
#' @param result_sets A data.frame. Metadata of results files stored on Azure.
#' @param run_stages List of two character elements. The Azure run-stage tags
#'   for the scenarios that you want to fetch results for. Elements should be
#'   named `ndg1` and `ndg2`. Values will be like `"final_report_ndg1"`. You
#'   must supply this argument or `scenario_files`.
#' @param scenario_files List of two character elements.The path to the scenario
#'   files on Azure that you want to fetch results for. Elements should be named
#'   `ndg1` and `ndg2`. Values will be like `"<production folder>/<model
#'   version>/<scheme code>/scenario-ndg1.json.gz"`. You must supply this argument or
#'   `run_stages`. Defaults to `NULL`, which means it's ignored in favour of
#'   `run_stages`.
#' @param template_path Character. Location of the output report template.
#'   Defaults to `NULL`, which fetches the template from ShareoOint.
#'
#' @return Nothing. A populated folder structure in `outputs/`.
#'
#' @noRd
populate_template <- function(
    scheme_code,
    site_codes = NULL,
    result_sets = get_nhp_result_sets(),
    run_stages = list(ndg1 = "final_report_ndg1", ndg2 = "final_report_ndg2"),
    scenario_files = NULL,
    template_path = NULL
) {

  # Make sure one of run_stages or scenario_files is provided
  if (
    is.null(run_stages) & is.null(scenario_files) |
    !is.null(run_stages) & !is.null(scenario_files)
  ) {
    cli::cli_abort(c(
      "!" = "Provide one of {.arg run_stages} or {.arg scenario_files}.",
      "i" = "Set the other to NULL."
    ))
  }

  # Check that scenario_files are for the provided scheme_code by checking for
  # the scheme code in the filepath (excluding the scenario name)
  if (!is.null(scenario_files)) {

    code_matches_path <- scenario_files |>
      purrr::map(
        \(path) path |>
          stringr::str_remove(basename(path)) |>
          stringr::str_detect(scheme_code)
      ) |>
      unlist() |>
      all()

    if (!code_matches_path) {
      cli::cli_abort(c(
        "!" = "{.arg scenario_files} must contain paths for the given {.arg scheme_code}.",
        "i" = "Results for scheme 'XYZ' would be on a path like 'example/example/XYZ/example.json.gz'."
      ))
    }

  }

  datetime <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
  site_scheme <- make_scheme_name(scheme_code, as_filestring = TRUE)
  output_dir_name <- glue::glue("{datetime}_{site_scheme}")
  output_dir <- file.path("outputs", output_dir_name)
  if (!dir.exists(output_dir)) dir.create(output_dir)

  log_path <- file.path(output_dir, output_dir_name)
  logr::log_open(log_path, logdir = FALSE, show_notes = FALSE, compact = TRUE)

  schemes <- readr::read_csv("data/scheme-lookup.csv", show_col_types = FALSE)
  scheme_name <- dplyr::filter(schemes, scheme == scheme_code) |> dplyr::pull(hosp_site)
  logr::log_print(glue::glue("* Scheme: {scheme_code} ({scheme_name})"))
  logr::log_print(glue::glue("* Execution datetime: {datetime}"))

  if (!is.null(scenario_files)) {
    ndg1_file <- scenario_files[["ndg1"]]
    ndg2_file <- scenario_files[["ndg2"]]
  }
  if (!is.null(run_stages)) {
    meta <- get_run_metadata(scheme_code, result_sets, run_stages)
    ndg1_file <- dplyr::pull(meta$metadata_ndg1, file)
    ndg2_file <- dplyr::pull(meta$metadata_ndg2, file)
  }
  logr::log_print(glue::glue(
    "* Scenario files:\n",
    "- NDG1 variant: {ndg1_file}\n",
    "- NDG2 variant: {ndg2_file}"
  ))

  if (is.null(site_codes)) site_codes <- get_sites(scheme_code)
  ip_sites  <- if(is.null(site_codes$ip))  "all" else site_codes$ip
  op_sites  <- if(is.null(site_codes$op))  "all" else site_codes$op
  aae_sites <- if(is.null(site_codes$aae)) "all" else site_codes$aae
  logr::log_print(glue::glue(
    "* Sites:\n",
    "- Inpatients:  {ip_sites}\n",
    "- Outpatients: {op_sites}\n",
    "- A&E:         {aae_sites}"
  ))

  logr::log_print(glue::glue("* Fetching results..."))
  r_ndg1 <- get_nhp_results(file = ndg1_file)
  r_ndg2 <- get_nhp_results(file = ndg2_file)

  logr::log_print(glue::glue("* Reading report template..."))
  if (is.null(template_path)) docx <- read_template_docx()
  if (!is.null(template_path)) docx <- officer::read_docx(template_path)

  logr::log_print(glue::glue("* Calculating values for insertion to the template..."))
  values_list <- generate_values_list(r_ndg1, r_ndg2, site_codes)
  values_df <- values_list |> tibble::enframe(name = "item") |> tidyr::unnest(value)

  val_dir <- file.path(output_dir, "values")
  if (!dir.exists(val_dir)) dir.create(val_dir)
  values_path <- file.path(val_dir, glue::glue("{output_dir_name}_values.csv"))
  logr::log_print(glue::glue("* Writing values to {paste0(values_path, '/...')}"))
  readr::write_csv(values_df, values_path)

  logr::log_print(glue::glue("* Calculating check values for writing to csv..."))
  check_list <- generate_check_values(r_ndg1, r_ndg2, site_codes, char_out = TRUE,values_list)
  check_list_df <- check_list[-c(14,15,16)] |> tibble::enframe(name = "item") |> tidyr::unnest(value)
  mitigators_df <- check_list[c(14,15,16)] |> tibble::enframe(name = "item") |> tidyr::unnest(value)
  checklist_path <- file.path(val_dir, glue::glue("{output_dir_name}_checklist.csv"))
  mitigators_path <- file.path(val_dir, glue::glue("{output_dir_name}_mitigators.csv"))
  logr::log_print(glue::glue("* Writing checklist values to {paste0(checklist_path, '/...')}"))
  readr::write_csv(check_list_df, checklist_path)
  logr::log_print(glue::glue("* Writing mitigator values to {paste0(mitigators_path, '/...')}"))
  readr::write_csv(mitigators_df, mitigators_path)

  logr::log_print(glue::glue("* Setting values as custom properties in the template..."))
  docx2 <- officer::set_doc_properties(docx, values = values_list)

  fig_dir <- file.path(output_dir, "figures")
  if (!dir.exists(fig_dir)) dir.create(fig_dir)
  logr::log_print(glue::glue("* Writing figures to {paste0(fig_dir, '/...')}"))
  write_all_figures(r_ndg1, r_ndg2, site_codes, fig_dir)
  docx3 <- populate_template_with_figures(docx2, fig_dir)

  docx_out_path <- glue::glue("{output_dir}/{datetime}_{site_scheme}_outputs-report_draft.docx")
  logr::log_print(glue::glue("* Writing populated report to {docx_out_path}..."))
  print(docx3, target = docx_out_path)
  logr::log_print(glue::glue("* Done."))

  logr::log_close()

}

get_run_metadata <- function(scheme_code, result_sets, run_stages) {

  scheme_results <- result_sets |> dplyr::filter(dataset == scheme_code)

  metadata_ndg1 <- dplyr::filter(scheme_results, run_stage == run_stages[["ndg1"]])
  metadata_ndg2 <- dplyr::filter(scheme_results, run_stage == run_stages[["ndg2"]])

  dplyr::lst(metadata_ndg1, metadata_ndg2)

}

get_sites <- function(
    scheme_code,
    container_support = Sys.getenv("AZ_STORAGE_CONTAINER_SUPPORT"),
    sites_file = "nhp-final-report-sites.json"
) {

  container <- get_container(container_name = container_support)

  temp_file <- withr::local_tempfile(fileext = ".json")
  AzureStor::download_blob(container, sites_file, temp_file)
  sites <- jsonlite::read_json(temp_file, simplifyVector = TRUE)

  scheme_sites <- sites[names(sites) == scheme_code]

  if (length(scheme_sites) == 0) {
    stop(glue::glue(
      "No sites listed for {scheme_code} in {sites_file} in container {container_support}.",
      call. = FALSE
    ))
  }

  scheme_sites[[scheme_code]][["sites"]]  # list with elements 'aae', 'ip', 'op'

}

read_template_docx <- function(
    sharepoint_site = Sys.getenv("SP_SU_SITE"),
    template_path = Sys.getenv("SP_TEMPLATE_PATH")
) {
  site <- Microsoft365R::get_sharepoint_site(sharepoint_site)
  drv <- site$get_drive()
  tmp_docx <- tempfile(fileext = ".docx")
  drv$download_file(template_path, dest = tmp_docx)
  docx <- officer::read_docx(tmp_docx)
  unlink(tmp_docx)
  docx
}

populate_template_with_figures <- function(docx, fig_dir) {

  image_paths <- list.files(fig_dir, pattern = ".png$", full.names = TRUE)

  for (image_path in image_paths) {

    cursor_text <- image_path |>
      basename() |>
      stringr::str_replace("figure_", "[Insert Figure ") |>
      stringr::str_replace(".png", "]")

    img <- png::readPNG(image_path)
    img_dim <- (dim(img) / 300)[1:2] |> setNames(c("height", "width"))

    docx <- docx |>
      insert_figure_on_cursor(
        cursor_text = cursor_text,
        image_path = image_path,
        image_width = img_dim["width"],
        image_height = img_dim["height"]
      )

  }

  docx

}

insert_figure_on_cursor <- function(
    docx = docx,
    cursor_text,
    image_path,
    image_width = NULL,
    image_height = NULL
) {

  cursor_text_unbracketed <- cursor_text |>
    stringr::str_remove("\\[") |>
    stringr::str_remove("\\]")

  cursor_can_reach <- docx |>
    officer::cursor_reach_test(cursor_text_unbracketed)

  if (!cursor_can_reach) {
    stop("Cursor can't reach '", cursor_text, "'", call. = FALSE)
  }

  docx |>
    officer::cursor_reach(cursor_text, fixed = TRUE) |>
    officer::body_add_img(
      image_path,
      width = image_width,
      height = image_height,
      pos = "on"
    )

}
