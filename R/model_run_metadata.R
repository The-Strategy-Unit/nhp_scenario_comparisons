get_metadata <- function(result_sets, scenario) {
  
  
  metadata <- scenario
  
  
  # Generate encrypted bit of the outputs app URL
  metadata$url_file_encrypted <- metadata$file |>
    purrr::map(encrypt_filename) |>
    unlist()
  
  
  metadata |>
    dplyr::mutate(
      create_datetime = create_datetime |>
        lubridate::as_datetime() |>
        format("%Y-%m-%d %H:%M:%S") |>
        as.character(),
      url_app_version = stringr::str_replace(app_version, "\\.", "-"),
      url_stub = glue::glue(
        "https://connect.strategyunitwm.nhs.uk/nhp/{url_app_version}/outputs/?"
      ),
      outputs_link = glue::glue("{url_stub}{url_file_encrypted}")
    ) |> 
    dplyr::mutate(
      outputs_app = glue::glue(
        "<a href='{outputs_link}' target='_blank'>Launch</a> \U1F517"
      ),
      .before = outputs_link
    ) |> 
    dplyr::select(
      user, dataset, scenario, seed, model_runs, start_year, end_year, app_version, create_datetime, outputs_app
    )
}

encrypt_filename <- function(
    filename,
    key_b64 = Sys.getenv("NHP_ENCRYPT_KEY")
) {
  
  key <- openssl::base64_decode(key_b64)
  
  f <- charToRaw(filename)
  
  ct <- openssl::aes_cbc_encrypt(f, key, NULL)
  hm <- as.raw(openssl::sha256(ct, key))
  
  openssl::base64_encode(c(hm, ct)) |>
    # Connect does something weird if it encounters strings of the form /w==,
    # where / can be any special character.
    URLencode(reserved = TRUE)
  
}


