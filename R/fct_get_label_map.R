# get_label_map() uses the unique scenario id to group data, while providing a 
# human-readable legend label. It also wraps long labels.
# get_label_map(...) should be supplied like this to 
# scale_*_manual(.., label = get_label_map(data)) where data is the data argument
# supplied to the plotting function
# id_col is the column name containing the unique "scenario+datetime"
# wrap_length can be used to adjust the character length for wrapping

get_label_map <- function(df, id_col = id, wrap_length = 50) {
  df |>
    dplyr::distinct({{id_col}}) |> 
    dplyr::mutate(name = stringr::str_replace_all(
      stringr::str_wrap(
        stringr::str_extract({{id_col}}, "^[^+]+"),
        width = wrap_length,
        whitespace_only = FALSE),
      "\\n",
      "<br>"
    ),
    
    name = dplyr::if_else(
      stringr::str_detect(name, "<br>", negate = TRUE) & 
        nchar(name) > wrap_length,
        stringr::str_replace_all(name, paste0("(.{", wrap_length, "})"), "\\1<br>"),
      name
    ),
    
    datetime = lubridate::ymd_hms(
      stringr::str_extract({{id_col}}, "\\d{8}_\\d{6}")),
    
    formatted_label = 
      paste0(
        "<b>", 
        name, 
        "</b><br>",
        "<span style='display:block; text-align:center;'>",
        "(", datetime, ")",
        "</span>"
      )
    )|> 
    dplyr::select({{id_col}}, formatted_label) |>
    tibble::deframe()
}
