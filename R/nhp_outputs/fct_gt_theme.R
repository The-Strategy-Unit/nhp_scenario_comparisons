gt_theme <- function(data) {
  gt::tab_options(
    data = data,
    heading.subtitle.font.size = 12,
    heading.align = "left",
    column_labels.font.weight = "bold",
    row_group.border.top.width = gt::px(2),
    row_group.border.top.color = "black",
    row_group.border.bottom.color = "black",
    row_group.background.color = "#686f73",
    table_body.hlines.color = "white",
    table.border.top.color = "white",
    table.border.top.width = gt::px(2),
    table.border.bottom.color = "white",
    table.border.bottom.width = gt::px(3),
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = gt::px(1),
    summary_row.background.color = "#b2b7b9",
    grand_summary_row.background.color = "#343739"
  )
}

gt_theme_param_diffs <- function(data) {
  gt::tab_options(
    data = data,
    table.font.names = "Arial",
    table.font.size = 14,
    table.align = "left",
    row_group.background.color = "grey90"
  )
}
