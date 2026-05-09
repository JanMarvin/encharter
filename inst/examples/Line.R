# Line chart with circle markers and globally enabled data labels (show_val,
# show_cat = FALSE, position "t" for top of marker, bold size 9). The markers
# are styled with a white fill and a blue stroke for a "ringed point" look.
# Sheet name has a space, so cell refs use single quotes. Chart at E2:M20.

line_example <- function() {
  require(openxlsx2)
  require(encharter)

  chart <- ec("lineChart")

  chart$set_chart_title("Line with Dots and Labels")

  chart$set_data_label_style(
    show_val  = TRUE,
    show_cat  = FALSE,
    pos       = "t",
    bold      = TRUE,
    font_size = 9
  )

  chart$add_series(
    name        = "'Sheet 1'!$B$1",
    data        = "'Sheet 1'!$B$2:$B$5",
    label       = "'Sheet 1'!$A$2:$A$5",
    color       = "#0000FF",
    marker      = "circle",
    marker_size = 7,
    marker_fill = "#FFFFFF",
    marker_line = "#0000FF"
  )

  wb <- wb_workbook() |>
    wb_add_worksheet("Sheet 1") |>
    wb_add_data(x = data.frame(Label = c("A", "B", "C", "D"),
                               Val   = c(10, 25, 15, 30))) |>
    wb_add_encharter(dims = "E2:M20", graph = chart)

  if (interactive()) wb$open()
  invisible(wb)
}

line_example()
