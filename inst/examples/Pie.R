# Pie chart with a viridis palette. Labels show both value and category and
# sit outside the pie (pos "outEnd"). Pie is rotated 300 degrees and exploded
# 10. Requires viridisLite. Chart at D2:L20.

pie_example <- function() {
  require(openxlsx2)
  require(encharter)

  for (p in "viridisLite") {
    if (!requireNamespace(p, quietly = TRUE))
      stop("install '", p, "' to run this example")
  }

  pie <- ec("pieChart")

  pie$set_chart_title("Market Share 2026")

  pie$set_data_label_style(
    show_val = TRUE,
    show_cat = TRUE,
    pos      = "outEnd"
  )

  pie$add_series(
    name  = "'Sheet 1'!$B$1",
    data  = "'Sheet 1'!$B$2:$B$5",
    label = "'Sheet 1'!$A$2:$A$5",
    color = wb_color(hex = viridisLite::viridis(5), format = "RGBA")
  )

  pie$set_pie_options(rotation = 300, expansion = 10)

  wb <- wb_workbook() |>
    wb_add_worksheet("Sheet 1") |>
    wb_add_data(x = data.frame(
      Product = c("Apples", "Bananas", "Cherries", "Dates"),
      Sales   = c(40, 30, 20, 10)
    )) |>
    wb_add_encharter(dims = "D2:L20", graph = pie)

  if (interactive()) wb$open()
  invisible(wb)
}

pie_example()
