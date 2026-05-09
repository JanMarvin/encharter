# Combo chart: bars on the primary axis (Volume, blue 4472C4), line on the
# secondary axis (Sales, orange ED7D31). The line is a bit thicker than the
# default (line_width 2.5 vs 1) and dashed. Legend at the bottom with font
# size 10. Chart placed at E2:M20.

bar_line_chart <- function() {
  require(openxlsx2)
  require(encharter)

  sales_data <- data.frame(
    Month  = month.abb,
    Volume = c(1200, 1150, 1300, 1250, 1400, 1350, 1100, 1050, 1200, 1500, 1800, 2000),
    Sales  = c(12000, 11500, 13000, 12500, 14000, 13500, 13200, 12600, 14400, 18000, 21600, 24000)
  )

  my_chart <- ec("barChart")
  my_chart$
    set_chart_title("Sales vs Volume")$
    set_legend_style(pos = "b", font_size = 10)$
    add_series(
      name  = "Sheet1!$B$1",
      data  = "Sheet1!$B$2:$B$13",
      label = "Sheet1!$A$2:$A$13",
      color = "4472C4",
      type  = "barChart"
    )$
    add_series(
      name       = "Sheet1!$C$1",
      data       = "Sheet1!$C$2:$C$13",
      color      = "ED7D31",
      type       = "lineChart",
      line_width = 2.5,
      line_type  = "dashed",
      secondary  = TRUE
    )

  wb <- wb_workbook() |>
    wb_add_worksheet("Sheet1") |>
    wb_add_data(x = sales_data) |>
    wb_add_encharter(dims = "E2:M20", graph = my_chart)

  if (interactive()) wb$open()
  invisible(wb)
}

bar_line_chart()
