# Heavy-styling combo chart. Volume as plain blue bars on primary axis. Sales
# as a styled secondary-axis line: dashed green 70AD47 line at width 3, plus
# circle markers with blue fill, red border and 1.5 stroke width. Y-axis has
# dashed major and dotted minor gridlines (gray shades) and a thick black
# axis line. X-axis is moved to crosses = "min" with a thick black line.
# Chart at E2:M20.

styled_bars <- function() {
  require(openxlsx2)
  require(encharter)

  sales_data <- data.frame(
    Month  = month.abb,
    Volume = c(1200, 1150, 1300, 1250, 1400, 1350, 1100, 1050, 1200, 1500, 1800, 2000),
    Sales  = c(12000, 11500, 13000, 12500, 14000, 13500, 13200, 12600, 14400, 18000, 21600, 24000)
  )

  my_chart <- ec("barChart")

  my_chart$
    set_chart_title("Advanced Style Combo Chart")$
    set_legend_style(pos = "b", font_size = 10)$
    add_series(
      name  = "Sheet1!$B$1",
      data  = "Sheet1!$B$2:$B$13",
      label = "Sheet1!$A$2:$A$13",
      color = "4472C4"
    )$
    add_series(
      name              = "Sheet1!$C$1",
      data              = "Sheet1!$C$2:$C$13",
      secondary         = TRUE,
      type              = "lineChart",
      line_color        = "70AD47",
      line_width        = 3,
      line_type         = "dash",
      marker            = "circle",
      marker_size       = 7,
      marker_fill       = "0000FF",
      marker_line       = "FF0000",
      marker_line_width = 1.5
    )$
    set_y_axis(
      line_width       = 2,
      color            = "000000",
      grid_lines       = "dash",
      grid_width       = 1.5,
      grid_color       = "D9D9D9",
      minor_grid_lines = "dotted",
      minor_grid_width = 1,
      minor_grid_color = "F2F2F2"
    )$
    set_x_axis(
      crosses    = "min",
      line_width = 2,
      color      = "000000"
    )

  wb <- wb_workbook() |>
    wb_add_worksheet("Sheet1") |>
    wb_add_data(x = sales_data) |>
    wb_add_encharter(dims = "E2:M20", graph = my_chart)

  if (interactive()) wb$open()
  invisible(wb)
}

styled_bars()
