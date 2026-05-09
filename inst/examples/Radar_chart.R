# Side-by-side radar comparison: a standard radar (lines + markers, two
# models) at E2:L20, and a filled radar (filled = TRUE switches to the
# radarStyle "filled" XML node) at E22:L40. Five metrics on the spokes.

radar_chart <- function() {
  require(openxlsx2)
  require(encharter)

  skill_data <- data.frame(
    Metric  = c("Speed", "Reliability", "Comfort", "Safety", "Efficiency"),
    Model_X = c(90, 60, 85, 70, 50),
    Model_Y = c(60, 90, 50, 85, 80)
  )

  radar_std <- ec()
  radar_std$
    set_chart_title("Standard Radar: Model Comparison")$
    set_legend_style(pos = "b")$
    add_series(
      name       = "Model X",
      label      = "Sheet1!$A$2:$A$6",
      data       = "Sheet1!$B$2:$B$6",
      color      = "4472C4",
      line_width = 2,
      marker     = "circle",
      type       = "radarChart"
    )$
    add_series(
      name       = "Model Y",
      label      = "Sheet1!$A$2:$A$6",
      data       = "Sheet1!$C$2:$C$6",
      color      = "ED7D31",
      line_width = 2,
      marker     = "square",
      type       = "radarChart"
    )

  radar_filled <- ec("radarChart")
  radar_filled$
    set_chart_title("Filled Radar: Area View")$
    add_series(
      name   = "Model X",
      label  = "Sheet1!$A$2:$A$6",
      data   = "Sheet1!$B$2:$B$6",
      color  = "4472C4",
      filled = TRUE
    )$
    add_series(
      name   = "Model Y",
      label  = "Sheet1!$A$2:$A$6",
      data   = "Sheet1!$C$2:$C$6",
      color  = "ED7D31",
      filled = TRUE
    )

  wb <- wb_workbook() |>
    wb_add_worksheet("Sheet1") |>
    wb_add_data(x = skill_data) |>
    wb_add_encharter(dims = "E2:L20",  graph = radar_std) |>
    wb_add_encharter(dims = "E22:L40", graph = radar_filled)

  if (interactive()) wb$open()
  invisible(wb)
}

radar_chart()
