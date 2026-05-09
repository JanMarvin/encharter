# Waterfall with full theme/styling pass: bars use accent2 (theme = "5") with
# black borders and gap_width = 1.5; X-axis uses Segoe UI bold 11 in #444444
# with outward major ticks; Y-axis uses Segoe UI italic 10 with #D9D9D9 grid
# pinned to a min of 0; legend bottom; plot area white on a black 1pt border.

waterfall3 <- function() {
  require(openxlsx2)
  require(encharter)

  wf_dates <- data.frame(
    Date   = seq(as.Date("2024-01-01"), by = "month", length.out = 6),
    Change = c(1000, -200, 150, -300, 400, 1050)
  )

  my_wf <- ec("waterfall")

  my_wf$add_series(
    name       = "Data2!$B$1",
    data       = "Data2!$B$2:$B$7",
    label      = "Data2!$A$2:$A$7",
    color      = wb_color(theme = "5"),
    line_color = "000000",
    type       = "waterfall",
    gap_width  = 1.5
  )

  my_wf$set_x_axis(
    font_size  = 11,
    font_name  = "Segoe UI",
    bold       = TRUE,
    color      = wb_color(hex = "#444444"),
    major_tick = "out"
  )

  my_wf$set_y_axis(
    font_size  = 10,
    font_name  = "Segoe UI",
    italic     = TRUE,
    grid_color = wb_color(hex = "#D9D9D9"),
    grid_lines = TRUE,
    min        = 0
  )

  my_wf$set_chart_title("2024 Performance", font_size = 14, bold = TRUE)$
    set_legend_style(pos = "b", font_size = 10)$
    set_plot_style(
      fill       = wb_color("white"),
      line       = wb_color("black"),
      line_width = 1
    )

  wb <- wb_workbook()$
    add_worksheet("Data2")$
    add_data(x = wf_dates)
  wb <- wb_add_encharter(wb, sheet = "Data2", graph = my_wf)

  if (interactive()) wb$open()
  invisible(wb)
}

waterfall3()
