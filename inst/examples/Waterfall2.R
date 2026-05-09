# Waterfall on a date-based X-axis. set_x_axis() uses format = "YYYY-MM-DD"
# with major ticks "out" and minor ticks "cross". Y-axis has dotted grid
# lines using a theme-3 color. The series uses gap_width = 0 so bars touch.

waterfall2 <- function() {
  require(openxlsx2)
  require(encharter)

  wf_dates <- data.frame(
    Date   = seq(as.Date("2024-01-01"), by = "month", length.out = 6),
    Change = c(1000, -200, 150, -300, 400, 1050)
  )

  wb <- wb_workbook()$
    add_worksheet("MonthlyFlow")$
    add_data(x = wf_dates)

  my_wf <- ec("waterfall")
  my_wf$set_chart_title("2024 Financial Performance")$
    set_x_axis(
      format     = "YYYY-MM-DD",
      major_tick = "out",
      minor_tick = "cross"
    )$
    set_y_axis(
      grid_color = wb_color(theme = "3"),
      grid_lines = "dot"
    )

  my_wf$add_series(
    name      = "'MonthlyFlow'!$B$1",
    data      = "'MonthlyFlow'!$B$2:$B$7",
    label     = "'MonthlyFlow'!$A$2:$A$7",
    type      = "waterfall",
    gap_width = 0
  )

  wb <- wb_add_encharter(wb, sheet = "MonthlyFlow", graph = my_wf)

  if (interactive()) wb$open()
  invisible(wb)
}

waterfall2()
