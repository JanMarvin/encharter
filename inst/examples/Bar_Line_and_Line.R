# Two independent demos in one file. Run bar_line_and_line_a() for a simple
# styled line chart with red axis lines and gray gridlines (E2:M20 on a sheet
# whose name contains a space, so cell refs use single quotes). Run
# bar_line_and_line_b() for a line+bar combo on monthly Volume/Sales (line
# primary, bars secondary, orange ED7D31). Both run when the file is sourced.

bar_line_and_line_a <- function() {
  require(openxlsx2)
  require(encharter)

  chart <- encharter("lineChart")
  chart$
    set_chart_title("Custom Styled Chart")$
    set_x_axis(
      color      = wb_color("red"),
      grid_color = wb_color("gray"),
      grid_lines = TRUE
    )$
    set_y_axis(
      color      = wb_color("red"),
      grid_color = wb_color("gray"),
      grid_lines = TRUE
    )$
    add_series(
      name  = "'Sheet 1'!$B$1",
      data  = "'Sheet 1'!$B$2:$B$5",
      label = "'Sheet 1'!$A$2:$A$5",
      color = wb_color("blue")
    )

  wb <- wb_workbook() |>
    wb_add_worksheet("Sheet 1") |>
    wb_add_data(x = data.frame(Label = c("A", "B", "C", "D"), Val = 1:4)) |>
    wb_add_encharter(dims = "E2:M20", graph = chart)

  if (interactive()) wb$open()
  invisible(wb)
}

bar_line_and_line_b <- function() {
  require(openxlsx2)
  require(encharter)

  sales_data <- data.frame(
    Month  = month.abb,
    Volume = c(1200, 1150, 1300, 1250, 1400, 1350, 1100, 1050, 1200, 1500, 1800, 2000),
    Price  = c(rep(10, 6), rep(12, 6))
  )
  sales_data$Sales <- sales_data$Volume * sales_data$Price

  my_chart <- ec("lineChart")$
    set_chart_title("Custom Styled Chart")
  my_chart$add_series("Sales", "Sheet1!$B$2:$B$10")
  my_chart$add_series("Volume", "Sheet1!$D$2:$D$10",
                      color = "ED7D31", type = "barChart", secondary = TRUE)

  wb <- wb_workbook() |>
    wb_add_worksheet("Sheet1") |>
    wb_add_data(x = sales_data) |>
    wb_add_encharter(dims = "E2:M20", graph = my_chart)

  if (interactive()) wb$open()
  invisible(wb)
}

bar_line_and_line_a()
bar_line_and_line_b()
