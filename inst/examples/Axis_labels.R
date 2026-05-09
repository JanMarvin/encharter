# Bar chart with negative values where the X-axis crosses at autoZero but
# labels are pushed to "low" so they sit at the bottom of the plot rather than
# on the zero line. Light gray solid major gridlines on Y. Chart at D2:L20.

axis_labels <- function() {
  require(openxlsx2)
  require(encharter)

  net_income <- data.frame(
    Month  = month.abb[1:6],
    Profit = c(150, -300, 450, -120, 600, -250)
  )

  my_chart <- ec("barChart")
  my_chart$
    set_chart_title("Profit Analysis (Negative Values)")$
    add_series(
      name  = "Profit",
      data  = "Sheet1!$B$2:$B$7",
      label = "Sheet1!$A$2:$A$7",
      color = "4472C4",
      type  = "barChart"
    )$
    set_x_axis(
      crosses   = "autoZero",
      label_pos = "low"
    )$
    set_y_axis(
      grid_lines = "solid",
      grid_color = "D9D9D9"
    )

  wb <- wb_workbook() |>
    wb_add_worksheet("Sheet1") |>
    wb_add_data(x = net_income) |>
    wb_add_encharter(dims = "D2:L20", graph = my_chart)

  if (interactive()) wb$open()
  invisible(wb)
}

axis_labels()
