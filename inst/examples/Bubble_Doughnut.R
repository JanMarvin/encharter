# Two charts on one sheet: a doughnut (hole_size 60) for Budget Allocation
# and a bubble chart for Investment Analysis (X = Investment, Y = Profit,
# bubble size = Risk_Size). Doughnut at B2:K12, bubble at B13:K25.

bubble_doughnut <- function() {
  require(openxlsx2)
  require(encharter)

  chart_data <- data.frame(
    Category   = c("Project A", "Project B", "Project C", "Project D"),
    Investment = c(10, 40, 25, 55),
    Profit     = c(15, 35, 10, 60),
    Risk_Size  = c(5, 20, 15, 10)
  )

  wb <- wb_workbook() |>
    wb_add_worksheet("Data") |>
    wb_add_data(x = chart_data)

  dn_chart <- ec("doughnutChart")
  dn_chart$set_chart_title("Budget Allocation")$set_pie_options(hole_size = 60)
  dn_chart$add_series(
    name  = "Data!$B$1",
    label = "Data!$A$2:$A$5",
    data  = "Data!$B$2:$B$5"
  )

  bb_chart <- ec("bubbleChart")
  bb_chart$set_chart_title("Investment Analysis")
  bb_chart$set_x_title("Investment")$set_y_title("Profit")
  bb_chart$add_series(
    name   = "Investment vs Profit",
    label  = "Data!$A$2:$A$5",
    data   = "Data!$C$2:$C$5",
    weight = "Data!$D$2:$D$5"
  )

  wb <- wb |>
    wb_add_encharter(dims = "B2:K12",  graph = dn_chart) |>
    wb_add_encharter(dims = "B13:K25", graph = bb_chart)

  if (interactive()) wb$open()
  invisible(wb)
}

bubble_doughnut()
