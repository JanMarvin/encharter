# Plain scatter chart of Ad Spend vs Conversions in red with show_line = FALSE
# (markers only, no connecting line). Data labels show both value and category
# in black. Chart at E2:M20.

scatter_example <- function() {
  require(openxlsx2)
  require(encharter)

  scatter_data <- data.frame(
    Ad_Spend    = c(100, 250, 400, 600, 850),
    Conversions = c(5, 12, 18, 30, 45),
    Item        = c("A", "A", "B", "B", "C")
  )

  scatter_plot <- ec("scatterChart")

  scatter_plot$add_series(
    name      = "Conversions",
    label     = "Sheet1!$A$2:$A$6",
    data      = "Sheet1!$B$2:$B$6",
    color     = wb_color(hex = "FF0000"),
    type      = "scatterChart",
    show_line = FALSE
  )

  scatter_plot$set_data_label_style(
    show_val        = TRUE,
    show_cat        = TRUE,
    show_legend_key = FALSE,
    color           = wb_color("black")
  )

  scatter_plot$set_chart_title("Ad Spend vs. Performance")
  scatter_plot$set_x_title("Investment ($)")
  scatter_plot$set_y_title("Conversions")

  wb <- wb_workbook() |>
    wb_add_worksheet("Sheet1") |>
    wb_add_data(x = scatter_data) |>
    wb_add_encharter(dims = "E2:M20", graph = scatter_plot)

  if (interactive()) wb$open()
  invisible(wb)
}

scatter_example()
