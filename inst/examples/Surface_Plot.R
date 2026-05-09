# Surface (contour) plot built from a 5x5 data frame. Each row of the matrix
# becomes a series whose data is one row of values; categories are the
# column-header X labels. Loop emits five series (rows 2..6 of the sheet).
# Chart at H2:P25.

surface_plot <- function() {
  require(openxlsx2)
  require(encharter)

  surface_data <- data.frame(
    Y_Axis = c("Y1", "Y2", "Y3", "Y4", "Y5"),
    X1 = c(10, 20, 30, 20, 10),
    X2 = c(20, 40, 60, 40, 20),
    X3 = c(30, 60, 90, 60, 30),
    X4 = c(20, 40, 60, 40, 20),
    X5 = c(10, 20, 30, 20, 10)
  )

  contour_plot <- ec("surfaceChart")

  for (i in 2:6) {
    contour_plot$add_series(
      name  = paste0("Sheet1!$A$", i),
      data  = paste0("Sheet1!$B$", i, ":$F$", i),
      label = "Sheet1!$B$1:$F$1",
      type  = "surfaceChart"
    )
  }

  wb <- wb_workbook() |>
    wb_add_worksheet("Sheet1") |>
    wb_add_data(x = surface_data, col_names = TRUE) |>
    wb_add_encharter(dims = "H2:P25", graph = contour_plot)

  if (interactive()) wb$open()
  invisible(wb)
}

surface_plot()
