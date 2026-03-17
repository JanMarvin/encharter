rm(list = ls())
library(openxlsx2)
library(encharter)

# 1. Create a Matrix of data (e.g., a 5x5 grid of elevations/values)
# Columns will be the X-axis, Rows will be the Y-axis
surface_data <- data.frame(
  Y_Axis = c("Y1", "Y2", "Y3", "Y4", "Y5"),
  X1 = c(10, 20, 30, 20, 10),
  X2 = c(20, 40, 60, 40, 20),
  X3 = c(30, 60, 90, 60, 30),
  X4 = c(20, 40, 60, 40, 20),
  X5 = c(10, 20, 30, 20, 10)
)

# 2. Setup the Chart
# We use "surfaceChart" type.
contour_plot <- ec("surfaceChart")

for (i in 2:6) {
  contour_plot$add_series(
    # Header: Single cell for the Y-axis label (e.g., A2, A3...)
    header = paste0("Sheet1!$A$", i),
    # Data: A single row of values (e.g., B2:F2, B3:F3...)
    data   = paste0("Sheet1!$B$", i, ":$F$", i),
    # Category: The shared X-axis labels (B1:F1)
    cat    = "Sheet1!$B$1:$F$1",
    type   = "surfaceChart"
  )
}

# 3. Create Workbook
wb <- wb_workbook() |>
  wb_add_worksheet("Sheet1") |>
  wb_add_data(x = surface_data, col_names = TRUE) |>
  wb_add_encharter(dims = "H2:P25", graph = contour_plot)

wb$open()
