rm(list = ls())

library(openxlsx2)
library(encharter)

# 1. Create the dataset
sales_data <- data.frame(
  Month = month.abb,
  Volume = c(1200, 1150, 1300, 1250, 1400, 1350, 1100, 1050, 1200, 1500, 1800, 2000),
  Sales = c(12000, 11500, 13000, 12500, 14000, 13500, 13200, 12600, 14400, 18000, 21600, 24000)
)

# 2. Setup the Chart Object
my_chart <- Chart$new("barChart")

my_chart$
  set_chart_title("Advanced Style Combo Chart")$
  set_legend_style(pos = "b", font_size = 10)$

  # --- SERIES STYLING ---
  # Primary Axis: Volume (Standard Bars)
  add_series(
    header = "Sheet1!$B$1",
    data   = "Sheet1!$B$2:$B$13",
    cat    = "Sheet1!$A$2:$A$13",
    color  = "4472C4" # Blue bars
  )$

  # Secondary Axis: Sales (Styled Line with Markers)
  add_series(
    header      = "Sheet1!$C$1",
    data        = "Sheet1!$C$2:$C$13",
    secondary   = TRUE,
    type        = "lineChart",
    # Line styling (Dashed Green)
    line_color  = "70AD47",
    line_width  = 3,
    line_type   = "dash",
    # Marker styling (Red border with Blue fill)
    marker      = "circle",
    marker_size = 7,
    marker_fill = "0000FF",    # Blue interior
    marker_line = "FF0000",    # Red border
    marker_line_width = 1.5
  )$

  # --- AXIS & GRIDLINE STYLING ---
  # Y-Axis (Primary): Solid black line, dashed major grid, dotted minor grid
  set_y_axis(
    line_width       = 2,
    color            = "000000",
    gridlines        = "dash",     # Major grid style
    grid_width       = 1.5,
    grid_color       = "D9D9D9",
    minor_gridlines  = "dotted",   # Minor grid style
    minor_grid_width = 1,
    minor_grid_color = "F2F2F2"
  )$

  # X-Axis: Moving to the bottom (min) and setting a thick line
  set_x_axis(
    crosses    = "min",
    line_width = 2,
    color      = "000000"
  )

# 3. Build and Save
wb <- wb_workbook() |>
  wb_add_worksheet("Sheet1") |>
  wb_add_data(x = sales_data) |>
  wb_add_encharter(dims = "E2:M20", chart_obj = my_chart)

wb$open()
