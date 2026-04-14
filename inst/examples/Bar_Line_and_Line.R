rm(list = ls())

library(openxlsx2)
library(encharter)

# Create chart
chart <- encharter("lineChart")
chart$
  set_chart_title("Custom Styled Chart")$
  set_x_axis(
    color = wb_color("red"),        # Axis line color
    grid_color = wb_color("gray"), # Gridline color
    grid_lines = TRUE
  )$
  set_y_axis(
    color = wb_color("red"),        # Axis line color
    grid_color = wb_color("gray"), # Gridline color
    grid_lines = TRUE
  )$
  add_series(
    header = "'Sheet 1'!$B$1",
    data = "'Sheet 1'!$B$2:$B$5",
    cat = "'Sheet 1'!$A$2:$A$5",
    color = wb_color("blue")
  )

# Build workbook
wb <- wb_workbook() |>
  wb_add_worksheet("Sheet 1") |>
  wb_add_data(x = data.frame(Label = c("A", "B", "C", "D"), Val = 1:4)) |>
  openxlsx2::wb_add_encharter(dims = "E2:M20", graph = chart)

wb$open()

#####################################################

# Create a 12-month dataset
sales_data <- data.frame(
  Month = month.abb,
  Volume = c(1200, 1150, 1300, 1250, 1400, 1350, 1100, 1050, 1200, 1500, 1800, 2000),
  Price = c(rep(10, 6), rep(12, 6)) # Price jump in July
)

# Calculate Sales revenue
sales_data$Sales <- sales_data$Volume * sales_data$Price

# Final look for the sheet
df_for_sheet <- sales_data[, c("Month", "Volume", "Sales")]

my_chart <- ec("lineChart")$
  set_chart_title("Custom Styled Chart")
# Primary axis series
my_chart$add_series("Sales", "Sheet1!$B$2:$B$10")
# Secondary axis series (Bars)
my_chart$add_series("Volume", "Sheet1!$D$2:$D$10", color = "ED7D31", type = "barChart", secondary = TRUE)

# Build workbook
wb <- wb_workbook() |>
  wb_add_worksheet("Sheet1") |>
  wb_add_data(x = sales_data) |>
  openxlsx2::wb_add_encharter(dims = "E2:M20", graph = my_chart)

if (interactive()) wb$open()
