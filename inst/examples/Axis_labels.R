rm(list = ls())

library(openxlsx2)
library(encharter)

# 1. Dataset with negative values
net_income <- data.frame(
  Month = month.abb[1:6],
  Profit = c(150, -300, 450, -120, 600, -250)
)

# 2. Setup the Chart
my_chart <- Chart$new("barChart")

my_chart$
  set_chart_title("Profit Analysis (Negative Values)")$
  add_series(
    header = "Profit",
    data   = "Sheet1!$B$2:$B$7",
    cat    = "Sheet1!$A$2:$A$7",
    color  = "4472C4"
  )$
  set_x_axis(
    crosses   = "autoZero",
    label_pos = "low"
  )$
  set_y_axis(
    gridlines = "solid",
    grid_color = "D9D9D9"
  )

# 3. Build the Workbook
wb <- wb_workbook() |>
  wb_add_worksheet("Sheet1") |>
  wb_add_data(x = net_income) |>
  wb_add_chart(dims = "D2:L20", chart_obj = my_chart)

wb$open()
