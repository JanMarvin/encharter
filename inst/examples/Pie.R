rm(list = ls())

library(openxlsx2)
library(encharter)


# 1. Initialize as a pieChart
pie <- ec("pieChart")

pie$set_chart_title("Market Share 2026")

# 2. Configure Labels (Very important for Pie charts)
pie$set_data_label_style(
  show_val = TRUE,
  show_cat = TRUE,   # Shows the name of the slice
  pos = "outEnd"      # Positions labels outside the pie
)

# 3. Add Series
# Note: color here is the base color, but varyColors logic
# in the render engine will ensure slices are distinct.
pie$add_series(
  header = "'Sheet 1'!$B$1",
  data   = "'Sheet 1'!$B$2:$B$5",
  cat    = "'Sheet 1'!$A$2:$A$5",
  color = wb_color(hex = viridisLite::viridis(5), format = "RGBA")
)

pie$set_pie_options(
  rotation = 300, expansion = 10
)

# 4. Build workbook
wb <- wb_workbook() |>
  wb_add_worksheet("Sheet 1") |>
  wb_add_data(x = data.frame(
    Product = c("Apples", "Bananas", "Cherries", "Dates"),
    Sales = c(40, 30, 20, 10)
  )) |>
  openxlsx2::wb_add_encharter(dims = "D2:L20", graph = pie)

wb$open()
