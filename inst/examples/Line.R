rm(list = ls())

library(openxlsx2)
library(encharter)


# 1. Initialize
chart <- ec("lineChart")

chart$set_chart_title("Line with Dots and Labels")

# NEW: Enable data labels globally
# show_val = TRUE shows the Y-axis value
# pos = "t" puts them at the Top of the marker
chart$set_data_label_style(
  show_val = TRUE,
  show_cat = FALSE,
  pos = "t",
  bold = TRUE,
  font_size = 9
)

chart$add_series(
  header = "'Sheet 1'!$B$1",
  data   = "'Sheet 1'!$B$2:$B$5",
  cat    = "'Sheet 1'!$A$2:$A$5",
  color  = "#0000FF",
  marker = "circle",
  marker_size = 7,
  marker_fill = "#FFFFFF",
  marker_line = "#0000FF"
)

# 2. Build workbook
wb <- wb_workbook() |>
  wb_add_worksheet("Sheet 1") |>
  wb_add_data(x = data.frame(Label = c("A", "B", "C", "D"), Val = c(10, 25, 15, 30))) |>
  openxlsx2::wb_add_encharter(dims = "E2:M20", graph = chart)

wb$open()
