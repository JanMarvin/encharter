rm(list = ls())

library(openxlsx2)
library(encharter)


# 1. The Data
scatter_data <- data.frame(
  Ad_Spend = c(100, 250, 400, 600, 850),
  Conversions = c(5, 12, 18, 30, 45),
  Item = c("A", "A", "B", "B", "C")
)

# 2. The Chart
scatter_plot <- Chart$new("scatterChart")

scatter_plot$add_series(
  header = "Conversions!",   # "Conversions"
  cat    = "Sheet1!$A$2:$A$6", # X-Axis (Ad Spend)
  data   = "Sheet1!$B$2:$B$6", # Y-Axis (Conversions)
  color  = wb_color(hex = "FF0000"),           # Red line/points
  type   = "scatterChart",
  show_line = FALSE
)

scatter_plot$set_data_label_style(
  show_val = TRUE, show_cat = TRUE, show_legend_key = FALSE, color = wb_color("black")
)

scatter_plot$set_chart_title("Ad Spend vs. Performance")
scatter_plot$set_x_title("Investment ($)")
scatter_plot$set_y_title("Conversions")

# 3. Render
scatter_xml <- scatter_plot$render()


wb <- wb_workbook() |>
  wb_add_worksheet("Sheet1") |>
  wb_add_data(x = scatter_data) |>
  wb_add_chart(dims = "E2:M20", chart_obj = scatter_plot)

wb$open()
