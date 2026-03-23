rm(list = ls())

library(openxlsx2)
library(encharter)


# 1. Initialize with Area Chart as the default base
combo_chart <- ec("barChart")

# 2. Add two series for the "Stacked" effect
# Note: In OOXML, Bar/Column charts use the same 'barChart' node.
combo_chart$add_series(
  header = "Sheet1!$B$1",
  data = "Sheet1!$B$2:$B$6",
  cat =  "Sheet1!$A$2:$A$6",
  color = "4472C4",
  type = "barChart",
  grouping = "clustered"
)

combo_chart$add_series(
  header = "Sheet1!$C$1",
  data = "Sheet1!$C$2:$C$6",
  cat =  "Sheet1!$A$2:$A$6",
  color = "A5A5A5",
  type = "barChart",
  grouping = "clustered"
)

# 3. Add an Area Chart series as an overlay
combo_chart$add_series(
  header = "Sheet1!$D$1",
  data = "Sheet1!$D$2:$D$6",
  color = "70AD47",
  type = "areaChart", # This will render in its own node
  secondary = TRUE
)

# 4. Final Polish
combo_chart$set_chart_title("Inventory vs Market Trend", name = "Times New Roman", bold = TRUE)
combo_chart$set_legend_style(pos = "b", sz = 10) # Put legend at the bottom
combo_chart$set_x_title("Months")
combo_chart$set_y_title("Values")
combo_chart$set_y2_title("Also Values")
combo_chart$set_x_axis(bold = FALSE, italic = TRUE, name = "Calibri", sz = 12)
combo_chart$set_y_axis(bold = TRUE, italic = FALSE, name = "Arial", sz = 12)

# 5. Generate XML
chart_xml <- combo_chart$render()

# Create the dataset
chart_data <- data.frame(
  Month = c("Jan", "Feb", "Mar", "Apr", "May"),
  Product_A = c(45, 52, 30, 48, 60),
  Product_B = c(25, 30, 45, 40, 35),
  Market_Trend = c(80, 85, 90, 100, 110)
)

# breaks with area charts
# combo_chart$set_data_label_style(
#   show_val = TRUE, show_cat = TRUE, show_legend_key = FALSE, color = wb_color("black")
# )

# Reference for your Chart series:
# Month:        "Sheet1!$A$2:$A$6" (Category)
# Product A:    "Sheet1!$B$2:$B$6" (Value)
# Product B:    "Sheet1!$C$2:$C$6" (Value)
# Market Trend: "Sheet1!$D$2:$D$6" (Value)

wb <- wb_workbook() |>
  wb_add_worksheet("Sheet1") |>
  wb_add_data(x = chart_data) |>
  openxlsx2::wb_add_encharter(dims = "E2:M20", graph = combo_chart)

wb$open()
