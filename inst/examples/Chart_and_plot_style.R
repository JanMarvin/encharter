library(openxlsx2)
library(encharter)

# 1. Create a styled chart
chart <- ec("lineChart")
chart$add_series(header = "S1!$B$1", data = "S1!$B$2:$B$5")

# Set Chart Background and Border (ChartSpace)
chart$set_chart_style(
  fill = "EDF2F7", # Light grey background
  line = "2D3748", # Dark blue/grey border
  line_width =  2.3  # Thick border
)

# Set Plot Area Background (PlotArea)
chart$set_plot_style(
  fill = "FFFFFF",   # White plot area
  line = "CBD5E0", # Subtle plot border
  line_width = 1     # Thin border
)

xml <- as.character(chart$render())

# 3. Visual Verification
wb <- wb_workbook()$add_worksheet("S1")$add_data(x = mtcars)$
  add_chart_xml(xml = xml, dims = "E2:M20")
wb$open()
