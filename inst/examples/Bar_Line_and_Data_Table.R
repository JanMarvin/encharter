library(openxlsx2)
library(encharter)

# 1. Create dummy data
data <- data.frame(
  Date   = as.Date("2025-01-01") + seq(0, 330, by = 30),
  Sales  = c(120, 150, 180, 170, 210, 250, 300, 280, 260, 310, 350, 400),
  Growth = c(0.02, 0.05, 0.08, -0.02, 0.10, 0.15, 0.20, -0.05, -0.02, 0.12, 0.10, 0.14)
)

# 2. Initialize Workbook and add data
wb <- wb_workbook()
wb$add_worksheet("Dashboard")
wb$add_data(sheet = "Dashboard", x = data)

# 3. Create the Chart Object
# We start with "barChart" as the base type
my_chart <- ec(type = "barChart")

# 6. Add Data Series
# Series 1: Sales (Primary Axis, Bar)
my_chart$add_series(
  header    = "Dashboard!$B$1",
  data      = "Dashboard!$B$2:$B$13",
  cat       = "Dashboard!$A$2:$A$13",
  color     = "4F81BD",
  type      = "barChart"
)

# Series 2: Growth (Secondary Axis, Line)
my_chart$add_series(
  header    = "Dashboard!$C$1",
  data      = "Dashboard!$C$2:$C$13",
  cat       = "Dashboard!$A$2:$A$13",
  type      = "lineChart",
  secondary = TRUE,
  color     = "C0504D",
  marker    = "circle",
  smooth    = TRUE
)


# Set Titles
my_chart$set_chart_title("Annual Sales & Growth", bold = TRUE, font_size = 16)
my_chart$set_x_title("Reporting Month")
my_chart$set_y_title("Revenue (k)")
my_chart$set_y2_title("Growth Rate (%)")

# 4. Configure the X-Axis (Dates)
# Major units: 2 Months, Minor units: 1 Month
my_chart$set_x_axis(
  major      = 2,
  major_time = "months",
  minor      = 1,
  minor_time = "months",
  format     = "mmm-yy"
)

# 5. Configure the Y-Axes
# Primary Y: Steps of 100
my_chart$set_y_axis(major = 100, min = 0, format = "#,##0")
# Secondary Y: Percentages with steps of 5%
my_chart$set_y2_axis(major = 0.05, min = -0.10, max = 0.30, format = "0%")

my_chart$set_data_table(TRUE)
my_chart$set_legend_style(pos = "none")

# 7. Add to Workbook
# We render the XML and pass it to openxlsx2
chart_xml <- my_chart$render()
wb$add_chart_xml(sheet = "Dashboard", xml = chart_xml, dims = "E2:M20")

# 8. Save/Open
# wb_save(wb, "Chart_Example.xlsx")
wb_open(wb)
