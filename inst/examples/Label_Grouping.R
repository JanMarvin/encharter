library(openxlsx2)
library(encharter)

# 1. Prepare Data
# Note: For Excel to group them nicely, only the first row
# of each category should have the label (e.g., "Smoker").
plot_data <- data.frame(
  Status = c("Smoker", "", "Non-Smoker", ""),
  Gender = c("Male", "Female", "Male", "Female"),
  Value  = c(25, 22, 15, 18)
)

# 2. Create Workbook and add data
wb <- wb_workbook()$add_worksheet("data")$add_data(x = plot_data)
wb$merge_cells(dims = "A2:A3;A4:A5")

# 3. Build the Chart
my_chart <- Chart$new()
my_chart$set_chart_title("Smokers by Gender", bold = TRUE)

my_chart$add_series(
  header = "Prevalence",
  # The values are in column C
  data   = "data!$C$2:$C$5",
  # The CATEGORIES span TWO columns (A and B)
  # This creates the multi-level hierarchy
  cat    = "data!$A$2:$B$5",
  color  = wb_color("#003C63"),
  type   = "barChart"
)

# 4. Render and Add to Workbook
chart_xml <- my_chart$render()
wb$add_chart_xml(xml = chart_xml, dims = "E2:M20")

# Open to view
wb_open(wb)
