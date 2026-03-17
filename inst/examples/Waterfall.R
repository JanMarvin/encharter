rm(list = ls())

library(openxlsx2)
library(encharter)

# Create a Financial Bridge dataset
waterfall_df <- data.frame(
  Category = c("Gross Revenue", "COGS", "Operating Exp", "Tax", "Other Income", "Net Income"),
  Amount = c(1200, -450, -300, -100, 50, 400)
)

# Add it to your workbook
wb <- wb_workbook()$
  add_worksheet("Data2")$
  add_data(x = waterfall_df)

my_wf <- ec("waterfall")
my_wf$set_chart_title("2024 Financial Performance")

# Mapping the "Financial Bridge" data
my_wf$add_series(
  header = "Data2!$B$1",         # "Amount"
  data   = "Data2!$B$2:$B$7",     # 1200, -450, -300...
  cat    = "Data2!$A$2:$A$7",     # Categories
  type         = "waterfall"
)

wb <- wb_add_encharter(wb, sheet = "Data2", dims = "D2:L20", graph = my_wf)

wb$charts$chartEx |> as_xml()

wb$get_named_regions()

wb$open()
