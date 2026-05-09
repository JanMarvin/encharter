# Multi-level category labels on a bar chart. The label range spans two
# columns (A:B) so spreadsheet software groups categories by Status and
# Gender. To make the grouping render cleanly only the first row of each
# category carries the label, and merge_cells() merges A2:A3 and A4:A5 in
# the source data. Chart at E2:M20, dark blue 003C63 bars.

label_grouping <- function() {
  require(openxlsx2)
  require(encharter)

  plot_data <- data.frame(
    Status = c("Smoker", "", "Non-Smoker", ""),
    Gender = c("Male", "Female", "Male", "Female"),
    Value  = c(25, 22, 15, 18)
  )

  wb <- wb_workbook()$add_worksheet("data")$add_data(x = plot_data)
  wb$merge_cells(dims = "A2:A3;A4:A5")

  my_chart <- ec("barChart")
  my_chart$set_chart_title("Smokers by Gender", bold = TRUE)

  my_chart$add_series(
    name  = "Prevalence",
    data  = "data!$C$2:$C$5",
    label = "data!$A$2:$B$5",
    color = wb_color("#003C63"),
    type  = "barChart"
  )

  chart_xml <- my_chart$render()
  wb$add_chart_xml(xml = chart_xml, dims = "E2:M20")

  if (interactive()) wb_open(wb)
  invisible(wb)
}

label_grouping()
