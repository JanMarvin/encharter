library(openxlsx2)
library(encharter)

# Dummy data for Box Plot
df_box <- data.frame(
  Group = rep("Test", 10),
  Score = c(10, 12, 11, 13, 12, 11, 14, 12, 45, 2) # 45 and 2 are outliers
)

ce <- ec("boxWhisker")

ce$add_series(
  header = "Scores",
  data = "Sheet1!$B$2:$B$11",
  cat = "Sheet1!$A$2:$A$11",
  statistics = "inclusive",
  visibility = list(
    meanLine = FALSE,
    meanMarker = TRUE,
    outliers = FALSE
  ),
  line_color = wb_color("lightgray")
)

ce$set_chart_title("Box Plot with Visibility Toggles")

wb <- wb_workbook() |>
  wb_add_worksheet("Sheet1") |>
  wb_add_data(x = df_box) |>
  wb_add_encharter(dims = "D2:L25", graph = ce)

if (interactive()) wb$open()
