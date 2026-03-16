rm(list = ls())

library(openxlsx2)
library(encharter)

dat <- iris |>
  dplyr::group_by(Sepal.Length, Species) |>
  dplyr::summarize(Sepal.Width = median(Sepal.Width), .groups = "drop") |>
  tidyr::pivot_wider(names_from = Species, values_from = Sepal.Width) |>
  dplyr::arrange(Sepal.Length)

wb <- wb_workbook() |>
  wb_add_worksheet("Sheet 1") |>
  wb_add_data(x = dat, na = NULL)

data <- wb |> wb_data()

chart <- Chart$new()

chart$add_series(
  header = "setosa",
  data   = data,
  cat    = "Sepal.Length",
  color  = wb_color(theme = 5),
  marker = "circle",
  marker_size = 7,
  type = "scatterChart" # or lineChart
)$add_series(
  header = "versicolor",
  data   = data,
  cat    = "Sepal.Length",
  color  = wb_color(theme = 6),
  marker = "circle",
  marker_size = 7
)$add_series(
  header = "virginica",
  data   = data,
  cat    = "Sepal.Length",
  color  = wb_color(theme = 7),
  marker = "circle"
)

chart$set_disp_blanks(
  "gap"
)

chart$set_chart_title("Median of Sepal.Width by Sepal.Length")
chart$set_x_title("Sepal.Length")
chart$set_y_title("Sepal.Width")
chart$set_x_axis(min = 4, minor_tick = "none")
chart$set_y_axis(min = 1.5, minor_tick = "none")
chart$set_legend_style(pos = "b")


wb <- wb |>
  wb_add_encharter(dims = "E2:M20", graph = chart)

wb$open()
