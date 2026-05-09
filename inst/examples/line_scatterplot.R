# nolint start: object_usage_linter.
# Three-series scatter chart using iris. Sepal.Width medians per
# (Sepal.Length, Species) are reshaped wide (one column per species) and the
# resulting NA holes are handled via set_disp_blanks("gap"). Each species
# gets a theme color (5/6/7), circle markers, the first two with size 7 and
# explicit type = "scatterChart". Axes start at 4 / 1.5; legend at bottom.
# Requires dplyr and tidyr. Chart at E2:M20.

line_scatterplot <- function() {
  require(openxlsx2)
  require(encharter)

  for (p in c("dplyr", "tidyr")) {
    if (!requireNamespace(p, quietly = TRUE))
      stop("install '", p, "' to run this example")
  }

  dat <- iris |>
    dplyr::group_by(Sepal.Length, Species) |>
    dplyr::summarize(Sepal.Width = median(Sepal.Width), .groups = "drop") |>
    tidyr::pivot_wider(names_from = Species, values_from = Sepal.Width) |>
    dplyr::arrange(Sepal.Length)

  wb <- wb_workbook() |>
    wb_add_worksheet("Sheet 1") |>
    wb_add_data(x = dat, na = NULL)

  data <- wb |> wb_data()

  chart <- ec("scatterChart")

  chart$add_series(
    name        = "setosa",
    data        = data,
    label       = "Sepal.Length",
    color       = wb_color(theme = 5),
    marker      = "circle",
    marker_size = 7,
    type        = "scatterChart"
  )$add_series(
    name        = "versicolor",
    data        = data,
    label       = "Sepal.Length",
    color       = wb_color(theme = 6),
    marker      = "circle",
    marker_size = 7
  )$add_series(
    name   = "virginica",
    data   = data,
    label  = "Sepal.Length",
    color  = wb_color(theme = 7),
    marker = "circle"
  )

  chart$set_disp_blanks("gap")

  chart$set_chart_title("Median of Sepal.Width by Sepal.Length")
  chart$set_x_title("Sepal.Length")
  chart$set_y_title("Sepal.Width")
  chart$set_x_axis(min = 4,   minor_tick = "none")
  chart$set_y_axis(min = 1.5, minor_tick = "none")
  chart$set_legend_style(pos = "b")

  wb <- wb |>
    wb_add_encharter(dims = "E2:M20", graph = chart)

  if (interactive()) wb$open()
  invisible(wb)
}

line_scatterplot()

# nolint end
