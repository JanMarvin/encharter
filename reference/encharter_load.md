# Load a chart from a workbook into an encharter object

Reads a chart stored in a workbook and returns it as a `Chart` or
`ChartEx` object, the counterpart of
[`openxlsx2::wb_add_encharter()`](https://janmarvin.github.io/openxlsx2/reference/wb_add_encharter.html).
Series references and cached values, styling, axes, titles and the
legend are restored, so the object can be modified like any other
encharter object (`$update_series()` to follow a longer data range,
`$add_series()`, `$set_chart_title()`, ...) and written back with
[`wb_add_encharter()`](https://janmarvin.github.io/openxlsx2/reference/wb_add_encharter.html).

## Usage

``` r
encharter_load(wb, chart = 1L, type = NULL)

ec_load(wb, chart = 1L, type = NULL)
```

## Arguments

- wb:

  A `wbWorkbook` containing at least one chart.

- chart:

  Integer; the row of `wb$charts` to load. Default `1`.

- type:

  `"chart"` for a standard chart or `"chartEx"` for an extended chart.
  The default takes whichever the row holds and errors if it holds both.

## Value

A `Chart` or `ChartEx` object.

## Details

Charts created with encharter are reproduced exactly. Charts written by
other software load as well; properties encharter has no field for
(manual layouts, per-point styling outside pie charts, theme color
modifiers, extension lists) are not carried over.

A workbook loaded from a file can hold a standard chart and an extended
chart (waterfall, treemap, ...) in the same row of `wb$charts`; use
`type` to pick one in that case.

## Examples

``` r
library(openxlsx2)
wb <- wb_workbook()$add_worksheet("Data")$add_data(x = data.frame(
  Month = month.abb[1:6],
  Sales = c(120, 135, 128, 160, 175, 190)
))
chart <- ec("line")$set_chart_title("Sales")$
  add_series(name = "Data!$B$1", label = "Data!$A$2:$A$7",
             data = "Data!$B$2:$B$7", marker = "circle")
wb$add_encharter(dims = "D2:L18", graph = chart)

# A month later: append a row and point the chart at the longer range
wb$add_data(x = data.frame(Month = "Jul", Sales = 240), dims = "A8", col_names = FALSE)
chart <- ec_load(wb)
chart$update_series(data = wb_data(wb), label = Month)
wb$add_encharter(dims = "D20:L36", graph = chart)
```
