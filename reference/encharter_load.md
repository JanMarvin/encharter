# Load a chart from a workbook

Reads a chart from a workbook and returns it as a `Chart` or `ChartEx`
object. This is the reverse of
[`openxlsx2::wb_add_encharter()`](https://janmarvin.github.io/openxlsx2/reference/wb_add_encharter.html):
series references and cached values, styling, axes, titles and legend
are restored, and the object can be changed and added to a workbook
again.

## Usage

``` r
encharter_load(wb, chart = 1L, type = NULL)

ec_load(wb, chart = 1L, type = NULL)
```

## Arguments

- wb:

  A `wbWorkbook` with at least one chart.

- chart:

  Integer; row of `wb$charts`. Default `1`.

- type:

  `"chart"` or `"chartEx"`. By default the type present in the row is
  used; an error is raised if both are present.

## Value

A `Chart` or `ChartEx` object.

## Details

Charts written by encharter are read back completely. Charts written by
Excel or other software are read as far as encharter has fields for
their content; manual layouts, per-point formatting outside pie charts,
theme color modifiers and extension lists are dropped.

A workbook loaded from a file can hold a standard chart and an extended
chart (waterfall, treemap, ...) in the same row of `wb$charts`. Use
`type` to select one.

This function is new and has only been tested against the charts in the
package examples and a few Excel files. Expect rough edges; please
report charts that do not load or do not render the same after loading.

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

# append a row and point the chart at the longer range
wb$add_data(x = data.frame(Month = "Jul", Sales = 240), dims = "A8", col_names = FALSE)
chart <- ec_load(wb)
chart$update_series(data = wb_data(wb), label = Month)
wb$add_encharter(dims = "D20:L36", graph = chart)
```
