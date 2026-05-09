# Encharter Base R6 Class

Abstract base class inherited by `Chart` and `ChartEx`. Holds all shared
fields (palette, titles, axis params, legend/label settings) and the
shared private helpers (`render_color_core`, `render_color`,
`set_axis_params`, `validate_input`).

Users should not instantiate `EncharterBase` directly; use
[`encharter()`](https://janmarvin.github.io/encharter/reference/encharter.md)
instead.

## Public fields

- `xml`:

  The raw xml2 object containing the chart space.

- `series_data`:

  A list containing all added data series and their styles.

- `type`:

  The default chart type for the object (e.g., `"lineChart"`).

- `palette`:

  A character vector of six-digit hex colors used for series when no
  explicit color is supplied. Defaults to the standard Office theme
  palette.

- `chart_title`:

  Named list with elements `text` (character) and `style` (list of
  font/fill/line options) for the main chart title.

- `x_title`:

  Named list with elements `text` and `style` for the primary X-axis
  title.

- `y_title`:

  Named list with elements `text` and `style` for the primary Y-axis
  title.

- `chart_style`:

  Named list controlling the outer chart area: `fill` (hex), `line`
  (hex), `line_width` (numeric).

- `plot_style`:

  Named list controlling the inner plot area: `fill` (hex), `line`
  (hex), `line_width` (numeric).

- `label_params`:

  Named list of global data label defaults: `show_val`, `show_cat`,
  `show_legend_key` (logicals), `pos` (character), `style` (list).

- `legend_params`:

  Named list of legend defaults: `pos` (character), `overlay` ("0"/"1"),
  `style` (list).

- `axis_params`:

  Named list with one entry per axis (`x`, `x2`, `y`, `y2`). Each entry
  is a named list of scaling, formatting, and style parameters. Modified
  via `$set_x_axis()`, etc.

## Methods

### Public methods

- [`EncharterBase$set_chart_title()`](#method-EncharterBase-set_chart_title)

- [`EncharterBase$set_x_title()`](#method-EncharterBase-set_x_title)

- [`EncharterBase$set_y_title()`](#method-EncharterBase-set_y_title)

- [`EncharterBase$set_x_axis()`](#method-EncharterBase-set_x_axis)

- [`EncharterBase$set_y_axis()`](#method-EncharterBase-set_y_axis)

- [`EncharterBase$set_data_label_style()`](#method-EncharterBase-set_data_label_style)

- [`EncharterBase$set_legend_style()`](#method-EncharterBase-set_legend_style)

- [`EncharterBase$set_chart_style()`](#method-EncharterBase-set_chart_style)

- [`EncharterBase$set_plot_style()`](#method-EncharterBase-set_plot_style)

- [`EncharterBase$print()`](#method-EncharterBase-print)

- [`EncharterBase$clone()`](#method-EncharterBase-clone)

------------------------------------------------------------------------

### `EncharterBase$set_chart_title()`

Set the chart's main title.

#### Usage

    EncharterBase$set_chart_title(
      text,
      font_size = NULL,
      font_name = NULL,
      font_color = NULL,
      bold = NULL,
      italic = NULL,
      fill = NULL,
      line = NULL,
      line_width = NULL
    )

#### Arguments

- `text`:

  Title string. Accepts a plain character or an
  [`openxlsx2::fmt_txt()`](https://janmarvin.github.io/openxlsx2/reference/fmt_txt.html)
  object for rich-text formatting.

- `font_size`:

  Numeric font size in points (e.g. `14`).

- `font_name`:

  Font typeface name (e.g. `"Arial"`).

- `font_color`:

  Six-digit hex color for the title text (e.g. `"FF0000"` for red).

- `bold`:

  Logical; `TRUE` renders the title in bold.

- `italic`:

  Logical; `TRUE` renders the title in italics.

- `fill`:

  Six-digit hex color for the title background box.

- `line`:

  Six-digit hex color for the title border.

- `line_width`:

  Numeric border width in points.

#### Examples

    ec("line")$set_chart_title("Monthly Sales", font_size = 14, bold = TRUE)

------------------------------------------------------------------------

### `EncharterBase$set_x_title()`

Set the primary X-axis title.

#### Usage

    EncharterBase$set_x_title(
      text,
      font_size = NULL,
      font_name = NULL,
      font_color = NULL,
      bold = NULL,
      italic = NULL,
      fill = NULL,
      line = NULL,
      line_width = NULL
    )

#### Arguments

- `text`:

  Title string.

- `font_size`:

  Numeric font size in points.

- `font_name`:

  Font typeface name.

- `font_color`:

  Six-digit hex color for the title text.

- `bold`:

  Logical.

- `italic`:

  Logical.

- `fill`:

  Six-digit hex color for the title background box.

- `line`:

  Six-digit hex color for the title border.

- `line_width`:

  Numeric border width in points.

#### Examples

    ec("line")$set_x_title("Month", font_color = "888888", italic = TRUE)

------------------------------------------------------------------------

### `EncharterBase$set_y_title()`

Set the primary Y-axis title.

#### Usage

    EncharterBase$set_y_title(
      text,
      font_size = NULL,
      font_name = NULL,
      font_color = NULL,
      bold = NULL,
      italic = NULL,
      fill = NULL,
      line = NULL,
      line_width = NULL
    )

#### Arguments

- `text`:

  Title string.

- `font_size`:

  Numeric font size in points.

- `font_name`:

  Font typeface name.

- `font_color`:

  Six-digit hex color for the title text.

- `bold`:

  Logical.

- `italic`:

  Logical.

- `fill`:

  Six-digit hex color for the title background box.

- `line`:

  Six-digit hex color for the title border.

- `line_width`:

  Numeric border width in points.

#### Examples

    ec("line")$set_y_title("Revenue (USD)", bold = TRUE)

------------------------------------------------------------------------

### `EncharterBase$set_x_axis()`

Set primary X-axis scaling, tick marks, and label formatting.

#### Usage

    EncharterBase$set_x_axis(
      min = NULL,
      max = NULL,
      major = NULL,
      minor = NULL,
      major_time = NULL,
      minor_time = NULL,
      base_time = NULL,
      major_tick = NULL,
      minor_tick = NULL,
      format = NULL,
      log_base = NULL,
      color = NULL,
      font_name = NULL,
      font_size = NULL,
      bold = NULL,
      italic = NULL,
      font_color = NULL,
      rotation = NULL,
      grid_color = NULL,
      grid_lines = NULL,
      minor_grid_color = NULL,
      minor_grid_lines = NULL,
      cross_between = NULL,
      line_width = NULL,
      grid_width = NULL,
      minor_grid_width = NULL,
      crosses = NULL,
      crosses_at = NULL,
      label_pos = NULL
    )

#### Arguments

- `min, max`:

  Numeric axis limits.

- `major, minor`:

  Numeric major/minor unit intervals. For date axes, unit is set by
  `major_time`/`minor_time`.

- `major_time, minor_time`:

  Time unit for major/minor steps on date axes: `"days"`, `"months"`, or
  `"years"`.

- `base_time`:

  Base time unit for date axes: `"days"`, `"months"`, or `"years"`.

- `major_tick, minor_tick`:

  Tick mark style: `"cross"`, `"in"`, `"out"`, or `"none"`.

- `format`:

  Number or date format string (e.g. `"#,##0"`, `"yyyy-mm-dd"`).

- `log_base`:

  Numeric base for logarithmic scaling (e.g. `10`).

- `color`:

  Six-digit hex color for the axis line.

- `font_name`:

  Font typeface name for tick labels.

- `font_size`:

  Numeric label font size in points.

- `bold, italic`:

  Logical font style for tick labels.

- `font_color`:

  Six-digit hex color for axis tick labels. Defaults to `color` when not
  set.

- `rotation`:

  Numeric label rotation in degrees.

- `grid_color, minor_grid_color`:

  Six-digit hex colors for major/minor grid_lines.

- `grid_lines, minor_grid_lines`:

  Show grid lines. `TRUE`/`FALSE` to toggle; or a dash style string
  (`"dash"`, `"dot"`, `"dashDot"`, etc.) to show styled lines.

- `cross_between`:

  Where the value axis crosses: `"between"` (default, between
  categories) or `"midCat"` (through categories).

- `line_width, grid_width, minor_grid_width`:

  Numeric widths in points for the axis line, major grid lines, and
  minor grid lines respectively.

- `crosses`:

  Where this axis crosses its perpendicular axis: `"autoZero"`
  (default), `"min"`, or `"max"`.

- `crosses_at`:

  Numeric axis value at which to cross. Overrides `crosses` when
  supplied.

- `label_pos`:

  Tick label position: `"nextTo"` (default), `"high"`, `"low"`, or
  `"none"`.

#### Examples

    ec("line")$set_x_axis(
      min = 0, max = 12,
      major_tick = "out",
      grid_lines = TRUE,
      font_color = "666666",
      rotation   = -45
    )

------------------------------------------------------------------------

### `EncharterBase$set_y_axis()`

Set primary Y-axis scaling, tick marks, and label formatting.

#### Usage

    EncharterBase$set_y_axis(
      min = NULL,
      max = NULL,
      major = NULL,
      minor = NULL,
      major_time = NULL,
      minor_time = NULL,
      base_time = NULL,
      major_tick = NULL,
      minor_tick = NULL,
      format = NULL,
      log_base = NULL,
      color = NULL,
      font_name = NULL,
      font_size = NULL,
      bold = NULL,
      italic = NULL,
      font_color = NULL,
      rotation = NULL,
      grid_color = NULL,
      grid_lines = NULL,
      minor_grid_color = NULL,
      minor_grid_lines = NULL,
      cross_between = NULL,
      line_width = NULL,
      grid_width = NULL,
      minor_grid_width = NULL,
      crosses = NULL,
      crosses_at = NULL,
      label_pos = NULL
    )

#### Arguments

- `min, max`:

  Numeric axis limits.

- `major, minor`:

  Numeric major/minor unit intervals.

- `major_time, minor_time`:

  Time unit for date axes: `"days"`, `"months"`, or `"years"`.

- `base_time`:

  Base time unit for date axes.

- `major_tick, minor_tick`:

  Tick mark style: `"cross"`, `"in"`, `"out"`, or `"none"`.

- `format`:

  Number format string.

- `log_base`:

  Numeric base for logarithmic scaling.

- `color`:

  Six-digit hex color for the axis line.

- `font_name`:

  Font typeface name.

- `font_size`:

  Numeric label font size in points.

- `bold, italic`:

  Logical font style.

- `font_color`:

  Six-digit hex color for axis tick labels.

- `rotation`:

  Numeric label rotation in degrees.

- `grid_color, minor_grid_color`:

  Hex colors for major/minor grid lines.

- `grid_lines, minor_grid_lines`:

  `TRUE`/`FALSE` or a dash style string.

- `cross_between`:

  `"between"` or `"midCat"`.

- `line_width, grid_width, minor_grid_width`:

  Numeric widths in points.

- `crosses`:

  `"autoZero"`, `"min"`, or `"max"`.

- `crosses_at`:

  Numeric crossing value; overrides `crosses`.

- `label_pos`:

  `"nextTo"`, `"high"`, `"low"`, or `"none"`.

#### Examples

    ec("bar")$set_y_axis(
      min        = 0,
      max        = 1000,
      major      = 200,
      format     = "#,##0",
      grid_lines = TRUE,
      grid_color = "DDDDDD"
    )

------------------------------------------------------------------------

### `EncharterBase$set_data_label_style()`

Configure global data label defaults for all series.

Per-series overrides can be set via the `show_val`/`show_cat` arguments
in `$add_series()`.

#### Usage

    EncharterBase$set_data_label_style(
      show_val = TRUE,
      show_cat = FALSE,
      show_legend_key = FALSE,
      pos = "t",
      ...
    )

#### Arguments

- `show_val`:

  Logical; show the data point value. Default `TRUE`.

- `show_cat`:

  Logical; show the category name. Default `FALSE`.

- `show_legend_key`:

  Logical; show the series color swatch next to each label. Default
  `FALSE`.

- `pos`:

  Label position: `"t"` (top, default), `"b"` (bottom), `"l"`, `"r"`,
  `"ctr"`, `"inEnd"`, `"outEnd"`, `"bestFit"`.

- `...`:

  Additional font style arguments passed to the label text properties
  (e.g. `font_size`, `font_color`, `bold`).

#### Examples

    ec("bar")$set_data_label_style(show_val = TRUE, pos = "outEnd", font_size = 9)

------------------------------------------------------------------------

### `EncharterBase$set_legend_style()`

Configure the chart legend.

#### Usage

    EncharterBase$set_legend_style(
      pos = "t",
      align = "ctr",
      overlay = FALSE,
      font_size = NULL,
      font_name = NULL,
      bold = NULL,
      italic = NULL,
      color = NULL
    )

#### Arguments

- `pos`:

  Legend position: `"t"`, `"b"`, `"l"`, `"r"` (default), or `"none"` to
  hide.

- `align`:

  Legend alignment relative to the chart: `"ctr"` (default), `"min"`, or
  `"max"`.

- `overlay`:

  Logical; if `TRUE` the legend overlaps the plot area.

- `font_size`:

  Numeric font size in points.

- `font_name`:

  Font typeface name.

- `bold, italic`:

  Logical font style.

- `color`:

  Six-digit hex color for the legend text.

#### Examples

    ec("line")$set_legend_style(pos = "b", font_size = 9)

------------------------------------------------------------------------

### `EncharterBase$set_chart_style()`

Style the outer chart area (background and border).

#### Usage

    EncharterBase$set_chart_style(fill = "FFFFFF", line = NULL, line_width = 1)

#### Arguments

- `fill`:

  Six-digit hex color for the chart background. Default `"FFFFFF"`.

- `line`:

  Six-digit hex color for the chart border. `NULL` for no border.

- `line_width`:

  Numeric border width in points. Default `1`.

#### Examples

    ec("bar")$set_chart_style(fill = "F5F5F5", line = "CCCCCC", line_width = 0.5)

------------------------------------------------------------------------

### `EncharterBase$set_plot_style()`

Style the inner plot area (background and border).

#### Usage

    EncharterBase$set_plot_style(fill = NULL, line = NULL, line_width = 1)

#### Arguments

- `fill`:

  Six-digit hex color for the plot area background. `NULL` for
  transparent.

- `line`:

  Six-digit hex color for the plot area border.

- `line_width`:

  Numeric border width in points. Default `1`.

#### Examples

    ec("line")$set_plot_style(fill = "FAFAFA")

------------------------------------------------------------------------

### `EncharterBase$print()`

Print a summary of the chart object.

#### Usage

    EncharterBase$print()

#### Examples

    ec("line")

------------------------------------------------------------------------

### `EncharterBase$clone()`

The objects of this class are cloneable with this method.

#### Usage

    EncharterBase$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r

## ------------------------------------------------
## Method `EncharterBase$set_chart_title()`
## ------------------------------------------------

ec("line")$set_chart_title("Monthly Sales", font_size = 14, bold = TRUE)

## ------------------------------------------------
## Method `EncharterBase$set_x_title()`
## ------------------------------------------------

ec("line")$set_x_title("Month", font_color = "888888", italic = TRUE)

## ------------------------------------------------
## Method `EncharterBase$set_y_title()`
## ------------------------------------------------

ec("line")$set_y_title("Revenue (USD)", bold = TRUE)

## ------------------------------------------------
## Method `EncharterBase$set_x_axis()`
## ------------------------------------------------

ec("line")$set_x_axis(
  min = 0, max = 12,
  major_tick = "out",
  grid_lines = TRUE,
  font_color = "666666",
  rotation   = -45
)

## ------------------------------------------------
## Method `EncharterBase$set_y_axis()`
## ------------------------------------------------

ec("bar")$set_y_axis(
  min        = 0,
  max        = 1000,
  major      = 200,
  format     = "#,##0",
  grid_lines = TRUE,
  grid_color = "DDDDDD"
)

## ------------------------------------------------
## Method `EncharterBase$set_data_label_style()`
## ------------------------------------------------

ec("bar")$set_data_label_style(show_val = TRUE, pos = "outEnd", font_size = 9)

## ------------------------------------------------
## Method `EncharterBase$set_legend_style()`
## ------------------------------------------------

ec("line")$set_legend_style(pos = "b", font_size = 9)

## ------------------------------------------------
## Method `EncharterBase$set_chart_style()`
## ------------------------------------------------

ec("bar")$set_chart_style(fill = "F5F5F5", line = "CCCCCC", line_width = 0.5)

## ------------------------------------------------
## Method `EncharterBase$set_plot_style()`
## ------------------------------------------------

ec("line")$set_plot_style(fill = "FAFAFA")

## ------------------------------------------------
## Method `EncharterBase$print()`
## ------------------------------------------------

ec("line")
#> An encharter object
#> Number of Series: 0 
```
