# encharter 0.12 (development)

## Bug fixes

* Bar series are written with `<c:invertIfNegative val="0"/>` unless
  `invert_if_negative = TRUE`. Without the element, negative bars appeared
  inverted (white with an outline) in Excel.

## New features

* `encharter_load()` / `ec_load()` read a chart from a workbook back into a
  `Chart` or `ChartEx` object, so charts can be reloaded, modified and added
  again. Charts written by other applications load as well. New `$update_series()`
  on both classes to change an existing series. Loading has seen limited testing;
  unusual charts may not survive a round trip unchanged.

* Chart templates: `encharter_to_crtx()` / `ec_to_crtx()` save a `Chart` as a
  chart template (`.crtx`), `encharter_from_crtx()` / `ec_from_crtx()`
  read one into a `Chart` whose `$add_series()` picks up the template's series
  styling, and `$apply_crtx()` copies a template's styling onto an existing
  chart.

* Loading keeps more of what a file says: deleted axes, per-series data
  label settings, labels and formatting of single points (`c:dLbl`,
  `c:dPt`, including manual label offsets), and a fixed plot area position
  (`c:manualLayout`). `render()` writes all of these back.

* Text properties round-trip more faithfully: a chart-level default text
  style (`c:chartSpace/c:txPr`, new field `$text_style`) is loaded, written and
  used by `plot()` for text without its own; the paragraph alignment of data
  labels (`align`), leader line settings (`leader_lines`), explicit "no fill"
  and "no line" of the chart and plot area (`"none"`), and literal categories
  next to referenced values are kept, as are text box insets and anchoring
  and the category axis label offset (`label_offset`). Text without an
  explicit size no longer gets `sz="1000"` written, so it inherits the chart
  default. Axis bounds, units and cached values are written without
  scientific notation. Outlines of bars, areas and single points (`border`)
  and the line width of category axes are kept, and horizontal bar charts
  get their axes positioned left and bottom. Templates hand bar direction,
  grouping, overlap, gap width, outlines and label settings of their series
  to series added later, and `$apply_crtx()` copies the chart text defaults.

* `plot()` methods for `Chart` and `ChartEx` objects draw the chart with grid,
  following what a spreadsheet application renders from the written OOXML:
  automatic axis scaling, primary and secondary axes, tick marks, gridlines,
  legend, data labels, trendlines, error bars, and the extended types waterfall,
  box-and-whisker, histogram, Pareto, funnel, treemap and sunburst. 3D, stock,
  surface, pie-of-pie and region map charts are not drawn.

# encharter 0.11

## New features

* Complete coverage of the ECMA-376 plot area chart types. Newly supported:
  `ofPieChart` (Pie of Pie / Bar of Pie, incl. `barOfPie` alias and
  `$set_of_pie_options()` for subtype, second plot size, and split logic) and
  the 3D types `bar3DChart`, `line3DChart`, `pie3DChart`, `area3DChart`, and
  `surface3DChart` with a new `$set_3d_options()` for the 3D view (rotation,
  perspective, height/depth percent, right-angle axes, gap depth, bar shape).

* New niche axis options: `tick_lbl_skip`, `tick_mark_skip` (category axes)
  and `disp_units` (value axes, built-in units like `"thousands"` or a custom
  numeric unit) on all `set_*_axis()` methods.

* `$set_data_label_style()` gains `show_ser_name`, `show_percent`,
  `show_bubble_size`, and a `format` number format string (the latter now
  also works for ChartEx labels).

* Input validation against the OOXML schema (ECMA-376 dml-chart.xsd). Values
  that would produce files Excel repairs or refuses to load now error at input
  time (e.g. `overlap` outside -100..100, `gap_width` > 500, `marker_size`
  outside 2..72, `log_base` outside 2..1000, polynomial trendline `order`
  outside 2..6). Structural problems Excel bails on despite valid XML are
  warned about at render time (stock charts without 3-4 series, pie types
  combined with axis-based types, log scale with non-positive minimum).
  Harmless out-of-schema values that Excel renders fine (e.g. `hole_size = 0`)
  remain accepted.

* Colors are validated at input time; R color names (e.g. `"red"`,
  `"steelblue"`) are now converted to hex instead of silently rendering black.

* `ChartEx$set_waterfall_colors(increase, decrease, total)` sets the semantic
  waterfall colors by replacing the first three entries of the chart's color
  style part (`colors{n}.xml`), which is what waterfall maps Increase,
  Decrease, and Total onto. Both the bars and the legend keys reflect the
  colors. Accepts hex, R color names, and `openxlsx2::wb_color()` including
  theme colors. Use a per-point `color` vector in `add_series()` for
  individual outliers instead.

* `ChartEx$set_color_cycle(colors)` replaces (and if needed extends) the
  color style part's cycle for all extended chart types: treemap/sunburst
  category colors, box & whisker and histogram series, funnel and Pareto
  colors — bars and legend alike. `ChartEx$set_region_map_colors(min, max,
  mid)` sets a region map's two- or three-color scale via the series'
  `cx:valueColors`.

* Chart options newly covered: trendline `forward`/`backward`/`intercept`,
  `error_bars$axis` (`"x"` error bars on scatter charts),
  `invert_if_negative` for bar series, and `size_represents` (`"area"`/`"w"`)
  in `set_bubble_options()`.

## Fixes

* `error_bars$direction` was documented but ignored (always `"both"`); it is
  now written to `errBarType`.

* ChartEx per-point colors were written as `cx:dPt` and ignored by Excel;
  the correct element is `cx:dataPt`, so passing a color vector (e.g. a
  corporate palette) to `add_series()` now colors extended charts the same
  way it colors standard charts.

* Legend settings that are valid for standard charts no longer produce broken
  ChartEx XML: `pos = "tr"` maps to `"t"` and side-style `align` values map
  onto the chartex `min`/`ctr`/`max` enum.

* ChartEx axes emitted `cx:spPr` before the gridline/tick/label elements;
  `CT_Axis` places it after `tickLabels` and `numFmt`. Excel tolerated the
  old order, but SDK-based validation (e.g. OOXMLValidator) flags it. Output
  changes for chartex axes: identical elements, schema order. A structural
  chartex validator driven by the Open XML SDK schema data ships in
  `inst/tools/cx_structure_check.py`.

* Color name resolution is delegated to `openxlsx2::wb_color()` instead of a
  separate implementation.

# encharter 0.10

## New features

* Support for `rev` to reverse axis [#11](https://github.com/JanMarvin/encharter/pull/11)

## Fixes

* Support for `log_base` argument (previously its input was not used) ([#10](https://github.com/JanMarvin/encharter/pull/10), @beckmj).

* Added a `NEWS.md` file to track changes to the package.
