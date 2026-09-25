is_missing <- function(x) {
  is.null(x) || xml_type(x) == "missing"
}

attr_or_null <- function(node, attr) {
  if (is_missing(node)) return(NULL)
  val <- xml_attr(node, attr)
  if (nzchar(val)) xml_unescape(val) else NULL
}

num_or_null <- function(node, attr = "val") {
  val <- attr_or_null(node, attr)
  if (is.null(val)) NULL else as.numeric(val)
}

int_or_null <- function(node, attr = "val") {
  val <- attr_or_null(node, attr)
  if (is.null(val)) NULL else as.integer(val)
}

emu_to_pt <- function(node, attr = "w") {
  val <- attr_or_null(node, attr)
  if (is.null(val)) NULL else as.numeric(val) / 12700
}

# Inverse of render_color_core(): reads the color child of `node`
# (a:solidFill, a:ln, a:defRPr, ...). Returns NULL when there is none.
load_color <- function(node) {
  if (is_missing(node)) return(NULL)
  srgb <- xml_find_first(node, "./a:srgbClr")
  if (!is_missing(srgb)) {
    hex <- toupper(xml_attr(srgb, "val"))
    alpha <- xml_find_first(srgb, "./a:alpha")
    if (!is_missing(alpha)) {
      aa <- round(as.numeric(xml_attr(alpha, "val")) / 100000 * 255)
      hex <- paste0(sprintf("%02X", as.integer(aa)), hex)
    }
    return(hex)
  }
  scheme <- xml_find_first(node, "./a:schemeClr")
  if (!is_missing(scheme)) {
    val <- xml_attr(scheme, "val")
    if (val == "accent1") return("auto")
    col <- openxlsx2::wb_color(theme = val)
    # luminance modifiers of the scheme color (e.g. tx1 at 65% + 35% for
    # the default gray text) are kept for plot(); render() writes the
    # plain scheme color
    lum_mod <- xml_find_first(scheme, "./a:lumMod")
    lum_off <- xml_find_first(scheme, "./a:lumOff")
    if (!is_missing(lum_mod)) attr(col, "lumMod") <- as.numeric(xml_attr(lum_mod, "val")) / 100000
    if (!is_missing(lum_off)) attr(col, "lumOff") <- as.numeric(xml_attr(lum_off, "val")) / 100000
    return(col)
  }
  NULL
}

# Inverse of render_line_style(): returns list(color, width, type, show)
load_line_style <- function(sppr) {
  out <- list(color = NULL, width = NULL, type = NULL, show = TRUE)
  if (is_missing(sppr)) return(out)
  ln <- xml_find_first(sppr, "./a:ln")
  if (is_missing(ln)) {
    if (!is_missing(xml_find_first(sppr, "./a:noFill"))) out$show <- FALSE
    return(out)
  }
  if (!is_missing(xml_find_first(ln, "./a:noFill"))) {
    out$show <- FALSE
    return(out)
  }
  out$color <- load_color(xml_find_first(ln, "./a:solidFill"))
  out$width <- emu_to_pt(ln)
  out$type  <- attr_or_null(xml_find_first(ln, "./a:prstDash"), "val")
  out
}

# Inverse of apply_sp_pr(): returns list(fill, line, line_width)
load_sp_pr <- function(sppr) {
  out <- list(fill = NULL, line = NULL, line_width = 1)
  if (is_missing(sppr)) return(out)
  out$fill <- load_color(xml_find_first(sppr, "./a:solidFill"))
  if (!is_missing(xml_find_first(sppr, "./a:noFill"))) out$fill <- "none"
  ln <- load_line_style(sppr)
  if (isTRUE(ln$show) && !is.null(ln$color)) {
    out$line <- ln$color
    out$line_width <- ln$width %||% 1
  } else if (!isTRUE(ln$show) && !is_missing(xml_find_first(sppr, "./a:ln"))) {
    out$line <- "none"
  }
  out
}

# Inverse of apply_text_style(): reads c:txPr below `node`
load_text_style <- function(node) {
  txpr <- xml_find_first(node, "./c:txPr")
  if (is_missing(txpr)) return(NULL)
  body <- xml_find_first(txpr, "./a:bodyPr")
  rpr  <- xml_find_first(txpr, "./a:p/a:pPr/a:defRPr")
  sz   <- num_or_null(rpr, "sz")
  rot  <- num_or_null(body, "rot")
  latin <- xml_find_first(rpr, "./a:latin")
  list(
    font_size  = if (is.null(sz)) NULL else sz / 100,
    font_name  = attr_or_null(latin, "typeface"),
    bold       = if (identical(attr_or_null(rpr, "b"), "1")) TRUE else NULL,
    italic     = if (identical(attr_or_null(rpr, "i"), "1")) TRUE else NULL,
    font_color = load_color(xml_find_first(rpr, "./a:solidFill")),
    rotation   = if (is.null(rot)) NULL else rot / 60000,
    align      = attr_or_null(xml_find_first(txpr, "./a:p/a:pPr"), "algn"),
    # insets and anchoring of the text box, kept as written
    body_pr    = Filter(Negate(is.null), sapply(c("lIns", "tIns", "rIns", "bIns", "wrap", "anchor", "anchorCtr"), function(a) attr_or_null(body, a), simplify = FALSE)),
    auto_fit   = if (is_missing(xml_find_first(body, "./a:spAutoFit"))) NULL else TRUE
  )
}

flag_or_null <- function(node, attr) {
  val <- attr_or_null(node, attr)
  if (is.null(val)) NULL else val == "1"
}

# Inverse of Chart$add_title_content() and ChartEx$add_rich_text(): returns
# list(text, style). A title with a single plain run is returned as text
# plus style; anything else (several runs, underline, strike, alpha colors)
# is rebuilt as an openxlsx2::fmt_txt() object.
load_title <- function(title_node, default_sz, ns = "c") {
  if (is_missing(title_node)) return(list(text = NULL, style = list()))
  runs <- xml_find_all(title_node, sprintf("./%s:tx/%s:rich/a:p/a:r", ns, ns))
  if (length(runs) == 0) return(list(text = NULL, style = list()))

  sppr <- load_sp_pr(xml_find_first(title_node, sprintf("./%s:spPr", ns)))
  rot  <- num_or_null(xml_find_first(title_node, sprintf("./%s:tx/%s:rich/a:bodyPr", ns, ns)), "rot")
  extra <- list(fill = sppr$fill, line = sppr$line,
                line_width = if (is.null(sppr$line)) NULL else sppr$line_width,
                rotation = if (is.null(rot)) NULL else rot / 60000)

  rpr <- xml_find_first(runs[[1]], "./a:rPr")
  is_plain <- length(runs) == 1 &&
    is_missing(xml_find_first(rpr, "./a:solidFill/a:srgbClr/a:alpha")) &&
    is_missing(xml_find_first(rpr, "./a:cs")) &&
    is.null(attr_or_null(rpr, "strike")) &&
    is.null(attr_or_null(rpr, "u"))

  if (is_plain) {
    sz <- num_or_null(rpr, "sz")
    latin <- xml_find_first(rpr, "./a:latin")
    style <- list(
      font_size  = if (is.null(sz) || identical(sz, default_sz)) NULL else sz / 100,
      font_name  = attr_or_null(latin, "typeface"),
      font_color = load_color(xml_find_first(rpr, "./a:solidFill")),
      bold       = flag_or_null(rpr, "b"),
      italic     = flag_or_null(rpr, "i")
    )
    return(list(text = xml_text(xml_find_first(runs[[1]], "./a:t")), style = c(style, extra)))
  }

  text <- NULL
  for (r in runs) {
    rpr <- xml_find_first(r, "./a:rPr")
    sz  <- num_or_null(rpr, "sz")
    latin <- xml_find_first(rpr, "./a:latin")
    srgb  <- xml_find_first(rpr, "./a:solidFill/a:srgbClr")
    run <- openxlsx2::fmt_txt(
      xml_text(xml_find_first(r, "./a:t")),
      bold      = identical(attr_or_null(rpr, "b"), "1"),
      italic    = identical(attr_or_null(rpr, "i"), "1"),
      underline = identical(attr_or_null(rpr, "u"), "sng"),
      strike    = identical(attr_or_null(rpr, "strike"), "sngStrike"),
      size      = if (is.null(sz)) NULL else sz / 100,
      color     = if (is_missing(srgb)) NULL else openxlsx2::wb_color(xml_attr(srgb, "val")),
      font      = attr_or_null(latin, "typeface"),
      charset   = attr_or_null(latin, "charset")
    )
    text <- if (is.null(text)) run else text + run
  }
  list(text = text, style = extra)
}

# Inverse of render_num_cache() / render_str_cache(). Returns NULL when the
# reference carries no cache.
load_cache <- function(ref_node) {
  nm <- xml_name(ref_node)
  if (nm %in% c("c:numLit", "c:strLit")) {
    cache <- ref_node
    is_num <- nm == "c:numLit"
  } else {
    cache <- xml_find_first(ref_node, "./c:numCache")
    is_num <- !is_missing(cache)
    if (!is_num) cache <- xml_find_first(ref_node, "./c:strCache")
    if (is_missing(cache)) return(NULL)
  }

  n <- as.integer(xml_attr(xml_find_first(cache, "./c:ptCount"), "val"))
  pts <- xml_find_all(cache, "./c:pt")
  idx <- integer()
  txt <- character()
  if (length(pts)) {
    idx <- as.integer(xml_attr(pts, "idx")) + 1L
    txt <- xml_text(xml_find_all(pts, "./c:v"))
  }

  if (!is_num) {
    vals <- rep(NA_character_, n)
    vals[idx] <- txt
    return(vals)
  }

  vals <- rep(NA_real_, n)
  vals[idx] <- as.numeric(txt)
  fmt <- xml_find_first(cache, "./c:formatCode")
  if (is_missing(fmt)) return(vals)
  fmt <- tolower(xml_text(fmt))
  is_date_fmt <- grepl("[ymdh]", fmt) && !grepl("[0#?]|general", fmt)
  if (!is_date_fmt) return(vals)
  if (all(is.na(vals) | vals == trunc(vals))) {
    as.Date(vals, origin = "1899-12-30")
  } else {
    as.POSIXct(vals * 86400, origin = "1899-12-30", tz = "UTC")
  }
}

# Reads c:f and the cache of a data reference node (c:val, c:cat, c:xVal, ...)
load_data_ref <- function(node) {
  if (is_missing(node)) return(list(ref = NULL, cache = NULL))
  ref_node <- xml_children(node)[[1]]
  f <- xml_find_first(ref_node, "./c:f")
  list(
    ref   = if (is_missing(f)) NULL else xml_text(f),
    cache = load_cache(ref_node)
  )
}

# Assigns "x", "y", "x2", "y2" to the axis nodes of a plot area. Category
# axes are x axes in document order; value axes are y axes in document
# order. When there are only value axes (scatter, bubble), the ones placed
# at the bottom or top are the x axes.
axis_roles <- function(ax_nodes) {
  if (length(ax_nodes) == 0) return(character())
  names <- xml_name(ax_nodes)
  pos <- vapply(ax_nodes, function(ax) xml_attr(xml_find_first(ax, "./c:axPos"), "val"), character(1))
  is_x <- if (all(names == "c:valAx")) pos %in% c("b", "t") else names != "c:valAx"
  roles <- rep(NA_character_, length(ax_nodes))
  roles[is_x]  <- c("x", "x2", rep(NA_character_, sum(is_x)))[seq_len(sum(is_x))]
  roles[!is_x] <- c("y", "y2", rep(NA_character_, sum(!is_x)))[seq_len(sum(!is_x))]
  roles
}

# Inverse of render_cat_ax() / render_val_ax(): returns the axis_params entry
load_axis <- function(ax, defaults) {
  p <- defaults
  scaling <- xml_find_first(ax, "./c:scaling")
  upd <- list(
    rev      = if (identical(attr_or_null(xml_find_first(scaling, "./c:orientation"), "val"), "maxMin")) TRUE else NULL,
    max      = num_or_null(xml_find_first(scaling, "./c:max")),
    min      = num_or_null(xml_find_first(scaling, "./c:min")),
    log_base = num_or_null(xml_find_first(scaling, "./c:logBase")),
    # a deleted axis is kept in the file but not drawn
    delete   = if (identical(attr_or_null(xml_find_first(ax, "./c:delete"), "val"), "1")) TRUE else NULL,
    # auto = 0 on a category axis keeps date categories as text
    auto     = if (identical(attr_or_null(xml_find_first(ax, "./c:auto"), "val"), "0")) FALSE else NULL,
    label_offset = int_or_null(xml_find_first(ax, "./c:lblOffset"))
  )

  for (which in c("major", "minor")) {
    gl <- xml_find_first(ax, sprintf("./c:%sGridlines", which))
    key <- if (which == "major") "" else "minor_"
    if (is_missing(gl)) {
      upd[[paste0(key, "grid_lines")]] <- FALSE
      next
    }
    ln <- load_line_style(xml_find_first(gl, "./c:spPr"))
    upd[[paste0(key, "grid_lines")]] <- ln$type %||% TRUE
    upd[[paste0(key, "grid_color")]] <- ln$color
    upd[[paste0(key, "grid_width")]] <- ln$width %||% 1
  }

  upd$format     <- attr_or_null(xml_find_first(ax, "./c:numFmt"), "formatCode")
  upd$major_tick <- attr_or_null(xml_find_first(ax, "./c:majorTickMark"), "val")
  upd$minor_tick <- attr_or_null(xml_find_first(ax, "./c:minorTickMark"), "val")
  upd$label_pos  <- attr_or_null(xml_find_first(ax, "./c:tickLblPos"), "val") %||% "nextTo"

  ln <- load_line_style(xml_find_first(ax, "./c:spPr"))
  upd$color <- if (isFALSE(ln$show)) "none" else ln$color %||% "000000"
  if (!is.null(ln$width)) upd$line_width <- ln$width

  ts <- load_text_style(ax)
  if (!is.null(ts)) upd[names(ts)] <- ts

  upd$crosses_at    <- num_or_null(xml_find_first(ax, "./c:crossesAt"))
  upd$crosses       <- attr_or_null(xml_find_first(ax, "./c:crosses"), "val")
  upd$cross_between <- attr_or_null(xml_find_first(ax, "./c:crossBetween"), "val") %||% p$cross_between
  upd$major         <- num_or_null(xml_find_first(ax, "./c:majorUnit"))
  upd$minor         <- num_or_null(xml_find_first(ax, "./c:minorUnit"))

  du <- xml_find_first(ax, "./c:dispUnits")
  if (!is_missing(du)) {
    upd$disp_units <- num_or_null(xml_find_first(du, "./c:custUnit")) %||%
      attr_or_null(xml_find_first(du, "./c:builtInUnit"), "val")
  }

  upd$tick_lbl_skip  <- int_or_null(xml_find_first(ax, "./c:tickLblSkip"))
  upd$tick_mark_skip <- int_or_null(xml_find_first(ax, "./c:tickMarkSkip"))

  if (xml_name(ax) == "c:dateAx") {
    upd$base_time  <- attr_or_null(xml_find_first(ax, "./c:baseTimeUnit"), "val")
    upd$major_time <- attr_or_null(xml_find_first(ax, "./c:majorTimeUnit"), "val")
    upd$minor_time <- attr_or_null(xml_find_first(ax, "./c:minorTimeUnit"), "val")
    if (is.null(upd$base_time) && is.null(upd$major_time) && is.null(upd$minor_time)) {
      upd$major_time <- "days"
    }
  }
  p[names(upd)] <- upd
  p
}

# Inverse of the c:dLbls block written in render_series_node()
load_label_params <- function(dlbls, type) {
  flag <- function(name) identical(attr_or_null(xml_find_first(dlbls, name), "val"), "1")
  # without a position the default of the chart type applies; it is filled
  # in once the grouping is known
  pos <- attr_or_null(xml_find_first(dlbls, "./c:dLblPos"), "val")
  if (type == "barChart" && !is.null(pos)) {
    if (pos == "outEnd") pos <- "t"
    if (pos == "inBase") pos <- "b"
  }
  ts <- load_text_style(dlbls)
  style <- if (is.null(ts)) list() else ts[c("font_size", "font_name", "bold", "italic", "font_color", "align", "body_pr", "auto_fit")]
  list(
    show_val         = flag("./c:showVal"),
    show_cat         = flag("./c:showCatName"),
    show_legend_key  = flag("./c:showLegendKey"),
    show_ser_name    = flag("./c:showSerName"),
    show_percent     = flag("./c:showPercent"),
    show_bubble_size = flag("./c:showBubbleSize"),
    pos    = pos,
    format = attr_or_null(xml_find_first(dlbls, "./c:numFmt"), "formatCode"),
    sep    = if (is_missing(xml_find_first(dlbls, "./c:separator"))) NULL else xml_text(xml_find_first(dlbls, "./c:separator")),
    fill   = load_color(xml_find_first(dlbls, "./c:spPr/a:solidFill")),
    leader_lines = flag_or_null(xml_find_first(dlbls, "./c:extLst/c:ext/c15:showLeaderLines"), "val") %||%
      flag_or_null(xml_find_first(dlbls, "./c:showLeaderLines"), "val"),
    style  = style
  )
}

# Inverse of the per-series part of render_series_node()
load_series <- function(ser, type, chart) {
  name <- NULL
  name_cache <- NULL
  tx <- xml_find_first(ser, "./c:tx")
  if (!is_missing(tx)) {
    f <- xml_find_first(tx, "./c:strRef/c:f")
    v <- xml_find_first(tx, "./c:v")
    name <- if (!is_missing(f)) xml_text(f) else if (!is_missing(v)) xml_text(v) else NULL
    cache <- xml_find_first(tx, "./c:strRef/c:strCache/c:pt/c:v")
    if (!is_missing(cache)) name_cache <- xml_text(cache)
  }

  color <- "4472C4"
  line <- list(color = color, width = 1, type = NULL, show = TRUE)
  sppr <- xml_find_first(ser, "./c:spPr")
  border <- NULL
  if (type %in% c("barChart", "areaChart", "bubbleChart", "bar3DChart", "area3DChart")) {
    fill <- load_color(xml_find_first(sppr, "./a:solidFill"))
    if (!is.null(fill)) line$color <- fill
    # the outline of bars and areas
    ls <- load_line_style(sppr)
    if (isTRUE(ls$show) && !is.null(ls$color)) border <- list(color = ls$color, width = ls$width %||% 0.75)
  } else if (type %in% c("lineChart", "scatterChart", "stockChart", "line3DChart")) {
    ls <- load_line_style(sppr)
    line$show <- ls$show
    if (!is.null(ls$color)) line$color <- ls$color
    if (!is.null(ls$width)) line$width <- ls$width
    line["type"] <- list(ls$type)
  }
  color <- line$color

  marker <- list(symbol = "none", size = 5, fill = color,
                 line = list(color = color, width = 0.75, show = TRUE))
  mkr <- xml_find_first(ser, "./c:marker")
  if (!is_missing(mkr)) {
    marker$symbol <- attr_or_null(xml_find_first(mkr, "./c:symbol"), "val") %||% "none"
    marker$size   <- int_or_null(xml_find_first(mkr, "./c:size")) %||% 5L
    m_sppr <- xml_find_first(mkr, "./c:spPr")
    if (!is_missing(m_sppr)) {
      kids <- xml_name(xml_children(m_sppr))
      if (kids[1] == "a:noFill") {
        marker$fill <- "none"
      } else {
        marker$fill <- load_color(xml_find_first(m_sppr, "./a:solidFill")) %||% color
      }
      ls <- load_line_style(m_sppr)
      if (!is.null(ls$color)) marker$line$color <- ls$color
      if (!is.null(ls$width)) marker$line$width <- ls$width
      if (!is.null(ls$type))  marker$line$type  <- ls$type
    }
  }

  trendline <- FALSE
  tl <- xml_find_first(ser, "./c:trendline")
  if (!is_missing(tl)) {
    tl_name <- xml_find_first(tl, "./c:name")
    trendline <- list(
      type      = attr_or_null(xml_find_first(tl, "./c:trendlineType"), "val") %||% "linear",
      name      = if (is_missing(tl_name)) NULL else xml_text(tl_name),
      color     = load_color(xml_find_first(tl, "./c:spPr/a:ln/a:solidFill")),
      order     = int_or_null(xml_find_first(tl, "./c:order")),
      period    = int_or_null(xml_find_first(tl, "./c:period")),
      forward   = num_or_null(xml_find_first(tl, "./c:forward")),
      backward  = num_or_null(xml_find_first(tl, "./c:backward")),
      intercept = num_or_null(xml_find_first(tl, "./c:intercept")),
      show_r2   = if (identical(attr_or_null(xml_find_first(tl, "./c:dispRSqr"), "val"), "0")) FALSE else NULL,
      show_eq   = if (identical(attr_or_null(xml_find_first(tl, "./c:dispEq"), "val"), "0")) FALSE else NULL
    )
    trendline <- Filter(Negate(is.null), trendline)
  }

  error_bars <- FALSE
  eb <- xml_find_first(ser, "./c:errBars")
  if (!is_missing(eb)) {
    error_bars <- list(
      type      = attr_or_null(xml_find_first(eb, "./c:errValType"), "val") %||% "fixedVal",
      value     = num_or_null(xml_find_first(eb, "./c:val")) %||% 5,
      direction = attr_or_null(xml_find_first(eb, "./c:errBarType"), "val") %||% "both",
      axis      = attr_or_null(xml_find_first(eb, "./c:errDir"), "val") %||% "y",
      color     = load_color(xml_find_first(eb, "./c:spPr/a:ln/a:solidFill"))
    )
    error_bars <- Filter(Negate(is.null), error_bars)
  }

  if (type %in% c("scatterChart", "bubbleChart")) {
    cat_ref  <- load_data_ref(xml_find_first(ser, "./c:xVal"))
    data_ref <- load_data_ref(xml_find_first(ser, "./c:yVal"))
    z_ref    <- load_data_ref(xml_find_first(ser, "./c:bubbleSize"))
  } else {
    cat_ref  <- load_data_ref(xml_find_first(ser, "./c:cat"))
    data_ref <- load_data_ref(xml_find_first(ser, "./c:val"))
    z_ref    <- list(ref = NULL, cache = NULL)
  }

  smooth <- identical(attr_or_null(xml_find_first(ser, "./c:smooth"), "val"), "1")
  # a missing element counts as TRUE, which is how Excel draws it
  invert <- !identical(attr_or_null(xml_find_first(ser, "./c:invertIfNegative"), "val"), "0")
  if (!type %in% c("barChart", "bar3DChart")) invert <- FALSE
  # per-point formatting: the fill of single points, "none" when invisible
  points <- list()
  for (dpt in xml_find_all(ser, "./c:dPt")) {
    idx <- as.integer(xml_attr(xml_find_first(dpt, "./c:idx"), "val"))
    sppr <- xml_find_first(dpt, "./c:spPr")
    fill <- if (is_missing(sppr)) NULL else load_color(xml_find_first(sppr, "./a:solidFill"))
    if (is.null(fill) && !is_missing(sppr) && !is_missing(xml_find_first(sppr, "./a:noFill"))) fill <- "none"
    pt_border <- NULL
    if (!is_missing(sppr)) {
      ls <- load_line_style(sppr)
      if (isTRUE(ls$show) && !is.null(ls$color)) pt_border <- list(color = ls$color, width = ls$width %||% 0.75)
      else if (!isTRUE(ls$show) && !is_missing(xml_find_first(sppr, "./a:ln"))) pt_border <- "none"
    }
    pt_marker <- NULL
    mk <- xml_find_first(dpt, "./c:marker")
    if (!is_missing(mk)) {
      m_sppr <- xml_find_first(mk, "./c:spPr")
      pt_marker <- list(
        symbol = attr_or_null(xml_find_first(mk, "./c:symbol"), "val"),
        size   = int_or_null(xml_find_first(mk, "./c:size")),
        fill   = if (is_missing(m_sppr)) NULL else load_color(xml_find_first(m_sppr, "./a:solidFill"))
      )
    }
    if (is.null(fill) && is.null(pt_marker) && is.null(pt_border)) next
    points[[length(points) + 1]] <- list(idx = idx, color = fill, border = pt_border, marker = pt_marker)
  }

  # label settings of this series, when they differ from the chart's, and
  # the labels of single points
  dlbls <- xml_find_first(ser, "./c:dLbls")
  label_params <- if (is_missing(dlbls)) NULL else load_label_params(dlbls, type)
  point_labels <- list()
  if (!is_missing(dlbls)) {
    for (dl in xml_find_all(dlbls, "./c:dLbl")) {
      idx <- as.integer(xml_attr(xml_find_first(dl, "./c:idx"), "val"))
      if (identical(attr_or_null(xml_find_first(dl, "./c:delete"), "val"), "1")) {
        point_labels[[length(point_labels) + 1]] <- list(idx = idx, delete = TRUE)
        next
      }
      pl <- load_label_params(dl, type)
      # a point without its own position takes the series' one
      if (is_missing(xml_find_first(dl, "./c:dLblPos"))) pl["pos"] <- list(NULL)
      # a manual offset from the default position, as fractions of the chart
      ml <- xml_find_first(dl, "./c:layout/c:manualLayout")
      if (!is_missing(ml)) {
        pl$dx <- num_or_null(xml_find_first(ml, "./c:x")) %||% 0
        pl$dy <- num_or_null(xml_find_first(ml, "./c:y")) %||% 0
      }
      point_labels[[length(point_labels) + 1]] <- c(list(idx = idx, delete = FALSE), pl)
    }
  }
  if (!is.null(label_params) && identical(label_params, chart$label_params)) label_params <- NULL
  # a series without labels of its own stays without labels when the chart's
  # settings would show some
  if (is_missing(dlbls) && plot_labels_on(chart$label_params)) {
    label_params <- list(show_val = FALSE, show_cat = FALSE, show_legend_key = FALSE, show_ser_name = FALSE,
                         show_percent = FALSE, show_bubble_size = FALSE, pos = "t", format = NULL, style = list())
  }

  list(
    name       = name,
    data       = data_ref$ref,
    label      = cat_ref$ref,
    weight     = z_ref$ref,
    name_cache = name_cache,
    data_cache = data_ref$cache,
    cat_cache  = cat_ref$cache,
    z_cache    = z_ref$cache,
    type       = type,
    sec_type   = "none",
    smooth     = smooth,
    filled     = FALSE,
    dir        = "col",
    grouping   = "standard",
    overlap    = NULL,
    gap_width  = NULL,
    error_bars = error_bars,
    trendline  = trendline,
    invert_if_negative = invert,
    line       = line,
    border     = border,
    marker     = marker,
    show_val   = (label_params %||% chart$label_params)$show_val,
    show_cat   = (label_params %||% chart$label_params)$show_cat,
    label_pos  = (label_params %||% chart$label_params)$pos,
    label_params = label_params,
    points       = points,
    point_labels = point_labels
  )
}

load_chart <- function(xml) {
  doc <- read_xml(xml)

  plot_area <- xml_find_first(doc, "/c:chartSpace/c:chart/c:plotArea")
  if (is_missing(plot_area)) stop("No plot area found in the chart XML.", call. = FALSE)
  type_nodes <- Filter(function(nd) xml_name(nd) %in% paste0("c:", ENCHARTER_STANDARD),
                       xml_children(plot_area))
  if (length(type_nodes) == 0) stop("No supported chart type found in the chart XML.", call. = FALSE)

  chart <- Chart$new(type = sub("^c:", "", xml_name(type_nodes[[1]])))
  chart_root <- xml_find_first(doc, "/c:chartSpace/c:chart")

  chart$chart_style <- load_sp_pr(xml_find_first(doc, "/c:chartSpace/c:spPr"))
  ts <- load_text_style(xml_find_first(doc, "/c:chartSpace"))
  chart$text_style  <- if (is.null(ts)) list() else Filter(Negate(is.null), ts[c("font_size", "font_name", "bold", "italic", "font_color", "body_pr")])
  chart$plot_style  <- load_sp_pr(xml_find_first(plot_area, "./c:spPr"))
  chart$chart_title <- load_title(xml_find_first(chart_root, "./c:title"), default_sz = 1400)

  v3d <- xml_find_first(chart_root, "./c:view3D")
  if (!is_missing(v3d)) {
    r_ang <- attr_or_null(xml_find_first(v3d, "./c:rAngAx"), "val")
    chart$view3d <- list(
      rot_x            = int_or_null(xml_find_first(v3d, "./c:rotX")),
      rot_y            = int_or_null(xml_find_first(v3d, "./c:rotY")),
      perspective      = int_or_null(xml_find_first(v3d, "./c:perspective")),
      depth_percent    = int_or_null(xml_find_first(v3d, "./c:depthPercent")),
      h_percent        = int_or_null(xml_find_first(v3d, "./c:hPercent")),
      right_angle_axes = if (is.null(r_ang)) NULL else r_ang == "1"
    )
  }

  ax_nodes <- Filter(function(nd) xml_name(nd) %in% c("c:catAx", "c:dateAx", "c:valAx"),
                     xml_children(plot_area))
  ax_roles <- axis_roles(ax_nodes)
  ax_ids <- list(x = NULL, y = NULL, x2 = NULL, y2 = NULL)
  for (i in seq_along(ax_nodes)) {
    ax <- ax_nodes[[i]]
    which <- ax_roles[i]
    if (is.na(which)) next
    ax_ids[[which]] <- xml_attr(xml_find_first(ax, "./c:axId"), "val")
    chart$axis_params[[which]] <- load_axis(ax, chart$axis_params[[which]])
    title <- load_title(xml_find_first(ax, "./c:title"), default_sz = 1000)
    chart[[paste0(which, "_title")]] <- title
  }

  chart$show_data_table <- !is_missing(xml_find_first(plot_area, "./c:dTable"))

  legend <- xml_find_first(chart_root, "./c:legend")
  if (is_missing(legend)) {
    chart$legend_params <- list(pos = "none", overlay = "0", style = list())
  } else {
    ts <- load_text_style(legend)
    style <- if (is.null(ts)) list() else
      list(font_size = ts$font_size, font_name = ts$font_name, bold = ts$bold,
           italic = ts$italic, color = ts$font_color)
    chart$legend_params <- list(
      pos     = attr_or_null(xml_find_first(legend, "./c:legendPos"), "val") %||% "r",
      overlay = attr_or_null(xml_find_first(legend, "./c:overlay"), "val") %||% "0",
      style   = style
    )
  }

  dba <- attr_or_null(xml_find_first(chart_root, "./c:dispBlanksAs"), "val")
  if (!is.null(dba)) chart$disp_blanks_as <- dba

  # fixed plot area position (edge mode only; offsets from the automatic
  # position are not kept)
  ml <- xml_find_first(plot_area, "./c:layout/c:manualLayout")
  if (!is_missing(ml)) {
    pos <- lapply(c(x = "x", y = "y", w = "w", h = "h"), function(nm) num_or_null(xml_find_first(ml, paste0("./c:", nm))))
    modes <- c(attr_or_null(xml_find_first(ml, "./c:xMode"), "val"), attr_or_null(xml_find_first(ml, "./c:yMode"), "val"))
    if (!any(vapply(pos, is.null, logical(1))) && all(modes == "edge") && length(modes) == 2) {
      chart$plot_layout <- c(pos, list(target = attr_or_null(xml_find_first(ml, "./c:layoutTarget"), "val") %||% "outer"))
    }
  }

  dlbls_all <- xml_find_all(plot_area, ".//c:ser/c:dLbls")
  if (length(dlbls_all)) {
    dlbls_types <- sub("^c:", "", xml_name(xml_find_first(dlbls_all, "../..")))
    lossy <- dlbls_types %in% c("barChart", ENCHARTER_PIE_FAMILY, ENCHARTER_3D)
    pick <- if (any(!lossy)) which(!lossy)[1] else 1L
    chart$label_params <- load_label_params(dlbls_all[[pick]], dlbls_types[pick])
  }

  series_list <- list()
  for (nd in type_nodes) {
    type <- sub("^c:", "", xml_name(nd))
    ids <- xml_attr(xml_find_all(nd, "./c:axId"), "val")
    sec_x <- length(ids) >= 1 && !is.null(ax_ids$x2) && ids[1] == ax_ids$x2
    sec_y <- length(ids) >= 2 && !is.null(ax_ids$y2) && ids[2] == ax_ids$y2
    sec_type <- if (sec_x && sec_y) "xy" else if (sec_y) "y" else if (sec_x) "x" else "none"

    dir       <- attr_or_null(xml_find_first(nd, "./c:barDir"), "val") %||% "col"
    grouping  <- attr_or_null(xml_find_first(nd, "./c:grouping"), "val") %||% "standard"
    # a 3D bar or area chart with a series axis places its series along
    # the depth whatever its grouping says
    if (type %in% c("bar3DChart", "area3DChart") && grouping == "clustered" && length(ids) == 3) grouping <- "standard"
    gap_width <- int_or_null(xml_find_first(nd, "./c:gapWidth"))
    overlap   <- int_or_null(xml_find_first(nd, "./c:overlap"))
    filled    <- identical(attr_or_null(xml_find_first(nd, "./c:radarStyle"), "val"), "filled") ||
      identical(attr_or_null(xml_find_first(nd, "./c:wireframe"), "val"), "1")

    if (type == "ofPieChart") {
      chart$of_pie_type     <- attr_or_null(xml_find_first(nd, "./c:ofPieType"), "val") %||% "pie"
      chart$split_type      <- attr_or_null(xml_find_first(nd, "./c:splitType"), "val")
      chart$second_pie_size <- int_or_null(xml_find_first(nd, "./c:secondPieSize"))
      cust <- xml_find_all(nd, "./c:custSplit/c:secondPiePt")
      if (length(cust)) {
        chart$split_pos <- as.integer(xml_attr(cust, "val"))
      } else {
        chart$split_pos <- num_or_null(xml_find_first(nd, "./c:splitPos"))
      }
    }
    if (type %in% c("pieChart", "doughnutChart")) {
      chart$first_slice_ang <- int_or_null(xml_find_first(nd, "./c:firstSliceAng"))
    }
    if (type == "doughnutChart") {
      chart$hole_size <- int_or_null(xml_find_first(nd, "./c:holeSize")) %||% 75
    }
    if (type == "bubbleChart") {
      chart$bubble_scale     <- num_or_null(xml_find_first(nd, "./c:bubbleScale")) %||% 100
      chart$show_neg_bubbles <- identical(attr_or_null(xml_find_first(nd, "./c:showNegBubbles"), "val"), "1")
      chart$size_represents  <- attr_or_null(xml_find_first(nd, "./c:sizeRepresents"), "val")
    }
    if (type %in% c("bar3DChart", "line3DChart", "area3DChart")) {
      chart$gap_depth <- int_or_null(xml_find_first(nd, "./c:gapDepth"))
    }
    if (type == "bar3DChart") {
      chart$bar_shape <- attr_or_null(xml_find_first(nd, "./c:shape"), "val")
    }
    if (!is_missing(xml_find_first(nd, "./c:dropLines")))  chart$drop_lines     <- TRUE
    if (!is_missing(xml_find_first(nd, "./c:hiLowLines"))) chart$high_low_lines <- TRUE
    if (!is_missing(xml_find_first(nd, "./c:upDownBars"))) {
      chart$up_down_bars <- TRUE
      gap_width <- int_or_null(xml_find_first(nd, "./c:upDownBars/c:gapWidth"))
    }

    ser_nodes <- xml_find_all(nd, "./c:ser")
    if (type %in% c("bubbleChart", ENCHARTER_PIE_FAMILY) && length(ser_nodes)) {
      dpt <- xml_find_all(ser_nodes[[1]], "./c:dPt/c:spPr/a:solidFill")
      if (length(dpt)) chart$palette <- unlist(lapply(dpt, load_color))
      expl <- int_or_null(xml_find_first(ser_nodes[[1]], "./c:explosion"))
      if (!is.null(expl)) chart$expansion <- expl
    }

    for (ser in ser_nodes) {
      s <- load_series(ser, type, chart)
      s$sec_type  <- sec_type
      s$dir       <- dir
      s$grouping  <- grouping
      s$gap_width <- gap_width
      s$overlap   <- overlap
      s$filled    <- filled
      # default label position: centered for stacked bars and areas, at the
      # outer end of clustered bars, to the right of line and scatter points
      default_pos <- if (type == "barChart" && grouping %in% c("stacked", "percentStacked")) "ctr"
        else if (type == "barChart") "t"
        else if (type %in% c("lineChart", "scatterChart", "bubbleChart")) "r"
        else "ctr"
      if (!is.null(s$label_params) && is.null(s$label_params$pos)) s$label_params$pos <- default_pos
      if (is.null(s$label_pos)) s$label_pos <- default_pos
      series_list[[length(series_list) + 1]] <- s
    }
  }
  if (!is.null(chart$label_params) && is.null(chart$label_params$pos)) {
    chart$label_params$pos <- series_list[[1]]$label_pos %||% "t"
    for (i in seq_along(series_list)) {
      s <- series_list[[i]]
      if (is.null(s$label_params) && !identical(s$label_pos, chart$label_params$pos)) {
        s$label_params <- chart$label_params
        s$label_params$pos <- s$label_pos
        series_list[[i]] <- s
      }
    }
  }
  chart$series_data <- series_list
  types <- vapply(series_list, function(s) s$type, character(1))
  chart$type <- types[length(types)]
  x_is_val <- length(ax_nodes) > 0 && all(xml_name(ax_nodes) == "c:valAx")
  if (x_is_val && !chart$type %in% c("scatterChart", "bubbleChart")) {
    chart$type <- types[types %in% c("scatterChart", "bubbleChart")][1]
  }

  chart
}

# Inverse of ChartEx$apply_label_style() / apply_legend_text_style() /
# apply_axis_style(): reads cx:txPr below `node`
load_cx_text_style <- function(node) {
  txpr <- xml_find_first(node, "./cx:txPr")
  if (is_missing(txpr)) return(NULL)
  rpr <- xml_find_first(txpr, "./a:p/a:pPr/a:defRPr")
  end <- xml_find_first(txpr, "./a:p/a:endParaRPr")
  rot <- num_or_null(xml_find_first(txpr, "./a:bodyPr"), "rot")
  sz  <- num_or_null(rpr, "sz")
  latin <- xml_find_first(rpr, "./a:latin")
  if (is_missing(latin)) latin <- xml_find_first(end, "./a:latin")
  list(
    font_size  = if (is.null(sz)) NULL else sz / 100,
    font_name  = attr_or_null(latin, "typeface"),
    bold       = flag_or_null(rpr, "b"),
    italic     = flag_or_null(rpr, "i"),
    font_color = load_color(xml_find_first(rpr, "./a:solidFill")),
    rotation   = if (is.null(rot)) NULL else rot / 60000
  )
}

# Inverse of ChartEx$render_axis_full()
load_cx_axis <- function(ax, defaults, is_x) {
  p <- defaults
  scaling <- xml_find_first(ax, if (is_x) "./cx:catScaling" else "./cx:valScaling")
  upd <- list()
  if (!is_x) {
    upd$min   <- num_or_null(scaling, "min")
    upd$max   <- num_or_null(scaling, "max")
    upd$major <- num_or_null(scaling, "majorUnit")
    upd$minor <- num_or_null(scaling, "minorUnit")
  }
  gl <- xml_find_first(ax, "./cx:majorGridlines")
  if (is_missing(gl)) {
    upd$grid_lines <- FALSE
  } else {
    ln <- load_line_style(xml_find_first(gl, "./cx:spPr"))
    upd$grid_lines <- ln$type %||% TRUE
    upd$grid_color <- ln$color
    upd$grid_width <- ln$width
  }
  upd$major_tick <- attr_or_null(xml_find_first(ax, "./cx:majorTickMarks"), "type")
  upd$format     <- attr_or_null(xml_find_first(ax, "./cx:numFmt"), "formatCode")
  ln <- load_line_style(xml_find_first(ax, "./cx:spPr"))
  upd$color      <- ln$color %||% "000000"
  upd$line_width <- ln$width %||% 0.75
  ts <- load_cx_text_style(ax)
  if (!is.null(ts)) {
    if (identical(ts$font_size, 10)) ts$font_size <- NULL
    upd[names(ts)] <- ts
  }
  p[names(upd)] <- upd
  list(params = p, gap_width = num_or_null(scaling, "gapWidth"))
}

# Resolves the _xlchart.v1.N names of a ChartEx to cell references
cx_ref_table <- function(wb) {
  regions <- wb$get_named_regions()
  if (is.null(regions) || !NROW(regions)) return(character())
  keep <- grepl("^_xlchart\\.v1\\.", regions$name)
  vals <- regions$value[keep]
  vals <- sub("^(.*!\\$?[A-Z]+\\$?[0-9]+):\\1$", "\\1", vals)
  vals <- sub("^(.*!)(\\$?[A-Z]+\\$?[0-9]+):\\2$", "\\1\\2", vals)
  stats::setNames(vals, regions$name[keep])
}

load_chartex <- function(xml, wb, chart_idx) {
  doc  <- read_xml(xml)
  refs <- cx_ref_table(wb)
  resolve <- function(node) {
    if (is_missing(node)) return(NULL)
    id <- xml_text(node)
    if (id %in% names(refs)) unname(refs[[id]]) else id
  }

  ser_nodes  <- xml_find_all(doc, "/cx:chartSpace/cx:chart/cx:plotArea/cx:plotAreaRegion/cx:series")
  data_nodes <- xml_find_all(doc, "/cx:chartSpace/cx:chartData/cx:data")
  if (length(ser_nodes) == 0) stop("No series found in the chartEx XML.", call. = FALSE)
  data_ids <- xml_attr(data_nodes, "id")

  chart <- ChartEx$new(type = xml_attr(ser_nodes[[1]], "layoutId"))
  if (nzchar(wb$charts$colors[[chart_idx]])) chart$color_xml <- wb$charts$colors[[chart_idx]]
  if (nzchar(wb$charts$style[[chart_idx]]))  chart$style_xml <- wb$charts$style[[chart_idx]]

  chart_root <- xml_find_first(doc, "/cx:chartSpace/cx:chart")
  plot_area  <- xml_find_first(chart_root, "./cx:plotArea")
  chart$chart_style <- load_sp_pr(xml_find_first(doc, "/cx:chartSpace/cx:spPr"))
  chart$plot_style  <- load_sp_pr(xml_find_first(plot_area, "./cx:plotAreaRegion/cx:plotSurface/cx:spPr"))
  chart$chart_title <- load_title(xml_find_first(chart_root, "./cx:title"), default_sz = NULL, ns = "cx")

  legend <- xml_find_first(chart_root, "./cx:legend")
  if (is_missing(legend)) {
    chart$legend_params <- list(pos = "none", align = "ctr", overlay = "0", style = list())
  } else {
    ts <- load_cx_text_style(legend)
    chart$legend_params <- list(
      pos     = attr_or_null(legend, "pos") %||% "t",
      align   = attr_or_null(legend, "align") %||% "ctr",
      overlay = attr_or_null(legend, "overlay") %||% "0",
      style   = if (is.null(ts)) list() else ts[c("font_size", "font_name", "bold", "italic", "font_color", "align", "body_pr", "auto_fit")]
    )
  }

  dlbls <- xml_find_first(plot_area, "./cx:plotAreaRegion/cx:series/cx:dataLabels")
  if (!is_missing(dlbls)) {
    vis <- xml_find_first(dlbls, "./cx:visibility")
    ts <- load_cx_text_style(dlbls)
    chart$label_params <- list(
      show_val        = identical(attr_or_null(vis, "value"), "1"),
      show_cat        = identical(attr_or_null(vis, "categoryName"), "1"),
      show_legend_key = identical(attr_or_null(vis, "seriesName"), "1"),
      pos             = attr_or_null(dlbls, "pos") %||% "outEnd",
      format          = attr_or_null(xml_find_first(dlbls, "./cx:numFmt"), "formatCode"),
      style           = if (is.null(ts)) list() else Filter(Negate(is.null), ts)
    )
  }

  series_list <- list()
  for (i in seq_along(ser_nodes)) {
    ser  <- ser_nodes[[i]]
    type <- xml_attr(ser, "layoutId")
    data_id <- xml_attr(xml_find_first(ser, "./cx:dataId"), "val")
    dat <- data_nodes[[match(data_id, data_ids)]]

    label <- resolve(xml_find_first(dat, "./cx:strDim/cx:f"))
    data  <- resolve(xml_find_first(dat, "./cx:numDim/cx:f"))

    name <- NA_character_
    tx <- xml_find_first(ser, "./cx:tx/cx:txData")
    if (!is_missing(tx)) {
      f <- xml_find_first(tx, "./cx:f")
      v <- xml_find_first(tx, "./cx:v")
      if (!is_missing(f)) name <- resolve(f) else if (!is_missing(v)) name <- xml_text(v)
    }

    color <- "auto"
    line_color <- NULL
    line_width <- 1
    sppr <- xml_find_first(ser, "./cx:spPr")
    if (!is_missing(sppr)) {
      color <- load_color(xml_find_first(sppr, "./a:solidFill")) %||% "auto"
      ln <- load_line_style(sppr)
      line_color <- ln$color
      if (!is.null(ln$width)) line_width <- ln$width
    }
    dpts <- xml_find_all(ser, "./cx:dataPt/cx:spPr/a:solidFill")
    if (length(dpts)) color <- unlist(lapply(dpts, load_color))

    vc <- xml_find_first(ser, "./cx:valueColors")
    if (!is_missing(vc)) {
      chart$region_colors <- list(
        min = load_color(xml_find_first(vc, "./cx:minColor")),
        mid = load_color(xml_find_first(vc, "./cx:midColor")),
        max = load_color(xml_find_first(vc, "./cx:maxColor"))
      )
    }

    lpr <- xml_find_first(ser, "./cx:layoutPr")
    parent_label <- attr_or_null(xml_find_first(lpr, "./cx:parentLabelLayout"), "val") %||% "overlapping"
    statistics   <- attr_or_null(xml_find_first(lpr, "./cx:statistics"), "quartileMethod")
    aggregation  <- if (is_missing(xml_find_first(lpr, "./cx:aggregation"))) NULL else TRUE

    visibility <- list()
    vis <- xml_find_first(lpr, "./cx:visibility")
    if (!is_missing(vis)) {
      for (nm in c("connectorLines", "meanLine", "meanMarker", "nonoutliers", "outliers")) {
        val <- attr_or_null(vis, nm)
        if (!is.null(val)) visibility[[nm]] <- val == "true"
      }
    }

    binning <- list()
    bn <- xml_find_first(lpr, "./cx:binning")
    if (!is_missing(bn)) {
      num_or_auto <- function(val) {
        if (is.null(val) || val == "auto") val else as.numeric(val)
      }
      binning <- Filter(Negate(is.null), list(
        binSize        = num_or_null(xml_find_first(bn, "./cx:binSize")),
        binCount       = int_or_null(xml_find_first(bn, "./cx:binCount")),
        intervalClosed = attr_or_null(bn, "intervalClosed"),
        underflow      = num_or_auto(attr_or_null(bn, "underflow")),
        overflow       = num_or_auto(attr_or_null(bn, "overflow"))
      ))
    }

    subtotals <- NULL
    sub_idx <- xml_find_all(lpr, "./cx:subtotals/cx:idx")
    if (length(sub_idx)) subtotals <- as.numeric(xml_attr(sub_idx, "val"))

    series_list[[i]] <- list(
      name         = name %||% NA_character_,
      data         = data,
      label        = label %||% NA_character_,
      type         = type,
      color        = color,
      line_color   = line_color,
      line_width   = line_width,
      gap_width    = NULL,
      subtotals    = subtotals,
      statistics   = statistics,
      geography    = NULL,
      aggregation  = aggregation,
      binning      = binning,
      visibility   = visibility,
      parent_label = parent_label
    )
  }

  for (ax in xml_find_all(plot_area, "./cx:axis")) {
    is_x <- xml_attr(ax, "id") == "0"
    which <- if (is_x) "x" else "y"
    res <- load_cx_axis(ax, chart$axis_params[[which]], is_x)
    chart$axis_params[[which]] <- res$params
    chart[[paste0(which, "_title")]] <- load_title(xml_find_first(ax, "./cx:title"), default_sz = NULL, ns = "cx")
    if (is_x && !is.null(res$gap_width)) series_list[[1]]$gap_width <- res$gap_width
  }

  chart$series_data <- series_list
  chart
}

# Picks the XML of chart `chart` from wb$charts. Workbooks loaded from a file
# can hold a standard chart and a chartEx in the same row, so `type` selects
# between them.
ec_chart_xml <- function(wb, chart, type) {
  if (!inherits(wb, "wbWorkbook")) stop("'wb' must be a wbWorkbook object.", call. = FALSE)
  n <- NROW(wb$charts)
  if (n == 0) stop("The workbook contains no charts.", call. = FALSE)
  if (!is.numeric(chart) || length(chart) != 1 || chart < 1 || chart > n) {
    stop(sprintf("'chart' must be a single number between 1 and %d.", n), call. = FALSE)
  }
  has_chart <- nzchar(wb$charts$chart[[chart]])
  has_ex    <- nzchar(wb$charts$chartEx[[chart]])
  if (is.null(type)) {
    if (has_chart && has_ex) {
      stop("Chart ", chart, " holds both a standard chart and a chartEx; pass type = \"chart\" or type = \"chartEx\".", call. = FALSE)
    }
    type <- if (has_ex) "chartEx" else "chart"
  }
  type <- match.arg(type, c("chart", "chartEx"))
  xml <- wb$charts[[type]][[chart]]
  if (!nzchar(xml)) stop("Chart ", chart, " contains no ", type, " XML.", call. = FALSE)
  list(xml = xml, is_ex = type == "chartEx")
}

#' Load a chart from a workbook into an encharter object
#'
#' @description
#' Reads a chart stored in a workbook and returns it as a `Chart` or
#' `ChartEx` object, the counterpart of [openxlsx2::wb_add_encharter()].
#' Series references and cached values, styling, axes, titles and the legend
#' are restored, so the object can be modified like any other encharter
#' object (`$update_series()` to follow a longer data range, `$add_series()`,
#' `$set_chart_title()`, ...) and written back with `wb_add_encharter()`.
#'
#' @details
#' Charts created with encharter are reproduced exactly. Charts written by
#' other software load as well; properties encharter has no field for
#' (manual layouts, per-point styling outside pie charts, theme color
#' modifiers, extension lists) are not carried over.
#'
#' A workbook loaded from a file can hold a standard chart and an extended
#' chart (waterfall, treemap, ...) in the same row of `wb$charts`; use
#' `type` to pick one in that case.
#'
#' @param wb A `wbWorkbook` containing at least one chart.
#' @param chart Integer; the row of `wb$charts` to load. Default `1`.
#' @param type `"chart"` for a standard chart or `"chartEx"` for an extended
#'   chart. The default takes whichever the row holds and errors if it
#'   holds both.
#' @return A `Chart` or `ChartEx` object.
#' @examples
#' library(openxlsx2)
#' wb <- wb_workbook()$add_worksheet("Data")$add_data(x = data.frame(
#'   Month = month.abb[1:6],
#'   Sales = c(120, 135, 128, 160, 175, 190)
#' ))
#' chart <- ec("line")$set_chart_title("Sales")$
#'   add_series(name = "Data!$B$1", label = "Data!$A$2:$A$7",
#'              data = "Data!$B$2:$B$7", marker = "circle")
#' wb$add_encharter(dims = "D2:L18", graph = chart)
#'
#' # A month later: append a row and point the chart at the longer range
#' wb$add_data(x = data.frame(Month = "Jul", Sales = 240), dims = "A8", col_names = FALSE)
#' chart <- ec_load(wb)
#' chart$update_series(data = wb_data(wb), label = Month)
#' wb$add_encharter(dims = "D20:L36", graph = chart)
#' @export
encharter_load <- function(wb, chart = 1L, type = NULL) {
  src <- ec_chart_xml(wb, chart, type)
  if (src$is_ex) load_chartex(src$xml, wb, chart) else load_chart(src$xml)
}

#' @rdname encharter_load
#' @export
ec_load <- encharter_load

# Ids of a chart in a workbook: the five axis ids `Chart$render()` takes, or
# `id_start` and `guid` for `ChartEx$render()`. Lets a loaded chart be
# rendered with the ids of the original.
#' @noRd
ec_axis_ids <- function(wb, chart = 1L, type = NULL) {
  src <- ec_chart_xml(wb, chart, type)
  doc <- read_xml(src$xml)
  if (src$is_ex) {
    ser <- xml_find_first(doc, "//cx:series")
    ids <- as.integer(sub("^_xlchart\\.v1\\.", "", xml_text(xml_find_all(doc, "//cx:numDim/cx:nf"))))
    return(list(id_start = min(ids) - 1L, guid = xml_attr(ser, "uniqueId")))
  }
  plot_area <- xml_find_first(doc, "/c:chartSpace/c:chart/c:plotArea")
  ids <- c(x = NA, y = NA, x2 = NA, y2 = NA, ser = NA)
  ser_ax <- xml_find_first(plot_area, "./c:serAx/c:axId")
  if (!is_missing(ser_ax)) ids["ser"] <- xml_attr(ser_ax, "val")
  ax_nodes <- Filter(function(nd) xml_name(nd) %in% c("c:catAx", "c:dateAx", "c:valAx"),
                     xml_children(plot_area))
  roles <- axis_roles(ax_nodes)
  for (i in seq_along(ax_nodes)) {
    if (!is.na(roles[i])) ids[roles[i]] <- xml_attr(xml_find_first(ax_nodes[[i]], "./c:axId"), "val")
  }
  defaults <- c("53178645", "60812428", "64752656", "81893617", "90007639")
  ids[is.na(ids)] <- defaults[is.na(ids)]
  unname(ids)
}
