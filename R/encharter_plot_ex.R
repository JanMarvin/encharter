ENCHARTER_PLOT_EX_TYPES <- c(
  "waterfall", "boxWhisker", "clusteredColumn", "paretoLine", "funnel", "treemap", "sunburst"
)

# Point colors of extended charts, sampled from Excel output:
# increase / series 1, decrease / series 2, total / series 3, then the
# remaining branch colors in the order they are assigned
ENCHARTER_CHARTEX_HEX <- c("2E5F7F", "DA7842", "34692E", "489CD0", "93358F", "65A542", "1B394C")

plot_ex_color <- function(i, override = NULL) {
  if (!is.null(override) && length(override) == 1 && !identical(override, "auto")) {
    return(plot_color(override, "#2E5F7F"))
  }
  pal <- ENCHARTER_CHARTEX_HEX
  paste0("#", pal[(i - 1) %% length(pal) + 1])
}

# Quartiles as QUARTILE.INC (R type 7) or
# QUARTILE.EXC (R type 6)
plot_ex_quartiles <- function(v, method) {
  v <- v[is.finite(v)]
  type <- if (identical(method, "exclusive")) 6 else 7
  stats::quantile(v, c(0.25, 0.5, 0.75), type = type, names = FALSE)
}

# Histogram bins: automatic width by Scott's rule,
# `binSize` or `binCount`, optional under- and overflow bins, intervals
# closed on the right ("(a, b]") unless intervalClosed = "l"
plot_ex_bins <- function(v, binning) {
  v <- v[is.finite(v)]
  lo <- min(v)
  hi <- max(v)
  closed_left <- identical(binning$intervalClosed, "l") || identical(binning$intervalClosed, "left")
  under <- if (is.numeric(binning$underflow)) binning$underflow else NULL
  over <- if (is.numeric(binning$overflow)) binning$overflow else NULL
  start <- under %||% lo
  end <- over %||% hi
  if (!is.null(binning$binSize)) {
    width <- binning$binSize
  } else if (!is.null(binning$binCount)) {
    width <- (end - start) / binning$binCount
  } else {
    width <- 3.5 * stats::sd(v) / length(v)^(1 / 3)
    if (!is.finite(width) || width <= 0) width <- max(1, (end - start) / 5)
  }
  n <- max(1, ceiling((end - start) / width - 1e-9))
  breaks <- start + width * (0:n)
  labels <- character(n)
  counts <- integer(n)
  for (i in seq_len(n)) {
    a <- breaks[i]
    b <- breaks[i + 1]
    # the outermost bin is closed on both ends unless an under-/overflow bin
    # takes over that side
    first_closed <- i == 1 && is.null(under)
    last_closed <- i == n && is.null(over)
    inside <- if (closed_left) v >= a & (v < b | (last_closed & v <= b)) else (v > a | (first_closed & v >= a)) & v <= b
    inside <- inside & v >= start & v <= end
    counts[i] <- sum(inside)
    open_a <- if (closed_left || first_closed) "[" else "("
    close_b <- if (closed_left && !last_closed) ")" else "]"
    labels[i] <- paste0(open_a, plot_format(a), ", ", plot_format(b), close_b)
  }
  # the underflow bin takes the boundary on the side the intervals are closed
  if (!is.null(under)) {
    counts <- c(if (closed_left) sum(v < under) else sum(v <= under), counts)
    labels <- c(paste0(if (closed_left) "< " else "\u2264 ", plot_format(under)), labels)
  }
  if (!is.null(over)) {
    counts <- c(counts, if (closed_left) sum(v >= over) else sum(v > over))
    labels <- c(labels, paste0(if (closed_left) "\u2265 " else "> ", plot_format(over)))
  }
  list(counts = counts, labels = labels)
}

# Squarified treemap layout of `sizes` inside the rectangle (x, y, w, h);
# returns one row per item with x, y, w, h
plot_ex_squarify <- function(sizes, x, y, w, h) {
  out <- matrix(NA_real_, length(sizes), 4, dimnames = list(NULL, c("x", "y", "w", "h")))
  ord <- order(sizes, decreasing = TRUE)
  total <- sum(sizes)
  if (total <= 0 || w <= 0 || h <= 0) return(out)
  scale <- w * h / total
  areas <- sizes[ord] * scale
  i <- 1
  while (i <= length(areas)) {
    vertical <- w >= h
    side <- if (vertical) h else w
    row <- integer()
    best <- Inf
    j <- i
    while (j <= length(areas)) {
      cand <- c(row, j)
      s <- sum(areas[cand])
      thick <- s / side
      worst <- max(vapply(areas[cand], function(a) {
        len <- a / thick
        max(len / thick, thick / len)
      }, numeric(1)))
      if (worst > best) break
      best <- worst
      row <- cand
      j <- j + 1
    }
    s <- sum(areas[row])
    thick <- s / side
    off <- 0
    for (k in row) {
      len <- areas[k] / thick
      if (vertical) {
        out[ord[k], ] <- c(x, y + h - off - len, thick, len)
      } else {
        out[ord[k], ] <- c(x + off, y + h - thick, len, thick)
      }
      off <- off + len
    }
    if (vertical) {
      x <- x + thick
      w <- w - thick
    } else {
      h <- h - thick
    }
    i <- j
  }
  out
}

# Builds the hierarchy of a treemap or sunburst from the level columns
# (outermost first) and the values; returns a data frame with one row per
# node: level, label, parent id, value, and the top-level branch index
plot_ex_hierarchy <- function(levels, values) {
  levels <- as.data.frame(lapply(levels, function(col) {
    col <- as.character(col)
    col[is.na(col)] <- ""
    col
  }), stringsAsFactors = FALSE)
  # blank outer cells continue the group above, as in the worksheet
  for (j in seq_len(ncol(levels))) {
    for (i in seq_len(nrow(levels))[-1]) {
      if (!nzchar(levels[i, j])) levels[i, j] <- levels[i - 1, j]
    }
  }
  values[is.na(values)] <- 0
  # a node is its path of labels, numbered in order of first appearance
  n_lev <- ncol(levels)
  paths <- character()
  for (i in seq_len(nrow(levels))) {
    for (j in seq_len(n_lev)) {
      pt <- paste(levels[i, seq_len(j)], collapse = "\r")
      if (!pt %in% paths) paths <- c(paths, pt)
    }
  }
  parts <- strsplit(paths, "\r", fixed = TRUE)
  level <- lengths(parts)
  parent_path <- vapply(parts, function(pp) paste(pp[-length(pp)], collapse = "\r"), character(1))
  row_path <- apply(levels, 1, paste, collapse = "\r")
  value <- vapply(paths, function(pt) sum(values[row_path == pt | startsWith(row_path, paste0(pt, "\r"))]), numeric(1))
  top <- vapply(parts, function(pp) pp[1], character(1))
  data.frame(
    id = seq_along(paths),
    level = level,
    label = vapply(parts, function(pp) pp[length(pp)], character(1)),
    parent = ifelse(level == 1, 0L, match(parent_path, paths)),
    value = unname(value),
    branch = match(top, unique(top)),
    stringsAsFactors = FALSE
  )
}

#' @rdname plot.Chart
#' @export
plot.ChartEx <- function(x, wb = NULL, newpage = TRUE, ...) {
  chart <- x
  if (length(chart$series_data) == 0) stop("The chart has no series.", call. = FALSE)
  plot_set_theme(wb)
  on.exit(plot_set_theme(NULL), add = TRUE)
  types <- unique(vapply(chart$series_data, function(s) s$type, character(1)))
  bad <- setdiff(types, ENCHARTER_PLOT_EX_TYPES)
  if (length(bad)) {
    stop("Chart type not supported by plot(): ", paste(bad, collapse = ", "), call. = FALSE)
  }
  if (is.null(wb)) stop("Extended charts hold no values; pass the workbook as 'wb'.", call. = FALSE)
  if (!inherits(wb, "wbWorkbook")) stop("'wb' must be a wbWorkbook object.", call. = FALSE)
  type <- types[1]

  series <- list()
  for (i in seq_along(chart$series_data)) {
    s <- chart$series_data[[i]]
    y <- plot_read_ref(wb, s$data)
    if (is.null(y)) stop(sprintf("series %d: range '%s' not found in the workbook", i, s$data), call. = FALSE)
    lab <- plot_read_ref(wb, s$label, levels = TRUE)
    if (inherits(lab, c("Date", "POSIXt"))) lab <- plot_format(lab)
    name <- if (is.na(s$name)) {
      paste0("Series", i)
    } else if (grepl("!.+", s$name)) {
      cell <- plot_read_ref(wb, s$name)
      if (is.null(cell)) paste0("Series", i) else as.character(cell[1])
    } else {
      gsub('^"|"$', "", s$name)
    }
    s$values <- suppressWarnings(as.numeric(y))
    s$levels <- if (is.data.frame(lab)) lab else if (!is.null(lab)) data.frame(l1 = as.character(lab), stringsAsFactors = FALSE) else NULL
    s$cats <- if (!is.null(s$levels)) s$levels[[ncol(s$levels)]] else if (type == "boxWhisker") rep("1", length(s$values)) else as.character(seq_along(s$values))
    s$label_text <- xml_unescape(name)
    series[[i]] <- s
  }

  if (isTRUE(newpage)) grid::grid.newpage()
  depth0 <- length(grid::current.vpPath())
  on.exit({
    extra <- length(grid::current.vpPath()) - depth0
    if (extra > 0) grid::upViewport(extra)
  }, add = TRUE)

  cs <- chart$chart_style
  grid::grid.rect(gp = grid::gpar(fill = plot_color(cs$fill, "#FFFFFF"), col = plot_color(cs$line, "#D9D9D9"),
                                  lwd = (cs$line_width %||% 1) * 96 / 72))
  chart_w <- grid::convertWidth(grid::unit(1, "npc"), "points", valueOnly = TRUE) - 16
  title_h <- plot_title_height(chart$chart_title, 14, chart_w)

  # ---- legend entries ----
  l_pos <- chart$legend_params$pos %||% "t"
  entries <- switch(type,
    waterfall = list(
      list(kind = "rect", label = "Increase", col = plot_ex_color(1)),
      list(kind = "rect", label = "Decrease", col = plot_ex_color(2)),
      list(kind = "rect", label = "Total", col = plot_ex_color(3))
    ),
    treemap = , sunburst = {
      nodes <- plot_ex_hierarchy(series[[1]]$levels %||% data.frame(l1 = series[[1]]$cats), series[[1]]$values)
      top <- nodes[nodes$level == 1, ]
      lapply(seq_len(nrow(top)), function(i) list(kind = "rect", label = top$label[i], col = plot_ex_color(i, series[[1]]$color)))
    },
    lapply(seq_along(series), function(i) list(kind = "rect", label = series[[i]]$label_text, col = plot_ex_color(i, series[[i]]$color)))
  )
  legend <- NULL
  if (l_pos != "none" && length(entries)) {
    legend <- plot_legend(entries, chart$legend_params, Filter(Negate(is.null), chart$legend_params$style), max_w = chart_w)
  }

  pad <- 8
  lw <- if (!is.null(legend) && legend$pos %in% c("l", "r")) legend$size[["w"]] + pad else 0
  lh <- if (!is.null(legend) && legend$pos %in% c("t", "b")) legend$size[["h"]] + pad else 0
  layout <- grid::grid.layout(
    5, 3,
    widths  = grid::unit(c(pad + if (identical(legend$pos, "l")) lw else 0, 1, pad + if (identical(legend$pos, "r")) lw else 0), c("points", "null", "points")),
    heights = grid::unit(c(pad + title_h, if (identical(legend$pos, "t")) lh else 0, 1, if (identical(legend$pos, "b")) lh else 0, pad), c("points", "points", "null", "points", "points"))
  )
  grid::pushViewport(grid::viewport(layout = layout))

  if (title_h > 0) {
    grid::pushViewport(grid::viewport(layout.pos.row = 1, layout.pos.col = 1:3))
    grid::pushViewport(grid::viewport(y = grid::unit(0, "npc"), height = grid::unit(title_h, "points"), just = "bottom"))
    plot_draw_title(chart$chart_title, 14, chart_w)
    grid::upViewport(2)
  }

  if (!is.null(legend)) {
    row <- switch(legend$pos, t = 2, b = 4, 3)
    col <- switch(legend$pos, l = 1, r = 3, 1:3)
    grid::pushViewport(grid::viewport(layout.pos.row = row, layout.pos.col = col))
    grid::pushViewport(grid::viewport(
      x = if (legend$pos == "r") grid::unit(1, "npc") - grid::unit(pad, "points") else if (legend$pos == "l") grid::unit(pad, "points") else grid::unit(0.5, "npc"),
      y = grid::unit(0.5, "npc"),
      width = grid::unit(legend$size[["w"]], "points"), height = grid::unit(legend$size[["h"]], "points"),
      just = if (legend$pos == "r") "right" else if (legend$pos == "l") "left" else "center"))
    legend$draw()
    grid::upViewport(2)
  }

  grid::pushViewport(grid::viewport(layout.pos.row = 3, layout.pos.col = 2))
  grid::pushViewport(grid::viewport(width = grid::unit(1, "npc") - grid::unit(2 * pad, "points"),
                                    height = grid::unit(1, "npc") - grid::unit(pad, "points")))
  switch(type,
    waterfall = plot_ex_waterfall(chart, series),
    boxWhisker = plot_ex_box(chart, series),
    clusteredColumn = plot_ex_histogram(chart, series, pareto = FALSE),
    paretoLine = plot_ex_histogram(chart, series, pareto = TRUE),
    funnel = plot_ex_funnel(chart, series),
    treemap = plot_ex_treemap(chart, series),
    sunburst = plot_ex_sunburst(chart, series)
  )
  grid::upViewport(3)
  invisible(chart)
}

# Plot area with a category axis (bottom) and a value axis (left) for the
# extended chart types; draws gridlines, axes, titles and pushes a viewport
# with native coordinates 0..n_cat horizontally and the value scale
# vertically. Returns the value scale.
plot_ex_frame <- function(chart, cats, lo, hi, y2 = NULL, y2_labels = NULL) {
  px <- chart$axis_params$x
  py <- chart$axis_params$y
  sc <- plot_scale(lo, hi, py)
  ticks <- plot_ticks(sc)
  labels <- plot_format(ticks, py$format)
  x_gp <- plot_gpar_text(px, 10, "#000000")
  y_gp <- plot_gpar_text(py, 10, "#000000")
  text_w <- function(labels, gp) max(vapply(labels, function(l) grid::convertWidth(grid::grobWidth(grid::textGrob(l, gp = gp)), "points", valueOnly = TRUE), numeric(1)))
  text_h <- function(gp) grid::convertHeight(grid::grobHeight(grid::textGrob("Xg", gp = gp)), "points", valueOnly = TRUE)
  # extended charts draw axis titles at the chart title size
  left_w <- text_w(labels, y_gp) + 8 + plot_title_height(chart$y_title, 14)
  right_w <- if (is.null(y2)) 4 else text_w(y2_labels, y_gp) + 8
  # crowded category labels are staggered over two rows (extended charts
  # do not rotate them)
  rot_x <- if (is.null(px$rotation) || abs(px$rotation) > 90) 0 else -px$rotation
  cat_w <- text_w(cats, x_gp)
  avail <- grid::convertWidth(grid::unit(1, "npc"), "points", valueOnly = TRUE) - left_w
  stagger <- rot_x == 0 && length(cats) && cat_w > avail / length(cats)
  lab_h <- if (rot_x != 0) abs(sin(rot_x * pi / 180)) * cat_w + text_h(x_gp) else if (stagger) 2 * text_h(x_gp) + 4 else text_h(x_gp)
  bottom_h <- lab_h + 8 + plot_title_height(chart$x_title, 14)

  grid::pushViewport(grid::viewport(layout = grid::grid.layout(
    2, 3,
    widths = grid::unit(c(left_w, 1, right_w), c("points", "null", "points")),
    heights = grid::unit(c(1, bottom_h), c("null", "points"))
  )))
  n <- length(cats)
  grid::pushViewport(grid::viewport(layout.pos.row = 1, layout.pos.col = 2, xscale = c(0, n), yscale = c(sc$min, sc$max), name = "plot"))

  ps <- chart$plot_style
  if (!is.null(ps$fill) || !is.null(ps$line)) {
    grid::grid.rect(gp = grid::gpar(fill = plot_color(ps$fill, NA), col = plot_color(ps$line, NA), lwd = (ps$line_width %||% 1) * 96 / 72))
  }
  if (!isFALSE(py$grid_lines) && !is.null(py$grid_lines)) {
    gp <- plot_grid_gp(py)
    for (t in ticks) grid::grid.lines(x = grid::unit(c(0, 1), "npc"), y = grid::unit(c(t, t), "native"), gp = gp)
  }
  if (!isFALSE(px$grid_lines) && !is.null(px$grid_lines)) {
    gp <- plot_grid_gp(px)
    for (t in 0:n) grid::grid.lines(x = grid::unit(c(t, t), "native"), y = grid::unit(c(0, 1), "npc"), gp = gp)
  }

  # axes: extended charts draw no tick marks unless asked for
  x_line_gp <- plot_axis_gp_line(px)
  y_line_gp <- plot_axis_gp_line(py)
  zero <- min(max(0, sc$min), sc$max)
  grid::grid.lines(x = grid::unit(c(0, 1), "npc"), y = grid::unit(c(zero, zero), "native"), gp = x_line_gp)
  if (!is.null(px$major_tick) && px$major_tick != "none") {
    for (t in 0:n) grid::grid.lines(x = grid::unit(c(t, t), "native"), y = grid::unit(zero, "native") + grid::unit(c(0, -4), "points"), gp = x_line_gp)
  }
  for (i in seq_len(n)) {
    drop <- if (stagger && i %% 2 == 0) text_h(x_gp) + 4 else 0
    grid::grid.text(cats[i], x = grid::unit(i - 0.5, "native"), y = grid::unit(0, "npc") - grid::unit(6 + drop, "points"),
                    just = if (rot_x > 0) c("right", "top") else if (rot_x < 0) c("left", "top") else c("center", "top"), rot = rot_x, gp = x_gp)
  }
  grid::grid.lines(x = grid::unit(0, "npc"), y = grid::unit(c(0, 1), "npc"), gp = y_line_gp)
  for (i in seq_along(ticks)) {
    if (!is.null(py$major_tick) && py$major_tick != "none") {
      grid::grid.lines(x = grid::unit(0, "npc") + grid::unit(c(0, -4), "points"), y = grid::unit(c(ticks[i], ticks[i]), "native"), gp = y_line_gp)
    }
    grid::grid.text(labels[i], x = grid::unit(0, "npc") - grid::unit(6, "points"), y = grid::unit(ticks[i], "native"), just = c("right", "center"), gp = y_gp)
  }
  if (!is.null(y2)) {
    grid::grid.lines(x = grid::unit(1, "npc"), y = grid::unit(c(0, 1), "npc"), gp = y_line_gp)
    for (i in seq_along(y2)) {
      grid::grid.text(y2_labels[i], x = grid::unit(1, "npc") + grid::unit(6, "points"), y = grid::unit(y2[i], "npc"), just = c("left", "center"), gp = y_gp)
    }
  }
  if (!is.null(chart$y_title$text)) {
    grid::grid.text(plot_title_text(chart$y_title), x = grid::unit(0, "npc") - grid::unit(left_w - plot_title_height(chart$y_title, 14) / 2, "points"),
                    rot = 90, gp = plot_gpar_text(chart$y_title$style, 14, "#000000"))
  }
  if (!is.null(chart$x_title$text)) {
    grid::grid.text(plot_title_text(chart$x_title), y = grid::unit(0, "npc") - grid::unit(bottom_h - plot_title_height(chart$x_title, 14) / 2, "points"),
                    gp = plot_gpar_text(chart$x_title$style, 14, "#000000"))
  }
  sc
}

plot_ex_labels_on <- function(lp) {
  isTRUE(lp$show_val) || isTRUE(lp$show_cat) || isTRUE(lp$show_legend_key)
}

# ---------------------------------------------------------------------------

plot_ex_waterfall <- function(chart, series) {
  s <- series[[1]]
  v <- s$values
  v[is.na(v)] <- 0
  n <- length(v)
  subtotal <- rep(FALSE, n)
  if (is.numeric(s$subtotals)) subtotal[s$subtotals + 1] <- TRUE
  base <- numeric(n)
  top <- numeric(n)
  running <- 0
  for (i in seq_len(n)) {
    if (subtotal[i]) {
      base[i] <- 0
      top[i] <- v[i]
      running <- v[i]
    } else {
      base[i] <- running
      top[i] <- running + v[i]
      running <- top[i]
    }
  }
  plot_ex_frame(chart, s$cats, min(0, base, top), max(0, base, top))
  gap <- s$gap_width %||% 0.35
  w <- 1 / (1 + gap)
  lp <- chart$label_params
  label_gp <- plot_gpar_text(lp$style, 9, "#000000")
  connectors <- !isFALSE(s$visibility$connectorLines)
  for (i in seq_len(n)) {
    kind <- if (subtotal[i]) 3 else if (v[i] >= 0) 1 else 2
    col <- plot_ex_color(kind, if (length(s$color) > 1) s$color[i] else NULL)
    grid::grid.rect(x = grid::unit(i - 0.5, "native"), y = grid::unit(min(base[i], top[i]), "native"),
                    width = max(grid::unit(w, "native"), grid::unit(0.75, "points")), height = grid::unit(abs(top[i] - base[i]), "native"),
                    just = c("center", "bottom"),
                    gp = grid::gpar(fill = col, col = plot_color(s$line_color, NA), lwd = (s$line_width %||% 1) * 96 / 72))
    if (connectors && i < n) {
      grid::grid.lines(x = grid::unit(c(i - 0.5 + w / 2, i + 0.5 - w / 2), "native"), y = grid::unit(c(top[i], top[i]), "native"),
                       gp = grid::gpar(col = "#000000", lwd = 0.75 * 96 / 72))
    }
    if (plot_ex_labels_on(lp)) {
      # labels sit above the bar, for decreases as well
      grid::grid.text(plot_label_text(lp, s$cats[i], s$values[i]), x = grid::unit(i - 0.5, "native"),
                      y = grid::unit(max(base[i], top[i]), "native") + grid::unit(3, "points"),
                      just = c("center", "bottom"), gp = label_gp)
    }
  }
  grid::upViewport(2)
}

plot_ex_box <- function(chart, series) {
  cats <- unique(unlist(lapply(series, function(s) s$cats)))
  cats <- cats[!is.na(cats)]
  all_v <- unlist(lapply(series, function(s) s$values))
  plot_ex_frame(chart, cats, min(all_v, na.rm = TRUE), max(all_v, na.rm = TRUE))
  k <- length(series)
  gap <- series[[1]]$gap_width %||% 0.35
  slot <- 1 / (k + gap)
  for (j in seq_len(k)) {
    s <- series[[j]]
    col <- plot_ex_color(j, s$color)
    border <- plot_color(s$line_color, NA)
    vis <- s$visibility
    show_inner <- !isFALSE(vis$nonoutliers)
    show_outliers <- !isFALSE(vis$outliers)
    show_mean_marker <- isTRUE(vis$meanMarker)
    show_mean_line <- isTRUE(vis$meanLine)
    means <- numeric()
    for (i in seq_along(cats)) {
      v <- s$values[s$cats == cats[i]]
      v <- v[is.finite(v)]
      cx <- i - 1 + gap * slot / 2 + (j - 0.5) * slot
      if (!length(v)) {
        means <- c(means, NA)
        next
      }
      q <- plot_ex_quartiles(v, s$statistics)
      iqr <- q[3] - q[1]
      inside <- v >= q[1] - 1.5 * iqr & v <= q[3] + 1.5 * iqr
      whisk <- range(v[inside])
      bw <- slot
      grid::grid.lines(x = grid::unit(c(cx, cx), "native"), y = grid::unit(c(whisk[1], q[1]), "native"), gp = grid::gpar(col = col))
      grid::grid.lines(x = grid::unit(c(cx, cx), "native"), y = grid::unit(c(q[3], whisk[2]), "native"), gp = grid::gpar(col = col))
      for (y in whisk) grid::grid.lines(x = grid::unit(c(cx - bw * 0.15, cx + bw * 0.15), "native"), y = grid::unit(c(y, y), "native"), gp = grid::gpar(col = col))
      grid::grid.rect(x = grid::unit(cx, "native"), y = grid::unit(q[1], "native"), width = grid::unit(bw, "native"),
                      height = grid::unit(q[3] - q[1], "native"), just = c("center", "bottom"),
                      gp = grid::gpar(fill = col, col = border, lwd = (s$line_width %||% 1) * 96 / 72))
      grid::grid.lines(x = grid::unit(c(cx - bw / 2, cx + bw / 2), "native"), y = grid::unit(c(q[2], q[2]), "native"),
                       gp = grid::gpar(col = if (is.na(border)) col else border))
      if (show_inner) {
        pts <- v[inside]
        grid::grid.points(x = grid::unit(rep(cx, length(pts)), "native"), y = grid::unit(pts, "native"), pch = 16,
                          size = grid::unit(4, "points"), gp = grid::gpar(col = col))
      }
      if (show_outliers) {
        pts <- v[!inside]
        if (length(pts)) grid::grid.points(x = grid::unit(rep(cx, length(pts)), "native"), y = grid::unit(pts, "native"), pch = 16,
                                           size = grid::unit(4, "points"), gp = grid::gpar(col = col))
      }
      if (show_mean_marker) {
        grid::grid.points(x = grid::unit(cx, "native"), y = grid::unit(mean(v), "native"), pch = 4,
                          size = grid::unit(6, "points"), gp = grid::gpar(col = "#FFFFFF", lwd = 1.5))
      }
      means <- c(means, mean(v))
    }
    if (show_mean_line && sum(is.finite(means)) > 1) {
      cx <- seq_along(cats) - 1 + gap * slot / 2 + (j - 0.5) * slot
      grid::grid.lines(x = grid::unit(cx[is.finite(means)], "native"), y = grid::unit(means[is.finite(means)], "native"),
                       gp = grid::gpar(col = col, lwd = 1.5))
    }
  }
  grid::upViewport(2)
}

# clusteredColumn without a binning element is a plain column chart of the
# values; with binning the values are counted into bins. A paretoLine series
# on its own draws only the cumulative share of the (descending) bins on the
# value axis when no column series accompanies it.
plot_ex_histogram <- function(chart, series, pareto = FALSE) {
  s <- series[[1]]
  if (length(s$binning) == 0 && !pareto) {
    counts <- s$values
    counts[is.na(counts)] <- 0
    labels <- s$cats
  } else {
    bins <- plot_ex_bins(s$values, s$binning %||% list())
    counts <- bins$counts
    labels <- bins$labels
  }
  lp <- chart$label_params
  col <- plot_ex_color(1, s$color)
  if (pareto) {
    ord <- order(counts, decreasing = TRUE)
    cum <- cumsum(counts[ord]) / sum(counts)
    plot_ex_frame(chart, labels[ord], 0, 1)
    grid::grid.lines(x = grid::unit(seq_along(cum) - 0.5, "native"), y = grid::unit(cum, "native"),
                     gp = grid::gpar(col = col, lwd = (s$line_width %||% 1) * 96 / 72))
    grid::upViewport(2)
    return(invisible())
  }
  plot_ex_frame(chart, labels, 0, max(counts))
  gap <- s$gap_width %||% 0.35
  w <- 1 / (1 + gap)
  for (i in seq_along(counts)) {
    grid::grid.rect(x = grid::unit(i - 0.5, "native"), y = grid::unit(0, "native"), width = grid::unit(w, "native"),
                    height = grid::unit(counts[i], "native"), just = c("center", "bottom"),
                    gp = grid::gpar(fill = col, col = plot_color(s$line_color, "#FFFFFF"), lwd = 0.75))
    if (plot_ex_labels_on(lp)) {
      grid::grid.text(plot_format(counts[i], lp$format), x = grid::unit(i - 0.5, "native"), y = grid::unit(counts[i], "native") + grid::unit(3, "points"),
                      just = c("center", "bottom"), gp = plot_gpar_text(lp$style, 9, "#000000"))
    }
  }
  grid::upViewport(2)
}

plot_ex_funnel <- function(chart, series) {
  s <- series[[1]]
  v <- s$values
  v[is.na(v)] <- 0
  n <- length(v)
  px <- chart$axis_params$x
  x_gp <- plot_gpar_text(px, 10, "#000000")
  lab_w <- max(vapply(s$cats, function(l) grid::convertWidth(grid::grobWidth(grid::textGrob(l, gp = x_gp)), "points", valueOnly = TRUE), numeric(1))) + 12
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 2, widths = grid::unit(c(lab_w, 1), c("points", "null")))))
  grid::pushViewport(grid::viewport(layout.pos.col = 2, xscale = c(-max(v), max(v)), yscale = c(n, 0)))
  grid::grid.lines(x = grid::unit(0, "npc"), y = grid::unit(c(0, 1), "npc"), gp = plot_axis_gp_line(px))
  col <- plot_ex_color(1, s$color)
  gap <- s$gap_width %||% 0.38
  h <- 1 / (1 + gap)
  lp <- chart$label_params
  for (i in seq_len(n)) {
    grid::grid.rect(x = grid::unit(0, "native"), y = grid::unit(i - 0.5, "native"), width = grid::unit(2 * v[i], "native"),
                    height = grid::unit(h, "native"), gp = grid::gpar(fill = col, col = plot_color(s$line_color, NA)))
    # data labels sit centered above the bar
    if (plot_ex_labels_on(lp)) {
      grid::grid.text(plot_label_text(lp, s$cats[i], v[i], sep = ", "), x = grid::unit(0, "native"),
                      y = grid::unit(i - 0.5 - h / 2, "native") + grid::unit(1, "points"),
                      just = c("center", "bottom"), gp = plot_gpar_text(lp$style, 8, "#000000"))
    }
  }
  grid::upViewport()
  grid::pushViewport(grid::viewport(layout.pos.col = 1, yscale = c(n, 0)))
  for (i in seq_len(n)) {
    grid::grid.text(s$cats[i], x = grid::unit(1, "npc") - grid::unit(6, "points"), y = grid::unit(i - 0.5, "native"),
                    just = c("right", "center"), gp = x_gp)
  }
  grid::upViewport(2)
}

plot_ex_treemap <- function(chart, series) {
  s <- series[[1]]
  nodes <- plot_ex_hierarchy(s$levels %||% data.frame(l1 = s$cats), s$values)
  depth <- max(nodes$level)
  banner <- identical(s$parent_label, "banner") && depth > 1
  lp <- chart$label_params
  labels_on <- plot_ex_labels_on(lp)
  leaf_gp <- plot_gpar_text(lp$style, 8, "#000000")
  parent_gp <- leaf_gp
  banner_h <- 20

  lay <- function(parent_id, x, y, w, h) {
    kids <- nodes[nodes$parent == parent_id, ]
    if (!nrow(kids)) return(invisible())
    rects <- plot_ex_squarify(kids$value, x, y, w, h)
    for (k in seq_len(nrow(kids))) {
      r <- rects[k, ]
      if (any(is.na(r))) next
      col <- plot_ex_color(kids$branch[k], s$color)
      is_leaf <- kids$level[k] == depth
      if (is_leaf) {
        grid::grid.rect(x = r[["x"]], y = r[["y"]], width = r[["w"]], height = r[["h"]], just = c("left", "bottom"),
                        default.units = "points", gp = grid::gpar(fill = col, col = plot_color(s$line_color, "#FFFFFF"), lwd = (s$line_width %||% 1) * 96 / 72))
        # leaf labels sit in the lower left corner, "category, value"
        txt <- if (labels_on) plot_label_text(lp, kids$label[k], kids$value[k], sep = ", ") else ""
        tw <- grid::convertWidth(grid::grobWidth(grid::textGrob(txt, gp = leaf_gp)), "points", valueOnly = TRUE)
        if (tw >= r[["w"]] - 4) {
          # a label that does not fit wraps after the comma
          txt <- plot_label_text(lp, kids$label[k], kids$value[k], sep = ",\n")
          tw <- grid::convertWidth(grid::grobWidth(grid::textGrob(txt, gp = leaf_gp)), "points", valueOnly = TRUE)
        }
        if (nzchar(txt) && tw < r[["w"]] - 4 && r[["h"]] > 14) {
          grid::grid.text(txt, x = r[["x"]] + 3, y = r[["y"]] + 3, just = c("left", "bottom"), default.units = "points", gp = leaf_gp)
        }
      } else {
        inner <- r
        # banner mode reserves a band above each top-level group; it stays
        # blank when data labels are off
        if (banner && kids$level[k] == 1) {
          if (labels_on) {
            grid::grid.rect(x = r[["x"]], y = r[["y"]] + r[["h"]] - banner_h, width = r[["w"]], height = banner_h, just = c("left", "bottom"),
                            default.units = "points", gp = grid::gpar(fill = grDevices::adjustcolor(col, 1, 0.7, 0.7, 0.7), col = NA))
            grid::grid.text(kids$label[k], x = r[["x"]] + r[["w"]] / 2, y = r[["y"]] + r[["h"]] - banner_h / 2, default.units = "points", gp = parent_gp)
          }
          inner[["h"]] <- r[["h"]] - banner_h
        }
        lay(kids$id[k], inner[["x"]], inner[["y"]], inner[["w"]], inner[["h"]])
        if (!banner && labels_on && kids$level[k] == 1 && !identical(s$parent_label, "none")) {
          grid::grid.text(kids$label[k], x = r[["x"]] + 4, y = r[["y"]] + r[["h"]] - 4, just = c("left", "top"), default.units = "points",
                          gp = leaf_gp)
        }
      }
    }
  }
  w <- grid::convertWidth(grid::unit(1, "npc"), "points", valueOnly = TRUE)
  h <- grid::convertHeight(grid::unit(1, "npc"), "points", valueOnly = TRUE)
  lay(0L, 0, 0, w, h)
}

plot_ex_sunburst <- function(chart, series) {
  s <- series[[1]]
  nodes <- plot_ex_hierarchy(s$levels %||% data.frame(l1 = s$cats), s$values)
  depth <- max(nodes$level)
  grid::pushViewport(grid::viewport(width = grid::unit(1, "snpc"), height = grid::unit(1, "snpc")))
  # the hole has the width of one ring
  r_max <- 0.48
  ring <- r_max / (depth + 1)
  lp <- chart$label_params
  gp <- plot_gpar_text(lp$style, 8, "#FFFFFF")
  border <- plot_color(s$line_color, "#FFFFFF")

  draw <- function(parent_id, a0, a1, level) {
    # segments run clockwise from the top in descending order of value
    kids <- nodes[nodes$parent == parent_id, ]
    kids <- kids[order(-kids$value), ]
    total <- sum(kids$value)
    if (!nrow(kids) || total <= 0) return(invisible())
    start <- a0
    for (k in seq_len(nrow(kids))) {
      sweep <- (a1 - a0) * kids$value[k] / total
      end <- start + sweep
      r_in <- level * ring
      r_out <- r_in + ring
      ang <- seq(start, end, length.out = max(2, ceiling(sweep * 60)))
      px <- c(0.5 + r_out * sin(ang), 0.5 + r_in * sin(rev(ang)))
      py <- c(0.5 + r_out * cos(ang), 0.5 + r_in * cos(rev(ang)))
      grid::grid.polygon(px, py, gp = grid::gpar(fill = plot_ex_color(kids$branch[k], s$color), col = border, lwd = (s$line_width %||% 1) * 96 / 72))
      mid <- (start + end) / 2
      arc_len <- sweep * (r_in + r_out) / 2
      if (plot_ex_labels_on(lp) && arc_len > 0.03) {
        rot <- 90 - mid * 180 / pi
        if (rot < -90) rot <- rot + 180
        grid::grid.text(kids$label[k], x = 0.5 + (r_in + r_out) / 2 * sin(mid), y = 0.5 + (r_in + r_out) / 2 * cos(mid), rot = rot, gp = gp)
      }
      draw(kids$id[k], start, end, level + 1)
      start <- end
    }
  }
  draw(0L, 0, 2 * pi, 1)
  grid::upViewport()
}
