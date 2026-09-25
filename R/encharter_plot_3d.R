# plot() for the 3D chart types: bar3DChart, line3DChart, area3DChart,
# pie3DChart, surfaceChart (the contour view) and surface3DChart.
#
# The plot box has the categories along x (width 1), the values along y and
# the depth along z. With right-angle axes (the default of the column, line
# and area types) the box is drawn in an oblique projection whose depth
# vector follows the two rotation angles and whose height is stretched to
# the plot area, as a spreadsheet application does; otherwise the box is
# rotated and seen in perspective. Faces are shaded as rendered there: the
# front keeps the color, the top is darker and the side darker still.

# Band colors of surface charts with the current Office theme, from the
# lowest band up
ENCHARTER_SURFACE_HEX <- c(
  "115472", "CE632B", "155E1F", "0C8BBC", "8D2582", "449328",
  "8599AA", "EEA18A", "869E87", "9DB0A9", "F2B99D", "A9B7A4"
)

# Multiplies the RGB components; the top of a box is drawn with 0.76, the
# side with 0.64
plot_shade <- function(col, factor) {
  if (is.na(col)) return(col)
  rgb <- pmin(1, grDevices::col2rgb(col) / 255 * factor)
  grDevices::rgb(rgb[1], rgb[2], rgb[3])
}

# Projection of a box of width `w`, height `h` and depth `d` onto the unit
# square; returns a function taking world coordinates. `stretch = TRUE`
# fits width and height separately, which is what happens when no height
# percentage is set.
plot_3d_projection <- function(chart, w, h, d, margin = c(0.1, 0.08, 0.1, 0.1), stretch = TRUE) {
  v <- chart$view3d
  rot_x <- v$rot_x %||% 15
  rot_y <- v$rot_y %||% 20
  r_ang <- v$right_angle_axes %||% TRUE
  persp <- v$perspective %||% 30
  ax <- rot_x * pi / 180
  ay <- rot_y * pi / 180

  # margin: top, right, bottom, left
  x_room <- 1 - margin[2] - margin[4]
  y_room <- 1 - margin[1] - margin[3]

  if (isTRUE(r_ang)) {
    # oblique: the depth vector uses the horizontal scale for both of its
    # components, so the height can be stretched on its own
    x_ext <- w + d * sin(ay)
    y_lift <- d * sin(ax)
    sx <- x_room / x_ext
    sy <- (y_room - y_lift * sx) / h
    if (!stretch) {
      sx <- min(sx, y_room / (h + y_lift))
      sy <- sx
    }
    x0 <- margin[4] + (x_room - sx * x_ext) / 2
    y0 <- margin[3] + (y_room - sy * h - y_lift * sx) / 2
    return(function(x, y, z) list(x = x0 + sx * (x + z * sin(ay)), y = y0 + sy * y + sx * z * sin(ax)))
  }

  # rotate the box around its vertical axis, tilt it and look from a
  # distance set by the perspective angle
  raw <- function(x, y, z) {
    x <- x - w / 2
    y <- y - h / 2
    z <- z - d / 2
    x1 <- x * cos(ay) + z * sin(ay)
    z1 <- -x * sin(ay) + z * cos(ay)
    y2 <- y * cos(ax) + z1 * sin(ax)
    z2 <- z1 * cos(ax) - y * sin(ax)
    if (persp > 0) {
      dist <- 5.8 * max(w, d) * tan(15 * pi / 180) / tan(persp / 2 * pi / 180)
      f <- dist / (dist + z2)
      list(x = x1 * f, y = y2 * f)
    } else {
      list(x = x1, y = y2)
    }
  }
  corners <- expand.grid(x = c(0, w), y = c(0, h), z = c(0, d))
  p <- raw(corners$x, corners$y, corners$z)
  xr <- range(p$x)
  yr <- range(p$y)
  sx <- x_room / diff(xr)
  sy <- y_room / diff(yr)
  if (!stretch) {
    sx <- min(sx, sy)
    sy <- sx
  }
  x0 <- margin[4] + (x_room - sx * diff(xr)) / 2
  y0 <- margin[3] + (y_room - sy * diff(yr)) / 2
  function(x, y, z) {
    p <- raw(x, y, z)
    list(x = x0 + (p$x - xr[1]) * sx, y = y0 + (p$y - yr[1]) * sy)
  }
}

plot_3d_poly <- function(proj, x, y, z, fill, col = NA, lwd = 0.75) {
  p <- proj(x, y, z)
  grid::grid.polygon(p$x, p$y, gp = grid::gpar(fill = fill, col = col, lwd = lwd * 96 / 72))
}

plot_3d_line <- function(proj, x, y, z, col, lwd = 0.75) {
  p <- proj(x, y, z)
  grid::grid.lines(p$x, p$y, gp = grid::gpar(col = col, lwd = lwd * 96 / 72))
}

# Box from (x0, y0, z0) to (x1, y1, z1); front, top and right face visible.
# `part` is c(bottom, top) of a piece of a stacked pyramid or cone as
# shares of the whole shape, which tapers to the top of the stack.
plot_3d_box <- function(proj, x0, x1, y0, y1, z0, z1, col, shape = "box", part = c(0, 1)) {
  if (y1 < y0) {
    tmp <- y0
    y0 <- y1
    y1 <- tmp
  }
  side <- plot_shade(col, 0.64)
  top <- plot_shade(col, 0.76)
  cx <- (x0 + x1) / 2
  cz <- (z0 + z1) / 2
  rx <- (x1 - x0) / 2
  rz <- (z1 - z0) / 2
  r_lo <- 1 - part[1]
  r_hi <- 1 - part[2]
  if (shape %in% c("cylinder", "cone", "coneToMax")) {
    ang <- seq(0, 2 * pi, length.out = 49)
    # the near half of the mantle has z below the center
    near <- 25:49
    if (shape == "cylinder") {
      r_lo <- 1
      r_hi <- 1
    }
    ex_lo <- cx + rx * r_lo * cos(ang)
    ez_lo <- cz + rz * r_lo * sin(ang)
    ex_hi <- cx + rx * r_hi * cos(ang)
    ez_hi <- cz + rz * r_hi * sin(ang)
    plot_3d_poly(proj, c(ex_lo[near], rev(ex_hi[near])), c(rep(y0, 25), rep(y1, 25)), c(ez_lo[near], rev(ez_hi[near])), col)
    if (r_hi > 0) plot_3d_poly(proj, ex_hi, rep(y1, 49), ez_hi, top)
    return(invisible())
  }
  if (shape %in% c("pyramid", "pyramidToMax")) {
    plot_3d_poly(proj, c(cx - rx * r_lo, cx + rx * r_lo, cx + rx * r_hi, cx - rx * r_hi), c(y0, y0, y1, y1),
                 c(cz - rz * r_lo, cz - rz * r_lo, cz - rz * r_hi, cz - rz * r_hi), col)
    plot_3d_poly(proj, c(cx + rx * r_lo, cx + rx * r_lo, cx + rx * r_hi, cx + rx * r_hi), c(y0, y0, y1, y1),
                 c(cz - rz * r_lo, cz + rz * r_lo, cz + rz * r_hi, cz - rz * r_hi), side)
    if (r_hi > 0) {
      plot_3d_poly(proj, c(cx - rx * r_hi, cx + rx * r_hi, cx + rx * r_hi, cx - rx * r_hi), rep(y1, 4),
                   c(cz - rz * r_hi, cz - rz * r_hi, cz + rz * r_hi, cz + rz * r_hi), top)
    }
    return(invisible())
  }
  plot_3d_poly(proj, c(x0, x1, x1, x0), c(y0, y0, y1, y1), rep(z0, 4), col)
  plot_3d_poly(proj, rep(x1, 4), c(y0, y0, y1, y1), c(z0, z1, z1, z0), side)
  plot_3d_poly(proj, c(x0, x1, x1, x0), rep(y1, 4), c(z0, z0, z1, z1), top)
}

# Walls, floor, gridlines and axis labels of a box whose values run along
# y (or along x for horizontal bars). `cat_pos` are the label positions on
# the category axis, `ser_pos` those of the series labels along z.
plot_3d_frame <- function(chart, proj, w, h, d, sc, cats, cat_pos, ser_labels = NULL, ser_pos = NULL,
                          horizontal = FALSE, floor_grid = FALSE) {
  px <- chart$axis_params$x
  py <- chart$axis_params$y
  wall <- plot_color(chart$plot_style$fill, NA)
  wall_line <- "#D9D9D9"
  plot_3d_poly(proj, c(0, w, w, 0), c(0, 0, h, h), rep(d, 4), wall, wall_line)
  plot_3d_poly(proj, rep(0, 4), c(0, 0, h, h), c(0, d, d, 0), wall, wall_line)
  plot_3d_poly(proj, c(0, w, w, 0), rep(0, 4), c(0, 0, d, d), wall, wall_line)
  ticks <- plot_ticks(sc)
  gp <- plot_grid_gp(py)
  val_at <- function(t) (t - sc$min) / (sc$max - sc$min)
  if (!isFALSE(py$grid_lines)) {
    for (t in ticks) {
      if (horizontal) {
        plot_3d_line(proj, rep(val_at(t) * w, 3), c(0, 0, h), c(0, d, d), gp$col, gp$lwd * 72 / 96)
      } else {
        plot_3d_line(proj, c(0, 0, w), rep(val_at(t) * h, 3), c(0, d, d), gp$col, gp$lwd * 72 / 96)
      }
    }
  }
  if (floor_grid) {
    for (x in cat_pos) plot_3d_line(proj, c(x, x), c(0, 0), c(0, d), wall_line)
    for (z in ser_pos) plot_3d_line(proj, c(0, w), c(0, 0), c(z, z), wall_line)
  }
  axis_col <- plot_color(if (identical(py$color, "none")) NA else py$color, "#000000")
  y_gp <- plot_gpar_text(py, 10, "#000000")
  x_gp <- plot_gpar_text(px, 10, "#000000")
  cat_labels <- plot_format(cats, px$format)
  cat_labels[is.na(cats)] <- ""
  show_val <- !isTRUE(py$delete) && !identical(py$label_pos, "none")
  show_cat <- !isTRUE(px$delete) && !identical(px$label_pos, "none")
  # the two front edges carry the axes
  plot_3d_line(proj, c(0, 0), c(0, h), c(0, 0), axis_col)
  plot_3d_line(proj, c(0, w), c(0, 0), c(0, 0), axis_col)
  below <- function(txt, p, gp) {
    grid::grid.text(txt, x = p$x, y = grid::unit(p$y, "npc") - grid::unit(6, "points"), just = c("center", "top"), gp = gp)
  }
  left_of <- function(txt, p, gp) {
    grid::grid.text(txt, x = grid::unit(p$x, "npc") - grid::unit(6, "points"), y = p$y, just = c("right", "center"), gp = gp)
  }
  if (horizontal) {
    if (show_val) {
      for (t in ticks) {
        p <- proj(val_at(t) * w, 0, 0)
        grid::grid.lines(grid::unit(c(p$x, p$x), "npc"), grid::unit(p$y, "npc") - grid::unit(c(0, 4), "points"), gp = grid::gpar(col = axis_col))
        below(plot_format(t, py$format), p, y_gp)
      }
    }
    if (show_cat) for (i in seq_along(cats)) left_of(cat_labels[i], proj(0, cat_pos[i], 0), x_gp)
  } else {
    if (show_val) {
      for (t in ticks) {
        p <- proj(0, val_at(t) * h, 0)
        grid::grid.lines(grid::unit(p$x, "npc") - grid::unit(c(0, 4), "points"), grid::unit(c(p$y, p$y), "npc"), gp = grid::gpar(col = axis_col))
        left_of(plot_format(t, py$format), p, y_gp)
      }
    }
    if (show_cat) for (i in seq_along(cats)) below(cat_labels[i], proj(cat_pos[i], 0, 0), x_gp)
  }
  # series axis along the right front-to-back edge
  if (length(ser_pos)) {
    plot_3d_line(proj, c(w, w), c(0, 0), c(0, d), axis_col)
    for (i in seq_along(ser_pos)) {
      p <- proj(w, 0, ser_pos[i])
      grid::grid.text(ser_labels[i], x = grid::unit(p$x, "npc") + grid::unit(6, "points"), y = p$y, just = c("left", "center"), gp = x_gp)
    }
  }
  invisible()
}

plot_3d_cartesian <- function(chart, series) {
  type <- series[[1]]$type
  cats <- series[[1]]$cats
  n_cat <- length(cats)
  n_ser <- length(series)
  grouping <- series[[1]]$grouping %||% "clustered"
  horizontal <- type == "bar3DChart" && identical(series[[1]]$dir, "bar")
  # the standard grouping (and every 3D line chart) puts each series into
  # its own row along the depth
  rows <- if (grouping == "standard" || type == "line3DChart") n_ser else 1

  vals <- lapply(series, function(s) {
    v <- s$values
    length(v) <- n_cat
    v
  })
  stacked <- grouping %in% c("stacked", "percentStacked")
  if (grouping == "percentStacked") {
    tot <- Reduce(`+`, lapply(vals, function(v) ifelse(is.na(v), 0, abs(v))))
    vals <- lapply(vals, function(v) ifelse(tot > 0, v / tot, 0))
  }
  bases <- vector("list", n_ser)
  tops <- vector("list", n_ser)
  pos <- rep(0, n_cat)
  neg <- rep(0, n_cat)
  for (j in seq_len(n_ser)) {
    v <- ifelse(is.na(vals[[j]]), 0, vals[[j]])
    if (stacked) {
      b <- ifelse(v >= 0, pos, neg)
      t <- b + v
      pos <- ifelse(v >= 0, t, pos)
      neg <- ifelse(v < 0, t, neg)
    } else {
      b <- rep(0, n_cat)
      t <- v
    }
    bases[[j]] <- b
    tops[[j]] <- t
  }
  all_v <- c(unlist(bases), unlist(tops), 0)
  py <- chart$axis_params$y
  if (grouping == "percentStacked") {
    py <- utils::modifyList(list(min = 0, max = 1, major = 0.1, format = "0%"), py[!vapply(py, is.null, logical(1))])
    chart$axis_params$y <- py
  }
  sc <- plot_scale(min(all_v, na.rm = TRUE), max(all_v, na.rm = TRUE), py)

  # a row of the box is as deep as a category slot is wide
  w <- 1
  slot <- w / n_cat
  row_d <- slot * (chart$view3d$depth_percent %||% 100) / 100
  d <- row_d * rows
  # a set height percentage is taken relative to the depth of the box
  h_pct <- chart$view3d$h_percent
  h <- if (is.null(h_pct)) 0.5 * w else h_pct / 100 * d
  proj0 <- plot_3d_projection(chart, w, h, d, stretch = is.null(h_pct))
  # for horizontal bars the categories run along y and the values along x;
  # the drawing code below always has categories along its first axis
  proj <- if (horizontal) function(x, y, z) proj0(y, x, z) else proj0
  cat_len <- if (horizontal) h else w
  val_len <- if (horizontal) w else h
  cat_slot <- cat_len / n_cat
  cat_pos <- (seq_len(n_cat) - 0.5) * cat_slot
  ser_pos <- if (rows > 1) (seq_len(rows) - 0.5) * row_d else NULL
  ser_labels <- if (rows > 1) vapply(series, function(s) s$label_text, character(1)) else NULL
  plot_3d_frame(chart, proj0, w, h, d, sc, cats, cat_pos, ser_labels, ser_pos, horizontal = horizontal)
  yy <- function(v) val_len * (v - sc$min) / (sc$max - sc$min)

  cols <- vapply(seq_len(n_ser), function(j) plot_color(series[[j]]$line$color, plot_auto_color(j, chart$palette)), character(1))
  lp <- chart$label_params
  labels <- list()

  if (type == "bar3DChart") {
    gap <- (series[[1]]$gap_width %||% 150) / 100
    gap_depth <- (chart$gap_depth %||% 150) / 100
    k <- if (grouping == "clustered") n_ser else 1
    bw <- cat_slot / (k + gap)
    bd <- row_d / (1 + gap_depth)
    shape <- chart$bar_shape %||% "box"
    stack_total <- if (stacked) Reduce(`+`, lapply(vals, function(v) ifelse(is.na(v), 0, v))) else NULL
    # back rows first, then from the left, then from the bottom
    for (r in rev(seq_len(rows))) {
      z0 <- (r - 1) * row_d + (row_d - bd) / 2
      for (i in seq_len(n_cat)) {
        for (j in seq_len(n_ser)) {
          if (rows > 1 && j != r) next
          if (is.na(vals[[j]][i])) next
          slot_j <- if (grouping == "clustered") j else 1
          x0 <- (i - 1) * cat_slot + gap / 2 * bw + (slot_j - 1) * bw
          y0 <- yy(bases[[j]][i])
          y1 <- yy(tops[[j]][i])
          pt_col <- cols[j]
          for (p in series[[j]]$points) if (p$idx == i - 1 && !is.null(p$color)) pt_col <- plot_color(p$color, cols[j])
          # a stacked pyramid or cone tapers over the whole stack
          part <- if (stacked && shape %in% c("pyramid", "cone") && stack_total[i] != 0) c(bases[[j]][i], tops[[j]][i]) / stack_total[i] else c(0, 1)
          plot_3d_box(proj, x0, x0 + bw, y0, y1, z0, z0 + bd, pt_col, shape, part)
          slp <- series[[j]]$label_params %||% lp
          if (plot_labels_on(slp)) {
            p <- proj(x0 + bw / 2, y1, z0)
            labels[[length(labels) + 1]] <- list(
              x = p$x, y = p$y, gp = plot_gpar_text(slp$style, 9, "#000000"),
              txt = plot_label_text(slp, cats[i], series[[j]]$values[i], name = series[[j]]$label_text)
            )
          }
        }
      }
    }
  } else if (type == "area3DChart") {
    for (r in rev(seq_len(rows))) {
      for (j in seq_len(n_ser)) {
        if (rows > 1 && j != r) next
        z0 <- (r - 1) * row_d
        z1 <- r * row_d
        xs <- cat_pos
        yb <- yy(bases[[j]])
        yt <- yy(tops[[j]])
        # top ribbon, right end and front face
        for (i in seq_len(n_cat - 1)) {
          plot_3d_poly(proj, c(xs[i], xs[i + 1], xs[i + 1], xs[i]), c(yt[i], yt[i + 1], yt[i + 1], yt[i]), c(z0, z0, z1, z1),
                       plot_shade(cols[j], 0.76))
        }
        plot_3d_poly(proj, rep(xs[n_cat], 4), c(yb[n_cat], yt[n_cat], yt[n_cat], yb[n_cat]), c(z0, z0, z1, z1), plot_shade(cols[j], 0.64))
        plot_3d_poly(proj, c(xs, rev(xs)), c(yb, rev(yt)), rep(z0, 2 * n_cat), cols[j])
      }
    }
  } else {
    # line3DChart: a flat ribbon per series, as deep as a bar would be and
    # slightly raised so its front shows
    gap_depth <- (chart$gap_depth %||% 150) / 100
    bd <- row_d / (1 + gap_depth)
    lift <- 0.02 * val_len
    for (r in rev(seq_len(rows))) {
      z0 <- (r - 1) * row_d + (row_d - bd) / 2
      z1 <- z0 + bd
      xs <- cat_pos
      ys <- yy(tops[[r]])
      for (i in seq_len(n_cat - 1)) {
        if (is.na(ys[i]) || is.na(ys[i + 1])) next
        plot_3d_poly(proj, c(xs[i], xs[i + 1], xs[i + 1], xs[i]), c(ys[i], ys[i + 1], ys[i + 1], ys[i]), c(z0, z0, z1, z1),
                     plot_shade(cols[r], 0.76))
        plot_3d_poly(proj, c(xs[i], xs[i + 1], xs[i + 1], xs[i]), c(ys[i] - lift, ys[i + 1] - lift, ys[i + 1], ys[i]), rep(z0, 4), cols[r])
      }
    }
  }
  for (l in labels) grid::grid.text(l$txt, x = l$x, y = grid::unit(l$y, "npc") + grid::unit(3, "points"), just = c("center", "bottom"), gp = l$gp)
  invisible()
}

plot_3d_pie <- function(chart, series) {
  s <- series[[1]]
  v <- abs(s$values)
  v[is.na(v)] <- 0
  n <- length(v)
  pal <- chart$palette
  if (length(s$line$color) > 1) pal <- s$line$color
  cols <- vapply(seq_len(n), function(i) plot_auto_color(i, pal), character(1))
  total <- sum(v)
  lp <- chart$label_params
  rot_x <- (chart$view3d$rot_x %||% 30) * pi / 180
  persp <- chart$view3d$perspective %||% 30
  # the disc lies in the x-z plane; its thickness is a share of the
  # diameter set by the height percentage
  r <- 0.5
  thick <- 0.134 * (chart$view3d$h_percent %||% 100) / 100
  dist <- 2.2 * tan(15 * pi / 180) / tan(max(persp, 1) / 2 * pi / 180)
  raw <- function(x, y, z) {
    y2 <- y * cos(rot_x) + z * sin(rot_x)
    z2 <- z * cos(rot_x) - y * sin(rot_x)
    f <- if (persp > 0) dist / (dist + z2) else 1
    list(x = x * f, y = y2 * f)
  }
  ang <- seq(0, 2 * pi, length.out = 181)
  rim <- raw(c(r * sin(ang), r * sin(ang)), c(rep(0, 181), rep(-thick, 181)), c(r * cos(ang), r * cos(ang)))
  xr <- range(rim$x)
  yr <- range(rim$y)
  w_pt <- grid::convertWidth(grid::unit(1, "npc"), "points", valueOnly = TRUE)
  h_pt <- grid::convertHeight(grid::unit(1, "npc"), "points", valueOnly = TRUE)
  scale <- min(w_pt * 0.9 / diff(xr), h_pt * 0.9 / diff(yr))
  cx <- w_pt / 2 - mean(xr) * scale
  cy <- h_pt / 2 - mean(yr) * scale
  to_pt <- function(x, y, z) {
    p <- raw(x, y, z)
    list(x = grid::unit(cx + p$x * scale, "points"), y = grid::unit(cy + p$y * scale, "points"))
  }
  start <- (chart$first_slice_ang %||% 0) * pi / 180
  edges <- list()
  a0 <- start
  for (i in seq_len(n)) {
    if (total == 0) break
    a1 <- a0 + 2 * pi * v[i] / total
    edges[[i]] <- c(a0, a1)
    a0 <- a1
  }
  # the rim of the near half, the slices farthest from the front first
  dist_front <- vapply(edges, function(e) abs(((mean(e) + pi) %% (2 * pi)) - pi), numeric(1))
  for (i in order(dist_front, decreasing = TRUE)) {
    e <- edges[[i]]
    a <- seq(e[1], e[2], length.out = max(2, ceiling((e[2] - e[1]) * 60)))
    a <- a[cos(a) < 0]
    if (length(a) < 2) next
    top <- to_pt(r * sin(a), 0, r * cos(a))
    bottom <- to_pt(r * sin(rev(a)), -thick, r * cos(rev(a)))
    grid::grid.polygon(grid::unit.c(top$x, bottom$x), grid::unit.c(top$y, bottom$y),
                       gp = grid::gpar(fill = plot_shade(cols[i], 0.6), col = NA))
  }
  center <- to_pt(0, 0, 0)
  for (i in seq_along(edges)) {
    e <- edges[[i]]
    a <- seq(e[1], e[2], length.out = max(2, ceiling((e[2] - e[1]) * 60)))
    p <- to_pt(r * sin(a), 0, r * cos(a))
    grid::grid.polygon(grid::unit.c(center$x, p$x), grid::unit.c(center$y, p$y), gp = grid::gpar(fill = cols[i], col = NA))
  }
  if (plot_labels_on(lp)) {
    for (i in seq_along(edges)) {
      if (v[i] == 0) next
      mid <- mean(edges[[i]])
      p <- to_pt(0.65 * r * sin(mid), 0, 0.65 * r * cos(mid))
      grid::grid.text(plot_label_text(lp, s$cats[i], s$values[i], pct = v[i] / total, name = s$label_text, sep = "\n"),
                      x = p$x, y = p$y, gp = plot_gpar_text(lp$style, 9, "#000000"))
    }
  }
  invisible()
}

# Value bands of a surface chart: breaks at the major unit and one color
# per band, from the lowest band up
plot_surface_bands <- function(chart, z, contour = FALSE) {
  # the axis ends at the major unit above the data; the contour view uses
  # twice the unit of the 3D view
  z <- z[is.finite(z)]
  if (!length(z)) z <- c(0, 1)
  sc <- plot_scale(min(z), max(z), chart$axis_params$y, pad = FALSE)
  if (contour && is.null(chart$axis_params$y$major)) {
    sc$major <- sc$major * 2
    if (is.null(chart$axis_params$y$max)) sc$max <- ceiling(sc$max / sc$major - 1e-9) * sc$major
    if (is.null(chart$axis_params$y$min)) sc$min <- floor(sc$min / sc$major + 1e-9) * sc$major
  }
  breaks <- plot_ticks(sc)
  n <- length(breaks) - 1
  accents <- plot_state$theme[paste0("accent", 1:6)]
  cols <- if (unname(accents[1]) %in% c("156082", "4472C4")) {
    paste0("#", ENCHARTER_SURFACE_HEX[(seq_len(n) - 1) %% length(ENCHARTER_SURFACE_HEX) + 1])
  } else {
    vapply(seq_len(n), function(i) plot_shade(paste0("#", accents[(i - 1) %% 6 + 1]), 0.87), character(1))
  }
  list(breaks = breaks, cols = cols, sc = sc)
}

# Cuts a planar polygon (x, y, z with the values `val` at its corners) to
# the part where `val` lies within [lo, hi]
plot_clip_band <- function(px, py, pz, val, lo, hi) {
  clip <- function(px, py, pz, val, at, above) {
    n <- length(px)
    ox <- oy <- oz <- ov <- numeric()
    for (i in seq_len(n)) {
      j <- if (i == n) 1 else i + 1
      in_i <- if (above) val[i] >= at else val[i] <= at
      in_j <- if (above) val[j] >= at else val[j] <= at
      if (in_i) {
        ox <- c(ox, px[i])
        oy <- c(oy, py[i])
        oz <- c(oz, pz[i])
        ov <- c(ov, val[i])
      }
      if (in_i != in_j) {
        t <- (at - val[i]) / (val[j] - val[i])
        ox <- c(ox, px[i] + t * (px[j] - px[i]))
        oy <- c(oy, py[i] + t * (py[j] - py[i]))
        oz <- c(oz, pz[i] + t * (pz[j] - pz[i]))
        ov <- c(ov, at)
      }
    }
    list(x = ox, y = oy, z = oz, v = ov)
  }
  a <- clip(px, py, pz, val, lo, TRUE)
  if (length(a$x) < 3) return(NULL)
  b <- clip(a$x, a$y, a$z, a$v, hi, FALSE)
  if (length(b$x) < 3) return(NULL)
  b
}

plot_surface <- function(chart, series) {
  # rows are series (along the depth), columns are categories
  cats <- series[[1]]$cats
  n_col <- length(cats)
  n_row <- length(series)
  z <- do.call(rbind, lapply(series, function(s) {
    v <- s$values
    length(v) <- n_col
    v
  }))
  contour <- series[[1]]$type == "surfaceChart"
  bands <- plot_surface_bands(chart, z, contour)
  sc <- bands$sc
  wire <- isTRUE(series[[1]]$filled)
  ser_labels <- vapply(series, function(s) s$label_text, character(1))
  px <- chart$axis_params$x
  x_gp <- plot_gpar_text(px, 10, "#000000")

  w <- 1
  if (contour) {
    # seen from above, the corners of the surface on the edges of the plot
    dd <- w
    h <- 0
    proj <- function(x, y, z) list(x = 0.08 + 0.8 * x / w, y = 0.08 + 0.84 * z / dd)
    xs <- (seq_len(n_col) - 1) * w / max(1, n_col - 1)
    zs <- (seq_len(n_row) - 1) * dd / max(1, n_row - 1)
    grid::grid.rect(x = 0.08, y = 0.08, width = 0.8, height = 0.84, just = c("left", "bottom"), gp = grid::gpar(fill = NA, col = "#D9D9D9"))
    for (i in seq_len(n_col)) {
      p <- proj(xs[i], 0, 0)
      grid::grid.text(plot_format(cats[i], px$format), x = p$x, y = grid::unit(p$y, "npc") - grid::unit(6, "points"),
                      just = c("center", "top"), gp = x_gp)
    }
    for (j in seq_len(n_row)) {
      p <- proj(w, 0, zs[j])
      grid::grid.text(ser_labels[j], x = grid::unit(p$x, "npc") + grid::unit(6, "points"), y = p$y, just = c("left", "center"), gp = x_gp)
    }
  } else {
    # the corners of the surface sit on the box edges
    dd <- w * (n_row - 1) / max(1, n_col - 1) * (chart$view3d$depth_percent %||% 100) / 100
    h_pct <- chart$view3d$h_percent
    h <- if (is.null(h_pct)) 0.5 * w else h_pct / 100 * dd
    proj <- plot_3d_projection(chart, w, h, dd, stretch = is.null(h_pct))
    xs <- (seq_len(n_col) - 1) * w / max(1, n_col - 1)
    zs <- (seq_len(n_row) - 1) * dd / max(1, n_row - 1)
    plot_3d_frame(chart, proj, w, h, dd, sc, cats, xs, ser_labels, zs, floor_grid = TRUE)
  }
  yy <- function(v) h * (v - sc$min) / (sc$max - sc$min)
  n_r <- nrow(z)
  n_c <- ncol(z)
  # cells from the back to the front, each split into four triangles
  # around its center so the bands follow the surface
  for (j in rev(seq_len(n_r - 1))) for (i in seq_len(n_c - 1)) {
    cx <- c(xs[i], xs[i + 1], xs[i + 1], xs[i])
    cz <- c(zs[j], zs[j], zs[j + 1], zs[j + 1])
    cv <- c(z[j, i], z[j, i + 1], z[j + 1, i + 1], z[j + 1, i])
    if (anyNA(cv)) next
    mx <- mean(cx)
    mz <- mean(cz)
    mv <- mean(cv)
    if (wire) {
      col <- bands$cols[min(length(bands$cols), max(1, findInterval(mv, bands$breaks, rightmost.closed = TRUE)))]
      plot_3d_line(proj, c(cx, cx[1]), yy(c(cv, cv[1])), c(cz, cz[1]), col, 1)
      next
    }
    for (t in 1:4) {
      u <- if (t == 4) 1 else t + 1
      tx <- c(cx[t], cx[u], mx)
      tz <- c(cz[t], cz[u], mz)
      tv <- c(cv[t], cv[u], mv)
      for (b in seq_along(bands$cols)) {
        lo <- bands$breaks[b]
        hi <- bands$breaks[b + 1]
        if (max(tv) < lo || min(tv) > hi) next
        part <- plot_clip_band(tx, yy(tv), tz, tv, lo, hi)
        if (is.null(part)) next
        plot_3d_poly(proj, part$x, part$y, part$z, bands$cols[b], if (contour) NA else bands$cols[b], 0.25)
      }
    }
  }
  invisible()
}
