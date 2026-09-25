#' Draw an interactive bias and tipping point explorer
#'
#' Asks how strong unmeasured confounding would have to be to change a
#' conclusion. The surface covers every pair of strengths an unmeasured
#' confounder could have, as a risk ratio with the exposure and a risk ratio
#' with the outcome, and is shaded by what would remain of the result under
#' it: an effect still clinically important with an interval clear of the
#' null, an interval clear of the null, an estimate on the same side of the
#' null, or nothing. Curves mark where each of these is lost, the E-values
#' for the estimate and for the confidence limit sit on the diagonal, and
#' measured covariates given as `benchmarks` show how strong confounding of
#' a known kind was.
#'
#' Beside the surface, the estimate as analyzed is drawn above the estimate
#' adjusted for a chosen confounder, one for each benchmark, and the ones at
#' the two E-values. In the widget, clicking the surface, or moving the two
#' sliders, chooses the confounder, and a sentence says what it would do.
#'
#' The adjustment divides the estimate by the bounding factor of Ding and
#' VanderWeele (2016), which is the most bias a confounder of those
#' strengths could cause, so the adjusted values are the worst case for each
#' pair. An odds ratio or a hazard ratio for a common outcome is first
#' converted to an approximate risk ratio, as VanderWeele and Ding (2017)
#' propose; set `rare = TRUE` when the outcome is rare, to use it as it is.
#'
#' @param estimate,lower,upper The estimate and its confidence interval, on
#'   the ratio scale.
#' @param measure `"RR"`, `"OR"` or `"HR"`.
#' @param rare Whether the outcome is rare, below about 15 percent, so that
#'   an odds ratio or hazard ratio can stand in for a risk ratio.
#' @param important The smallest effect that would matter clinically, on the
#'   same scale and on the same side of 1 as the estimate, such as 1.25 or
#'   0.8. Optional.
#' @param benchmarks Optional data frame of measured covariates to compare
#'   with, with columns `label`, `exposure` and `outcome`: each covariate's
#'   risk ratio with the exposure and with the outcome.
#' @param max_strength The largest strength on the axes. Defaults to a
#'   little past the E-value.
#' @param xlab,ylab Axis labels of the surface.
#' @param title,caption Title above the plot and note below it.
#' @param family Font family. The package ships Lato and registers it on load.
#'
#' @return An object of class `ggsensitivity`, which prints as an interactive
#'   widget. Use [graph_widget()], [graph_plot()] or [graph_save()] for the
#'   widget, a static ggplot or a file. The field `evalues` holds the
#'   E-values and `benchmarks` the adjusted estimates for each benchmark.
#' @export
#'
#' @examples
#' ggsensitivity(1.8, 1.4, 2.31, important = 1.25,
#'               benchmarks = data.frame(label = c("Age", "Smoking"),
#'                                       exposure = c(1.6, 2.3), outcome = c(1.9, 1.5)))
ggsensitivity <- function(estimate, lower, upper, measure = c("RR", "OR", "HR"),
                          rare = FALSE, important = NULL, benchmarks = NULL,
                          max_strength = NULL,
                          xlab = "Confounder with the exposure (risk ratio)",
                          ylab = "Confounder with the outcome (risk ratio)",
                          title = NULL, caption = NULL, family = "Lato") {
  measure <- match.arg(measure)
  sv <- sens_prepare(estimate, lower, upper, measure, rare, important, benchmarks, max_strength)
  lay <- sens_layout(sv, xlab, ylab, title, caption, family)
  structure(
    list(plot = sens_draw(lay), width = lay$page$width / 72, height = lay$page$height / 72,
         title = title, on_render = "ggextremeSensitivity(el, data);",
         render_data = sens_render(lay), hover_inv = "opacity:1;",
         evalues = data.frame(target = c("estimate", "confidence limit"),
                              evalue = c(sv$e_est, sv$e_lim)),
         benchmarks = sv$bench),
    class = c("ggsensitivity", "ggx_graph")
  )
}

sens_bias <- function(a, b) a * b / (a + b - 1)

# The E-value of a risk ratio of at least 1.
sens_evalue <- function(rr) if (rr <= 1) 1 else rr + sqrt(rr * (rr - 1))

sens_prepare <- function(estimate, lower, upper, measure, rare, important, benchmarks, max_strength) {
  one <- function(v, what) {
    if (!is.numeric(v) || length(v) != 1 || !is.finite(v) || v <= 0) {
      rlang::abort(paste0("`", what, "` must be one positive ratio."))
    }
  }
  one(estimate, "estimate")
  one(lower, "lower")
  one(upper, "upper")
  if (!(lower <= estimate && estimate <= upper)) rlang::abort("The interval must hold the estimate.")
  # An odds ratio or a hazard ratio for a common outcome becomes an
  # approximate risk ratio.
  convert <- function(v) {
    if (measure == "RR" || rare) return(v)
    if (measure == "OR") return(sqrt(v))
    (1 - 0.5^sqrt(v)) / (1 - 0.5^sqrt(1 / v))
  }
  est <- convert(estimate)
  lo <- convert(lower)
  hi <- convert(upper)
  # Work with an effect above 1; a protective one is mirrored.
  up <- est >= 1
  flip <- function(v) if (up) v else 1 / v
  e <- flip(est)
  limit <- if (up) lo else 1 / hi
  far <- if (up) hi else 1 / lo
  imp <- NULL
  if (!is.null(important)) {
    one(important, "important")
    imp <- flip(convert(important))
    if (imp <= 1) rlang::abort("`important` must be on the same side of 1 as the estimate.")
  }
  e_est <- sens_evalue(e)
  e_lim <- sens_evalue(limit)
  bench <- NULL
  if (!is.null(benchmarks)) {
    if (!is.data.frame(benchmarks) || !all(c("label", "exposure", "outcome") %in% names(benchmarks))) {
      rlang::abort("`benchmarks` must be a data frame with columns label, exposure and outcome.")
    }
    bx <- pmax(benchmarks$exposure, 1 / benchmarks$exposure)
    by <- pmax(benchmarks$outcome, 1 / benchmarks$outcome)
    b <- sens_bias(bx, by)
    unflip <- function(v) if (up) v else 1 / v
    bench <- data.frame(label = as.character(benchmarks$label), exposure = bx, outcome = by,
                        bias = b, estimate = unflip(e / b),
                        lower = pmin(unflip(limit / b), unflip(far / b)),
                        upper = pmax(unflip(limit / b), unflip(far / b)), stringsAsFactors = FALSE)
  }
  top <- if (is.null(max_strength)) {
    max(3, ceiling(1.35 * e_est), if (!is.null(bench)) ceiling(1.15 * max(bench$exposure, bench$outcome)))
  } else max_strength
  list(estimate = estimate, lower = lower, upper = upper, measure = measure, rare = rare,
       converted = measure != "RR" && !rare, up = up, e = e, limit = limit, far = far,
       imp = imp, important = important, e_est = e_est, e_lim = e_lim, bench = bench, top = top)
}

sens_ink <- list(strong = "#22928F", clear = "#22928F", same = "#9A9A9A", marker = "#C0603F",
                 bench = "#7A5AA6", important = "#C88A1E")

sens_dims <- function() {
  utils::modifyList(graph_dims, list(surface = 300, side_w = 300, gap = 76, row_h = 46,
                                     text_pt = 10, small_pt = 9))
}

sens_num <- function(v) formatC(v, format = "f", digits = 2)

# The rows beside the surface: as analyzed, then adjusted estimates.
sens_rows <- function(sv, chosen) {
  unflip <- function(v) if (sv$up) v else 1 / v
  adj <- function(a, b) {
    bias <- sens_bias(a, b)
    c(unflip(sv$e / bias), sort(c(unflip(sv$limit / bias), unflip(sv$far / bias))))
  }
  rows <- list(list(label = "As analyzed", sub = "", v = c(sv$estimate, sv$lower, sv$upper), raw = TRUE))
  rows[[2]] <- list(label = "Under the chosen confounder", id = "chosen",
                    sub = paste0(sens_num(chosen[1]), " with the exposure, ", sens_num(chosen[2]), " with the outcome"),
                    v = adj(chosen[1], chosen[2]))
  for (i in seq_len(if (is.null(sv$bench)) 0 else nrow(sv$bench))) {
    b <- sv$bench[i, ]
    rows[[length(rows) + 1]] <- list(label = paste("As strong as", b$label),
                                     sub = paste0(sens_num(b$exposure), " and ", sens_num(b$outcome)),
                                     v = adj(b$exposure, b$outcome))
  }
  rows[[length(rows) + 1]] <- list(label = "At the E-value for the limit",
                                   sub = paste0(sens_num(sv$e_lim), " each: the interval reaches 1"),
                                   v = adj(sv$e_lim, sv$e_lim))
  rows[[length(rows) + 1]] <- list(label = "At the E-value for the estimate",
                                   sub = paste0(sens_num(sv$e_est), " each: the estimate reaches 1"),
                                   v = adj(sv$e_est, sv$e_est))
  rows
}

sens_scale_word <- function(sv) {
  if (sv$measure == "RR") "risk ratio" else if (sv$converted) "approximate risk ratio" else
    paste0(if (sv$measure == "OR") "odds ratio" else "hazard ratio", ", read as a risk ratio")
}

sens_evalue_sentence <- function(sv) {
  paste0("E-value ", sens_num(sv$e_est), ": an unmeasured confounder associated with both the exposure ",
         "and the outcome by a risk ratio of ", sens_num(sv$e_est), " each, beyond the measured covariates, ",
         "could explain away the estimate; one of ", sens_num(sv$e_lim), " each could move the ",
         "confidence limit to 1. Weaker confounding could not.")
}

sens_layout <- function(sv, xlab, ylab, title, caption, family) {
  dims <- sens_dims()
  s0 <- 46
  s1 <- s0 + dims$surface
  top <- 24
  bottom <- top + dims$surface
  f0 <- s1 + dims$gap
  f1 <- f0 + dims$side_w
  M <- sv$top
  X <- function(a) s0 + (a - 1) / (M - 1) * (s1 - s0)
  Y <- function(b) bottom - (b - 1) / (M - 1) * (bottom - top)
  # A confounder of moderate strength to start from, which the widget moves.
  chosen <- rep(min(2, (1 + M) / 2), 2)
  rows <- sens_rows(sv, chosen)
  # A log axis for the side panel, holding every row and the null.
  vals <- unlist(lapply(rows, `[[`, "v"))
  vals <- vals[is.finite(vals) & vals > 0]
  lo <- min(c(vals, 1, sv$important)) / 1.08
  hi <- max(c(vals, 1, sv$important)) * 1.08
  Fx <- function(v) f0 + (log(v) - log(lo)) / (log(hi) - log(lo)) * (f1 - f0)
  rows_top <- top + 8
  rows_bottom <- rows_top + length(rows) * dims$row_h
  words <- wrap_words(sens_evalue_sentence(sv), f1 - f0, dims$small_pt, family)
  side_bottom <- rows_bottom + 52 + length(words) * dims$small_pt * 1.4
  keys <- 3 + !is.null(sv$imp)
  surface_bottom <- bottom + 46 + keys * 14
  cap <- c(caption, wrap_words(paste0(
    "Adjusted values divide by the bounding factor of Ding and VanderWeele, the most a confounder of ",
    "each strength could explain, so they are the worst case. On the ", sens_scale_word(sv), " scale",
    if (sv$converted) paste0(", converted from the ", if (sv$measure == "OR") "odds ratio" else "hazard ratio",
                             " for a common outcome") else "",
    ". The solid curve is where the estimate reaches 1, the dashed one where the confidence limit does",
    if (!is.null(sv$imp)) ", and the dotted one where the estimate falls to the clinically important value" else "",
    ". Benchmarks are measured covariates, shown for comparison; an unmeasured confounder need not be like them."
  ), f1 - 20, dims$caption_pt, family))
  page <- graph_canvas(c(0, f1 + 20), c(0, max(surface_bottom, side_bottom)), title, cap, character(0),
                       character(0), NULL, dims, family)
  list(sv = sv, dims = dims, page = page, family = family, s0 = s0, s1 = s1, top = top, bottom = bottom,
       f0 = f0, f1 = f1, M = M, X = X, Y = Y, rows = rows, chosen = chosen, lo = lo, hi = hi, Fx = Fx,
       rows_top = rows_top, rows_bottom = rows_bottom, words = words, xlab = xlab, ylab = ylab)
}

# The part of the surface where the bias stays below `t`, as a polygon in
# strength units. Where a > t the bias never reaches t; past it, the edge
# is the curve b = t (a - 1) / (a - t).
sens_region <- function(t, M) {
  if (t <= 1) return(NULL)
  if (t >= M) return(cbind(c(1, 1, M, M), c(1, M, M, 1)))
  start <- t * (M - 1) / (M - t)
  a <- seq(start, M, length.out = 80)
  b <- t * (a - 1) / (a - t)
  cbind(c(1, 1, a, M), c(1, M, b, 1))
}

sens_curve <- function(t, M) {
  if (t <= 1 || t >= M) return(NULL)
  start <- t * (M - 1) / (M - t)
  a <- seq(start, M, length.out = 80)
  cbind(a, t * (a - 1) / (a - t))
}

sens_draw <- function(lay) {
  sv <- lay$sv
  dims <- lay$dims
  px <- lay$page$px
  py <- lay$page$py
  X <- lay$X
  Y <- lay$Y
  M <- lay$M
  texts <- function(label, x, y, pt, color, hjust = 0, face = "plain") {
    if (!length(label)) return(empty_texts())
    data.frame(x = px(x), y = py(y), label = as.character(label), size = pt / .pt,
               colour = color, hjust = hjust, fontface = face, stringsAsFactors = FALSE)
  }
  seg <- function(x, xend, y, yend) data.frame(x = px(x), xend = px(xend), y = py(y), yend = py(yend))
  poly <- function(m, id, fill, alpha) {
    if (is.null(m)) return(NULL)
    data.frame(x = px(X(m[, 1])), y = py(Y(m[, 2])), id = id, fill = fill, alpha = alpha,
               stringsAsFactors = FALSE)
  }
  strong_t <- if (!is.null(sv$imp)) min(sv$limit, sv$e / sv$imp) else NULL
  regions <- rbind(
    poly(sens_region(sv$e, M), "rg1", sens_ink$same, 0.16),
    poly(sens_region(sv$limit, M), "rg2", sens_ink$clear, 0.2),
    if (!is.null(strong_t)) poly(sens_region(strong_t, M), "rg3", sens_ink$strong, 0.45)
  )
  curve_df <- function(t, id) {
    m <- sens_curve(t, M)
    if (is.null(m)) return(NULL)
    data.frame(x = px(X(m[, 1])), y = py(Y(m[, 2])), id = id, stringsAsFactors = FALSE)
  }
  ticks <- scales::breaks_extended(6)(c(1, M))
  ticks <- ticks[ticks >= 1 & ticks <= M]
  frames <- rbind(
    seg(lay$s0, lay$s1, lay$bottom, lay$bottom), seg(lay$s0, lay$s0, lay$top, lay$bottom),
    seg(X(ticks), X(ticks), lay$bottom, lay$bottom + 4), seg(lay$s0 - 4, lay$s0, Y(ticks), Y(ticks))
  )
  ev <- data.frame(x = px(X(c(sv$e_est, sv$e_lim))), y = py(Y(c(sv$e_est, sv$e_lim))),
                   fill = c(graph_ink$title, graph_ink$page))
  ev <- ev[c(sv$e_est, sv$e_lim) <= M, , drop = FALSE]
  labels <- rbind(
    texts(format(ticks), X(ticks), rep(lay$bottom + 13, length(ticks)), dims$small_pt, graph_ink$muted, hjust = 0.5),
    texts(format(ticks), lay$s0 - 7, Y(ticks), dims$small_pt, graph_ink$muted, hjust = 1),
    texts(lay$xlab, (lay$s0 + lay$s1) / 2, lay$bottom + 29, dims$text_pt, graph_ink$text, hjust = 0.5),
    texts(lay$ylab, lay$s0 - 32, lay$top - 12, dims$text_pt, graph_ink$text),
    if (sv$e_est <= M) texts(paste("E-value", sens_num(sv$e_est)), X(sv$e_est) + 7, Y(sv$e_est) - 7,
                             dims$small_pt, graph_ink$title, face = "bold"),
    if (sv$e_lim <= M) texts(paste("for the limit", sens_num(sv$e_lim)), X(sv$e_lim) + 7, Y(sv$e_lim) + 9,
                             dims$small_pt, graph_ink$title)
  )
  bench_pts <- NULL
  if (!is.null(sv$bench)) {
    b <- sv$bench[sv$bench$exposure <= M & sv$bench$outcome <= M, ]
    if (nrow(b)) {
      bench_pts <- do.call(rbind, lapply(seq_len(nrow(b)), function(i) {
        cx <- X(b$exposure[i])
        cy <- Y(b$outcome[i])
        data.frame(x = px(cx + c(0, 5.5, 0, -5.5)), y = py(cy + c(-5.5, 0, 5.5, 0)), group = paste0("b", i))
      }))
      labels <- rbind(labels, texts(b$label, X(b$exposure) + 8, Y(b$outcome), dims$small_pt, sens_ink$bench,
                                    face = "bold"))
    }
  }

  # Rows beside the surface.
  Fx <- lay$Fx
  rows <- lay$rows
  row_y <- function(i) lay$rows_top + (i - 0.5) * lay$dims$row_h
  bars <- NULL
  dots <- NULL
  live <- empty_texts()
  live$id <- character(0)
  for (i in seq_along(rows)) {
    r <- rows[[i]]
    y <- row_y(i) + 6
    id <- if (identical(r$id, "chosen")) "ch" else paste0("r", i)
    v <- pmin(pmax(r$v, lay$lo), lay$hi)
    bars <- rbind(bars, data.frame(seg(Fx(v[2]), Fx(v[3]), y, y), id = paste0(id, "b"),
                                   colour = if (i == 1) graph_ink$title else sens_ink$marker))
    dots <- rbind(dots, data.frame(x = px(Fx(v[1])), y = py(y), id = paste0(id, "p"),
                                   fill = if (i == 1) graph_ink$title else sens_ink$marker))
    vtxt <- paste0(sens_num(r$v[1]), " (", sens_num(r$v[2]), " to ", sens_num(r$v[3]), ")")
    live <- rbind(live,
      data.frame(texts(r$label, lay$f0, y - 13, dims$small_pt, graph_ink$title, face = "bold"), id = paste0(id, "l")),
      data.frame(texts(vtxt, lay$f1, y - 13, dims$small_pt, graph_ink$text, hjust = 1), id = paste0(id, "v")),
      if (nzchar(r$sub)) data.frame(texts(r$sub, lay$f0, y + 12, dims$small_pt - 1, graph_ink$muted), id = paste0(id, "s")))
  }
  axis_ticks <- c(0.1, 0.2, 0.25, 0.5, 0.67, 0.8, 1, 1.25, 1.5, 2, 3, 4, 5, 10, 20)
  axis_ticks <- axis_ticks[axis_ticks >= lay$lo & axis_ticks <= lay$hi]
  side_frames <- rbind(
    seg(lay$f0, lay$f1, lay$rows_bottom + 6, lay$rows_bottom + 6),
    seg(Fx(axis_ticks), Fx(axis_ticks), lay$rows_bottom + 6, lay$rows_bottom + 10)
  )
  refs <- rbind(
    data.frame(seg(Fx(1), Fx(1), lay$rows_top, lay$rows_bottom + 6), colour = graph_ink$muted),
    if (!is.null(sv$important)) data.frame(seg(Fx(sv$important), Fx(sv$important), lay$rows_top, lay$rows_bottom + 6),
                                           colour = sens_ink$important)
  )
  labels <- rbind(labels,
    texts(format(axis_ticks), Fx(axis_ticks), rep(lay$rows_bottom + 19, length(axis_ticks)), dims$small_pt,
          graph_ink$muted, hjust = 0.5),
    texts(paste0(capitalize(sens_scale_word(sv)), " (log scale)"), (lay$f0 + lay$f1) / 2, lay$rows_bottom + 33,
          dims$text_pt, graph_ink$text, hjust = 0.5),
    if (!is.null(sv$important)) texts("clinically important", Fx(sv$important) + 3, lay$rows_top - 4,
                                      dims$small_pt - 1, sens_ink$important),
    texts(lay$words, lay$f0, lay$rows_bottom + 52 + (seq_along(lay$words) - 1) * dims$small_pt * 1.4,
          dims$small_pt, graph_ink$text)
  )
  # A key to the shading, under the surface.
  key <- data.frame(label = c(if (!is.null(sv$imp)) "Still important, interval clear of 1",
                              "Interval clear of 1", "Estimate on the same side of 1", "Could be explained away"),
                    fill = c(if (!is.null(sv$imp)) sens_ink$strong, sens_ink$clear, sens_ink$same, graph_ink$page),
                    alpha = c(if (!is.null(sv$imp)) 0.6, 0.3, 0.25, 1), stringsAsFactors = FALSE)
  key_y <- lay$bottom + 46 + (seq_len(nrow(key)) - 1) * 14
  key_rects <- data.frame(xmin = px(lay$s0), xmax = px(lay$s0 + 10), ymin = py(key_y + 5), ymax = py(key_y - 5),
                          fill = key$fill, alpha = key$alpha)
  labels <- rbind(labels, texts(key$label, lay$s0 + 15, key_y, dims$small_pt, graph_ink$text))
  marker <- data.frame(x = px(X(lay$chosen[1])), y = py(Y(lay$chosen[2])))

  p <- ggplot() +
    ggiraph::geom_polygon_interactive(
      data = regions, aes(x = .data$x, y = .data$y, group = .data$id, fill = .data$fill,
                          alpha = .data$alpha, data_id = .data$id), colour = NA)
  curves <- list(list(sv$e, "solid"), list(sv$limit, "22"))
  if (!is.null(sv$imp)) curves[[3]] <- list(sv$e / sv$imp, "12")
  for (cv in curves) {
    d <- curve_df(cv[[1]], "cv")
    if (!is.null(d)) p <- p + geom_path(data = d, aes(x = .data$x, y = .data$y), colour = graph_ink$title,
                                        linewidth = 1 / .pt, linetype = cv[[2]])
  }
  p <- p +
    geom_segment(data = frames, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
                 colour = graph_ink$text, linewidth = 0.8 / .pt) +
    geom_segment(data = seg(X(1), X(M), Y(1), Y(M)), aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
                 colour = graph_ink$plain_border, linewidth = 0.5 / .pt, linetype = "22") +
    geom_point(data = ev, aes(x = .data$x, y = .data$y, fill = .data$fill), shape = 21, size = 2.4,
               colour = graph_ink$title, stroke = 0.8) +
    (if (!is.null(bench_pts)) geom_polygon(data = bench_pts, aes(x = .data$x, y = .data$y, group = .data$group),
                                           fill = sens_ink$bench)) +
    ggiraph::geom_point_interactive(data = marker, aes(x = .data$x, y = .data$y, data_id = "mk"),
                                    shape = 21, size = 3.6, fill = NA, colour = sens_ink$marker, stroke = 1.4) +
    geom_segment(data = refs, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend,
                                  colour = .data$colour), linewidth = 0.8 / .pt, linetype = "22") +
    geom_segment(data = side_frames, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
                 colour = graph_ink$text, linewidth = 0.8 / .pt) +
    ggiraph::geom_segment_interactive(
      data = bars, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend, colour = .data$colour,
                       data_id = .data$id), linewidth = 2.2 / .pt, lineend = "round") +
    ggiraph::geom_point_interactive(
      data = dots, aes(x = .data$x, y = .data$y, fill = .data$fill, data_id = .data$id),
      shape = 21, size = 2.6, colour = graph_ink$page, stroke = 0.8) +
    geom_rect(data = key_rects, aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, ymax = .data$ymax,
                                    fill = .data$fill, alpha = .data$alpha), colour = graph_ink$plain_border,
              linewidth = 0.4 / .pt) +
    draw_text(labels, lay$family) +
    ggiraph::geom_text_interactive(
      data = live, aes(x = .data$x, y = .data$y, label = .data$label, data_id = .data$id),
      hjust = live$hjust, size = live$size, colour = live$colour, fontface = live$fontface,
      family = lay$family) +
    graph_frame(lay$page, lay$family)
  p
}

sens_render <- function(lay) {
  sv <- lay$sv
  list(up = sv$up, e = sv$e, limit = sv$limit, far = sv$far, imp = sv$imp, important = sv$important,
       top = lay$M, chosen = lay$chosen, lo = lay$lo, hi = lay$hi, scale = sens_scale_word(sv),
       dx = lay$page$px(0), dy = lay$page$height - lay$page$py(0),
       s0 = lay$s0, s1 = lay$s1, stop = lay$top, sbottom = lay$bottom, f0 = lay$f0, f1 = lay$f1)
}
