#' Draw an interactive responder threshold plot
#'
#' Shows what a responder definition keeps and what it throws away. For
#' two arms, the curves give the share of patients in each arm who improved
#' by at least each amount, so the whole distribution of change stays in
#' view, with the prespecified threshold marked. Beside them, the
#' difference in responders is drawn across every possible threshold with
#' its confidence band, which shows how much the conclusion depends on
#' where the line is drawn. Under both, a table gives the responders in
#' each arm, their difference, the number needed to treat, and the mean
#' difference, which uses every patient.
#'
#' In the widget, a slider or a drag on either panel moves the threshold,
#' and every number and a sentence follow; a button returns to the
#' prespecified threshold.
#'
#' Responders have Wilson score intervals and their difference the hybrid
#' score interval of Newcombe (1998). The number needed to treat is the
#' reciprocal of the difference; when the interval of the difference
#' includes zero, its interval runs from benefit through infinity to harm,
#' as Altman (1998) describes. The mean difference has a Welch interval.
#'
#' @param formula A formula `change ~ arm`, where `change` is each patient's
#'   change from baseline and `arm` has two values.
#' @param data A data frame holding both.
#' @param threshold The prespecified improvement that makes a responder, in
#'   the units of `change`, such as a minimal important difference.
#' @param higher_is_better Whether a rise in `change` is an improvement.
#'   When `FALSE`, as for pain, the change is turned around so that
#'   improvement is positive.
#' @param reference The control arm. Defaults to the first level of `arm`.
#' @param xlab Label of the improvement axis, with its unit.
#' @param level Confidence level for the intervals.
#' @param title,caption Title above the plot and note below it.
#' @param family Font family. The package ships Lato and registers it on load.
#'
#' @return An object of class `ggresponder`, which prints as an interactive
#'   widget. Use [graph_widget()], [graph_plot()] or [graph_save()] for the
#'   widget, a static ggplot or a file. The field `responders` holds the
#'   measures at the threshold and `curve` the difference at every
#'   threshold.
#' @export
#'
#' @examples
#' set.seed(3)
#' pain <- data.frame(arm = rep(c("Placebo", "Active"), each = 120),
#'                    change = c(rnorm(120, -1.3, 2), rnorm(120, -2.2, 2)))
#' ggresponder(change ~ arm, pain, threshold = 2, higher_is_better = FALSE,
#'             xlab = "Improvement in pain (points on a 0 to 10 scale)")
ggresponder <- function(formula, data, threshold, higher_is_better = TRUE, reference = NULL,
                        xlab = "Improvement from baseline", level = 0.95, title = NULL,
                        caption = NULL, family = "Lato") {
  rs <- resp_prepare(formula, data, threshold, higher_is_better, reference, level)
  lay <- resp_layout(rs, xlab, title, caption, family)
  structure(
    list(plot = resp_draw(lay), width = lay$page$width / 72, height = lay$page$height / 72,
         title = title, on_render = "ggextremeResponder(el, data);", render_data = resp_render(lay),
         hover_inv = "opacity:1;", responders = rs$table, curve = rs$curve),
    class = c("ggresponder", "ggx_graph")
  )
}

# The responders in each arm at a threshold, their difference and the
# number needed to treat, with intervals.
resp_at <- function(y1, y0, t, z) {
  k1 <- sum(y1 >= t)
  k0 <- sum(y0 >= t)
  w1 <- wilson(k1, length(y1), z)
  w0 <- wilson(k0, length(y0), z)
  d <- w1[1] - w0[1]
  lo <- d - sqrt((w1[1] - w1[2])^2 + (w0[3] - w0[1])^2)
  hi <- d + sqrt((w1[3] - w1[1])^2 + (w0[1] - w0[2])^2)
  list(k1 = k1, k0 = k0, p1 = w1, p0 = w0, diff = c(d, lo, hi))
}

resp_prepare <- function(formula, data, threshold, higher_is_better, reference, level) {
  if (!inherits(formula, "formula") || length(formula) != 3) rlang::abort("`formula` must be `change ~ arm`.")
  if (!is.data.frame(data)) rlang::abort("`data` must be a data frame.")
  if (missing(threshold) || !is.numeric(threshold) || length(threshold) != 1 || !is.finite(threshold)) {
    rlang::abort("`threshold` must be one number: the prespecified improvement that makes a responder.")
  }
  y <- eval(formula[[2]], data, environment(formula))
  g <- eval(formula[[3]], data, environment(formula))
  if (!is.numeric(y)) rlang::abort("The change must be numeric.")
  f <- if (is.factor(g)) droplevels(g) else factor(g)
  if (nlevels(f) != 2) rlang::abort("The arm must have exactly two values.")
  if (!is.null(reference)) {
    if (!reference %in% levels(f)) rlang::abort("`reference` must be one of the arms.")
    f <- stats::relevel(f, reference)
  }
  keep <- !is.na(y) & !is.na(f)
  y <- if (higher_is_better) y[keep] else -y[keep]
  f <- f[keep]
  arms <- levels(f)
  y1 <- sort(y[f == arms[2]])
  y0 <- sort(y[f == arms[1]])
  if (length(y1) < 2 || length(y0) < 2) rlang::abort("Each arm needs at least two patients.")
  z <- stats::qnorm(1 - (1 - level) / 2)
  at <- resp_at(y1, y0, threshold, z)
  md <- mean(y1) - mean(y0)
  v1 <- stats::var(y1) / length(y1)
  v0 <- stats::var(y0) / length(y0)
  df <- (v1 + v0)^2 / (v1^2 / (length(y1) - 1) + v0^2 / (length(y0) - 1))
  md_ci <- md + c(-1, 1) * stats::qt(1 - (1 - level) / 2, df) * sqrt(v1 + v0)
  span <- range(c(y1, y0))
  grid <- seq(span[1], span[2], length.out = 121)
  curve <- do.call(rbind, lapply(grid, function(t) {
    a <- resp_at(y1, y0, t, z)
    data.frame(threshold = t, difference = a$diff[1], lower = a$diff[2], upper = a$diff[3])
  }))
  table <- data.frame(
    measure = c(paste("Responders,", arms[2]), paste("Responders,", arms[1]), "Difference", "Mean difference"),
    estimate = c(at$p1[1], at$p0[1], at$diff[1], md),
    lower = c(at$p1[2], at$p0[2], at$diff[2], md_ci[1]),
    upper = c(at$p1[3], at$p0[3], at$diff[3], md_ci[2]), stringsAsFactors = FALSE)
  list(y1 = y1, y0 = y0, arms = arms, threshold = threshold, z = z, level = level, at = at,
       md = c(md, md_ci), curve = curve, table = table, dropped = sum(!keep),
       higher_is_better = higher_is_better)
}

resp_ink <- list(active = "#22928F", control = "#8A8A8A", diff = "#7A5AA6")

resp_pct <- function(v) paste0(formatC(100 * v, format = "f", digits = 0), "%")

# The number needed to treat, in words, with Altman's interval.
resp_nnt <- function(d, level) {
  f <- function(v) formatC(1 / abs(v), format = "f", digits = 1)
  lead <- paste0(format(level * 100), "% CI ")
  if (abs(d[1]) < 1e-12) return(c(v = "none", u = "no difference in responders"))
  kind <- if (d[1] > 0) "NNTB" else "NNTH"
  v <- paste(kind, f(d[1]))
  u <- if (d[2] > 0) paste0(lead, "NNTB ", f(d[3]), " to ", f(d[2])) else
    if (d[3] < 0) paste0(lead, "NNTH ", f(d[2]), " to ", f(d[3])) else
      paste0(lead, "NNTB ", f(d[3]), " to \u221e to NNTH ", f(d[2]))
  c(v = v, u = u)
}

resp_cells <- function(rs) {
  at <- rs$at
  lead <- paste0(format(rs$level * 100), "% CI ")
  ci <- function(v) paste0(lead, resp_pct(v[2]), " to ", resp_pct(v[3]))
  nnt <- resp_nnt(at$diff, rs$level)
  signed <- function(v) paste0(if (v > 0) "+" else if (v < 0) "\u2212" else "", formatC(abs(100 * v), format = "f", digits = 1))
  num <- function(v) formatC(v, format = "f", digits = 2)
  list(
    list(k = paste("Responders,", rs$arms[2]), v = resp_pct(at$p1[1]),
         u = paste0(at$k1, " of ", length(rs$y1), "; ", ci(at$p1))),
    list(k = paste("Responders,", rs$arms[1]), v = resp_pct(at$p0[1]),
         u = paste0(at$k0, " of ", length(rs$y0), "; ", ci(at$p0))),
    list(k = "Difference", v = signed(at$diff[1]),
         u = paste0("percentage points; ", lead, signed(at$diff[2]), " to ", signed(at$diff[3]))),
    list(k = "Number needed to treat", v = nnt[["v"]], u = nnt[["u"]]),
    list(k = "Mean difference", v = num(rs$md[1]),
         u = paste0(lead, num(rs$md[2]), " to ", num(rs$md[3]), "; uses every patient"))
  )
}

resp_sentence <- function(rs) {
  at <- rs$at
  paste0("With a threshold of ", format(rs$threshold), ", ", resp_pct(at$p1[1]), " respond on ", rs$arms[2],
         " and ", resp_pct(at$p0[1]), " on ", rs$arms[1], ": a difference of ",
         formatC(100 * at$diff[1], format = "f", digits = 1), " percentage points (",
         formatC(100 * at$diff[2], format = "f", digits = 1), " to ", formatC(100 * at$diff[3], format = "f", digits = 1), ").")
}

resp_layout <- function(rs, xlab, title, caption, family) {
  dims <- utils::modifyList(graph_dims, list(panel_w = 340, panel_h = 200, gap = 60, text_pt = 10,
                                             small_pt = 9, value_pt = 13))
  a0 <- 46
  a1 <- a0 + dims$panel_w
  b0 <- a1 + dims$gap
  b1 <- b0 + dims$panel_w
  top <- 22
  bottom <- top + dims$panel_h
  lo <- min(c(rs$y1, rs$y0))
  hi <- max(c(rs$y1, rs$y0))
  X <- function(v) a0 + (v - lo) / (hi - lo) * (a1 - a0)
  Xb <- function(v) b0 + (v - lo) / (hi - lo) * (b1 - b0)
  Y <- function(p) bottom - p * (bottom - top)
  dr <- range(c(rs$curve$lower, rs$curve$upper, 0))
  dr <- dr + c(-1, 1) * 0.05 * diff(dr)
  Yd <- function(v) bottom - (v - dr[1]) / diff(dr) * (bottom - top)
  breaks <- scales::breaks_extended(6)(c(lo, hi))
  breaks <- breaks[breaks >= lo & breaks <= hi]
  dbreaks <- scales::breaks_extended(5)(dr)
  dbreaks <- dbreaks[dbreaks >= dr[1] & dbreaks <= dr[2]]
  sentence_y <- bottom + 50
  table_top <- sentence_y + 26
  right <- b1 + 20
  cap <- c(caption, wrap_words(paste0(
    "The threshold of ", format(rs$threshold), " was prespecified. ",
    "A responder improved by at least the threshold",
    if (!rs$higher_is_better) ", with the change turned around so that improvement is positive" else "",
    ". Responders have Wilson intervals and their difference Newcombe's; ", format(rs$level * 100),
    "% intervals. Splitting patients at a threshold discards how much each one changed; the mean difference ",
    "uses every patient.",
    if (rs$dropped) paste0(" ", rs$dropped, " patients without a change were left out.") else ""
  ), right - 20, dims$caption_pt, family))
  page <- graph_canvas(c(0, right), c(0, table_top + 44), title, cap, character(0), character(0), NULL,
                       dims, family)
  list(rs = rs, dims = dims, page = page, family = family, a0 = a0, a1 = a1, b0 = b0, b1 = b1, top = top,
       bottom = bottom, lo = lo, hi = hi, X = X, Xb = Xb, Y = Y, Yd = Yd, dr = dr, breaks = breaks,
       dbreaks = dbreaks, sentence_y = sentence_y, table_top = table_top, right = right, xlab = xlab)
}

resp_draw <- function(lay) {
  rs <- lay$rs
  dims <- lay$dims
  px <- lay$page$px
  py <- lay$page$py
  texts <- function(label, x, y, pt, color, hjust = 0, face = "plain") {
    if (!length(label)) return(empty_texts())
    data.frame(x = px(x), y = py(y), label = as.character(label), size = pt / .pt,
               colour = color, hjust = hjust, fontface = face, stringsAsFactors = FALSE)
  }
  seg <- function(x, xend, y, yend) data.frame(x = px(x), xend = px(xend), y = py(y), yend = py(yend))
  X <- lay$X
  Y <- lay$Y
  # The share improving by at least each amount: a falling step curve.
  steps <- function(v, id, colour) {
    s <- sort(v)
    n <- length(s)
    i <- seq_len(n)
    xs <- c(lay$lo, rep(s, each = 2), lay$hi)
    ys <- c(1, as.vector(rbind((n - i + 1) / n, (n - i) / n)), 0)
    data.frame(x = px(X(xs)), y = py(Y(ys)), id = id, colour = colour)
  }
  curves <- rbind(steps(rs$y1, "ca", resp_ink$active), steps(rs$y0, "cc", resp_ink$control))
  cv <- rs$curve
  band <- data.frame(x = px(lay$Xb(c(cv$threshold, rev(cv$threshold)))),
                     y = py(lay$Yd(c(cv$lower, rev(cv$upper)))))
  diff_line <- data.frame(x = px(lay$Xb(cv$threshold)), y = py(lay$Yd(cv$difference)))
  at <- rs$at
  t <- rs$threshold
  marks <- rbind(
    data.frame(seg(X(t), X(t), lay$top, lay$bottom), id = "ta"),
    data.frame(seg(lay$Xb(t), lay$Xb(t), lay$top, lay$bottom), id = "tb"),
    data.frame(seg(lay$Xb(t), lay$Xb(t), lay$Yd(at$diff[2]), lay$Yd(at$diff[3])), id = "db")
  )
  pts <- data.frame(x = px(c(X(t), X(t), lay$Xb(t))), y = py(c(Y(at$p1[1]), Y(at$p0[1]), lay$Yd(at$diff[1]))),
                    id = c("pa", "pc", "pd"), fill = c(resp_ink$active, resp_ink$control, resp_ink$diff),
                    stringsAsFactors = FALSE)
  frames <- rbind(
    seg(lay$a0, lay$a1, lay$bottom, lay$bottom), seg(lay$a0, lay$a0, lay$top, lay$bottom),
    seg(X(lay$breaks), X(lay$breaks), lay$bottom, lay$bottom + 4),
    seg(lay$a0 - 4, lay$a0, Y(c(0, 0.5, 1)), Y(c(0, 0.5, 1))),
    seg(lay$b0, lay$b1, lay$bottom, lay$bottom), seg(lay$b0, lay$b0, lay$top, lay$bottom),
    seg(lay$Xb(lay$breaks), lay$Xb(lay$breaks), lay$bottom, lay$bottom + 4),
    seg(lay$b0 - 4, lay$b0, lay$Yd(lay$dbreaks), lay$Yd(lay$dbreaks))
  )
  labels <- rbind(
    texts("Share who improved at least this much", lay$a0, lay$top - 12, dims$text_pt, graph_ink$title, face = "bold"),
    texts("Difference in responders (percentage points), by threshold", lay$b0, lay$top - 12, dims$text_pt, graph_ink$title, face = "bold"),
    texts(format(lay$breaks, trim = TRUE), X(lay$breaks), rep(lay$bottom + 13, length(lay$breaks)), dims$small_pt,
          graph_ink$muted, hjust = 0.5),
    texts(format(lay$breaks, trim = TRUE), lay$Xb(lay$breaks), rep(lay$bottom + 13, length(lay$breaks)), dims$small_pt,
          graph_ink$muted, hjust = 0.5),
    texts(c("0%", "50%", "100%"), lay$a0 - 7, Y(c(0, 0.5, 1)), dims$small_pt, graph_ink$muted, hjust = 1),
    texts(paste0(formatC(100 * lay$dbreaks, format = "f", digits = 0), ""), lay$b0 - 7, lay$Yd(lay$dbreaks),
          dims$small_pt, graph_ink$muted, hjust = 1),
    texts(lay$xlab, (lay$a0 + lay$a1) / 2, lay$bottom + 28, dims$text_pt, graph_ink$text, hjust = 0.5),
    texts("Threshold for a responder", (lay$b0 + lay$b1) / 2, lay$bottom + 28, dims$text_pt, graph_ink$text, hjust = 0.5),
    texts(rs$arms[2:1], lay$a1, lay$top + c(4, 18), dims$small_pt, c(resp_ink$active, resp_ink$control),
          hjust = 1, face = "bold"),
    texts("prespecified", lay$Xb(t) + 4, lay$bottom - 8, dims$small_pt - 1, graph_ink$muted)
  )
  live <- rbind(
    data.frame(texts(paste("\u2265", format(t)), X(t) + 4, lay$top + 4, dims$small_pt, graph_ink$title, face = "bold"),
               id = "tl"),
    data.frame(texts(resp_sentence(rs), lay$a0, lay$sentence_y, dims$text_pt, graph_ink$text), id = "sn")
  )
  cells <- resp_cells(rs)
  col_w <- (lay$right - lay$a0) / length(cells)
  for (i in seq_along(cells)) {
    cx <- lay$a0 + (i - 1) * col_w
    labels <- rbind(labels, texts(toupper(cells[[i]]$k), cx, lay$table_top, dims$small_pt - 1.5, graph_ink$muted, face = "bold"))
    live <- rbind(live,
      data.frame(texts(cells[[i]]$v, cx, lay$table_top + 15, dims$value_pt, graph_ink$title, face = "bold"), id = paste0("rv", i)),
      data.frame(texts(cells[[i]]$u, cx, lay$table_top + 30, dims$small_pt - 1, graph_ink$muted), id = paste0("ru", i)))
  }
  ggplot() +
    geom_segment(data = seg(lay$a0, lay$a1, Y(0.5), Y(0.5)), aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
                 colour = graph_ink$faint, linewidth = 0.6 / .pt) +
    geom_polygon(data = band, aes(x = .data$x, y = .data$y), fill = resp_ink$diff, alpha = 0.16) +
    geom_segment(data = seg(lay$b0, lay$b1, lay$Yd(0), lay$Yd(0)), aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
                 colour = graph_ink$muted, linewidth = 0.8 / .pt) +
    geom_path(data = diff_line, aes(x = .data$x, y = .data$y), colour = resp_ink$diff, linewidth = 1.2 / .pt) +
    geom_path(data = curves, aes(x = .data$x, y = .data$y, group = .data$id, colour = .data$colour),
              linewidth = 1.3 / .pt) +
    geom_segment(data = frames, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
                 colour = graph_ink$text, linewidth = 0.8 / .pt) +
    geom_segment(data = seg(lay$Xb(t), lay$Xb(t), lay$top, lay$bottom), aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
                 colour = graph_ink$plain_border, linewidth = 0.8 / .pt, linetype = "22") +
    ggiraph::geom_segment_interactive(
      data = marks, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend, data_id = .data$id),
      colour = c(graph_ink$title, graph_ink$title, resp_ink$diff), linewidth = c(1, 1, 2) / .pt) +
    ggiraph::geom_point_interactive(
      data = pts, aes(x = .data$x, y = .data$y, fill = .data$fill, data_id = .data$id),
      shape = 21, size = 2.6, colour = graph_ink$page, stroke = 0.8) +
    draw_text(labels, lay$family) +
    ggiraph::geom_text_interactive(
      data = live, aes(x = .data$x, y = .data$y, label = .data$label, data_id = .data$id),
      hjust = live$hjust, size = live$size, colour = live$colour, fontface = live$fontface,
      family = lay$family) +
    graph_frame(lay$page, lay$family)
}

resp_render <- function(lay) {
  rs <- lay$rs
  list(y1 = rs$y1, y0 = rs$y0, arms = rs$arms, threshold = rs$threshold, z = rs$z, level = rs$level,
       md = rs$md, lo = lay$lo, hi = lay$hi, dr = lay$dr, a0 = lay$a0, a1 = lay$a1, b0 = lay$b0,
       b1 = lay$b1, top = lay$top, bottom = lay$bottom, dx = lay$page$px(0),
       dy = lay$page$height - lay$page$py(0))
}
