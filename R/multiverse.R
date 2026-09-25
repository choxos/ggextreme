#' Draw an interactive multiverse of analyses
#'
#' Draws a specification curve: every reasonable analysis of the same
#' question, one per row of `data`, sorted by its estimate, with its
#' confidence interval, above a grid that marks the choices each analysis
#' made. The primary analysis stays marked, the reference value is drawn,
#' and beside each choice sits the median estimate of the analyses that
#' made it, so the choices that move the result stand out.
#'
#' In the widget, hovering over an analysis shows its estimate and every
#' choice behind it. Dragging across the curve selects a run of analyses and
#' says which choices they share. Clicking a choice in the grid keeps only
#' the analyses that made it, and several choices can be combined; the
#' primary analysis is never hidden.
#'
#' The share of analyses with intervals that exclude the reference value
#' is a description of this set of analyses, not a probability that the
#' effect is real, and the analyses are not independent.
#'
#' @param data A data frame with one row per analysis.
#' @param estimate,lower,upper Bare columns of `data` with each estimate and
#'   its confidence interval.
#' @param decisions Names of the columns of `data` that hold the choices,
#'   such as the outcome definition, the adjustment set and the model.
#' @param primary Optional logical expression of the columns of `data`, or a
#'   row number, marking the primary analysis.
#' @param ratio Whether the estimates are ratios, drawn on a log scale
#'   around 1. Defaults to `TRUE` when every lower limit is positive.
#' @param ylab Label of the estimate axis, such as `"Odds ratio"`.
#' @param title,caption Title above the plot and note below it.
#' @param family Font family. The package ships Lato and registers it on load.
#'
#' @return An object of class `ggmultiverse`, which prints as an interactive
#'   widget. Use [graph_widget()], [graph_plot()] or [graph_save()] for the
#'   widget, a static ggplot or a file. The field `influence` holds the
#'   median estimate for every choice.
#' @export
#'
#' @examples
#' specs <- expand.grid(outcome = c("Primary", "Broad"),
#'                      adjustment = c("Minimal", "Standard", "Extended"),
#'                      model = c("Logistic", "Log-binomial"),
#'                      stringsAsFactors = FALSE)
#' set.seed(1)
#' log_or <- -0.3 + 0.12 * (specs$outcome == "Broad") +
#'   0.1 * match(specs$adjustment, c("Minimal", "Standard", "Extended")) +
#'   rnorm(nrow(specs), 0, 0.03)
#' specs$or <- exp(log_or)
#' specs$lo <- exp(log_or - 1.96 * 0.09)
#' specs$hi <- exp(log_or + 1.96 * 0.09)
#' ggmultiverse(specs, or, lo, hi, decisions = c("outcome", "adjustment", "model"),
#'              primary = outcome == "Primary" & adjustment == "Standard" & model == "Logistic",
#'              ylab = "Odds ratio")
ggmultiverse <- function(data, estimate, lower, upper, decisions, primary = NULL,
                         ratio = NULL, ylab = "Estimate", title = NULL, caption = NULL,
                         family = "Lato") {
  if (!is.data.frame(data)) rlang::abort("`data` must be a data frame.")
  get <- function(q, what) {
    v <- rlang::eval_tidy(q, data)
    if (!is.numeric(v) || length(v) != nrow(data)) rlang::abort(paste0("`", what, "` must be a numeric column of `data`."))
    v
  }
  est <- get(rlang::enquo(estimate), "estimate")
  lo <- get(rlang::enquo(lower), "lower")
  hi <- get(rlang::enquo(upper), "upper")
  if (!is.character(decisions) || !length(decisions) || !all(decisions %in% names(data))) {
    rlang::abort("`decisions` must name columns of `data`.")
  }
  if (nrow(data) > 500) {
    rlang::abort(paste0("There are ", nrow(data), " analyses; a curve of more than 500 cannot be read. ",
                        "Fix some choices first, or draw a sample of analyses."))
  }
  keep <- is.finite(est) & is.finite(lo) & is.finite(hi)
  if (!all(keep)) rlang::warn(paste0(sum(!keep), " analyses without an estimate or interval were left out."))
  q_primary <- rlang::enquo(primary)
  prim <- rep(FALSE, nrow(data))
  if (!rlang::quo_is_null(q_primary)) {
    v <- rlang::eval_tidy(q_primary, data)
    if (is.numeric(v) && length(v) == 1) prim[v] <- TRUE else if (is.logical(v) && length(v) == nrow(data)) prim <- v %in% TRUE
    else rlang::abort("`primary` must be a logical expression of the columns or a row number.")
    if (sum(prim) != 1) rlang::abort("`primary` must pick exactly one analysis.")
  }
  if (is.null(ratio)) ratio <- all(lo[keep] > 0)
  if (ratio && any(lo[keep] <= 0)) rlang::abort("A ratio needs positive estimates and limits.")
  data <- data[keep, , drop = FALSE]
  mv <- list(est = est[keep], lo = lo[keep], hi = hi[keep], primary = prim[keep], ratio = ratio,
             null = if (ratio) 1 else 0, decisions = decisions, ylab = ylab,
             choices = lapply(decisions, function(d) {
               v <- data[[d]]
               lv <- if (is.factor(v)) levels(droplevels(v)) else unique(as.character(v))
               list(name = column_labels(data, d), levels = lv, of = match(as.character(v), lv))
             }))
  lay <- mv_layout(mv, title, caption, family)
  structure(
    list(plot = mv_draw(lay), width = lay$page$width / 72, height = lay$page$height / 72,
         title = title, on_render = "ggextremeMultiverse(el, data);", render_data = mv_render(lay),
         hover_inv = "opacity:0.35;", influence = lay$influence),
    class = c("ggmultiverse", "ggx_graph")
  )
}

mv_ink <- list(below = "#22928F", above = "#C0603F", neither = "#9A9A9A", primary = "#1F1F1F")

mv_fmt <- function(v, ratio) formatC(v, format = "f", digits = if (ratio && v >= 10) 1 else 2)

# One sentence on the whole set of analyses.
mv_summary <- function(est, lo, hi, primary, null, ratio, ylab) {
  n <- length(est)
  below <- sum(hi < null)
  above <- sum(lo > null)
  f <- function(v) mv_fmt(v, ratio)
  out <- paste0(n, " analyses: median ", tolower(ylab), " ", f(stats::median(est)), ", from ", f(min(est)),
                " to ", f(max(est)), ". ", below + above, " (", round(100 * (below + above) / n),
                "%) have intervals that exclude ", format(null))
  out <- paste0(out, if (below && above) paste0(", ", below, " below and ", above, " above it") else
    if (below) ", all below it" else if (above) ", all above it" else "", ".")
  if (any(primary)) {
    i <- which(primary)
    out <- paste0(out, " The primary analysis: ", f(est[i]), " (", f(lo[i]), " to ", f(hi[i]), ").")
  }
  out
}

mv_layout <- function(mv, title, caption, family) {
  dims <- utils::modifyList(graph_dims, list(curve_h = 200, row_h = 13, gap_block = 7,
                                             text_pt = 10, small_pt = 9))
  n <- length(mv$est)
  ord <- order(mv$est, seq_len(n))
  rank <- integer(n)
  rank[ord] <- seq_len(n)
  labels_w <- max(text_width_card(unlist(lapply(mv$choices, `[[`, "levels")), dims$small_pt, family, 1)) + 14
  names_w <- max(text_width_card(vapply(mv$choices, `[[`, "", "name"), dims$small_pt, family, 1, bold = TRUE))
  x0 <- max(names_w + 12 + labels_w, 150)
  plot_w <- max(360, min(760, n * 9))
  x1 <- x0 + plot_w
  step <- plot_w / n
  X <- function(r) x0 + (r - 0.5) * step
  tr <- if (mv$ratio) log else identity
  all_v <- tr(c(mv$lo, mv$hi, mv$null))
  pad <- 0.05 * diff(range(all_v))
  ylo <- min(all_v) - pad
  yhi <- max(all_v) + pad
  top <- 20
  bottom <- top + dims$curve_h
  Y <- function(v) bottom - (tr(v) - ylo) / (yhi - ylo) * dims$curve_h
  ticks <- if (mv$ratio) {
    cand <- c(0.1, 0.2, 0.25, 0.33, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1, 1.25, 1.5, 2, 3, 4, 5, 10)
    cand[log(cand) >= ylo & log(cand) <= yhi]
  } else {
    b <- scales::breaks_extended(5)(c(ylo, yhi))
    b[b >= ylo & b <= yhi]
  }
  # The grid of choices, one row per option, and each option's median.
  rows <- list()
  y <- bottom + 34
  for (d in seq_along(mv$choices)) {
    ch <- mv$choices[[d]]
    for (k in seq_along(ch$levels)) {
      use <- ch$of == k
      rows[[length(rows) + 1]] <- list(d = d, k = k, y = y, name = if (k == 1) ch$name else "",
                                       level = ch$levels[k], use = use,
                                       median = stats::median(mv$est[use]), n = sum(use))
      y <- y + dims$row_h
    }
    y <- y + dims$gap_block
  }
  grid_bottom <- y
  influence <- do.call(rbind, lapply(rows, function(r) {
    data.frame(decision = mv$decisions[r$d], choice = r$level, analyses = r$n, median = r$median,
               stringsAsFactors = FALSE)
  }))
  words <- mv_summary(mv$est, mv$lo, mv$hi, mv$primary, mv$null, mv$ratio, mv$ylab)
  right <- x1 + 120
  cap <- c(caption, wrap_words(paste0(words, " The share of intervals that exclude ", format(mv$null),
                                      " describes these analyses; it is not a probability that the effect is real."),
                               right - 20, dims$caption_pt, family))
  page <- graph_canvas(c(0, right), c(0, grid_bottom), title, cap, character(0), character(0), NULL,
                       dims, family)
  list(mv = mv, dims = dims, page = page, family = family, n = n, rank = rank, x0 = x0, x1 = x1,
       step = step, X = X, Y = Y, top = top, bottom = bottom, ticks = ticks, rows = rows,
       grid_bottom = grid_bottom, influence = influence, words = words, right = right)
}

mv_draw <- function(lay) {
  mv <- lay$mv
  dims <- lay$dims
  px <- lay$page$px
  py <- lay$page$py
  X <- lay$X
  Y <- lay$Y
  texts <- function(label, x, y, pt, color, hjust = 0, face = "plain") {
    if (!length(label)) return(empty_texts())
    data.frame(x = px(x), y = py(y), label = as.character(label), size = pt / .pt,
               colour = color, hjust = hjust, fontface = face, stringsAsFactors = FALSE)
  }
  seg <- function(x, xend, y, yend) data.frame(x = px(x), xend = px(xend), y = py(y), yend = py(yend))
  f <- function(v) mv_fmt(v, mv$ratio)
  ids <- paste0("s", seq_len(lay$n))
  side <- ifelse(mv$hi < mv$null, "below", ifelse(mv$lo > mv$null, "above", "neither"))
  colour <- unname(unlist(mv_ink[side]))
  tips <- vapply(seq_len(lay$n), function(i) {
    paste0('<div class="ggx-tip-title">', esc(mv$ylab), " ", f(mv$est[i]), " (", f(mv$lo[i]), " to ",
           f(mv$hi[i]), ")</div>",
           if (mv$primary[i]) '<div class="ggx-tip-sub">The primary analysis</div>',
           tip_rows(stats::setNames(vapply(mv$choices, function(ch) ch$levels[ch$of[i]], ""),
                                    vapply(mv$choices, `[[`, "", "name"))),
           '<div class="ggx-tip-hint">Drag across the curve to select a run of analyses.</div>')
  }, character(1))
  xs <- X(lay$rank)
  cis <- data.frame(seg(xs, xs, Y(mv$lo), Y(mv$hi)), id = ids, colour = colour, tip = tips,
                    stringsAsFactors = FALSE)
  pts <- data.frame(x = px(xs), y = py(Y(mv$est)), id = ids, fill = colour, tip = tips,
                    size = ifelse(mv$primary, 3.2, 1.9), stringsAsFactors = FALSE)
  grid_pts <- do.call(rbind, lapply(lay$rows, function(r) {
    data.frame(x = px(xs), y = py(r$y), id = ids, fill = ifelse(r$use, graph_ink$title, graph_ink$ghost),
               tip = tips, stringsAsFactors = FALSE)
  }))
  hits <- data.frame(xmin = px(xs - lay$step / 2), xmax = px(xs + lay$step / 2), ymin = py(lay$grid_bottom),
                     ymax = py(lay$top), id = ids, tip = tips, stringsAsFactors = FALSE)
  frames <- rbind(seg(lay$x0, lay$x0, lay$top, lay$bottom), seg(lay$x0 - 4, lay$x0, Y(lay$ticks), Y(lay$ticks)))
  labels <- rbind(
    texts(format(lay$ticks), lay$x0 - 7, Y(lay$ticks), dims$small_pt, graph_ink$muted, hjust = 1),
    texts(paste0(mv$ylab, if (mv$ratio) " (log scale)" else ""), lay$x0, lay$top - 12, dims$text_pt,
          graph_ink$title, face = "bold"),
    texts(paste("Analyses, sorted by", tolower(mv$ylab)), lay$x0, lay$bottom + 14, dims$small_pt, graph_ink$muted),
    texts("Median", lay$x1 + 12, lay$bottom + 22, dims$small_pt, graph_ink$muted, face = "bold"),
    # A key to the colors, in their own colors.
    texts(c(paste("Interval below", format(mv$null)), paste("Includes", format(mv$null)),
            paste("Interval above", format(mv$null))),
          lay$x1 - c(250, 145, 75), rep(lay$top - 12, 3), dims$small_pt,
          c(mv_ink$below, mv_ink$neither, mv_ink$above), face = "bold")
  )
  live <- empty_texts()
  live$id <- character(0)
  for (j in seq_along(lay$rows)) {
    r <- lay$rows[[j]]
    labels <- rbind(labels, texts(r$name, 4, r$y, dims$small_pt, graph_ink$title, face = "bold"))
    live <- rbind(live,
      data.frame(texts(r$level, lay$x0 - 8, r$y, dims$small_pt, graph_ink$text, hjust = 1), id = paste0("o", j)),
      data.frame(texts(f(r$median), lay$x1 + 12, r$y, dims$small_pt, graph_ink$text), id = paste0("m", j)))
  }
  if (any(mv$primary)) {
    i <- which(mv$primary)
    labels <- rbind(labels, texts("Primary", X(lay$rank[i]), Y(mv$hi[i]) - 8, dims$small_pt, graph_ink$title,
                                  hjust = 0.5, face = "bold"))
  }
  ggplot() +
    geom_segment(data = seg(lay$x0, lay$x1, Y(lay$ticks), Y(lay$ticks)),
                 aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
                 colour = graph_ink$faint, linewidth = 0.6 / .pt) +
    geom_segment(data = seg(lay$x0, lay$x1, Y(mv$null), Y(mv$null)),
                 aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
                 colour = graph_ink$muted, linewidth = 0.8 / .pt) +
    geom_segment(data = frames, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
                 colour = graph_ink$text, linewidth = 0.8 / .pt) +
    ggiraph::geom_segment_interactive(
      data = cis, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend,
                      colour = .data$colour, data_id = .data$id),
      linewidth = 0.9 / .pt, alpha = 0.7) +
    ggiraph::geom_point_interactive(
      data = pts, aes(x = .data$x, y = .data$y, fill = .data$fill, data_id = .data$id),
      shape = 21, size = pts$size, colour = ifelse(mv$primary, graph_ink$title, graph_ink$page),
      stroke = ifelse(mv$primary, 1.2, 0.3)) +
    ggiraph::geom_point_interactive(
      data = grid_pts, aes(x = .data$x, y = .data$y, fill = .data$fill, data_id = .data$id),
      shape = 21, size = 1.6, colour = graph_ink$page, stroke = 0) +
    ggiraph::geom_rect_interactive(
      data = hits, aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, ymax = .data$ymax,
                       data_id = .data$id, tooltip = .data$tip),
      fill = "#FFFFFF02", colour = NA) +
    draw_text(labels, lay$family) +
    ggiraph::geom_text_interactive(
      data = live, aes(x = .data$x, y = .data$y, label = .data$label, data_id = .data$id),
      hjust = live$hjust, size = live$size, colour = live$colour, fontface = live$fontface,
      family = lay$family) +
    graph_frame(lay$page, lay$family)
}

mv_render <- function(lay) {
  mv <- lay$mv
  list(est = mv$est, lo = mv$lo, hi = mv$hi, primary = mv$primary, null = mv$null, ratio = mv$ratio,
       ylab = mv$ylab, rank = lay$rank, x0 = lay$x0, step = lay$step, top = lay$top,
       bottom = lay$bottom, dx = lay$page$px(0), dy = lay$page$height - lay$page$py(0),
       choices = lapply(mv$choices, function(ch) list(name = ch$name, levels = as.list(ch$levels), of = ch$of)),
       rows = lapply(lay$rows, function(r) list(d = r$d, k = r$k)))
}
