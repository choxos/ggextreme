#' Draw network estimates against a range of little difference
#'
#' Draws every network estimate of a network meta-analysis with its
#' confidence interval and prediction interval against a range of little
#' difference, whose lower and upper limits the reader can move on their
#' own. Beside each estimate, in words, is what its confidence interval and
#' its prediction interval are compatible with: an important benefit, little
#' difference or an important harm of the first treatment against the
#' second, or more than one of these; and the imprecision and heterogeneity
#' judgments that CINeMA's rules give at these limits.
#'
#' In the widget, two sliders move the lower and the upper limit; the
#' prespecified limits stay marked with dashed lines and a button returns to
#' them. Every reading, judgment and the summary under the plot follow. Under
#' the estimates, a threshold sensitivity strip for each limit shows, row by
#' row, where each reading changes as that limit moves with the other held
#' where it is: a change of color is a change of reading. Clicking or tapping
#' a strip moves that limit there; the sliders do the same from the
#' keyboard. A switch hides the prediction intervals.
#'
#' The limits apply to every comparison as written, the first treatment
#' against the second, in the order set by [cinema_judge()]. A prediction
#' interval shows where the effect in a comparable new setting is expected
#' to lie, not the effect for an individual patient, and with few studies
#' the heterogeneity behind it is poorly estimated. The plot describes what
#' each interval is compatible with; it never calls a comparison equivalent.
#'
#' @param x Judgments from [cinema_judge()] that include a `threshold`, or a
#'   network meta-analysis from [netmeta::netmeta()], judged with the
#'   arguments in `...`.
#' @param ... When `x` is a netmeta fit, arguments for [cinema_judge()]:
#'   `threshold`, the prespecified limits of little difference, which is
#'   required, and `small_values`, `order` and `pooled`.
#' @param reference Optionally, one treatment: only its comparisons with the
#'   others are drawn, such as every treatment against placebo.
#' @param prediction Draw the prediction intervals, when the model has them.
#' @param title,caption Title above the plot and note below it. A caption
#'   you give is added above the default notes.
#' @param family Font family. The package ships Lato and registers it on load.
#'
#' @inheritSection cinema_judge Sources
#'
#' @return An object of class `cinema_clinical`, which prints as an
#'   interactive widget. Use [graph_widget()], [graph_plot()] or
#'   [graph_save()] for the widget, a static ggplot or a file. The field
#'   `readings` gives, for each comparison drawn, what its intervals are
#'   compatible with and the imprecision and heterogeneity judgments at the
#'   prespecified limits.
#' @export
#'
#' @examples
#' \donttest{
#' if (requireNamespace("netmeta", quietly = TRUE) &&
#'     requireNamespace("meta", quietly = TRUE)) {
#'   pw <- meta::pairwise(treat = treatment, event = pasi75_r,
#'                        n = pasi75_n, studlab = study,
#'                        data = psoriasis_nma, sm = "OR")
#'   nma <- netmeta::netmeta(pw, common = FALSE)
#'   cinema_clinical(nma, threshold = c(0.8, 1.25),
#'                   small_values = "undesirable")
#' }
#' }
cinema_clinical <- function(x, ..., reference = NULL, prediction = TRUE, title = NULL,
                            caption = NULL, family = "Lato") {
  cn <- cinema_input(x, ..., fn = "cinema_clinical")
  if (is.null(cn$threshold)) {
    rlang::abort("cinema_clinical() needs the prespecified limits of little difference: give `threshold`, such as 1.25 or c(0.8, 1.25).")
  }
  pr <- cn$pairs
  rows <- seq_len(nrow(pr))
  if (!is.null(reference)) {
    reference <- as.character(reference)
    if (length(reference) != 1 || !reference %in% cn$trts) {
      rlang::abort("`reference` must name one treatment of the network.")
    }
    rows <- which(pr$a == reference | pr$b == reference)
  }
  prediction <- isTRUE(prediction) && all(is.finite(pr$plo[rows]) & is.finite(pr$phi[rows]))
  lay <- clin_layout(cn, rows, prediction, title, caption, family)
  structure(
    list(plot = clin_draw(lay), width = lay$page$width / 72, height = lay$page$height / 72,
         title = title, on_render = "ggextremeCinemaClinical(el, data);",
         render_data = clin_render(lay), hover_inv = "opacity:1;",
         readings = clin_readings(lay), judgments = cn),
    class = c("cinema_clinical", "ggx_graph")
  )
}

# The color of a reading: teal when it holds only a benefit of the first
# treatment, pink only a harm, gray only little difference, paler for a
# benefit or a harm together with little difference, and blue for all three.
clin_ink <- function(m, small_values) {
  benefit <- if (small_values == "undesirable") 4L else 1L
  harm <- 5L - benefit
  col <- rep(cinema_ink$wide, length(m))
  alpha <- rep(0.9, length(m))
  col[m == benefit] <- cinema_ink$first
  alpha[m == benefit] <- 1
  col[m == benefit + 2L] <- cinema_ink$first
  alpha[m == benefit + 2L] <- 0.5
  col[m == 2L] <- cinema_ink$little
  alpha[m == 2L] <- 0.6
  col[m == harm + 2L] <- cinema_ink$second
  alpha[m == harm + 2L] <- 0.5
  col[m == harm] <- cinema_ink$second
  alpha[m == harm] <- 1
  list(col = col, alpha = alpha)
}

# The six readings in the order they are listed in the key.
clin_key_masks <- function(small_values) {
  b <- if (small_values == "undesirable") 4L else 1L
  h <- 5L - b
  c(b, b + 2L, 2L, h + 2L, h, 7L)
}

# Run-length bands of a reading as one limit moves over `grid`, the other
# held; on the scale of the analysis.
clin_strip <- function(l, u, grid, held, which) {
  m <- if (which == "upper") cinema_zones(l, u, held, grid) else cinema_zones(l, u, grid, held)
  r <- rle(m)
  end <- cumsum(r$lengths)
  start <- c(1, utils::head(end, -1) + 1)
  data.frame(from = grid[start], to = grid[pmin(end + 1, length(grid))], mask = r$values)
}

clin_layout <- function(cn, rows, prediction, title, caption, family) {
  dims <- utils::modifyList(graph_dims, list(label_pt = 9, small_pt = 8.5, text_pt = 9, row_h = 36,
                                             strip_row = 17, forest_w = 230))
  pr <- cn$pairs[rows, , drop = FALSE]
  ratio <- cn$ratio
  lim <- log_or(cn$threshold, ratio)
  sv <- cn$small_values
  n <- nrow(pr)
  width <- function(s, pt, bold = FALSE) max(text_width_card(s, pt, family, 1, bold = bold))
  labels <- paste(pr$a, "vs", pr$b)
  estimates <- cinema_estimate(pr$te, pr$lo, pr$hi, ratio)

  # How far the sliders reach, on the scale of the analysis.
  if (ratio) {
    reach <- c(min(log(0.5), 2 * lim[1]), max(log(2), 2 * lim[2]))
  } else {
    span <- 2.5 * max(abs(lim))
    if (span == 0) span <- max(stats::median(abs(pr$te)), 1)
    reach <- c(-span, span)
  }
  lab_w <- max(width(c(labels, estimates), dims$label_pt), 120)
  words <- cinema_reading(0:7, sv)
  read_w <- width(paste0("PI: ", words[c(2, 3, 5, 7, 8)], " (changed)"), dims$text_pt) + 14
  judg_w <- width(c("Heterogeneity: major concerns", "CINeMA judgment at these limits"), dims$text_pt) + 13
  f0 <- lab_w + 14
  f1 <- f0 + dims$forest_w
  r0 <- f1 + 18
  g0 <- r0 + read_w + 8
  g1 <- g0 + judg_w
  legend_h <- 20
  head_y <- legend_h + 14
  rows_top <- head_y + 12
  row_y <- rows_top + (seq_len(n) - 1) * dims$row_h
  rows_bottom <- rows_top + n * dims$row_h

  # The forest's axis holds every interval and the sliders' reach.
  ends <- c(pr$lo, pr$hi, reach, if (prediction) c(pr$plo, pr$phi))
  a <- min(ends, na.rm = TRUE)
  b <- max(ends, na.rm = TRUE)
  if (ratio) {
    cand <- log(c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000))
    a <- max(c(cand[cand <= a + 1e-9], log(0.001)))
    b <- min(c(cand[cand >= b - 1e-9], log(1000)))
    ticks <- cand[cand >= a - 1e-9 & cand <= b + 1e-9]
    # Thin a long axis to the 1, 10, 100 pattern, keeping no effect.
    if (length(ticks) > 10) ticks <- ticks[abs(ticks / log(10) - round(ticks / log(10))) < 1e-9 | abs(ticks) < 1e-9]
  } else {
    ticks <- scales::breaks_extended(6)(c(a, b))
    a <- min(a, ticks)
    b <- max(b, ticks)
  }
  X <- function(v) f0 + (pmin(pmax(v, a), b) - a) / (b - a) * (f1 - f0)

  # The two threshold sensitivity strips, under the axis.
  axis_y <- rows_bottom + 4
  strip_top <- axis_y + 52
  block_h <- 20 + n * dims$strip_row + 34
  blocks <- lapply(1:2, function(k) {
    top <- strip_top + (k - 1) * block_h
    which <- c("upper", "lower")[k]
    range <- if (k == 1) c(0, reach[2]) else c(reach[1], 0)
    list(which = which, top = top, rows = top + 18, range = range, x0 = f0, x1 = g1,
         pre = if (k == 1) lim[2] else lim[1], held = if (k == 1) lim[1] else lim[2])
  })
  key_y <- strip_top + 2 * block_h + 4
  bottom <- key_y + 18

  pct <- paste0(format(100 * cn$level), "%")
  cap <- c(caption, clin_summary(cn, pr, lim, prediction, pct),
           paste0("Network ", tolower(cn$measure), "s with their ", pct, " CIs",
                  if (prediction) paste0(" and ", pct, " prediction intervals") else "", ", ", cn$pooled,
                  " effects. Benefit and harm are of the first treatment against the second",
                  if (sv == "undesirable") ", larger values being better." else ", smaller values being better.",
                  " The range of little difference is a clinical choice to make before seeing the results; ",
                  "the plot says what each interval is compatible with and never calls a comparison equivalent."),
           if (prediction) paste0("A prediction interval is where the effect in a comparable new setting is expected to lie, ",
                                  "not the effect for one patient. The between-study variance is estimated as ",
                                  formatC(cn$tau2, format = "f", digits = 3), " from ", cn$n_studies,
                                  " studies; with few studies it is poorly estimated."),
           "Imprecision and heterogeneity follow the rules of CINeMA (Nikolakopoulou et al. 2020).")
  cap <- cinema_wrap(cap, g1 - 10, graph_dims$caption_pt, family)
  page <- graph_canvas(c(0, g1), c(0, bottom), title, cap, character(0), character(0), NULL, dims, family)
  list(cn = cn, pr = pr, rows = rows, n = n, dims = dims, page = page, family = family,
       prediction = prediction, lim = lim, reach = reach, labels = labels, estimates = estimates,
       lab_w = lab_w, f0 = f0, f1 = f1, r0 = r0, g0 = g0, g1 = g1, legend_h = legend_h, head_y = head_y,
       rows_top = rows_top, row_y = row_y, rows_bottom = rows_bottom, a = a, b = b, ticks = ticks, X = X,
       axis_y = axis_y, blocks = blocks, block_h = block_h, key_y = key_y, bottom = bottom, pct = pct)
}

# One sentence on the readings at the given limits, as the widget writes it.
clin_summary <- function(cn, pr, lim, prediction, pct) {
  m <- cinema_zones(pr$lo, pr$hi, lim[1], lim[2])
  single <- sum(cinema_bits(m) == 1)
  imp <- cinema_step(pr$te, pr$lo, pr$hi, lim[1], lim[2])
  out <- paste0("At the prespecified limits, ", paste(cinema_num(lim, cn$ratio), collapse = " and "), ", the ", pct, " CI of ",
                single, " of ", nrow(pr), " comparisons is compatible with one reading only",
                if (single < nrow(pr)) paste0(" and ", nrow(pr) - single, " with more than one") else "", ". ")
  counts <- function(v) {
    tab <- table(factor(v, levels = 0:2))
    paste(paste(tab[tab > 0], tolower(cinema_concern_words[tab > 0])), collapse = ", ")
  }
  out <- paste0(out, "Imprecision: ", counts(imp), ".")
  if (prediction) {
    mp <- cinema_zones(pr$plo, pr$phi, lim[1], lim[2])
    het <- pmax(0L, cinema_step(pr$te, pr$plo, pr$phi, lim[1], lim[2]) - imp)
    out <- paste0(out, " The prediction interval changes the reading for ", sum(mp != m), ". Heterogeneity: ",
                  counts(het), ".")
  }
  out
}

clin_readings <- function(lay) {
  pr <- lay$pr
  lim <- lay$lim
  m <- cinema_zones(pr$lo, pr$hi, lim[1], lim[2])
  imp <- cinema_step(pr$te, pr$lo, pr$hi, lim[1], lim[2])
  out <- data.frame(treat1 = pr$a, treat2 = pr$b, ci = cinema_reading(m, lay$cn$small_values, long = TRUE),
                    imprecision = cinema_concern_words[imp + 1], stringsAsFactors = FALSE)
  if (lay$prediction) {
    mp <- cinema_zones(pr$plo, pr$phi, lim[1], lim[2])
    out$pi <- cinema_reading(mp, lay$cn$small_values, long = TRUE)
    out$heterogeneity <- cinema_concern_words[pmax(0L, cinema_step(pr$te, pr$plo, pr$phi, lim[1], lim[2]) - imp) + 1]
  }
  out
}

clin_draw <- function(lay) {
  cn <- lay$cn
  pr <- lay$pr
  dims <- lay$dims
  px <- lay$page$px
  py <- lay$page$py
  X <- lay$X
  ratio <- cn$ratio
  sv <- cn$small_values
  lim <- lay$lim
  n <- lay$n
  texts <- function(label, x, y, pt, color, hjust = 0, face = "plain") {
    if (!length(label)) return(empty_texts())
    data.frame(x = px(x), y = py(y), label = as.character(label), size = pt / .pt, colour = color,
               hjust = hjust, fontface = face, stringsAsFactors = FALSE)
  }
  seg <- function(x, xend, y, yend) data.frame(x = px(x), xend = px(xend), y = py(y), yend = py(yend))
  rect <- function(x0, x1, y0, y1) data.frame(xmin = px(x0), xmax = px(x1), ymin = py(y1), ymax = py(y0))
  top <- lay$rows_top - 6
  bottom <- lay$rows_bottom

  # The key along the top: a swatch or a line, then its words.
  key_words <- c("Range of little difference", "Prespecified limits", paste(lay$pct, "CI"),
                 if (lay$prediction) paste(lay$pct, "prediction interval"))
  key_at <- cumsum(c(0, utils::head(text_width_card(key_words, dims$text_pt, lay$family, 1) + 44, -1)))
  key <- rect(key_at[1], key_at[1] + 12, 4, 14)
  key_lines <- rbind(
    data.frame(seg(key_at[2] + 6, key_at[2] + 6, 3, 15), colour = cinema_ink$prespecified, width = 1.3, type = "22"),
    data.frame(seg(key_at[3], key_at[3] + 24, 9, 9), colour = graph_ink$title, width = 3, type = "solid"),
    if (lay$prediction) data.frame(seg(key_at[4], key_at[4] + 24, 9, 9), colour = graph_ink$muted, width = 1.2,
                                   type = "solid")
  )
  labels <- rbind(
    texts(key_words, key_at + c(17, 13, 29, 29)[seq_along(key_words)], 9, dims$text_pt, graph_ink$text),
    texts("Comparison", 0, lay$head_y, dims$small_pt, graph_ink$muted),
    texts("What the interval is compatible with", lay$r0, lay$head_y, dims$small_pt, graph_ink$muted),
    texts("CINeMA judgment at these limits", lay$g0, lay$head_y, dims$small_pt, graph_ink$muted),
    texts(lay$labels, 0, lay$row_y + 10, dims$label_pt, graph_ink$title),
    texts(lay$estimates, 0, lay$row_y + 23, dims$small_pt, graph_ink$muted)
  )

  # The forest.
  grid <- seg(X(lay$ticks), X(lay$ticks), top, bottom)
  null_line <- seg(X(0), X(0), top, bottom)
  region <- data.frame(rect(X(lim[1]), X(lim[2]), top, bottom), id = "rg")
  pre <- seg(X(lim), X(lim), top - 6, bottom)
  pre_labels <- texts(cinema_num(lim, ratio), X(lim) + c(-3, 3), top - 10, dims$small_pt - 1,
                      cinema_ink$prespecified, hjust = c(1, 0))
  tick_labels <- texts(if (ratio) format(signif(exp(lay$ticks), 3), trim = TRUE, drop0trailing = TRUE, scientific = FALSE) else
    format(lay$ticks, trim = TRUE), X(lay$ticks), lay$axis_y + 11, dims$small_pt, graph_ink$muted, hjust = 0.5)
  axis <- rbind(seg(lay$f0, lay$f1, lay$axis_y, lay$axis_y), seg(X(lay$ticks), X(lay$ticks), lay$axis_y, lay$axis_y + 4))
  axis_title <- texts(paste0(cn$measure, ", first treatment against the second", if (ratio) " (log scale)" else ""),
                      (lay$f0 + lay$f1) / 2, lay$axis_y + 25, dims$small_pt, graph_ink$text, hjust = 0.5)
  y <- lay$row_y + 14
  tip <- vapply(seq_len(n), function(i) {
    paste0('<div class="ggx-tip-title">', esc(lay$labels[i]), "</div>",
           '<div class="ggx-tip-sub">', esc(cn$measure), ", ", cn$pooled, " effects</div>",
           tip_rows(c(stats::setNames(lay$estimates[i], paste0("Estimate (", lay$pct, " CI)")),
                      if (lay$prediction) stats::setNames(cinema_interval(pr$plo[i], pr$phi[i], ratio),
                                                          paste(lay$pct, "prediction interval")),
                      "Evidence" = c(mixed = "direct and indirect", direct = "direct only",
                                     indirect = "indirect only")[[pr$type[i]]])),
           '<div class="ggx-tip-hint">What each interval is compatible with is written to the right.</div>')
  }, character(1))
  ci <- data.frame(seg(X(pr$lo), X(pr$hi), y, y), id = paste0("r", seq_len(n)), tooltip = tip)
  pi <- if (lay$prediction) data.frame(seg(X(pr$plo), X(pr$phi), y, y), id = paste0("pi", seq_len(n)))
  pi_ends <- if (lay$prediction) {
    ends <- c(pr$plo, pr$phi)
    keep <- ends > lay$a & ends < lay$b
    data.frame(seg(X(ends), X(ends), rep(y, 2) - 4, rep(y, 2) + 4), id = rep(paste0("pi", seq_len(n)), 2))[keep, ]
  }
  arrows <- do.call(rbind, lapply(seq_len(n), function(i) {
    out <- NULL
    if (pr$lo[i] < lay$a) out <- rbind(out, data.frame(x = px(X(lay$a) + c(0, 6, 6)), y = py(y[i] + c(0, -3.5, 3.5)),
                                                       g = paste0("al", i)))
    if (pr$hi[i] > lay$b) out <- rbind(out, data.frame(x = px(X(lay$b) + c(0, -6, -6)), y = py(y[i] + c(0, -3.5, 3.5)),
                                                       g = paste0("ah", i)))
    out
  }))
  points <- data.frame(rect(X(pr$te) - 3.5, X(pr$te) + 3.5, y - 3.5, y + 3.5), id = paste0("r", seq_len(n)), tooltip = tip)
  hit <- data.frame(rect(0, lay$g1, lay$row_y + 1, lay$row_y + dims$row_h - 1), id = paste0("r", seq_len(n)),
                    tooltip = tip)

  # Readings and judgments at the prespecified limits; the widget rewrites them.
  m <- cinema_zones(pr$lo, pr$hi, lim[1], lim[2])
  imp <- cinema_step(pr$te, pr$lo, pr$hi, lim[1], lim[2])
  ink <- clin_ink(m, sv)
  live <- rbind(
    data.frame(texts(paste("CI:", cinema_reading(m, sv)), lay$r0 + 13, lay$row_y + 10, dims$text_pt, graph_ink$title),
               id = paste0("rc", seq_len(n))),
    data.frame(texts(paste("Imprecision:", tolower(cinema_concern_words[imp + 1])), lay$g0 + 13, lay$row_y + 10,
                     dims$text_pt, graph_ink$title), id = paste0("jc", seq_len(n)))
  )
  swatches <- rbind(
    data.frame(rect(lay$r0, lay$r0 + 9, lay$row_y + 5.5, lay$row_y + 14.5), fill = ink$col, alpha = ink$alpha,
               id = paste0("qc", seq_len(n))),
    data.frame(rect(lay$g0, lay$g0 + 9, lay$row_y + 5.5, lay$row_y + 14.5), fill = cinema_ink$level[imp + 1],
               alpha = 1, id = paste0("mc", seq_len(n)))
  )
  if (lay$prediction) {
    mp <- cinema_zones(pr$plo, pr$phi, lim[1], lim[2])
    het <- pmax(0L, cinema_step(pr$te, pr$plo, pr$phi, lim[1], lim[2]) - imp)
    inkp <- clin_ink(mp, sv)
    live <- rbind(live,
      data.frame(texts(paste0("PI: ", cinema_reading(mp, sv), ifelse(mp != m, " (changed)", "")),
                       lay$r0 + 13, lay$row_y + 23, dims$small_pt, graph_ink$text), id = paste0("rp", seq_len(n))),
      data.frame(texts(paste("Heterogeneity:", tolower(cinema_concern_words[het + 1])), lay$g0 + 13, lay$row_y + 23,
                       dims$small_pt, graph_ink$text), id = paste0("jp", seq_len(n))))
    swatches <- rbind(swatches,
      data.frame(rect(lay$r0, lay$r0 + 9, lay$row_y + 19, lay$row_y + 27), fill = inkp$col, alpha = inkp$alpha,
                 id = paste0("qp", seq_len(n))),
      data.frame(rect(lay$g0, lay$g0 + 9, lay$row_y + 19, lay$row_y + 27), fill = cinema_ink$level[het + 1],
                 alpha = 1, id = paste0("mp", seq_len(n))))
  }

  # The threshold sensitivity strips at the prespecified limits.
  strips <- NULL
  strip_lines <- NULL
  for (k in 1:2) {
    B <- lay$blocks[[k]]
    SX <- function(v) B$x0 + (v - B$range[1]) / (B$range[2] - B$range[1]) * (B$x1 - B$x0)
    grid_v <- seq(B$range[1], B$range[2], length.out = 201)
    for (i in seq_len(n)) {
      yy <- B$rows + (i - 1) * dims$strip_row
      bands <- list(list(l = pr$lo[i], u = pr$hi[i], y0 = yy, y1 = yy + 8))
      if (lay$prediction) bands[[2]] <- list(l = pr$plo[i], u = pr$phi[i], y0 = yy + 10, y1 = yy + 13)
      for (bd in bands) {
        s <- clin_strip(bd$l, bd$u, grid_v, B$held, B$which)
        inks <- clin_ink(s$mask, sv)
        strips <- rbind(strips, data.frame(rect(SX(s$from), SX(s$to), bd$y0, bd$y1), fill = inks$col,
                                           alpha = inks$alpha))
      }
      labels <- rbind(labels, texts(lay$labels[i], 0, yy + 6, dims$small_pt, graph_ink$text))
    }
    yb <- B$rows + n * dims$strip_row
    held_text <- cinema_num(B$held, ratio)
    title_k <- if (k == 1) {
      paste0("Upper limit from ", cinema_num(B$range[1], ratio), " to ", cinema_num(B$range[2], ratio),
             ", with the lower limit held at ", held_text)
    } else {
      paste0("Lower limit from ", cinema_num(B$range[1], ratio), " to ", cinema_num(B$range[2], ratio),
             ", with the upper limit held at ", held_text)
    }
    live <- rbind(live, data.frame(texts(title_k, 0, B$top + 4, dims$text_pt, graph_ink$title, face = "bold"),
                                   id = paste0("st", k)))
    sticks <- if (ratio) {
      v <- log(c(0.2, 0.25, 0.33, 0.4, 0.5, 0.6, 0.67, 0.7, 0.75, 0.8, 0.9, 1, 1.1, 1.25, 1.33, 1.5, 1.75, 2, 2.5, 3, 4, 5))
      v <- v[v >= B$range[1] - 1e-9 & v <= B$range[2] + 1e-9]
      if (length(v) > 8) v <- v[round(seq(1, length(v), length.out = 7))]
      v
    } else {
      v <- scales::breaks_extended(6)(B$range)
      v[v >= B$range[1] - 1e-9 & v <= B$range[2] + 1e-9]
    }
    strip_lines <- rbind(strip_lines,
      data.frame(seg(B$x0, B$x1, yb + 2, yb + 2), colour = graph_ink$text, width = 0.8, type = "solid", id = NA),
      data.frame(seg(SX(sticks), SX(sticks), yb + 2, yb + 6), colour = graph_ink$text, width = 0.8, type = "solid", id = NA),
      data.frame(seg(SX(B$pre), SX(B$pre), B$rows - 4, yb), colour = cinema_ink$prespecified, width = 1.3,
                 type = "22", id = NA))
    labels <- rbind(labels, texts(cinema_num(sticks, ratio), SX(sticks), yb + 14, dims$small_pt, graph_ink$muted,
                                  hjust = 0.5))
    live <- rbind(live, data.frame(texts(paste("now", cinema_num(B$pre, ratio)), SX(B$pre), B$rows - 9,
                                         dims$small_pt, graph_ink$title, hjust = 0.5, face = "bold"),
                                   id = paste0("sl", k)))
    strip_now <- data.frame(seg(SX(B$pre), SX(B$pre), B$rows - 5, yb + 2), id = paste0("sn", k))
    strip_lines <- rbind(strip_lines, data.frame(strip_now[1:4], colour = graph_ink$title, width = 1.6,
                                                 type = "solid", id = strip_now$id))
  }

  # The key to the reading colors.
  km <- clin_key_masks(sv)
  kink <- clin_ink(km, sv)
  kw <- paste0(cinema_reading(km, sv), c("", "", "", "", "", ""))
  kx <- cumsum(c(0, utils::head(text_width_card(kw, dims$small_pt, lay$family, 1) + 26, -1)))
  swatches <- rbind(swatches, data.frame(rect(kx, kx + 9, lay$key_y + 1, lay$key_y + 10), fill = kink$col,
                                         alpha = kink$alpha, id = paste0("key", seq_along(km))))
  labels <- rbind(labels, texts(kw, kx + 13, lay$key_y + 5.5, dims$small_pt, graph_ink$text))

  static_lines <- strip_lines[is.na(strip_lines$id), ]
  moving <- strip_lines[!is.na(strip_lines$id), ]
  p <- ggplot() +
    geom_segment(data = grid, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
                 colour = graph_ink$faint, linewidth = 0.6 / .pt) +
    ggiraph::geom_rect_interactive(data = hit, aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin,
                                                   ymax = .data$ymax, data_id = .data$id, tooltip = .data$tooltip),
                                   fill = graph_ink$page, colour = NA, alpha = 0.01) +
    ggiraph::geom_rect_interactive(data = region, aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin,
                                                      ymax = .data$ymax, data_id = .data$id),
                                   fill = cinema_ink$little, alpha = 0.22, colour = NA) +
    geom_rect(data = key, aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, ymax = .data$ymax),
              fill = cinema_ink$little, alpha = 0.22) +
    geom_segment(data = null_line, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
                 colour = graph_ink$muted, linewidth = 0.8 / .pt) +
    geom_segment(data = pre, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
                 colour = cinema_ink$prespecified, linewidth = 1.3 / .pt, linetype = "22") +
    geom_segment(data = key_lines, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend,
                                       colour = .data$colour, linewidth = .data$width / .pt,
                                       linetype = .data$type)) +
    (if (!is.null(pi)) ggiraph::geom_segment_interactive(
      data = pi, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend, data_id = .data$id),
      colour = graph_ink$muted, linewidth = 1.2 / .pt)) +
    (if (!is.null(pi_ends) && nrow(pi_ends)) ggiraph::geom_segment_interactive(
      data = pi_ends, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend, data_id = .data$id),
      colour = graph_ink$muted, linewidth = 1.2 / .pt)) +
    ggiraph::geom_segment_interactive(
      data = ci, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend, data_id = .data$id,
                     tooltip = .data$tooltip), colour = graph_ink$title, linewidth = 3 / .pt) +
    (if (!is.null(arrows)) geom_polygon(data = arrows, aes(x = .data$x, y = .data$y, group = .data$g),
                                        fill = graph_ink$title)) +
    ggiraph::geom_rect_interactive(
      data = points, aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, ymax = .data$ymax,
                         data_id = .data$id, tooltip = .data$tooltip), fill = graph_ink$title, colour = NA) +
    geom_segment(data = axis, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
                 colour = graph_ink$text, linewidth = 0.8 / .pt) +
    ggiraph::geom_rect_interactive(
      data = strips, aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, ymax = .data$ymax,
                         fill = .data$fill, alpha = .data$alpha, data_id = "sb"), colour = NA) +
    geom_segment(data = static_lines, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend,
                                          colour = .data$colour, linewidth = .data$width / .pt,
                                          linetype = .data$type)) +
    ggiraph::geom_segment_interactive(data = moving, aes(x = .data$x, xend = .data$xend, y = .data$y,
                                                         yend = .data$yend, data_id = .data$id),
                                      colour = graph_ink$title, linewidth = 1.6 / .pt) +
    ggiraph::geom_rect_interactive(
      data = swatches, aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, ymax = .data$ymax,
                           fill = .data$fill, alpha = .data$alpha, data_id = .data$id), colour = NA) +
    draw_text(rbind(labels, pre_labels, tick_labels, axis_title), lay$family) +
    ggiraph::geom_text_interactive(
      data = live, aes(x = .data$x, y = .data$y, label = .data$label, data_id = .data$id),
      hjust = live$hjust, size = live$size, colour = live$colour, fontface = live$fontface,
      family = lay$family) +
    scale_linewidth_identity() +
    graph_frame(lay$page, lay$family)
  p
}

# What the widget needs to recompute every reading at any limits, in the
# widget's own units.
clin_render <- function(lay) {
  cn <- lay$cn
  pr <- lay$pr
  page <- lay$page
  km <- clin_key_masks(cn$small_values)
  list(
    dx = page$px(0), dy = page$height - page$py(0),
    ratio = cn$ratio, small_values = cn$small_values, prediction = lay$prediction,
    pct = lay$pct, pre = lay$lim, reach = lay$reach,
    f0 = lay$f0, f1 = lay$f1, a = lay$a, b = lay$b, top = lay$rows_top - 6, bottom = lay$rows_bottom,
    row_h = lay$dims$row_h, strip_row = lay$dims$strip_row,
    blocks = lapply(lay$blocks, function(B) B[c("which", "top", "rows", "range", "x0", "x1")]),
    rows = lapply(seq_len(lay$n), function(i) list(label = lay$labels[i], te = pr$te[i], lo = pr$lo[i], hi = pr$hi[i],
                                                   plo = pr$plo[i], phi = pr$phi[i])),
    words = as.list(stats::setNames(cinema_reading(0:7, cn$small_values), 0:7)),
    long = as.list(stats::setNames(cinema_reading(0:7, cn$small_values, long = TRUE), 0:7)),
    ink = lapply(0:7, function(m) { k <- clin_ink(m, cn$small_values); list(col = k$col, alpha = k$alpha) }),
    levels = cinema_ink$level, concern = cinema_concern_words,
    tau2 = cn$tau2, n_studies = cn$n_studies
  )
}
