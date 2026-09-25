#' Draw direct, indirect and network estimates side by side
#'
#' Draws, for every comparison of a network meta-analysis, the direct
#' estimate from the studies that compare the pair head to head, the
#' indirect estimate from the rest of the network and the network estimate
#' that combines them, as separated by [netmeta::netsplit()]. Beside them is
#' the inconsistency factor, the ratio of the direct to the indirect estimate
#' for a ratio measure or their difference otherwise, with its confidence
#' interval and p-value, and the incoherence judgment that CINeMA's rules
#' give.
#'
#' A comparison with only direct or only indirect evidence cannot be checked
#' this way. It is kept visibly apart, marked as not assessable locally,
#' because the absence of a test is not agreement; CINeMA then judges it
#' from the global design by treatment interaction test, given under the
#' plot. Both tests have low power, above all with few studies, so a large
#' p-value is weak evidence that direct and indirect evidence agree, and the
#' interval of the inconsistency factor shows how large a disagreement the
#' data still allow.
#'
#' Hovering over a comparison gives its numbers; clicking it, or pressing
#' Enter on it, opens a panel that says in words how far direct and indirect
#' evidence could disagree, lists the studies behind each estimate and gives
#' the reason for the judgment.
#'
#' @param x Judgments from [cinema_judge()], or a network meta-analysis from
#'   [netmeta::netmeta()], judged with the arguments in `...`.
#' @param ... When `x` is a netmeta fit, arguments for [cinema_judge()], such
#'   as `threshold`, which shades the range of little difference and lets
#'   the rule judge comparisons whose test gives p of 0.10 or less, `split`,
#'   `small_values`, `order` and `contributions`, which list the studies
#'   behind each indirect estimate.
#' @param title,caption Title above the plot and note below it. A caption
#'   you give is added above the default notes.
#' @param family Font family. The package ships Lato and registers it on load.
#'
#' @inheritSection cinema_judge Sources
#'
#' @return An object of class `cinema_incoherence`, which prints as an
#'   interactive widget. Use [graph_widget()], [graph_plot()] or
#'   [graph_save()] for the widget, a static ggplot or a file. The field
#'   `comparisons` holds the direct, indirect and network estimates and the
#'   inconsistency factors on the scale of the effect, and `global` the
#'   design by treatment test.
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
#'   cinema_incoherence(nma, threshold = 1.25, small_values = "undesirable")
#' }
#' }
cinema_incoherence <- function(x, ..., title = NULL, caption = NULL, family = "Lato") {
  cn <- cinema_input(x, ..., fn = "cinema_incoherence")
  lay <- incoh_layout(cn, title, caption, family)
  structure(
    list(plot = incoh_draw(lay), width = lay$page$width / 72, height = lay$page$height / 72,
         title = title, on_render = "ggextremeCinemaKeys(el, 'r');", hover_inv = "opacity:1;",
         comparisons = cn$comparisons, global = cn$global, judgments = cn),
    class = c("cinema_incoherence", "ggx_graph")
  )
}

# Ticks on a log or linear axis between a and b, on the scale of the
# analysis, and the axis ends snapped to them.
cinema_axis <- function(a, b, ratio, most = 10) {
  if (ratio) {
    cand <- log(c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000))
    a <- max(c(cand[cand <= a + 1e-9], log(0.001)))
    b <- min(c(cand[cand >= b - 1e-9], log(1000)))
    ticks <- cand[cand >= a - 1e-9 & cand <= b + 1e-9]
    if (length(ticks) > most) ticks <- ticks[abs(ticks / log(10) - round(ticks / log(10))) < 1e-9 | abs(ticks) < 1e-9]
  } else {
    ticks <- scales::breaks_extended(6)(c(a, b))
    a <- min(a, ticks)
    b <- max(b, ticks)
  }
  list(a = a, b = b, ticks = ticks)
}

cinema_tick_labels <- function(ticks, ratio) {
  if (ratio) format(signif(exp(ticks), 3), trim = TRUE, drop0trailing = TRUE, scientific = FALSE) else
    format(ticks, trim = TRUE)
}

incoh_layout <- function(cn, title, caption, family) {
  dims <- utils::modifyList(graph_dims, list(label_pt = 9, small_pt = 8.5, sub_h = 11, row_h = 44, forest_w = 230,
                                             factor_w = 150))
  pr <- cn$pairs
  ratio <- cn$ratio
  n <- nrow(pr)
  width <- function(s, pt) max(text_width_card(s, pt, family, 1))
  labels <- paste(pr$a, "vs", pr$b)
  lab_w <- width(labels, dims$label_pt) + 4
  f0 <- lab_w + 14
  f1 <- f0 + dims$forest_w
  i0 <- f1 + 28
  i1 <- i0 + dims$factor_w
  p0 <- i1 + 12
  j0 <- p0 + 52
  j1 <- j0 + width(c("Some concerns", "Major concerns", "global test", "CINeMA judgment"), dims$small_pt) + 20
  legend_h <- 18
  head_y <- legend_h + 16
  rows_top <- head_y + 10
  row_y <- rows_top + (seq_len(n) - 1) * dims$row_h
  rows_bottom <- rows_top + n * dims$row_h
  lim <- if (!is.null(cn$threshold)) log_or(cn$threshold, ratio)
  ends <- c(pr$d_lo, pr$d_hi, pr$i_lo, pr$i_hi, pr$lo, pr$hi, lim)
  fa <- cinema_axis(min(ends, na.rm = TRUE), max(ends, na.rm = TRUE), ratio)
  f_ends <- c(pr$f_lo, pr$f_hi)
  f_ends <- f_ends[is.finite(f_ends)]
  span <- if (length(f_ends)) max(abs(f_ends)) else if (ratio) log(2) else 1
  span <- max(span, if (ratio) log(2) else 0.5)
  if (ratio) span <- min(span, log(100))
  ia <- cinema_axis(-span, span, ratio, most = 7)
  axis_y <- rows_bottom + 4
  bottom <- axis_y + 30
  pct <- paste0(format(100 * cn$level), "%")
  g <- cn$global
  cap <- c(caption,
           if (is.null(g)) "The global design by treatment interaction test cannot be computed: the network has no closed loop, and CINeMA then gives major concerns to every comparison." else
             paste0("Global design by treatment interaction test, for the whole network: Q = ", formatC(g$Q, format = "f", digits = 2),
                    " on ", g$df, " df, ", cinema_p(g$p), ". It judges the comparisons that cannot be tested locally."),
           paste0("Both tests have low power, above all with few studies: a large p-value is weak evidence of agreement, not proof, ",
                  "and the interval of the inconsistency factor shows how large a disagreement the data still allow. ",
                  "A comparison with only direct or only indirect evidence cannot be checked locally, which is not the same as agreement."),
           paste0("Direct and indirect estimates are separated by netmeta::netsplit() (", cn$split_method, "), ", cn$pooled,
                  " effects, with ", pct, " CIs; the inconsistency factor is ",
                  if (ratio) "the ratio of the direct to the indirect estimate." else "the direct minus the indirect estimate.",
                  if (!is.null(lim)) paste0(" Shaded: the range of little difference, ",
                                            paste(format(cn$threshold, digits = 3), collapse = " to "), ".") else ""))
  cap <- cinema_wrap(cap, j1 - 10, graph_dims$caption_pt, family)
  page <- graph_canvas(c(0, j1), c(0, bottom), title, cap, character(0), character(0), NULL, dims, family)
  list(cn = cn, pr = pr, n = n, dims = dims, page = page, family = family, labels = labels, lab_w = lab_w,
       f0 = f0, f1 = f1, i0 = i0, i1 = i1, p0 = p0, j0 = j0, j1 = j1, legend_h = legend_h, head_y = head_y,
       rows_top = rows_top, row_y = row_y, rows_bottom = rows_bottom, lim = lim, fa = fa, ia = ia,
       axis_y = axis_y, pct = pct)
}

incoh_draw <- function(lay) {
  cn <- lay$cn
  pr <- lay$pr
  dims <- lay$dims
  px <- lay$page$px
  py <- lay$page$py
  n <- lay$n
  ratio <- cn$ratio
  fam <- lay$family
  texts <- function(label, x, y, pt, color, hjust = 0, face = "plain") {
    if (!length(label)) return(empty_texts())
    data.frame(x = px(x), y = py(y), label = as.character(label), size = pt / .pt, colour = color,
               hjust = hjust, fontface = face, stringsAsFactors = FALSE)
  }
  rect <- function(x0, x1, y0, y1) data.frame(xmin = px(x0), xmax = px(x1), ymin = py(y1), ymax = py(y0))
  seg <- function(x, xend, y, yend) data.frame(x = px(x), xend = px(xend), y = py(y), yend = py(yend))
  FX <- function(v) lay$f0 + (pmin(pmax(v, lay$fa$a), lay$fa$b) - lay$fa$a) / (lay$fa$b - lay$fa$a) * (lay$f1 - lay$f0)
  IX <- function(v) lay$i0 + (pmin(pmax(v, lay$ia$a), lay$ia$b) - lay$ia$a) / (lay$ia$b - lay$ia$a) * (lay$i1 - lay$i0)
  ink <- c(direct = cinema_ink$first, indirect = cinema_ink$prespecified, network = graph_ink$title)
  top <- lay$rows_top - 4
  bottom <- lay$rows_bottom

  # The key and the column heads.
  key_words <- c("Direct", "Indirect", "Network", if (!is.null(lay$lim)) "Range of little difference")
  key_at <- cumsum(c(0, utils::head(text_width_card(key_words, dims$small_pt, fam, 1) + 40, -1)))
  labels <- rbind(
    texts(key_words, key_at + 26, 8, dims$small_pt, graph_ink$text),
    texts(c("Comparison", "Direct, indirect and network estimates", "Inconsistency factor", "p-value", "CINeMA judgment"),
          c(0, lay$f0, lay$i0, lay$p0, lay$j0), lay$head_y, dims$small_pt, graph_ink$muted)
  )
  key_lines <- data.frame(seg(key_at[1:3], key_at[1:3] + 20, 8, 8), colour = unname(ink))
  key_band <- if (!is.null(lay$lim)) rect(key_at[4], key_at[4] + 20, 3, 13)

  grid <- rbind(seg(FX(lay$fa$ticks), FX(lay$fa$ticks), top, bottom), seg(IX(lay$ia$ticks), IX(lay$ia$ticks), top, bottom))
  nulls <- rbind(seg(FX(0), FX(0), top, bottom), seg(IX(0), IX(0), top, bottom))
  band <- if (!is.null(lay$lim)) rect(FX(lay$lim[1]), FX(lay$lim[2]), top, bottom)
  axis <- rbind(seg(lay$f0, lay$f1, lay$axis_y, lay$axis_y), seg(FX(lay$fa$ticks), FX(lay$fa$ticks), lay$axis_y, lay$axis_y + 4),
                seg(lay$i0, lay$i1, lay$axis_y, lay$axis_y), seg(IX(lay$ia$ticks), IX(lay$ia$ticks), lay$axis_y, lay$axis_y + 4))
  labels <- rbind(labels,
    texts(cinema_tick_labels(lay$fa$ticks, ratio), FX(lay$fa$ticks), lay$axis_y + 12, dims$small_pt, graph_ink$muted, hjust = 0.5),
    texts(cinema_tick_labels(lay$ia$ticks, ratio), IX(lay$ia$ticks), lay$axis_y + 12, dims$small_pt, graph_ink$muted, hjust = 0.5),
    texts(paste0(cn$measure, if (ratio) " (log scale)" else ""), (lay$f0 + lay$f1) / 2, lay$axis_y + 25, dims$small_pt,
          graph_ink$text, hjust = 0.5),
    texts(if (ratio) "Direct / indirect (log scale)" else "Direct minus indirect", (lay$i0 + lay$i1) / 2, lay$axis_y + 25,
          dims$small_pt, graph_ink$text, hjust = 0.5))

  tips <- vapply(seq_len(n), function(i) incoh_tip(cn, i), character(1))
  clicks <- pin_js(paste0("r", seq_len(n)), vapply(seq_len(n), function(i) incoh_panel(cn, i), character(1)))
  hit <- data.frame(rect(0, lay$j1, lay$row_y + 1, lay$row_y + dims$row_h - 1), id = paste0("r", seq_len(n)),
                    tooltip = tips, onclick = clicks, stringsAsFactors = FALSE)
  type_words <- c(mixed = "mixed evidence", direct = "direct evidence only", indirect = "indirect evidence only")
  studies_words <- ifelse(pr$k == 0, "no direct studies", paste(pr$k, ifelse(pr$k == 1, "study", "studies"), "head to head"))
  labels <- rbind(labels,
    texts(lay$labels, 0, lay$row_y + 12, dims$label_pt, graph_ink$title),
    texts(paste0(type_words[pr$type], ", ", studies_words), 0, lay$row_y + 25, dims$small_pt, graph_ink$muted))

  # Three estimates per comparison.
  lines <- NULL
  marks <- NULL
  arrows <- NULL
  notes <- NULL
  add <- function(i, k, te, lo, hi, what) {
    y <- lay$row_y[i] + 9 + (k - 1) * dims$sub_h
    if (!is.finite(te)) {
      notes <<- rbind(notes, texts(if (what == "direct") "no study compares them directly" else "no independent indirect evidence",
                                   FX(lay$fa$a) + 4, y, dims$small_pt - 0.5, graph_ink$muted))
      return()
    }
    lines <<- rbind(lines, data.frame(seg(FX(lo), FX(hi), y, y), colour = ink[[what]], width = if (what == "network") 2.4 else 1.8))
    x <- FX(te)
    s <- if (what == "network") 4 else 3
    shape <- if (what == "network") data.frame(x = x + c(-s, 0, s, 0), y = y + c(0, -s, 0, s)) else
      data.frame(x = x + c(-s, s, s, -s), y = y + c(-s, -s, s, s))
    marks <<- rbind(marks, data.frame(x = px(shape$x), y = py(shape$y), g = paste0(what, i), fill = ink[[what]]))
    if (lo < lay$fa$a) arrows <<- rbind(arrows, data.frame(x = px(FX(lo) + c(0, 5, 5)), y = py(y + c(0, -3, 3)), g = paste0("l", what, i), fill = ink[[what]]))
    if (hi > lay$fa$b) arrows <<- rbind(arrows, data.frame(x = px(FX(hi) + c(0, -5, -5)), y = py(y + c(0, -3, 3)), g = paste0("h", what, i), fill = ink[[what]]))
  }
  for (i in seq_len(n)) {
    add(i, 1, pr$d_te[i], pr$d_lo[i], pr$d_hi[i], "direct")
    add(i, 2, pr$i_te[i], pr$i_lo[i], pr$i_hi[i], "indirect")
    add(i, 3, pr$te[i], pr$lo[i], pr$hi[i], "network")
  }

  # The inconsistency factor, or a band that says it cannot be assessed.
  mixed <- pr$type == "mixed"
  yf <- lay$row_y + dims$row_h / 2
  f_lines <- if (any(mixed)) seg(IX(pr$f_lo[mixed]), IX(pr$f_hi[mixed]), yf[mixed], yf[mixed])
  f_points <- if (any(mixed)) data.frame(x = px(IX(pr$f_te[mixed])), y = py(yf[mixed]))
  f_arrows <- NULL
  for (i in which(mixed)) {
    if (pr$f_lo[i] < lay$ia$a) f_arrows <- rbind(f_arrows, data.frame(x = px(IX(pr$f_lo[i]) + c(0, 5, 5)), y = py(yf[i] + c(0, -3, 3)), g = paste0("fl", i)))
    if (pr$f_hi[i] > lay$ia$b) f_arrows <- rbind(f_arrows, data.frame(x = px(IX(pr$f_hi[i]) + c(0, -5, -5)), y = py(yf[i] + c(0, -3, 3)), g = paste0("fh", i)))
  }
  na_band <- if (any(!mixed)) rect(lay$i0, lay$i1, yf[!mixed] - 8, yf[!mixed] + 8)
  labels <- rbind(labels,
    if (any(!mixed)) texts(ifelse(pr$type[!mixed] == "direct", "not assessable locally: direct only",
                                  "not assessable locally: indirect only"), (lay$i0 + lay$i1) / 2, yf[!mixed],
                           dims$small_pt - 0.5, graph_ink$muted, hjust = 0.5),
    if (any(mixed)) texts(cinema_estimate(pr$f_te[mixed], pr$f_lo[mixed], pr$f_hi[mixed], ratio), lay$i0, yf[mixed] - 11,
                          dims$small_pt - 0.5, graph_ink$text),
    if (any(mixed)) texts(vapply(pr$f_p[mixed], function(p) sub("^p = ", "", sub("^p < ", "< ", cinema_p(p))), ""),
                          lay$p0, yf[mixed], dims$small_pt, graph_ink$title))

  # The judgment.
  d <- cn$domains$incoherence
  jw <- ifelse(is.na(d$level), "Not judged", cinema_concern_words[d$level + 1])
  basis <- ifelse(d$source == "Yours", "your judgment",
                  ifelse(grepl("global", d$source), "global test", ifelse(grepl("local", d$source), "local test", "")))
  jmarks <- data.frame(rect(lay$j0, lay$j0 + 9, yf - 9.5, yf - 0.5),
                       fill = ifelse(is.na(d$level), graph_ink$page, cinema_ink$level[pmin(d$level, 2) + 1]),
                       border = ifelse(is.na(d$level), graph_ink$muted, NA))
  labels <- rbind(labels, texts(jw, lay$j0 + 13, yf - 5, dims$small_pt, graph_ink$title),
                  texts(basis, lay$j0 + 13, yf + 7, dims$small_pt - 0.5, graph_ink$muted))

  ggplot() +
    ggiraph::geom_rect_interactive(data = hit, aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, ymax = .data$ymax,
                                                   data_id = .data$id, tooltip = .data$tooltip, onclick = .data$onclick),
                                   fill = graph_ink$page, alpha = 0.01, colour = NA) +
    (if (!is.null(band)) geom_rect(data = band, aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, ymax = .data$ymax),
                                   fill = cinema_ink$little, alpha = 0.22)) +
    (if (!is.null(key_band)) geom_rect(data = key_band, aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, ymax = .data$ymax),
                                       fill = cinema_ink$little, alpha = 0.22)) +
    geom_segment(data = grid, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
                 colour = graph_ink$faint, linewidth = 0.6 / .pt) +
    geom_segment(data = nulls, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
                 colour = graph_ink$muted, linewidth = 0.8 / .pt) +
    (if (!is.null(na_band)) geom_rect(data = na_band, aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, ymax = .data$ymax),
                                      fill = graph_ink$faint, colour = graph_ink$plain_border, linetype = "22", linewidth = 0.8 / .pt)) +
    (if (!is.null(lines)) geom_segment(data = lines, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend,
                                                         colour = .data$colour, linewidth = .data$width / .pt))) +
    geom_segment(data = key_lines, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend, colour = .data$colour),
                 linewidth = 2 / .pt) +
    (if (!is.null(marks)) geom_polygon(data = marks, aes(x = .data$x, y = .data$y, group = .data$g, fill = .data$fill))) +
    (if (!is.null(arrows)) geom_polygon(data = arrows, aes(x = .data$x, y = .data$y, group = .data$g, fill = .data$fill))) +
    (if (!is.null(f_lines)) geom_segment(data = f_lines, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
                                         colour = graph_ink$title, linewidth = 1.8 / .pt)) +
    (if (!is.null(f_arrows)) geom_polygon(data = f_arrows, aes(x = .data$x, y = .data$y, group = .data$g), fill = graph_ink$title)) +
    (if (!is.null(f_points)) geom_point(data = f_points, aes(x = .data$x, y = .data$y), shape = 21, size = 2.4,
                                        fill = graph_ink$page, colour = graph_ink$title, stroke = 1)) +
    geom_segment(data = axis, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
                 colour = graph_ink$text, linewidth = 0.8 / .pt) +
    geom_rect(data = jmarks, aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, ymax = .data$ymax,
                                 fill = .data$fill, colour = .data$border), linewidth = 1 / .pt) +
    draw_text(rbind(labels, notes), fam) +
    scale_linewidth_identity() +
    graph_frame(lay$page, fam)
}

incoh_tip <- function(cn, i) {
  pr <- cn$pairs
  ratio <- cn$ratio
  est <- function(te, lo, hi) if (is.finite(te)) cinema_estimate(te, lo, hi, ratio) else "none"
  d <- cn$domains$incoherence
  paste0('<div class="ggx-tip-title">', esc(pr$a[i]), " vs ", esc(pr$b[i]), "</div>",
         '<div class="ggx-tip-sub">', esc(cn$measure), " (", format(100 * cn$level), "% CI)</div>",
         tip_rows(c("Direct" = est(pr$d_te[i], pr$d_lo[i], pr$d_hi[i]),
                    "Indirect" = est(pr$i_te[i], pr$i_lo[i], pr$i_hi[i]),
                    "Network" = est(pr$te[i], pr$lo[i], pr$hi[i]),
                    "Inconsistency factor" = if (pr$type[i] == "mixed")
                      paste0(est(pr$f_te[i], pr$f_lo[i], pr$f_hi[i]), ", ", cinema_p(pr$f_p[i])) else "not assessable locally",
                    "Incoherence" = if (is.na(d$level[i])) "Not judged" else cinema_concern_words[d$level[i] + 1])),
         '<div class="ggx-tip-hint">Click or press Enter for the studies behind each estimate.</div>')
}

# How far apart the direct and indirect estimates could be, in words.
incoh_range <- function(f_lo, f_hi, ratio) {
  if (!ratio) {
    return(paste0("anywhere from ", cinema_num(f_lo, FALSE), " to ", cinema_num(f_hi, FALSE), " away from the indirect one"))
  }
  lo <- exp(f_lo)
  hi <- exp(f_hi)
  pc <- function(v) paste0(formatC(100 * v, format = "f", digits = 0), "%")
  times <- function(v) paste0(cinema_num(log(v), TRUE), " times")
  if (hi <= 1) return(paste0("between ", pc(1 - hi), " and ", pc(1 - lo), " lower than the indirect one"))
  if (lo >= 1) return(paste0("between ", cinema_num(log(lo), TRUE), " and ", times(hi), " as high as the indirect one"))
  if (hi < 2) return(paste0("anywhere from ", pc(1 - lo), " lower to ", pc(hi - 1), " higher than the indirect one"))
  paste0("anywhere from ", pc(1 - lo), " lower than the indirect one to ", times(hi), " as high")
}

incoh_panel <- function(cn, i) {
  pr <- cn$pairs
  ratio <- cn$ratio
  x <- cn$fit
  pct <- paste0(format(100 * cn$level), "%")
  est <- function(te, lo, hi) cinema_estimate(te, lo, hi, ratio)
  d <- cn$domains$incoherence
  label <- paste(pr$a[i], "vs", pr$b[i])
  direct <- cn$direct[[i]]
  reading <- ""
  if (!is.null(cn$threshold)) {
    lim <- log_or(cn$threshold, ratio)
    parts <- c(if (is.finite(pr$d_te[i])) paste0("direct, ", cinema_reading(cinema_zones(pr$d_lo[i], pr$d_hi[i], lim[1], lim[2]), cn$small_values)),
               if (is.finite(pr$i_te[i])) paste0("indirect, ", cinema_reading(cinema_zones(pr$i_lo[i], pr$i_hi[i], lim[1], lim[2]), cn$small_values)),
               paste0("network, ", cinema_reading(cinema_zones(pr$lo[i], pr$hi[i], lim[1], lim[2]), cn$small_values)))
    reading <- paste0(" Against the range of little difference, ", paste(format(cn$threshold, digits = 3), collapse = " to "),
                      ", each ", pct, " CI is compatible with: ", paste(parts, collapse = "; "), ".")
  }
  lead <- if (pr$type[i] == "mixed") {
    paste0("The direct ", if (length(direct) == 1) "study gives " else "studies give ", est(pr$d_te[i], pr$d_lo[i], pr$d_hi[i]),
           " and the indirect evidence ", est(pr$i_te[i], pr$i_lo[i], pr$i_hi[i]), ". Their ",
           if (ratio) "ratio" else "difference", " is ", est(pr$f_te[i], pr$f_lo[i], pr$f_hi[i]),
           ", so the data are compatible with a direct estimate ", incoh_range(pr$f_lo[i], pr$f_hi[i], ratio), " (",
           cinema_p(pr$f_p[i]), "). A test this weak cannot show agreement.")
  } else if (pr$type[i] == "direct") {
    paste0(label, " has direct evidence only, from ", cinema_names(direct),
           ": no independent indirect estimate exists, so incoherence is not assessable locally. That is not evidence of agreement.")
  } else {
    paste0("No study compares ", pr$a[i], " and ", pr$b[i],
           " head to head, so incoherence is not assessable locally. That is not evidence of agreement.")
  }
  # Each direct study's own estimate, from the pairwise data of the fit.
  z <- stats::qnorm(1 - (1 - cn$level) / 2)
  rows <- vapply(direct, function(s) {
    hit <- which(x$studlab == s & ((x$treat1 == pr$a[i] & x$treat2 == pr$b[i]) | (x$treat1 == pr$b[i] & x$treat2 == pr$a[i])))[1]
    sign <- if (x$treat1[hit] == pr$a[i]) 1 else -1
    e <- sign * x$TE[hit]
    paste0('<tr><th scope="row">', esc(s), "</th><td>", esc(est(e, e - z * x$seTE[hit], e + z * x$seTE[hit])), "</td></tr>")
  }, character(1))
  direct_table <- if (length(direct)) {
    paste0('<div class="ggx-refs-head">Studies behind the direct estimate</div><div class="ggx-table"><table><thead><tr><td></td>',
           '<th scope="col">', esc(cn$measure), " (", pct, " CI)</th></tr></thead><tbody class=\"ggx-num\">",
           paste(rows, collapse = ""), "</tbody></table></div>")
  } else ""
  indirect_table <- ""
  if (is.finite(pr$i_te[i]) && !is.null(cn$contrib)) {
    ct <- cn$contrib[cn$contrib$key == pr$key[i] & cn$contrib$share > 1e-6, , drop = FALSE]
    arms <- function(s) unique(c(x$treat1[x$studlab == s], x$treat2[x$studlab == s]))
    keep <- !ct$study %in% direct | vapply(ct$study, function(s) length(arms(s)) > 2, logical(1))
    ct <- ct[keep, , drop = FALSE]
    ct <- ct[order(-ct$share), , drop = FALSE]
    if (nrow(ct)) {
      route <- vapply(ct$study, function(s) {
        if (s %in% direct) paste0("its other arms (", paste(setdiff(arms(s), c(pr$a[i], pr$b[i])), collapse = ", "),
                                  "); it also compares the pair directly") else "other treatments only"
      }, character(1))
      indirect_table <- paste0('<div class="ggx-refs-head">Studies behind the indirect estimate</div>',
                               '<div class="ggx-table"><table><thead><tr><td></td><th scope="col">Reaches this comparison through</th>',
                               '<th scope="col">Share of the network estimate</th></tr></thead><tbody>',
                               paste0('<tr><th scope="row">', esc(ct$study), "</th><td>", esc(route), "</td><td>",
                                      cinema_pct(ct$share), "</td></tr>", collapse = ""), "</tbody></table></div>")
    }
  }
  lv <- d$level[i]
  paste0('<div class="ggx-title">', esc(label), "</div>",
         '<div class="ggx-sub">', esc(c(mixed = "Direct and indirect evidence", direct = "Direct evidence only",
                                        indirect = "Indirect evidence only")[[pr$type[i]]]),
         "; ", round(100 * pr$prop[i]), "% of the network estimate's information comes from direct studies</div>",
         "<p>", esc(lead), esc(reading), "</p>",
         "<p><b>Incoherence:</b> ", cinema_chip(lv, if (is.na(lv)) "Not judged" else cinema_concern_words[lv + 1]), ". ",
         esc(d$reason[i]), "</p>", direct_table, indirect_table,
         '<p class="ggx-note">netmeta::netsplit() separates direct from indirect evidence by ', esc(cn$split_method),
         ", so a multi-arm study can feed both.",
         if (nzchar(indirect_table)) paste0(" ", esc(cn$contribution_method)) else "", "</p>")
}
