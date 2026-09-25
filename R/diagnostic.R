#' Draw an interactive diagnostic threshold explorer
#'
#' Shows what a cutoff on a continuous test means for the people tested. The
#' marker's distribution in those with and without the condition sits beside
#' the ROC curve and the predictive values across prevalence, above a grid
#' of 1,000 people who are found, missed, falsely alarmed or correctly
#' cleared, and a table of sensitivity, specificity, predictive values and
#' likelihood ratios with their confidence intervals.
#'
#' In the widget, dragging the cutoff on the distributions or moving its
#' slider updates everything at once, and a second slider sets the
#' prevalence of the population the test will be used in, which changes the
#' predictive values and the grid but not sensitivity or specificity. A
#' sentence under the plot says what the current cutoff does in words.
#'
#' A cutoff chosen in the same data it is judged on looks better than it
#' will in new patients, so a prespecified `cutoff` is marked as such, and
#' the default, the cutoff that maximizes Youden's index, is labeled as
#' chosen in these data. A test cutoff is not a treatment threshold, and in
#' a case control sample the prevalence in the data is not the prevalence
#' in practice; set `prevalence` to the one that applies.
#'
#' @param formula A formula `outcome ~ marker`. The outcome is logical, 0 and
#'   1, or a factor or text with two values, whose second level (or `TRUE`,
#'   or 1) means the condition is present. The marker is numeric.
#' @param data A data frame holding both.
#' @param cutoff The prespecified cutoff. A result at or beyond it, in the
#'   direction of `direction`, is positive. Defaults to the cutoff that
#'   maximizes Youden's index in these data.
#' @param prevalence The prevalence of the condition where the test will be
#'   used, between 0 and 1. Defaults to the prevalence in `data`.
#' @param direction Whether `"higher"` or `"lower"` values point to the
#'   condition. `"auto"` picks the direction with an area under the curve of
#'   at least one half.
#' @param labels Names for those without and with the condition, in that
#'   order.
#' @param marker_label Axis label for the marker, with its unit.
#' @param level Confidence level for the intervals.
#' @param title,caption Title above the plot and note below it.
#' @param family Font family. The package ships Lato and registers it on load.
#'
#' @section Intervals:
#' Sensitivity and specificity have Wilson score intervals. Predictive values
#' at a set prevalence use the logit intervals of Mercaldo, Lau and Zhou
#' (2007), likelihood ratios the log method, and the area under the curve
#' the method of DeLong, DeLong and Clarke-Pearson (1988). Intervals that
#' need a count of zero are not shown.
#'
#' @return An object of class `ggdiagnostic`, which prints as an interactive
#'   widget. Use [graph_widget()], [graph_plot()] or [graph_save()] for the
#'   widget, a static ggplot or a file. The field `accuracy` holds the
#'   measures at the cutoff, `roc` the curve and `auc` the area under it.
#' @export
#'
#' @examples
#' if (requireNamespace("MASS", quietly = TRUE)) {
#'   pima <- rbind(MASS::Pima.tr, MASS::Pima.te)
#'   ggdiagnostic(type ~ glu, pima, cutoff = 126, prevalence = 0.1,
#'                labels = c("No diabetes", "Diabetes"),
#'                marker_label = "Plasma glucose (mg/dL)")
#' }
ggdiagnostic <- function(formula, data, cutoff = NULL, prevalence = NULL,
                         direction = c("auto", "higher", "lower"),
                         labels = NULL, marker_label = NULL, level = 0.95,
                         title = NULL, caption = NULL, family = "Lato") {
  direction <- match.arg(direction)
  dx <- dx_prepare(formula, data, labels, marker_label)
  dx <- dx_compute(dx, cutoff, prevalence, direction, level)
  lay <- dx_layout(dx, title, caption, family)
  structure(
    list(plot = dx_draw(lay), width = lay$page$width / 72, height = lay$page$height / 72,
         title = title, on_render = "ggextremeDiagnostic(el, data);",
         render_data = dx_render(lay), hover_inv = "opacity:1;",
         accuracy = dx$table, roc = dx$roc, auc = dx$auc[c("auc", "lower", "upper")],
         cutoff = dx$cutoff, prevalence = dx$prev, direction = dx$direction),
    class = c("ggdiagnostic", "ggx_graph")
  )
}

dx_prepare <- function(formula, data, labels, marker_label) {
  if (!inherits(formula, "formula") || length(formula) != 3) {
    rlang::abort("`formula` must be `outcome ~ marker`.")
  }
  if (!is.data.frame(data)) rlang::abort("`data` must be a data frame.")
  y <- eval(formula[[2]], data, environment(formula))
  x <- eval(formula[[3]], data, environment(formula))
  if (!is.numeric(x)) rlang::abort("The marker must be numeric.")
  y_name <- deparse(formula[[2]])
  x_name <- deparse(formula[[3]])
  default <- c(paste("No", y_name), capitalize(y_name))
  present <- if (is.logical(y)) {
    y
  } else if (is.numeric(y)) {
    if (!all(y[!is.na(y)] %in% c(0, 1))) rlang::abort("A numeric outcome must be 0 or 1.")
    y == 1
  } else {
    f <- if (is.factor(y)) droplevels(y) else factor(y)
    if (nlevels(f) != 2) rlang::abort("The outcome must have exactly two values.")
    default <- levels(f)
    f == levels(f)[2]
  }
  keep <- !is.na(present) & !is.na(x)
  x <- x[keep]
  present <- present[keep]
  if (sum(present) < 2 || sum(!present) < 2) {
    rlang::abort("Each outcome group needs at least two people with a marker value.")
  }
  if (is.null(labels)) labels <- default
  if (length(labels) != 2) rlang::abort("`labels` must name those without and with the condition.")
  if (is.null(marker_label)) {
    marker_label <- if (is.name(formula[[3]]) && x_name %in% names(data)) column_labels(data, x_name) else x_name
  }
  list(x1 = sort(x[present]), x0 = sort(x[!present]), labels = as.character(labels),
       marker_label = marker_label, dropped = sum(!keep))
}

# The area under the curve and its DeLong interval, from placement values
# computed with midranks.
dx_auc <- function(x1, x0, level) {
  n1 <- length(x1)
  n0 <- length(x0)
  r <- rank(c(x1, x0))
  auc <- (sum(r[seq_len(n1)]) - n1 * (n1 + 1) / 2) / (n1 * n0)
  v10 <- (r[seq_len(n1)] - rank(x1)) / n0
  v01 <- 1 - (r[n1 + seq_len(n0)] - rank(x0)) / n1
  se <- sqrt(stats::var(v10) / n1 + stats::var(v01) / n0)
  z <- stats::qnorm(1 - (1 - level) / 2)
  list(auc = auc, se = se, lower = max(0, auc - z * se), upper = min(1, auc + z * se))
}

wilson <- function(k, n, z) {
  p <- k / n
  mid <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  c(p, mid - half, mid + half)
}

# Accuracy at one cutoff: counts, then every measure with its interval. NA
# marks an interval that needs a count of zero.
dx_measures <- function(tp, fn, fp, tn, prev, z) {
  n1 <- tp + fn
  n0 <- fp + tn
  se <- tp / n1
  sp <- tn / n0
  logit_ci <- function(est, v) {
    if (!is.finite(v)) return(c(est, NA, NA))
    l <- stats::qlogis(est)
    c(est, stats::plogis(l - z * sqrt(v)), stats::plogis(l + z * sqrt(v)))
  }
  inner <- se > 0 && se < 1 && sp > 0 && sp < 1
  ppv <- se * prev / (se * prev + (1 - sp) * (1 - prev))
  npv <- sp * (1 - prev) / (sp * (1 - prev) + (1 - se) * prev)
  ppv <- logit_ci(ppv, if (inner) (1 - se) / (se * n1) + sp / ((1 - sp) * n0) else Inf)
  npv <- logit_ci(npv, if (inner) se / ((1 - se) * n1) + (1 - sp) / (sp * n0) else Inf)
  log_ci <- function(est, v) {
    if (!is.finite(v) || !is.finite(est) || est <= 0) return(c(est, NA, NA))
    c(est, exp(log(est) - z * sqrt(v)), exp(log(est) + z * sqrt(v)))
  }
  lrp <- log_ci(se / (1 - sp), if (tp && fp) 1 / tp - 1 / n1 + 1 / fp - 1 / n0 else Inf)
  lrn <- log_ci((1 - se) / sp, if (fn && tn) 1 / fn - 1 / n1 + 1 / tn - 1 / n0 else Inf)
  out <- rbind(sensitivity = wilson(tp, n1, z), specificity = wilson(tn, n0, z),
               ppv = ppv, npv = npv, lr_positive = lrp, lr_negative = lrn)
  data.frame(measure = rownames(out), estimate = out[, 1], lower = out[, 2], upper = out[, 3],
             row.names = NULL, stringsAsFactors = FALSE)
}

dx_compute <- function(dx, cutoff, prevalence, direction, level) {
  x1 <- dx$x1
  x0 <- dx$x0
  z <- stats::qnorm(1 - (1 - level) / 2)
  if (direction == "auto") {
    direction <- if (dx_auc(x1, x0, level)$auc >= 0.5) "higher" else "lower"
  }
  sign <- if (direction == "higher") 1 else -1
  # Every distinct value is a possible cutoff; a result at or beyond it is
  # positive.
  cuts <- sort(unique(c(x1, x0)))
  pos1 <- function(cc) if (sign > 0) sum(x1 >= cc) else sum(x1 <= cc)
  pos0 <- function(cc) if (sign > 0) sum(x0 >= cc) else sum(x0 <= cc)
  tp <- vapply(cuts, pos1, numeric(1))
  fp <- vapply(cuts, pos0, numeric(1))
  sens <- tp / length(x1)
  spec <- 1 - fp / length(x0)
  youden <- cuts[which.max(sens + spec - 1)]
  prespecified <- !is.null(cutoff)
  if (is.null(cutoff)) cutoff <- youden
  if (!is.numeric(cutoff) || length(cutoff) != 1 || !is.finite(cutoff)) {
    rlang::abort("`cutoff` must be one number.")
  }
  sample_prev <- length(x1) / (length(x1) + length(x0))
  if (is.null(prevalence)) prevalence <- sample_prev
  if (!is.numeric(prevalence) || length(prevalence) != 1 || prevalence <= 0 || prevalence >= 1) {
    rlang::abort("`prevalence` must be a proportion between 0 and 1.")
  }
  auc <- dx_auc(sign * x1, sign * x0, level)
  a <- pos1(cutoff)
  b <- pos0(cutoff)
  table <- dx_measures(a, length(x1) - a, b, length(x0) - b, prevalence, z)
  ord <- order(1 - spec, sens)
  roc <- data.frame(cutoff = cuts, sensitivity = sens, specificity = spec)
  c(dx, list(direction = direction, sign = sign, cuts = cuts, sens = sens, spec = spec,
             youden = youden, cutoff = cutoff, prespecified = prespecified,
             prev = prevalence, sample_prev = sample_prev, auc = auc, z = z, level = level,
             table = table, counts = c(tp = a, fn = length(x1) - a, fp = b, tn = length(x0) - b),
             roc = roc[rev(seq_len(nrow(roc))), ]))
}

dx_ink <- list(absent = "#22928F", present = "#C0603F", ppv = "#7A5AA6", npv = "#22928F",
               found = "#C0603F", false_alarm = "#C88A1E", cleared = "#D6DCDC")

dx_dims <- function() {
  utils::modifyList(graph_dims, list(
    panel_h = 190, dist_w = 290, roc_w = 190, pv_w = 190, gap = 46,
    grid_cols = 100, grid_rows = 10, cell_gap = 1.4,
    text_pt = 10, small_pt = 9, value_pt = 13
  ))
}

dx_fmt <- function(v, digits = 2) {
  ifelse(is.na(v), "", formatC(v, format = "f", digits = digits))
}

# Percentages near 0 or 100 keep a decimal, so 99.4% does not read as 99%.
dx_pct <- function(v) {
  ifelse(is.na(v), "", paste0(formatC(100 * v, format = "f",
                                      digits = ifelse(!is.na(v) & (v > 0.95 | v < 0.05), 1, 0)), "%"))
}

# A prevalence keeps two significant digits below 10%.
dx_prev_fmt <- function(v) {
  if (v >= 0.1) return(dx_pct(v))
  paste0(formatC(100 * v, format = "fg", digits = 2, flag = "#"), "%")
}

# What goes in the eight cells of the table, for R and for the widget.
dx_cells <- function(dx) {
  t <- dx$table
  row <- function(m) t[t$measure == m, ]
  lead <- paste0(format(dx$level * 100), "% CI ")
  ci <- function(r, f) if (is.na(r$lower)) "interval not estimable" else
    paste0(lead, f(r$lower), " to ", f(r$upper))
  c1 <- dx$counts
  n1 <- c1[["tp"]] + c1[["fn"]]
  n0 <- c1[["fp"]] + c1[["tn"]]
  list(
    list(k = "Sensitivity", v = dx_pct(row("sensitivity")$estimate),
         u = paste0(c1[["tp"]], " of ", n1, "; ", ci(row("sensitivity"), dx_pct))),
    list(k = "Specificity", v = dx_pct(row("specificity")$estimate),
         u = paste0(c1[["tn"]], " of ", n0, "; ", ci(row("specificity"), dx_pct))),
    list(k = "Positive predictive value", v = dx_pct(row("ppv")$estimate), u = ci(row("ppv"), dx_pct)),
    list(k = "Negative predictive value", v = dx_pct(row("npv")$estimate), u = ci(row("npv"), dx_pct)),
    list(k = "Positive likelihood ratio", v = dx_fmt(row("lr_positive")$estimate), u = ci(row("lr_positive"), dx_fmt)),
    list(k = "Negative likelihood ratio", v = dx_fmt(row("lr_negative")$estimate), u = ci(row("lr_negative"), dx_fmt)),
    list(k = "Area under the curve", v = dx_fmt(dx$auc$auc),
         u = paste0(lead, dx_fmt(dx$auc$lower), " to ", dx_fmt(dx$auc$upper), "; over all cutoffs")),
    list(k = "Cutoff", v = paste(if (dx$sign > 0) "\u2265" else "\u2264", format(dx$cutoff)),
         u = if (dx$prespecified) "prespecified" else "chosen in these data (Youden)")
  )
}

# People in a grid of 1,000 at the set prevalence.
dx_people <- function(se, sp, prev) {
  nd <- round(1000 * prev)
  tp <- round(nd * se)
  fp <- round((1000 - nd) * (1 - sp))
  c(found = tp, missed = nd - tp, false_alarm = fp, cleared = 1000 - nd - fp)
}

dx_grid_head <- function(prev, sample_prev) {
  paste0("1,000 people tested at a prevalence of ", dx_prev_fmt(prev),
         if (abs(prev - sample_prev) < 1e-9) ", as in these data" else "")
}

dx_sentence <- function(dx) {
  t <- dx$table
  p <- dx_people(t$estimate[1], t$estimate[2], dx$prev)
  ppv <- t$estimate[t$measure == "ppv"]
  paste0("Of 1,000 people tested where ", p[["found"]] + p[["missed"]], " have ",
         tolower(dx$labels[2]), ", a cutoff of ", if (dx$sign > 0) "\u2265 " else "\u2264 ",
         format(dx$cutoff), " finds ", p[["found"]], " and misses ", p[["missed"]],
         ", and raises ", p[["false_alarm"]], " false alarms. A positive result means ",
         tolower(dx$labels[2]), " ", dx_pct(ppv), " of the time.")
}

dx_layout <- function(dx, title, caption, family) {
  dims <- dx_dims()
  h <- dims$panel_h
  # Panels, left to right, in top down points; the grid and table below.
  a0 <- 34
  a1 <- a0 + dims$dist_w
  b0 <- a1 + dims$gap
  b1 <- b0 + dims$roc_w
  c0 <- b1 + dims$gap
  c1 <- c0 + dims$pv_w
  top <- 20
  bottom <- top + h
  right <- c1 + 30
  grid_top <- bottom + 70
  grid_x0 <- a0
  dims$cell <- (right - grid_x0) / dims$grid_cols
  grid_bottom <- grid_top + dims$grid_rows * dims$cell
  table_top <- grid_bottom + 40
  table_bottom <- table_top + 2 * 46

  # The marker axis, padded past the data.
  xs <- c(dx$x1, dx$x0)
  span <- diff(range(xs))
  lo <- min(xs) - 0.04 * span
  hi <- max(xs) + 0.04 * span
  breaks <- scales::breaks_extended(5)(c(lo, hi))
  breaks <- breaks[breaks >= lo & breaks <= hi]
  X <- function(v) a0 + (v - lo) / (hi - lo) * (a1 - a0)
  grid_x <- seq(lo, hi, length.out = 181)
  dens <- function(v) {
    d <- stats::density(v, from = lo, to = hi, n = 181)
    d$y
  }
  d0 <- dens(dx$x0)
  d1 <- dens(dx$x1)
  ymax <- max(d0, d1) * 1.12
  Yd <- function(v) bottom - v / ymax * h
  R <- function(v) b0 + v * (b1 - b0)
  Ry <- function(v) bottom - v * h
  plo <- 0.001
  phi <- 0.999
  P <- function(v) c0 + (log(v) - log(plo)) / (log(phi) - log(plo)) * (c1 - c0)

  cap <- c(caption, wrap_words(paste0(
    if (dx$prespecified) paste0("The cutoff of ", format(dx$cutoff), " was prespecified. ") else
      "The cutoff maximizes Youden's index in these data, so it will do less well in new patients. ",
    "Sensitivity and specificity come from these data; predictive values also depend on the ",
    "prevalence. ", format(dx$level * 100), "% intervals. A test cutoff is not a treatment threshold.",
    if (dx$dropped) paste0(" ", dx$dropped, " people without a marker or an outcome were left out.") else ""
  ), right - 20, dims$caption_pt, family))
  page <- graph_canvas(c(0, right), c(0, table_bottom), title, cap, character(0),
                       character(0), NULL, dims, family)
  list(dx = dx, dims = dims, page = page, family = family,
       a0 = a0, a1 = a1, b0 = b0, b1 = b1, c0 = c0, c1 = c1, top = top, bottom = bottom,
       grid_top = grid_top, grid_x0 = grid_x0, grid_bottom = grid_bottom,
       table_top = table_top, right = right,
       lo = lo, hi = hi, breaks = breaks, X = X, grid_x = grid_x, d0 = d0, d1 = d1,
       ymax = ymax, Yd = Yd, R = R, Ry = Ry, P = P, plo = plo, phi = phi)
}

dx_draw <- function(lay) {
  dx <- lay$dx
  dims <- lay$dims
  px <- lay$page$px
  py <- lay$page$py
  X <- lay$X
  texts <- function(label, x, y, pt, color, hjust = 0, face = "plain") {
    if (!length(label)) return(empty_texts())
    data.frame(x = px(x), y = py(y), label = as.character(label), size = pt / .pt,
               colour = color, hjust = hjust, fontface = face, stringsAsFactors = FALSE)
  }
  seg <- function(x, xend, y, yend) data.frame(x = px(x), xend = px(xend), y = py(y), yend = py(yend))
  positive <- if (dx$sign > 0) lay$grid_x >= dx$cutoff else lay$grid_x <= dx$cutoff
  area <- function(d, keep, id, fill, alpha) {
    xs <- lay$grid_x[keep]
    ys <- d[keep]
    if (length(xs) < 2) return(NULL)
    data.frame(x = px(X(c(xs, rev(xs)))), y = py(c(lay$Yd(ys), rep(lay$bottom, length(xs)))),
               id = id, fill = fill, alpha = alpha, stringsAsFactors = FALSE)
  }
  areas <- rbind(
    area(lay$d0, TRUE, "dd0", dx_ink$absent, 0.12),
    area(lay$d1, TRUE, "dd1", dx_ink$present, 0.12),
    area(lay$d0, positive, "dt0", dx_ink$absent, 0.4),
    area(lay$d1, positive, "dt1", dx_ink$present, 0.45)
  )
  curves <- rbind(
    data.frame(x = px(X(lay$grid_x)), y = py(lay$Yd(lay$d0)), id = "dc0", colour = dx_ink$absent),
    data.frame(x = px(X(lay$grid_x)), y = py(lay$Yd(lay$d1)), id = "dc1", colour = dx_ink$present)
  )
  t <- dx$table
  se <- t$estimate[1]
  sp <- t$estimate[2]
  R <- lay$R
  Ry <- lay$Ry
  roc <- dx$roc
  roc_df <- data.frame(x = px(R(c(0, 1 - roc$specificity, 1))), y = py(Ry(c(0, roc$sensitivity, 1))))
  roc_df <- roc_df[order(roc_df$x, roc_df$y), ]
  ci_se <- t[1, c("lower", "upper")]
  ci_sp <- t[2, c("lower", "upper")]
  P <- lay$P
  prev_grid <- exp(seq(log(lay$plo), log(lay$phi), length.out = 120))
  ppv_c <- se * prev_grid / (se * prev_grid + (1 - sp) * (1 - prev_grid))
  npv_c <- sp * (1 - prev_grid) / (sp * (1 - prev_grid) + (1 - se) * prev_grid)
  pv <- rbind(
    data.frame(x = px(P(prev_grid)), y = py(Ry(ppv_c)), id = "pc1", colour = dx_ink$ppv),
    data.frame(x = px(P(prev_grid)), y = py(Ry(npv_c)), id = "pc2", colour = dx_ink$npv)
  )
  ppv <- t$estimate[3]
  npv <- t$estimate[4]
  pts <- data.frame(x = px(c(R(1 - sp), P(dx$prev), P(dx$prev))),
                    y = py(c(Ry(se), Ry(ppv), Ry(npv))), id = c("rp", "pp", "pn"),
                    fill = c(dx_ink$present, dx_ink$ppv, dx_ink$npv), stringsAsFactors = FALSE)
  marks <- rbind(
    data.frame(seg(X(dx$cutoff), X(dx$cutoff), lay$top - 4, lay$bottom), id = "dcut", colour = graph_ink$title),
    data.frame(seg(R(1 - sp), R(1 - sp), Ry(ci_se$lower), Ry(ci_se$upper)), id = "rpy", colour = dx_ink$present),
    data.frame(seg(R(1 - ci_sp$upper), R(1 - ci_sp$lower), Ry(se), Ry(se)), id = "rpx", colour = dx_ink$present),
    data.frame(seg(P(dx$prev), P(dx$prev), lay$top, lay$bottom), id = "pl", colour = graph_ink$muted)
  )
  frames <- rbind(
    seg(lay$a0, lay$a1, lay$bottom, lay$bottom),
    seg(X(lay$breaks), X(lay$breaks), lay$bottom, lay$bottom + 4),
    seg(R(0), R(1), lay$bottom, lay$bottom), seg(R(0), R(0), lay$top, lay$bottom),
    seg(R(c(0, 0.5, 1)), R(c(0, 0.5, 1)), lay$bottom, lay$bottom + 4),
    seg(R(0) - 4, R(0), Ry(c(0, 0.5, 1)), Ry(c(0, 0.5, 1))),
    seg(P(lay$plo), P(lay$phi), lay$bottom, lay$bottom), seg(P(lay$plo), P(lay$plo), lay$top, lay$bottom),
    seg(P(c(0.001, 0.01, 0.1, 0.5)), P(c(0.001, 0.01, 0.1, 0.5)), lay$bottom, lay$bottom + 4),
    seg(P(lay$plo) - 4, P(lay$plo), Ry(c(0, 0.5, 1)), Ry(c(0, 0.5, 1)))
  )
  grids <- rbind(
    seg(R(0.5), R(0.5), lay$top, lay$bottom), seg(R(0), R(1), Ry(0.5), Ry(0.5)),
    seg(P(lay$plo), P(lay$phi), Ry(0.5), Ry(0.5))
  )
  op <- if (dx$sign > 0) "\u2265" else "\u2264"
  labels <- rbind(
    texts(dx$marker_label, (lay$a0 + lay$a1) / 2, lay$bottom + 28, dims$text_pt, graph_ink$text, hjust = 0.5),
    texts(format(lay$breaks, trim = TRUE), X(lay$breaks), rep(lay$bottom + 13, length(lay$breaks)),
          dims$small_pt, graph_ink$muted, hjust = 0.5),
    texts(dx$labels, lay$a1 - 12, lay$top + c(4, 18), dims$small_pt, c(dx_ink$absent, dx_ink$present),
          hjust = 1, face = "bold"),
    texts("Distribution of the marker", lay$a0, lay$top - 12, dims$text_pt, graph_ink$title, face = "bold"),
    texts("ROC curve", R(0), lay$top - 12, dims$text_pt, graph_ink$title, face = "bold"),
    texts(paste0("AUC ", dx_fmt(dx$auc$auc), " (", dx_fmt(dx$auc$lower), " to ", dx_fmt(dx$auc$upper), ")"),
          R(1), Ry(0.08), dims$small_pt, graph_ink$text, hjust = 1),
    texts(c("0", "0.5", "1"), R(c(0, 0.5, 1)), rep(lay$bottom + 13, 3), dims$small_pt, graph_ink$muted, hjust = 0.5),
    texts(c("0", "0.5", "1"), R(0) - 7, Ry(c(0, 0.5, 1)), dims$small_pt, graph_ink$muted, hjust = 1),
    texts("1 \u2212 specificity", (R(0) + R(1)) / 2, lay$bottom + 28, dims$text_pt, graph_ink$text, hjust = 0.5),
    texts("Predictive values by prevalence", P(lay$plo), lay$top - 12, dims$text_pt, graph_ink$title, face = "bold"),
    texts(c("0.1%", "1%", "10%", "50%"), P(c(0.001, 0.01, 0.1, 0.5)), rep(lay$bottom + 13, 4),
          dims$small_pt, graph_ink$muted, hjust = 0.5),
    texts(c("0", "0.5", "1"), P(lay$plo) - 7, Ry(c(0, 0.5, 1)), dims$small_pt, graph_ink$muted, hjust = 1),
    texts("Prevalence (log scale)", (lay$c0 + lay$c1) / 2, lay$bottom + 28, dims$text_pt, graph_ink$text, hjust = 0.5),
    texts(c("PPV", "NPV"), lay$c1 + 4, Ry(c(ppv_c[120], npv_c[120])) + c(-6, 6), dims$small_pt,
          c(dx_ink$ppv, dx_ink$npv), face = "bold")
  )
  # Values that the widget rewrites carry ids.
  live <- rbind(
    data.frame(texts(paste(op, format(dx$cutoff)), X(dx$cutoff) + 4, lay$top + 4, dims$small_pt, graph_ink$title,
                     face = "bold"), id = "dcl"),
    data.frame(texts(paste0("Sensitivity ", dx_pct(se), ", specificity ", dx_pct(sp)), R(1), Ry(0.18),
                     dims$small_pt, graph_ink$text, hjust = 1), id = "rpl"),
    data.frame(texts(paste0("Prevalence ", dx_prev_fmt(dx$prev)), P(dx$prev) + 4, lay$bottom - 8, dims$small_pt,
                     graph_ink$title, face = "bold"), id = "plt")
  )
  # The grid of 1,000 people: found, missed, false alarms, cleared.
  people <- dx_people(se, sp, dx$prev)
  k <- seq_len(1000) - 1
  kind <- rep(c("found", "missed", "false_alarm", "cleared"), people)
  fill <- unname(c(found = dx_ink$found, missed = dx_ink$found, false_alarm = dx_ink$false_alarm,
                   cleared = dx_ink$cleared)[kind])
  cells <- data.frame(
    xmin = px(lay$grid_x0 + (k %% dims$grid_cols) * dims$cell),
    xmax = px(lay$grid_x0 + (k %% dims$grid_cols) * dims$cell + dims$cell - dims$cell_gap),
    ymin = py(lay$grid_top + (k %/% dims$grid_cols) * dims$cell + dims$cell - dims$cell_gap),
    ymax = py(lay$grid_top + (k %/% dims$grid_cols) * dims$cell),
    fill = fill, alpha = ifelse(kind == "missed", 0.3, 1), stringsAsFactors = FALSE
  )
  key <- data.frame(label = c("Found", "Missed", "False alarm", "Correctly cleared"),
                    fill = c(dx_ink$found, dx_ink$found, dx_ink$false_alarm, dx_ink$cleared),
                    alpha = c(1, 0.3, 1, 1), stringsAsFactors = FALSE)
  key$x <- lay$grid_x0 + c(0, 70, 142, 236)
  key_rects <- rbind(
    data.frame(xmin = px(key$x), xmax = px(key$x + 10), ymin = py(lay$grid_bottom + 18),
               ymax = py(lay$grid_bottom + 8), fill = key$fill, alpha = key$alpha),
    data.frame(xmin = px(lay$a1 - 8), xmax = px(lay$a1), ymin = py(lay$top + c(8, 22)),
               ymax = py(lay$top + c(0, 14)), fill = c(dx_ink$absent, dx_ink$present), alpha = 0.45)
  )
  labels <- rbind(labels, texts(key$label, key$x + 14, lay$grid_bottom + 13, dims$small_pt, graph_ink$text))
  live <- rbind(live, data.frame(texts(dx_grid_head(dx$prev, dx$sample_prev), lay$grid_x0, lay$grid_top - 28,
                                       dims$text_pt, graph_ink$title, face = "bold"), id = "gh"))
  live <- rbind(live, data.frame(texts(dx_sentence(dx), lay$grid_x0, lay$grid_top - 12, dims$small_pt,
                                       graph_ink$text), id = "gl"))
  # The table of measures.
  cells_txt <- dx_cells(dx)
  col_w <- (lay$right - lay$a0) / 4
  for (i in seq_along(cells_txt)) {
    cx <- lay$a0 + ((i - 1) %% 4) * col_w
    cy <- lay$table_top + ((i - 1) %/% 4) * 46
    labels <- rbind(labels, texts(toupper(cells_txt[[i]]$k), cx, cy, dims$small_pt - 1.5, graph_ink$muted, face = "bold"))
    live <- rbind(live,
      data.frame(texts(cells_txt[[i]]$v, cx, cy + 15, dims$value_pt, graph_ink$title, face = "bold"), id = paste0("sv", i)),
      data.frame(texts(cells_txt[[i]]$u, cx, cy + 30, dims$small_pt - 0.5, graph_ink$muted), id = paste0("su", i)))
  }

  ggplot() +
    geom_segment(data = grids, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
                 colour = graph_ink$faint, linewidth = 0.6 / .pt) +
    geom_segment(data = seg(R(0), R(1), Ry(0), Ry(1)), aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
                 colour = graph_ink$plain_border, linewidth = 0.6 / .pt, linetype = "22") +
    ggiraph::geom_polygon_interactive(
      data = areas, aes(x = .data$x, y = .data$y, group = .data$id, fill = .data$fill,
                        alpha = .data$alpha, data_id = .data$id), colour = NA) +
    ggiraph::geom_path_interactive(
      data = curves, aes(x = .data$x, y = .data$y, group = .data$id, colour = .data$colour,
                         data_id = .data$id), linewidth = 1 / .pt) +
    geom_path(data = roc_df, aes(x = .data$x, y = .data$y), colour = graph_ink$title, linewidth = 1.2 / .pt) +
    ggiraph::geom_path_interactive(
      data = pv, aes(x = .data$x, y = .data$y, group = .data$id, colour = .data$colour,
                     data_id = .data$id), linewidth = 1.2 / .pt) +
    geom_segment(data = frames, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
                 colour = graph_ink$text, linewidth = 0.8 / .pt) +
    ggiraph::geom_segment_interactive(
      data = marks, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend,
                        colour = .data$colour, data_id = .data$id), linewidth = 1.2 / .pt) +
    ggiraph::geom_point_interactive(
      data = pts, aes(x = .data$x, y = .data$y, fill = .data$fill, data_id = .data$id),
      shape = 21, size = 2.6, colour = graph_ink$page, stroke = 0.8) +
    ggiraph::geom_rect_interactive(
      data = cells, aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, ymax = .data$ymax,
                        fill = .data$fill, alpha = .data$alpha, data_id = "grid"), colour = NA) +
    geom_rect(data = key_rects, aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin,
                                    ymax = .data$ymax, fill = .data$fill, alpha = .data$alpha)) +
    draw_text(labels, lay$family) +
    ggiraph::geom_text_interactive(
      data = live, aes(x = .data$x, y = .data$y, label = .data$label, data_id = .data$id),
      hjust = live$hjust, size = live$size, colour = live$colour, fontface = live$fontface,
      family = lay$family) +
    graph_frame(lay$page, lay$family)
}

# Everything the widget needs to recompute the plot at any cutoff and
# prevalence, in the widget's own units.
dx_render <- function(lay) {
  dx <- lay$dx
  page <- lay$page
  list(
    x1 = dx$x1, x0 = dx$x0, cuts = dx$cuts, sign = dx$sign, cutoff = dx$cutoff,
    youden = dx$youden, prespecified = dx$prespecified, prev = dx$prev,
    sample_prev = dx$sample_prev, z = dx$z, level = dx$level, labels = dx$labels, marker = dx$marker_label,
    auc = dx$auc[c("auc", "lower", "upper")],
    dx = page$px(0), dy = page$height - page$py(0),
    a0 = lay$a0, a1 = lay$a1, b0 = lay$b0, b1 = lay$b1, c0 = lay$c0, c1 = lay$c1,
    top = lay$top, bottom = lay$bottom, lo = lay$lo, hi = lay$hi,
    grid_x = lay$grid_x, d0 = lay$d0, d1 = lay$d1, ymax = lay$ymax,
    plo = lay$plo, phi = lay$phi,
    grid_x0 = lay$grid_x0, grid_top = lay$grid_top, cols = lay$dims$grid_cols,
    cell = lay$dims$cell, cell_gap = lay$dims$cell_gap, ink = dx_ink
  )
}
