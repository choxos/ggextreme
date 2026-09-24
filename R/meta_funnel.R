#' Draw an interactive funnel plot for a meta-analysis
#'
#' Draws each study's effect against its standard error, with the most
#' precise studies at the top, to show small-study effects. The shaded
#' contours mark where a study would be statistically significant against
#' no effect (Peters et al. 2008), so a gap in the unshaded area, where
#' studies would not be significant, points to publication bias rather than
#' heterogeneity alone. The solid line is the pooled estimate and the dashed
#' lines around it the region where 95% of studies would fall without
#' heterogeneity or bias.
#'
#' Hovering over a study shows its effect, weight, risk of bias, the columns
#' named in `hover` and the significance zone it falls in. Clicking it opens
#' the pooled estimate with that study left out, beside its full record.
#'
#' With `tests = TRUE`, a section under the plot, collapsed until the reader
#' opens it, gives Egger's regression test (Egger et al. 1997), Begg's rank
#' correlation test (Begg and Mazumdar 1994) and, with `trim_fill = TRUE`,
#' the trim and fill estimate (Duval and Tweedie 2000), as computed by
#' 'metafor' or 'meta', with a note on what each asks. Egger's test is the
#' classical one, `metafor::regtest(model = "lm")`, which 'meta' also
#' computes; metafor's own default, `regtest()` with `model = "rma"`, gives a
#' different p value. The two packages' trim and fill estimators can also
#' impute different numbers of studies from the same data. The table is also
#' returned as the field `tests`. These tests have little power with fewer
#' than ten studies, and asymmetry can come from heterogeneity, chance or the
#' quality of small studies as well as from publication bias.
#'
#' With `trim_fill = TRUE`, the studies trim and fill imputes are drawn as
#' hollow circles and the adjusted estimate as a dashed line, and the widget
#' gets a switch that hides them.
#'
#' @inheritParams ggmeta
#' @param rob Name of the column of `data` holding each study's overall risk
#'   of bias judgement, which colors its point. Judgements are matched by
#'   their wording, as in [ggmeta()].
#' @param contours Significance levels for the shaded contours, against no
#'   effect. `NULL` draws none.
#' @param trim_fill Add the studies imputed by trim and fill and the adjusted
#'   estimate.
#' @param tests Add the tests for small-study effects, in a collapsed section
#'   under the plot.
#' @param xlim Optional limits of the effect axis, on the scale shown.
#'
#' @return An object of class `ggfunnel`, which prints as an interactive
#'   widget. Use [graph_widget()], [graph_plot()] or [graph_save()] for the
#'   widget, a static ggplot or a file. The field `tests` holds the tests as
#'   a data frame.
#' @export
#'
#' @examples
#' if (requireNamespace("metafor", quietly = TRUE)) {
#'   dat <- metafor::escalc(measure = "RR", ai = tpos, bi = tneg,
#'                          ci = cpos, di = cneg, data = metadat::dat.bcg,
#'                          slab = paste(author, year))
#'   fit <- metafor::rma(yi, vi, data = dat)
#'   f <- ggfunnel(fit, hover = "alloc", trim_fill = TRUE)
#'   f
#'   f$tests
#' }
ggfunnel <- function(x, data = NULL, rob = NULL, hover = NULL,
                     contours = c(0.1, 0.05, 0.01),
                     trim_fill = FALSE,
                     tests = TRUE,
                     exponentiate = NULL,
                     xlim = NULL,
                     xlab = NULL,
                     title = NULL,
                     caption = NULL,
                     family = "Lato") {
  if (length(rob) > 1) rlang::abort("`rob` must name one column, the overall judgement.")
  if (!is.null(contours) && (!is.numeric(contours) || any(!(contours > 0 & contours < 1)))) {
    rlang::abort("`contours` must be significance levels between 0 and 1.")
  }
  input <- meta_prepare(x, data, NULL, rob, hover, exponentiate, xlim, NULL, xlab,
                        title, caption, family)
  if (length(input$m$yi) < 3) rlang::abort("A funnel plot needs at least three studies.")
  extra <- funnel_extras(x, input, isTRUE(trim_fill), isTRUE(tests))
  lay <- funnel_layout(input, extra, sort(unique(contours), decreasing = TRUE))
  on_render <- paste(c(
    if (!is.null(extra$trim)) "ggextremeFunnel(el);",
    if (!is.null(lay$tests_html)) paste0("ggextremeDetails(el, ", js_string("Small-study effects"),
                                         ", ", js_string(lay$tests_html), ");")
  ), collapse = " ")
  structure(
    list(plot = funnel_draw(lay), width = lay$page$width / 72,
         height = lay$page$height / 72, title = title,
         on_render = if (nzchar(on_render)) on_render, layout = lay,
         tests = extra$tests),
    class = c("ggfunnel", "ggx_graph")
  )
}

funnel_dims <- utils::modifyList(graph_dims, list(
  plot_w = 440,
  plot_h = 300,
  text_pt = 10,
  small_pt = 9,
  r_min = 3,
  r_max = 8.5,
  key_pt = 8.5
))

funnel_notes <- c(
  "Egger's regression test" = paste(
    "Weighted regression of the effects on their standard errors (Egger et al. 1997).",
    "A slope away from zero means smaller studies report different effects from larger ones."),
  "Begg's rank correlation" = paste(
    "Rank correlation between the standardized effects and their variances",
    "(Begg and Mazumdar 1994)."),
  "Trim and fill" = paste(
    "Imputes the studies the asymmetry suggests are missing, mirrored about the pooled",
    "estimate, and pools again (Duval and Tweedie 2000). It is a sensitivity analysis,",
    "not a correction."),
  "Reading the tests" = paste(
    "Asymmetry can come from heterogeneity, chance or the quality of small studies as",
    "well as from publication bias, and the tests have little power with fewer than",
    "ten studies.")
)

# Leave one out estimates, trim and fill and the tests, from whichever
# package fitted the model.
funnel_extras <- function(x, input, trim_fill, tests) {
  m <- input$m
  k <- length(m$yi)
  fmt <- effect_format(input$exponentiate)
  safe <- function(expr) tryCatch(expr, error = function(e) NULL)
  out <- list()
  if (inherits(x, "rma.uni")) {
    loo <- metafor::leave1out(x)
    out$loo <- data.frame(est = loo$estimate, lo = loo$ci.lb, hi = loo$ci.ub, i2 = loo$I2)
    if (trim_fill) {
      tf <- metafor::trimfill(x)
      fill <- as.logical(tf$fill)
      out$trim <- list(yi = as.numeric(tf$yi[fill]), se = sqrt(as.numeric(tf$vi[fill])),
                       k0 = as.integer(tf$k0), side = tf$side, est = as.numeric(tf$b[1]),
                       lo = as.numeric(tf$ci.lb), hi = as.numeric(tf$ci.ub))
    }
    if (tests) {
      eg <- safe(metafor::regtest(x, model = "lm"))
      rk <- safe(metafor::ranktest(x))
      egger <- if (!is.null(eg)) list(stat = sprintf("t = %.2f, %d df", eg$zval, as.integer(eg$dfs)), p = eg$pval)
      begg <- if (!is.null(rk)) list(stat = sprintf("Kendall's \u03c4 = %.2f", as.numeric(rk$tau)), p = rk$pval)
    }
  } else {
    random <- isTRUE(x$random)
    inf <- meta::metainf(x, pooled = if (random) "random" else "common")
    out$loo <- data.frame(est = inf$TE[seq_len(k)], lo = inf$lower[seq_len(k)],
                          hi = inf$upper[seq_len(k)], i2 = 100 * inf$I2[seq_len(k)])
    if (trim_fill) {
      tf <- meta::trimfill(x)
      fill <- as.logical(tf$trimfill)
      pick <- function(r, c) as.numeric(if (random) r else c)[1]
      out$trim <- list(yi = tf$TE[fill], se = tf$seTE[fill], k0 = as.integer(tf$k0),
                       side = if (isTRUE(tf$left)) "left" else "right",
                       est = pick(tf$TE.random, tf$TE.common),
                       lo = pick(tf$lower.random, tf$lower.common),
                       hi = pick(tf$upper.random, tf$upper.common))
    }
    if (tests) {
      eg <- safe(meta::metabias(x, method.bias = "Egger", k.min = 3))
      rk <- safe(meta::metabias(x, method.bias = "Begg", k.min = 3))
      egger <- if (!is.null(eg)) list(stat = sprintf("t = %.2f, %d df", as.numeric(eg$statistic), as.integer(eg$df)),
                                      p = as.numeric(eg$p.value))
      begg <- if (!is.null(rk)) list(stat = sprintf("z = %.2f", as.numeric(rk$statistic)),
                                     p = as.numeric(rk$p.value))
    }
  }
  if (tests) {
    rows <- list(
      if (!is.null(egger)) data.frame(test = "Egger's regression test", statistic = egger$stat, p = egger$p),
      if (!is.null(begg)) data.frame(test = "Begg's rank correlation", statistic = begg$stat, p = begg$p),
      if (!is.null(out$trim)) data.frame(
        test = "Trim and fill",
        statistic = paste0(out$trim$k0, if (out$trim$k0 == 1) " study" else " studies",
                           " imputed on the ", out$trim$side, "; ", input$xlab, " ",
                           fmt(out$trim$est), " (", fmt(out$trim$lo), ", ", fmt(out$trim$hi), ")"),
        p = NA_real_)
    )
    out$tests <- do.call(rbind, rows)
    if (!is.null(out$tests)) rownames(out$tests) <- NULL
  }
  out
}

# One band of a contour: where a study's z against no effect lies between
# `z1` and `z2` on one side, clipped to the axis. The bands are bounded by
# lines through the apex, so a clip is exact once the points where those
# lines cross the axis limits are added.
funnel_band <- function(z1, z2, side, span, smax) {
  ss <- c(0, smax)
  for (z in c(z1, z2)) {
    if (!is.finite(z)) next
    s <- span / (side * z)
    ss <- c(ss, s[s > 0 & s < smax])
  }
  ss <- sort(unique(ss))
  inner <- side * z1 * ss
  outer <- if (is.finite(z2)) side * z2 * ss else rep(if (side > 0) span[2] else span[1], length(ss))
  list(x = pmin(pmax(c(inner, rev(outer)), span[1]), span[2]), s = c(ss, rev(ss)))
}

funnel_layout <- function(input, extra, contours) {
  dims <- funnel_dims
  m <- input$m
  family <- input$family
  k <- length(m$yi)
  fmt <- effect_format(input$exponentiate)
  z95 <- stats::qnorm(0.975)
  interval <- function(e, s) paste0(fmt(e), " (", fmt(e - z95 * s), ", ", fmt(e + z95 * s), ")")
  width <- function(s, pt = dims$small_pt, bold = FALSE) {
    if (!length(s)) return(0)
    max(text_width_card(as.character(s), pt, family, 1, bold = bold))
  }
  p <- m$pooled
  trim <- extra$trim
  smax <- max(c(m$se, trim$se)) * 1.05
  span <- if (is.null(input$xlim)) {
    r <- range(c(m$yi, trim$yi, p$est + c(-1, 1) * z95 * smax, 0), na.rm = TRUE)
    r + c(-1, 1) * 0.04 * diff(r)
  } else if (input$exponentiate) log(sort(input$xlim)) else sort(input$xlim)
  ticks <- effect_ticks(span, input$exponentiate)
  se_ticks <- scales::breaks_extended(5)(c(0, smax))
  se_ticks <- se_ticks[se_ticks >= 0 & se_ticks <= smax]
  se_labels <- format(se_ticks, trim = TRUE, drop0trailing = TRUE)

  x0 <- width(se_labels) + 8
  x1 <- x0 + dims$plot_w
  top <- 18
  bottom <- top + dims$plot_h
  X <- function(v) x0 + (v - span[1]) / diff(span) * dims$plot_w
  Y <- function(s) top + s / smax * dims$plot_h

  # Contours, lightest band first, mirrored about no effect.
  bands <- NULL
  key <- NULL
  if (length(contours)) {
    zs <- stats::qnorm(1 - contours / 2)
    nb <- length(zs)
    alpha <- if (nb == 1) 0.24 else seq(0.08, 0.26, length.out = nb)
    pct <- function(v) formatC(v, format = "fg", digits = 2)
    labels <- c(if (nb > 1) paste(pct(contours[-1]), "to", pct(contours[-nb])),
                paste("p <", pct(contours[nb])))
    for (j in seq_len(nb)) {
      for (side in c(-1, 1)) {
        b <- funnel_band(zs[j], if (j < nb) zs[j + 1] else Inf, side, span, smax)
        bands <- rbind(bands, data.frame(x = X(b$x), y = Y(b$s), group = paste(j, side),
                                         alpha = alpha[j], stringsAsFactors = FALSE))
      }
    }
    key <- list(labels = labels, alpha = alpha)
  }

  # The pooled estimate's pseudo 95% limits, cut at the axis limits.
  limit <- function(est, dir) {
    edge <- if (dir > 0) span[2] else span[1]
    end <- est + dir * z95 * smax
    s <- if ((dir > 0 && end > edge) || (dir < 0 && end < edge)) (edge - est) / (dir * z95) else smax
    c(x = X(est + dir * z95 * s), y = Y(s))
  }
  inside <- function(v) v >= span[1] && v <= span[2]

  # Studies.
  ids <- paste0("s", seq_len(k))
  data <- input$data
  rob <- if (length(input$rob)) as.character(data[[input$rob]]) else rep(NA_character_, k)
  rob_cls <- rob_class(rob)
  colors <- rep(if (length(input$rob)) graph_ink$plain_border else "#22928F", k)
  judged <- !is.na(rob_cls)
  colors[judged] <- vapply(rob_cls[judged], function(c) rob_classes[[c]]$color, character(1))
  radius <- dims$r_min + (dims$r_max - dims$r_min) * sqrt(m$weight / max(m$weight))
  zone <- function(y, s) {
    if (!length(contours)) return(NULL)
    pv <- 2 * stats::pnorm(-abs(y / s))
    hit <- which(pv < contours)
    if (!length(hit)) paste0("p \u2265 ", formatC(contours[1], format = "fg", digits = 2))
    else key$labels[max(hit)]
  }
  hover_values <- lapply(seq_len(k), function(i) {
    if (!length(input$hover)) return(character(0))
    v <- vapply(input$hover, function(col) format_values(data[[col]])[i], character(1))
    stats::setNames(ifelse(is.na(v), "", v), input$hover_heads)
  })
  weight_text <- paste0(formatC(m$weight, format = "f", digits = 1), "%")
  tip <- vapply(seq_len(k), function(i) {
    paste0('<div class="ggx-tip-title">', esc(m$label[i]), '</div><div class="ggx-tip-sub">Weight ',
           weight_text[i], "</div>",
           tip_rows(c(stats::setNames(interval(m$yi[i], m$se[i]), paste(input$xlab, "(95% CI)")),
                      "Standard error" = formatC(m$se[i], format = "f", digits = 3),
                      hover_values[[i]],
                      if (length(input$rob)) c("Risk of bias" = if (is.na(rob[i])) "" else rob[i]),
                      if (length(contours)) c("Against no effect" = zone(m$yi[i], m$se[i])))),
           '<div class="ggx-tip-hint">Click for the pooled estimate without it.</div>')
  }, character(1))
  pooled_text <- paste0(fmt(p$est), " (", fmt(p$lo), ", ", fmt(p$hi), ")")
  loo <- extra$loo
  i2_text <- function(v) if (is.finite(v)) paste0(formatC(v, format = "f", digits = 1), "%") else ""
  click <- vapply(seq_len(k), function(i) {
    record <- if (is.null(data)) character(0) else {
      keep <- setdiff(names(data), c("yi", "vi"))
      keep <- keep[!vapply(data[keep], is.list, logical(1))]
      v <- vapply(keep, function(col) format_values(data[[col]])[i], character(1))
      stats::setNames(v, column_labels(data, keep))[!is.na(v)]
    }
    paste0('<div class="ggx-title">', esc(m$label[i]), '</div><div class="ggx-sub">',
           esc(input$xlab), " ", interval(m$yi[i], m$se[i]), "; weight ", weight_text[i], "</div>",
           '<div class="ggx-table"><table><thead><tr><td></td><th scope="col">', esc(m$model),
           ' (95% CI)</th><th scope="col">I\u00b2</th></tr></thead><tbody>',
           '<tr><th scope="row">All ', k, " studies</th><td>", pooled_text, "</td><td>",
           i2_text(p$i2), "</td></tr>",
           '<tr><th scope="row">Without this study</th><td>',
           fmt(loo$est[i]), " (", fmt(loo$lo[i]), ", ", fmt(loo$hi[i]), ")</td><td>",
           i2_text(loo$i2[i]), "</td></tr></tbody></table></div>",
           if (length(record)) paste0('<div class="ggx-refs-head">Record</div><dl>',
                                      paste0("<dt>", esc(names(record)), "</dt><dd>",
                                             vapply(record, link_reference, character(1)),
                                             "</dd>", collapse = ""), "</dl>"))
  }, character(1))
  pooled_tip <- paste0(
    '<div class="ggx-tip-title">', esc(m$model), '</div><div class="ggx-tip-sub">', k,
    " studies</div>",
    tip_rows(c(stats::setNames(pooled_text, paste(input$xlab, "(95% CI)")),
               if (is.finite(p$tau2)) c("\u03c4\u00b2" = formatC(p$tau2, format = "f", digits = 3)),
               if (is.finite(p$i2)) c("I\u00b2" = i2_text(p$i2)))),
    '<div class="ggx-tip-hint">The dashed lines mark where 95% of studies would fall without heterogeneity.</div>'
  )
  trim_tip <- if (!is.null(trim)) {
    list(
      points = vapply(seq_along(trim$yi), function(j) {
        paste0('<div class="ggx-tip-title">Imputed study</div>',
               '<div class="ggx-tip-sub">Added by trim and fill</div>',
               tip_rows(c(stats::setNames(interval(trim$yi[j], trim$se[j]), paste(input$xlab, "(95% CI)")),
                          "Standard error" = formatC(trim$se[j], format = "f", digits = 3))))
      }, character(1)),
      line = paste0('<div class="ggx-tip-title">Trim and fill estimate</div>',
                    '<div class="ggx-tip-sub">', trim$k0, if (trim$k0 == 1) " study" else " studies",
                    " imputed on the ", trim$side, "</div>",
                    tip_rows(stats::setNames(paste0(fmt(trim$est), " (", fmt(trim$lo), ", ",
                                                    fmt(trim$hi), ")"),
                                             paste(input$xlab, "(95% CI)"))))
    )
  }

  # Tests, as the section the widget adds under the plot.
  tests_html <- NULL
  if (!is.null(extra$tests)) {
    tt <- extra$tests
    notes <- c(tt$test, "Reading the tests")
    tests_html <- paste0(
      '<div class="ggx-table"><table><thead><tr><th scope="col">Test</th>',
      '<th scope="col">Result</th><th scope="col">p</th></tr></thead><tbody>',
      paste0("<tr><th scope=\"row\">", esc(tt$test), "</th><td>", esc(tt$statistic),
             "</td><td>", ifelse(is.na(tt$p), "", fmt_p(tt$p)), "</td></tr>", collapse = ""),
      "</tbody></table></div>",
      '<dl class="ggx-notes-list">',
      paste0("<dt>", esc(notes), "</dt><dd>", esc(funnel_notes[notes]), "</dd>", collapse = ""),
      "</dl>"
    )
  }

  # Risk of bias key, in order of severity.
  key_labels <- character(0)
  key_colors <- character(0)
  if (length(input$rob)) {
    seen <- unique(rob[!is.na(rob)])
    cls <- rob_class(seen)
    o <- order(match(cls, names(rob_classes)))
    key_labels <- capitalize(seen[o])
    key_colors <- vapply(cls[o], function(c) rob_classes[[c]]$color, character(1))
  }
  right <- x1 + 6
  page <- graph_canvas(c(0, right), c(0, bottom + 40), input$title, input$caption,
                       key_labels, unname(key_colors),
                       if (length(key_labels)) "Risk of bias", dims, family)

  list(
    dims = dims, page = page, family = family, X = X, Y = Y, x0 = x0, x1 = x1,
    top = top, bottom = bottom, span = span, smax = smax,
    ticks = unname(ticks), tick_labels = names(ticks), se_ticks = se_ticks,
    se_labels = se_labels, xlab = input$xlab, bands = bands, key = key,
    pooled = list(est = p$est, tip = pooled_tip, left = limit(p$est, -1),
                  right = limit(p$est, 1), shown = inside(p$est)),
    null_shown = inside(0),
    studies = data.frame(x = X(m$yi), y = Y(m$se), r = radius, color = colors, id = ids,
                         tip = tip, click = pin_js(ids, click), stringsAsFactors = FALSE),
    trim = if (!is.null(trim)) list(
      points = data.frame(x = X(trim$yi), y = Y(trim$se), id = paste0("tf", seq_along(trim$yi)),
                          tip = trim_tip$points, stringsAsFactors = FALSE),
      est = trim$est, tip = trim_tip$line, shown = inside(trim$est)
    ),
    tests_html = tests_html
  )
}

funnel_draw <- function(lay) {
  dims <- lay$dims
  px <- lay$page$px
  py <- lay$page$py
  texts <- function(label, x, y, pt, color, hjust = 0, face = "plain") {
    if (!length(label)) return(empty_texts())
    data.frame(x = px(x), y = py(y), label = as.character(label), size = pt / .pt,
               colour = color, hjust = hjust, fontface = face, stringsAsFactors = FALSE)
  }
  segs <- function(x0, x1, y0, y1, colour) {
    data.frame(x = px(x0), xend = px(x1), y = py(y0), yend = py(y1), colour = colour,
               stringsAsFactors = FALSE)
  }
  circle <- function(x, y, r, n = 28) {
    a <- seq(0, 2 * pi, length.out = n + 1)[-1]
    list(x = x + r * cos(a), y = y + r * sin(a))
  }

  grid <- rbind(
    segs(lay$X(lay$ticks), lay$X(lay$ticks), lay$top, lay$bottom, graph_ink$faint),
    segs(lay$x0, lay$x1, lay$Y(lay$se_ticks), lay$Y(lay$se_ticks), graph_ink$faint)
  )
  axis <- rbind(
    segs(lay$x0, lay$x1, lay$bottom, lay$bottom, graph_ink$text),
    segs(lay$X(lay$ticks), lay$X(lay$ticks), lay$bottom, lay$bottom + 4, graph_ink$text),
    if (lay$null_shown) segs(lay$X(0), lay$X(0), lay$top, lay$bottom, graph_ink$muted)
  )
  labels <- rbind(
    texts(lay$tick_labels, lay$X(lay$ticks), rep(lay$bottom + 13, length(lay$ticks)),
          dims$small_pt, graph_ink$muted, hjust = 0.5),
    texts(lay$se_labels, rep(lay$x0 - 6, length(lay$se_ticks)), lay$Y(lay$se_ticks),
          dims$small_pt, graph_ink$muted, hjust = 1),
    texts(lay$xlab, (lay$x0 + lay$x1) / 2, lay$bottom + 29, dims$text_pt, graph_ink$text,
          hjust = 0.5),
    texts("Standard error", 0, lay$top - 11, dims$small_pt, graph_ink$muted),
    if (lay$null_shown) texts("No effect", lay$X(0) + 4, lay$top + 7, dims$key_pt,
                              graph_ink$muted)
  )

  # A key to the contours in whichever top corner covers fewer studies.
  key_box <- NULL
  key_swatch <- NULL
  if (!is.null(lay$key)) {
    n <- length(lay$key$labels)
    row <- 13
    w <- max(text_width_card(lay$key$labels, dims$key_pt, lay$family, 1)) + 26
    by0 <- lay$top + 6
    by1 <- by0 + n * row + 8
    st <- rbind(lay$studies[c("x", "y", "r")],
                if (!is.null(lay$trim)) cbind(lay$trim$points[c("x", "y")], r = dims$r_min + 1.5))
    covered <- function(bx0) {
      sum(st$x + st$r > bx0 & st$x - st$r < bx0 + w & st$y + st$r > by0 & st$y - st$r < by1)
    }
    corners <- c(lay$x1 - 6 - w, lay$x0 + 6)
    bx0 <- corners[which.min(vapply(corners, covered, numeric(1)))]
    bx1 <- bx0 + w
    key_box <- data.frame(xmin = px(bx0), xmax = px(bx1), ymin = py(by1), ymax = py(by0))
    ys <- by0 + 4 + (seq_len(n) - 0.5) * row
    key_swatch <- data.frame(xmin = px(bx0 + 7), xmax = px(bx0 + 16), ymin = py(ys + 4.5),
                             ymax = py(ys - 4.5), alpha = lay$key$alpha)
    labels <- rbind(labels, texts(lay$key$labels, rep(bx0 + 21, n), ys, dims$key_pt,
                                  graph_ink$text))
  }

  pooled <- lay$pooled
  lines <- rbind(
    segs(lay$X(pooled$est), pooled$left[["x"]], lay$top, pooled$left[["y"]], graph_ink$muted),
    segs(lay$X(pooled$est), pooled$right[["x"]], lay$top, pooled$right[["y"]], graph_ink$muted)
  )
  st <- lay$studies
  dots <- do.call(rbind, lapply(seq_len(nrow(st)), function(i) {
    c0 <- circle(st$x[i], st$y[i], st$r[i])
    data.frame(x = px(c0$x), y = py(c0$y), id = st$id[i], tip = st$tip[i],
               click = st$click[i], fill = st$color[i], stringsAsFactors = FALSE)
  }))
  hollow <- NULL
  if (!is.null(lay$trim)) {
    tp <- lay$trim$points
    hollow <- do.call(rbind, lapply(seq_len(nrow(tp)), function(i) {
      c0 <- circle(tp$x[i], tp$y[i], dims$r_min + 1.5)
      data.frame(x = px(c0$x), y = py(c0$y), id = tp$id[i], tip = tp$tip[i],
                 stringsAsFactors = FALSE)
    }))
  }
  brick <- "#C0603F"

  p <- ggplot()
  if (!is.null(lay$bands)) {
    b <- lay$bands
    p <- p + geom_polygon(data = data.frame(x = px(b$x), y = py(b$y), group = b$group, alpha = b$alpha),
                          aes(x = .data$x, y = .data$y, group = .data$group, alpha = .data$alpha),
                          fill = graph_ink$muted)
  }
  p <- p +
    geom_segment(data = grid, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
                 colour = grid$colour, linewidth = 0.6 / .pt) +
    geom_segment(data = axis, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
                 colour = axis$colour, linewidth = 0.8 / .pt) +
    geom_segment(data = lines, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
                 colour = lines$colour, linewidth = 0.9 / .pt, linetype = "22")
  if (pooled$shown) {
    p <- p + ggiraph::geom_segment_interactive(
      data = data.frame(x = px(lay$X(pooled$est)), y = py(lay$top), yend = py(lay$bottom),
                        id = "pool", tip = pooled$tip),
      aes(x = .data$x, xend = .data$x, y = .data$y, yend = .data$yend, data_id = .data$id,
          tooltip = .data$tip),
      colour = graph_ink$title, linewidth = 1.3 / .pt
    )
  }
  if (!is.null(lay$trim) && lay$trim$shown) {
    p <- p + ggiraph::geom_segment_interactive(
      data = data.frame(x = px(lay$X(lay$trim$est)), y = py(lay$top), yend = py(lay$bottom),
                        id = "tfline", tip = lay$trim$tip),
      aes(x = .data$x, xend = .data$x, y = .data$y, yend = .data$yend, data_id = .data$id,
          tooltip = .data$tip),
      colour = brick, linewidth = 1.3 / .pt, linetype = "42"
    )
  }
  if (!is.null(hollow)) {
    p <- p + ggiraph::geom_polygon_interactive(
      data = hollow,
      aes(x = .data$x, y = .data$y, group = .data$id, data_id = .data$id, tooltip = .data$tip),
      fill = graph_ink$page, colour = brick, linewidth = 1.2 / .pt, linetype = "32"
    )
  }
  p <- p + ggiraph::geom_polygon_interactive(
    data = dots,
    aes(x = .data$x, y = .data$y, group = .data$id, fill = .data$fill, data_id = .data$id,
        tooltip = .data$tip, onclick = .data$click),
    alpha = 0.9, colour = graph_ink$page, linewidth = 0.8 / .pt
  )
  if (!is.null(key_box)) {
    p <- p +
      geom_rect(data = key_box, aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin,
                                    ymax = .data$ymax),
                fill = graph_ink$page, colour = graph_ink$ghost, linewidth = 0.6 / .pt) +
      geom_rect(data = key_swatch, aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin,
                                       ymax = .data$ymax, alpha = .data$alpha),
                fill = graph_ink$muted)
  }
  p + draw_text(labels, lay$family) + graph_frame(lay$page, lay$family)
}
