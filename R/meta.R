#' Draw an interactive forest plot for a meta-analysis
#'
#' Draws the forest plot of a fitted meta-analysis, with the data behind
#' every study a hover and a click away. Hovering over a study shows its
#' effect, weight and the columns named in `hover`; clicking it opens its
#' full record, every column of `data`, under the plot. Hovering over the
#' pooled diamond shows the heterogeneity statistics and the prediction
#' interval. Risk of bias judgements, given in `rob`, are drawn as traffic
#' lights beside each study.
#'
#' Square area follows the study's weight in the model. The diamond is the
#' pooled estimate with its confidence interval, and the line through it the
#' prediction interval for a random effects model. Ratio measures such as
#' risk, odds and hazard ratios are shown on a log axis. A confidence
#' interval that runs past the axis ends in an arrow.
#'
#' With `cumulative = TRUE`, each row shows the pooled estimate from the
#' studies up to and including it, in the order of the data, so sort the
#' data by year before fitting the model for a cumulative meta-analysis
#' over time. [animate_meta()] replays that sequence as an animation.
#'
#' @param x A fitted meta-analysis: an `rma.uni` object from
#'   [metafor::rma()], or a `meta` object from the 'meta' package, such as
#'   the result of [meta::metabin()] or [meta::metagen()]. Models with
#'   moderators are not supported.
#' @param data Optional data frame with one row per study, in the order of
#'   the model, holding the columns to show. Defaults to the data stored in
#'   the fit, which is there when the model was fitted with a `data`
#'   argument.
#' @param columns Names of columns of `data` to print as text columns beside
#'   the study labels, such as event counts. Name the vector to set the
#'   column headers.
#' @param rob Names of columns of `data` holding risk of bias judgements,
#'   one per domain, drawn as traffic lights. Name the vector to set the
#'   column headers, such as `c(D1 = "rob.R", Overall = "rob.overall")`.
#'   Judgements are matched by their wording: "low", "some concerns" or
#'   "moderate", "unclear", "high" or "serious", "critical", and "no
#'   information", in any case.
#' @param hover Names of columns of `data` shown in each study's hover card.
#'   Defaults to `columns`.
#' @param cumulative Show the cumulative meta-analysis rather than the
#'   individual studies.
#' @param exponentiate Show effects on the ratio scale. Defaults to `TRUE`
#'   for ratio measures (`RR`, `OR`, `HR`, `IRR`, `ROM` and Peto odds
#'   ratios), which are modeled on the log scale.
#' @param xlim Optional limits of the effect axis, on the scale shown.
#' @param favors Optional labels for the two sides of the null line, such as
#'   `c("Favors treatment", "Favors control")`.
#' @param xlab Axis label. Defaults to the name of the effect measure.
#' @param title,caption Title above the plot and note below it.
#' @param family Font family. The package ships Lato and registers it on load.
#'
#' @return An object of class `ggmeta`, which prints as an interactive
#'   widget. Use [graph_widget()], [graph_plot()] or [graph_save()] for the
#'   widget, a static ggplot or a file, and [animate_meta()] for the
#'   cumulative replay.
#' @export
#'
#' @examples
#' if (requireNamespace("metafor", quietly = TRUE)) {
#'   dat <- metafor::escalc(
#'     measure = "OR", ai = p2y12.mi, n1i = p2y12.total,
#'     ci = aspirin.mi, n2i = aspirin.total,
#'     data = metadat::dat.chiarito2020, slab = paste(study, year)
#'   )
#'   dat <- dat[!is.na(dat$yi), ]
#'   dat$p2y12 <- paste0(dat$p2y12.mi, "/", dat$p2y12.total)
#'   dat$aspirin <- paste0(dat$aspirin.mi, "/", dat$aspirin.total)
#'   fit <- metafor::rma(yi, vi, data = dat)
#'
#'   ggmeta(
#'     fit,
#'     columns = c("P2Y12 inhibitor" = "p2y12", Aspirin = "aspirin"),
#'     rob = c(R = "rob.R", D = "rob.D", Mi = "rob.Mi", Me = "rob.Me",
#'             S = "rob.S", Overall = "rob.overall"),
#'     favors = c("Favors P2Y12 inhibitor", "Favors aspirin")
#'   )
#' }
ggmeta <- function(x, data = NULL, columns = NULL, rob = NULL, hover = NULL,
                   cumulative = FALSE,
                   exponentiate = NULL,
                   xlim = NULL,
                   favors = NULL,
                   xlab = NULL,
                   title = NULL,
                   caption = NULL,
                   family = "Lato") {
  input <- meta_prepare(x, data, columns, rob, hover, exponentiate, xlim,
                        favors, xlab, title, caption, family)
  lay <- meta_layout(input, cumulative)
  structure(
    list(plot = meta_draw(lay), width = lay$page$width / 72,
         height = lay$page$height / 72, title = title, input = input,
         cumulative = cumulative),
    class = c("ggmeta", "ggx_graph")
  )
}

#' Animate a cumulative meta-analysis
#'
#' Replays a meta-analysis one study at a time, in the order of the data,
#' and writes it to a GIF or MP4. Each study fades in as it is added, and
#' the pooled diamond eases to its new position over `swap` seconds, the
#' same motion as [ggrace()], before holding for `hold` seconds.
#'
#' @param x A forest plot from [ggmeta()].
#' @param file Output path, ending in `.gif` or `.mp4`.
#' @param time Optional labels, one per study, shown large behind the plot
#'   as each study is added, such as the year of publication.
#' @param hold Seconds to hold on each step.
#' @param swap Seconds the pooled estimate takes to move to its new value.
#' @param end_pause Seconds to hold the final frame.
#' @param fps Frames per second.
#' @param res Output resolution in pixels per inch.
#' @param loop Loop the GIF. Ignored for video.
#' @param cores Number of cores to draw frames on.
#' @param quiet Suppress the progress bar.
#'
#' @return `file`, invisibly.
#' @export
#'
#' @examples
#' \donttest{
#' if (requireNamespace("metafor", quietly = TRUE)) {
#'   dat <- metafor::escalc(measure = "RR", ai = tpos, bi = tneg,
#'                          ci = cpos, di = cneg, data = metadat::dat.bcg,
#'                          slab = paste(author, year))
#'   dat <- dat[order(dat$year), ]
#'   fit <- metafor::rma(yi, vi, data = dat)
#'   animate_meta(ggmeta(fit), tempfile(fileext = ".gif"), time = dat$year,
#'                fps = 10, cores = 1)
#' }
#' }
animate_meta <- function(x, file = "meta.gif", time = NULL, hold = 0.8,
                         swap = 0.45, end_pause = 2, fps = 30, res = 150,
                         loop = TRUE,
                         cores = max(1L, parallel::detectCores() - 1L),
                         quiet = FALSE) {
  stopifnot(inherits(x, "ggmeta"))
  lay <- meta_layout(x$input, cumulative = TRUE)
  k <- length(lay$rows$y)
  if (!is.null(time) && length(time) != k) {
    rlang::abort(paste0("`time` must have one label per study (", k, ")."))
  }
  cum <- lay$steps

  # One entry per frame: the step, and how far the diamond has moved from
  # the previous pooled estimate towards this one.
  swap_n <- max(1L, round(swap * fps))
  hold_n <- max(1L, round(hold * fps))
  plan <- do.call(rbind, lapply(seq_len(k), function(s) {
    u <- if (s == 1) rep(1, swap_n) else seq_len(swap_n) / swap_n
    data.frame(step = s, u = c(u, rep(1, hold_n)))
  }))
  plan <- rbind(plan, data.frame(step = k, u = rep(1, round(end_pause * fps))))
  eased <- ifelse(plan$u < 0.5, 4 * plan$u^3, 1 - (-2 * plan$u + 2)^3 / 2)
  from <- pmax(plan$step - 1, 1)
  mix <- function(col) cum[[col]][from] + (cum[[col]][plan$step] - cum[[col]][from]) * eased
  pooled <- data.frame(est = mix("est"), lo = mix("lo"), hi = mix("hi"))

  # Held frames repeat, so each distinct frame is drawn once and copied.
  key <- paste(plan$step, round(eased, 6))
  first <- !duplicated(key)
  unique_i <- which(first)
  width <- round(x$width * res)
  height <- round(x$height * res)
  dir <- tempfile("ggextreme")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  drawn <- render_frames(
    file.path(dir, "unique"), length(unique_i),
    function(i) {
      j <- unique_i[i]
      meta_draw(lay, step = plan$step[j], pooled = pooled[j, ],
                time_label = if (!is.null(time)) as.character(time[plan$step[j]]))
    },
    width, height, res, graph_ink$page, cores, quiet
  )
  files <- file.path(dir, sprintf("frame%05d.png", seq_len(nrow(plan))))
  ok <- file.copy(drawn[match(key, key[first])], files)
  if (!all(ok)) rlang::abort("Frames could not be assembled.")
  encode_frames(files, file, fps = fps, loop = loop, width = width,
                height = height)
  invisible(file)
}

ratio_measures <- c("RR", "OR", "HR", "IRR", "ROM", "PETO")

measure_names <- c(
  RR = "Risk ratio", OR = "Odds ratio", HR = "Hazard ratio",
  IRR = "Incidence rate ratio", ROM = "Ratio of means",
  PETO = "Peto odds ratio", MD = "Mean difference",
  SMD = "Standardized mean difference", RD = "Risk difference"
)

meta_dims <- utils::modifyList(graph_dims, list(
  row_h = 20,
  header_h = 26,
  text_pt = 10,
  header_pt = 10,
  small_pt = 9,
  gap = 16,
  plot_w = 210,
  square_min = 3,
  square_max = 12,
  rob_r = 6,
  rob_pitch = 17,
  diamond_h = 6,
  row_diamond_h = 4.5,
  arrow = 4
))

rob_classes <- list(
  low = list(match = "low", color = "#568E4F", symbol = "+"),
  some = list(match = "some|moderate", color = "#C28A4A", symbol = "\u2212"),
  unclear = list(match = "unclear", color = "#C28A4A", symbol = "?"),
  high = list(match = "high|serious", color = "#C76253", symbol = "\u00d7"),
  critical = list(match = "critical", color = "#7A2E2E", symbol = "!"),
  none = list(match = "no information|not reported|^na$|missing", color = "#9A9A9A",
              symbol = "?")
)

# The class of a judgement, taken from its wording.
rob_class <- function(v) {
  v <- tolower(trimws(as.character(v)))
  out <- rep(NA_character_, length(v))
  # Check the wordings that contain another's key first.
  for (cls in c("none", "critical", "some", "unclear", "high", "low")) {
    hit <- is.na(out) & !is.na(v) & grepl(rob_classes[[cls]]$match, v)
    out[hit] <- cls
  }
  out
}

# Everything ggmeta() needs from the fit, in one shape for both packages.
meta_input <- function(x) {
  if (inherits(x, "rma.uni")) {
    rlang::check_installed("metafor")
    if (!isTRUE(x$int.only)) {
      rlang::abort("`x` must be a model without moderators.")
    }
    common <- x$method %in% c("FE", "EE", "CE")
    z <- stats::qnorm(1 - x$level / 2)
    yi <- as.numeric(x$yi)
    se <- sqrt(as.numeric(x$vi))
    pred <- stats::predict(x)
    cum <- metafor::cumul(x)
    data <- x$data
    if (!is.null(data) && !is.null(x$not.na) && nrow(data) == length(x$not.na)) {
      data <- data[x$not.na, , drop = FALSE]
    }
    return(list(
      label = as.character(x$slab), yi = yi, lo = yi - z * se, hi = yi + z * se,
      weight = as.numeric(stats::weights(x)),
      pooled = list(est = as.numeric(pred$pred), lo = as.numeric(pred$ci.lb),
                    hi = as.numeric(pred$ci.ub),
                    pi_lo = if (common) NA_real_ else as.numeric(pred$pi.lb),
                    pi_hi = if (common) NA_real_ else as.numeric(pred$pi.ub),
                    tau2 = if (common) NA_real_ else x$tau2,
                    i2 = x$I2, k = x$k),
      model = if (common) "Common effect model" else "Random effects model",
      measure = x$measure,
      cumulative = data.frame(est = as.numeric(cum$estimate),
                              lo = as.numeric(cum$ci.lb),
                              hi = as.numeric(cum$ci.ub)),
      data = if (is.null(data)) NULL else as.data.frame(data)
    ))
  }
  if (inherits(x, "meta")) {
    rlang::check_installed("meta")
    random <- isTRUE(x$random)
    keep <- !is.na(x$TE) & !is.na(x$seTE)
    z <- stats::qnorm(1 - (1 - x$level) / 2)
    yi <- x$TE[keep]
    se <- x$seTE[keep]
    w <- (if (random) x$w.random else x$w.common)[keep]
    cum <- meta::metacum(x, pooled = if (random) "random" else "common")
    k <- sum(keep)
    data <- x$data
    if (!is.null(data)) {
      data <- as.data.frame(data)[keep, !startsWith(names(data), "."), drop = FALSE]
    }
    num <- function(v) if (is.null(v) || !length(v)) NA_real_ else as.numeric(v[1])
    return(list(
      label = as.character(x$studlab[keep]), yi = yi,
      lo = yi - z * se, hi = yi + z * se,
      weight = 100 * w / sum(w),
      pooled = list(
        est = num(if (random) x$TE.random else x$TE.common),
        lo = num(if (random) x$lower.random else x$lower.common),
        hi = num(if (random) x$upper.random else x$upper.common),
        pi_lo = if (random) num(x$lower.predict) else NA_real_,
        pi_hi = if (random) num(x$upper.predict) else NA_real_,
        tau2 = if (random) num(x$tau2) else NA_real_,
        i2 = 100 * num(x$I2), k = k
      ),
      model = if (random) "Random effects model" else "Common effect model",
      measure = x$sm,
      cumulative = data.frame(est = cum$TE[seq_len(k)], lo = cum$lower[seq_len(k)],
                              hi = cum$upper[seq_len(k)]),
      data = data
    ))
  }
  rlang::abort("`x` must be an `rma.uni` fit from metafor or a `meta` object.")
}

# Check the arguments once, so every later layout, static or animated,
# starts from the same settled input.
meta_prepare <- function(x, data, columns, rob, hover, exponentiate, xlim,
                         favors, xlab, title, caption, family) {
  m <- meta_input(x)
  k <- length(m$yi)
  if (!is.null(data)) data <- as.data.frame(data)
  if (is.null(data)) data <- m$data
  if (!is.null(data) && nrow(data) != k) {
    rlang::abort(paste0("`data` must have one row per study in the model (", k, ")."))
  }
  if (is.null(hover)) hover <- columns
  named <- c(columns, rob, hover)
  if (length(named)) {
    if (is.null(data)) {
      rlang::abort("`columns`, `rob` and `hover` need `data`, or a fit that stores its data.")
    }
    unknown <- setdiff(named, names(data))
    if (length(unknown)) {
      rlang::abort(paste0("These columns are not in `data`: ",
                          paste(unique(unknown), collapse = ", ")))
    }
  }
  heading <- function(v) {
    if (!length(v)) return(character(0))
    out <- names(v)
    if (is.null(out)) out <- rep("", length(v))
    out[!nzchar(out)] <- column_labels(data, v[!nzchar(out)])
    out
  }
  if (length(rob)) {
    unmatched <- unique(unlist(lapply(rob, function(col) {
      v <- data[[col]]
      v[!is.na(v) & is.na(rob_class(v))]
    })))
    if (length(unmatched)) {
      rlang::abort(paste0("These risk of bias judgements are not recognized: ",
                          paste(unmatched, collapse = ", ")))
    }
  }
  if (is.null(exponentiate)) exponentiate <- m$measure %in% ratio_measures
  if (!is.null(favors) && length(favors) != 2) {
    rlang::abort("`favors` must hold two labels, for the left and right sides.")
  }
  if (!is.null(xlim) && (length(xlim) != 2 || any(!is.finite(xlim)) ||
                         (exponentiate && any(xlim <= 0)))) {
    rlang::abort("`xlim` must be two finite limits, above zero on a ratio scale.")
  }
  if (is.null(xlab)) {
    xlab <- if (m$measure %in% names(measure_names)) measure_names[[m$measure]] else "Effect"
  }
  list(m = m, data = data, columns = unname(columns), column_heads = heading(columns),
       rob = unname(rob), rob_heads = heading(rob), hover = unname(hover),
       hover_heads = heading(hover), exponentiate = exponentiate, xlim = xlim,
       favors = favors, xlab = xlab, title = title, caption = caption,
       family = family)
}

# Positions, strings and cards for every element, in points on a top down
# canvas. Drawing is left to meta_draw(), so an animation can redraw the
# same layout many times with different states.
meta_layout <- function(input, cumulative) {
  dims <- meta_dims
  m <- input$m
  data <- input$data
  family <- input$family
  k <- length(m$yi)
  shown <- if (input$exponentiate) exp else identity
  fmt <- function(v) {
    v <- shown(v)
    vapply(v, function(a) {
      if (!is.finite(a)) return("")
      if (input$exponentiate) {
        formatC(a, format = "f", digits = if (a >= 10) 1 else if (a < 0.1) 3 else 2)
      } else {
        formatC(a, format = "f", digits = 2)
      }
    }, character(1))
  }
  interval <- function(e, l, h) paste0(fmt(e), " (", fmt(l), ", ", fmt(h), ")")
  width <- function(s, pt = dims$text_pt, bold = FALSE) {
    if (!length(s)) return(0)
    max(text_width_card(as.character(s), pt, family, 1, bold = bold))
  }

  est <- if (cumulative) m$cumulative$est else m$yi
  lo <- if (cumulative) m$cumulative$lo else m$lo
  hi <- if (cumulative) m$cumulative$hi else m$hi
  labels <- if (cumulative) c(m$label[1], paste("+", m$label[-1])) else m$label
  p <- m$pooled
  est_text <- interval(est, lo, hi)
  pooled_text <- interval(p$est, p$lo, p$hi)
  est_head <- paste0(if (cumulative) "Pooled so far" else input$xlab, " (95% CI)")
  weight_text <- paste0(formatC(m$weight, format = "f", digits = 1), "%")
  show_weight <- !cumulative

  # Columns, left to right.
  x <- 0
  label_w <- max(width(labels), width("Study", dims$header_pt, TRUE),
                 width(m$model, dims$text_pt, TRUE))
  label_x <- x
  x <- x + label_w + dims$gap
  col_x <- numeric(0)
  for (j in seq_along(input$columns)) {
    values <- format_values(data[[input$columns[j]]])
    values[is.na(values)] <- ""
    col_x[j] <- x
    x <- x + max(width(values), width(input$column_heads[j], dims$header_pt, TRUE)) +
      dims$gap
  }
  rob_x <- numeric(0)
  for (j in seq_along(input$rob)) {
    w <- max(dims$rob_pitch, width(input$rob_heads[j], dims$small_pt, TRUE) + 6)
    rob_x[j] <- x + w / 2
    x <- x + w
  }
  if (length(input$rob)) x <- x + dims$gap - 4
  plot_x0 <- x
  plot_x1 <- x + dims$plot_w
  est_x <- plot_x1 + dims$gap
  est_w <- max(width(c(est_text, pooled_text)), width(est_head, dims$header_pt, TRUE))
  weight_x <- est_x + est_w + dims$gap
  weight_w <- if (show_weight) max(width(weight_text), width("Weight", dims$header_pt, TRUE)) else 0
  right <- if (show_weight) weight_x + weight_w else est_x + est_w

  # Rows, top to bottom.
  row_top <- dims$header_h + 6
  row_y <- row_top + (seq_len(k) - 0.5) * dims$row_h
  rows_end <- row_top + k * dims$row_h
  pooled_y <- rows_end + 20
  het_y <- pooled_y + 17
  axis_y <- het_y + 16
  xlab_y <- axis_y + 30
  favors_y <- xlab_y + 15
  bottom <- if (is.null(input$favors)) xlab_y + 8 else favors_y + 8

  # The effect axis, on the model's scale.
  if (is.null(input$xlim)) {
    span <- range(c(lo, hi, p$lo, p$hi, p$pi_lo, p$pi_hi, 0), na.rm = TRUE)
    span <- span + c(-1, 1) * 0.04 * diff(span)
  } else {
    span <- if (input$exponentiate) log(sort(input$xlim)) else sort(input$xlim)
  }
  X <- function(v) plot_x0 + (v - span[1]) / diff(span) * (plot_x1 - plot_x0)
  inside <- function(v) pmin(pmax(v, span[1]), span[2])
  ticks <- if (input$exponentiate) {
    nice <- c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10,
              20, 50, 100, 200, 500, 1000)
    t <- nice[log(nice) >= span[1] & log(nice) <= span[2]]
    if (length(t) > 7) {
      at <- match(1, t)
      t <- t[seq(if (is.na(at)) 1 else (at - 1) %% 2 + 1, length(t), by = 2)]
    }
    if (length(t) < 3) t <- c(0.25, 0.5, 0.75, 1, 1.5, 2, 4)[
      log(c(0.25, 0.5, 0.75, 1, 1.5, 2, 4)) >= span[1] &
        log(c(0.25, 0.5, 0.75, 1, 1.5, 2, 4)) <= span[2]]
    log(t)
  } else {
    t <- scales::breaks_extended(5)(span)
    t[t >= span[1] & t <= span[2]]
  }
  tick_labels <- vapply(shown(ticks), function(v) format(signif(v, 3), trim = TRUE,
                                                         drop0trailing = TRUE),
                        character(1))

  # Hover cards and click panels.
  ids <- paste0("s", seq_len(k))
  hover_values <- lapply(seq_len(k), function(i) {
    if (!length(input$hover)) return(character(0))
    v <- vapply(input$hover, function(col) format_values(data[[col]])[i], character(1))
    stats::setNames(ifelse(is.na(v), "", v), input$hover_heads)
  })
  rob_values <- lapply(seq_len(k), function(i) {
    if (!length(input$rob)) return(character(0))
    stats::setNames(vapply(input$rob, function(col) as.character(data[[col]][i]),
                           character(1)), input$rob_heads)
  })
  row_tip <- vapply(seq_len(k), function(i) {
    if (cumulative) {
      head <- paste0("Pooled over the first ", i, if (i == 1) " study" else " studies")
      body <- tip_rows(c("Pooled so far" = est_text[i],
                         "This study alone" = interval(m$yi[i], m$lo[i], m$hi[i])))
    } else {
      head <- paste0("Weight ", weight_text[i])
      body <- tip_rows(c(stats::setNames(est_text[i], input$xlab), hover_values[[i]],
                         rob_values[[i]]))
    }
    paste0('<div class="ggx-tip-title">', esc(if (cumulative) paste("Adding", m$label[i]) else m$label[i]),
           '</div><div class="ggx-tip-sub">', esc(head), "</div>", body,
           if (!is.null(data)) '<div class="ggx-tip-hint">Click for the full record.</div>')
  }, character(1))
  row_panel <- vapply(seq_len(k), function(i) {
    record <- if (is.null(data)) character(0) else {
      keep <- setdiff(names(data), c("yi", "vi"))
      keep <- keep[!vapply(data[keep], is.list, logical(1))]
      v <- vapply(keep, function(col) format_values(data[[col]])[i], character(1))
      stats::setNames(v, column_labels(data, keep))[!is.na(v)]
    }
    panel_html(m$label[i],
               paste0(input$xlab, " ", interval(m$yi[i], m$lo[i], m$hi[i]),
                      "; weight ", weight_text[i]),
               "", list(record), list(character(0)))
  }, character(1))
  pooled_tip <- paste0(
    '<div class="ggx-tip-title">', esc(m$model), '</div><div class="ggx-tip-sub">',
    p$k, if (p$k == 1) " study" else " studies", "</div>",
    tip_rows(c(
      stats::setNames(pooled_text, input$xlab),
      if (is.finite(p$pi_lo)) c("Prediction interval" = paste(fmt(p$pi_lo), "to", fmt(p$pi_hi))),
      if (is.finite(p$tau2)) c("\u03c4\u00b2" = formatC(p$tau2, format = "f", digits = 3)),
      if (is.finite(p$i2)) c("I\u00b2" = paste0(formatC(p$i2, format = "f", digits = 1), "%"))
    ))
  )
  het <- paste(c(
    if (is.finite(p$i2)) paste0("I\u00b2 = ", formatC(p$i2, format = "f", digits = 1), "%"),
    if (is.finite(p$tau2)) paste0("\u03c4\u00b2 = ", formatC(p$tau2, format = "f", digits = 3)),
    if (is.finite(p$pi_lo)) paste("prediction interval", fmt(p$pi_lo), "to", fmt(p$pi_hi))
  ), collapse = "; ")

  # Risk of bias key, in order of severity.
  key_labels <- character(0)
  key_colors <- character(0)
  if (length(input$rob)) {
    all <- unlist(lapply(input$rob, function(col) as.character(data[[col]])))
    all <- unique(all[!is.na(all)])
    cls <- rob_class(all)
    o <- order(match(cls, names(rob_classes)))
    key_labels <- capitalize(all[o])
    key_colors <- vapply(cls[o], function(c) rob_classes[[c]]$color, character(1))
  }
  y_extent <- c(0, bottom)
  x_extent <- c(0, right)
  page <- graph_canvas(x_extent, y_extent, input$title, input$caption, key_labels,
                       unname(key_colors), if (length(key_labels)) "Risk of bias",
                       dims, family)

  list(
    dims = dims, page = page, family = family, cumulative = cumulative,
    exponentiate = input$exponentiate, favors = input$favors, xlab = input$xlab,
    X = X, inside = inside, span = span, ticks = ticks, tick_labels = tick_labels,
    plot_x0 = plot_x0, plot_x1 = plot_x1, right = right,
    label_x = label_x, col_x = col_x, rob_x = rob_x, est_x = est_x,
    weight_x = weight_x + weight_w, show_weight = show_weight,
    header_y = dims$header_h / 2 + 2, header_rule = dims$header_h,
    row_top = row_top, rows_end = rows_end, pooled_y = pooled_y, het_y = het_y,
    axis_y = axis_y, xlab_y = xlab_y, favors_y = favors_y,
    rows = list(y = row_y, label = labels, est = est, lo = lo, hi = hi,
                est_text = est_text, weight = m$weight, weight_text = weight_text,
                id = ids, tip = row_tip, click = pin_js(ids, row_panel)),
    columns = lapply(input$columns, function(col) {
      v <- format_values(data[[col]])
      ifelse(is.na(v), "", v)
    }),
    column_heads = input$column_heads,
    rob = lapply(input$rob, function(col) as.character(data[[col]])),
    rob_heads = input$rob_heads,
    est_head = est_head,
    pooled = list(est = p$est, lo = p$lo, hi = p$hi, pi_lo = p$pi_lo,
                  pi_hi = p$pi_hi, text = pooled_text, model = m$model,
                  het = het, tip = pooled_tip),
    steps = m$cumulative,
    fmt = fmt, interval = interval
  )
}

# Draw a layout. `step` fades the rows after it and `pooled` moves the
# diamond, which is how animate_meta() draws each frame.
meta_draw <- function(lay, step = NULL, pooled = NULL, time_label = NULL) {
  dims <- lay$dims
  px <- lay$page$px
  py <- lay$page$py
  X <- lay$X
  r <- lay$rows
  k <- length(r$y)
  ghost <- graph_ink$ghost
  faded <- if (is.null(step)) rep(FALSE, k) else seq_len(k) > step
  ink <- ifelse(faded, ghost, graph_ink$text)
  muted <- ifelse(faded, ghost, graph_ink$muted)
  accent <- race_palette(1)
  p <- lay$pooled
  if (!is.null(pooled)) {
    p$est <- pooled$est
    p$lo <- pooled$lo
    p$hi <- pooled$hi
    p$text <- lay$interval(p$est, p$lo, p$hi)
    p$model <- paste0("Pooled after ", step, " of ", k)
  }
  interactive <- is.null(step)

  texts <- function(label, x, y, pt, color, hjust = 0, face = "plain") {
    if (!length(label)) return(empty_texts())
    data.frame(x = px(x), y = py(y), label = label, size = pt / .pt,
               colour = color, hjust = hjust, fontface = face,
               stringsAsFactors = FALSE)
  }
  segs <- function(x0, x1, y0, y1, id = "", tip = "", click = "", colour = graph_ink$text) {
    data.frame(x = px(x0), xend = px(x1), y = py(y0), yend = py(y1), id = id,
               tooltip = tip, onclick = click, colour = colour,
               stringsAsFactors = FALSE)
  }

  # Row hover targets, full width, under everything. ggiraph keys hover
  # styles by id, so everything that shares a row's id takes the global
  # hover style rather than one of its own.
  hits <- data.frame(
    xmin = px(0), xmax = px(lay$right), ymin = py(r$y + dims$row_h / 2),
    ymax = py(r$y - dims$row_h / 2), id = r$id, tooltip = r$tip, onclick = r$click,
    stringsAsFactors = FALSE
  )

  # Confidence intervals, clipped to the axis with an arrow where they run out.
  lo_in <- lay$inside(r$lo)
  hi_in <- lay$inside(r$hi)
  ci <- segs(X(lo_in), X(hi_in), r$y, r$y, r$id, r$tip, r$click, ink)
  arrows <- do.call(rbind, lapply(seq_len(k), function(i) {
    out <- NULL
    a <- dims$arrow
    if (r$lo[i] < lay$span[1]) {
      x0 <- X(lay$span[1])
      out <- rbind(out, data.frame(x = px(c(x0, x0 + a, x0 + a)),
                                   y = py(c(r$y[i], r$y[i] - a / 1.4, r$y[i] + a / 1.4)),
                                   group = paste0(r$id[i], "l")))
    }
    if (r$hi[i] > lay$span[2]) {
      x1 <- X(lay$span[2])
      out <- rbind(out, data.frame(x = px(c(x1, x1 - a, x1 - a)),
                                   y = py(c(r$y[i], r$y[i] - a / 1.4, r$y[i] + a / 1.4)),
                                   group = paste0(r$id[i], "r")))
    }
    if (!is.null(out)) {
      out$id <- r$id[i]
      out$tooltip <- r$tip[i]
      out$onclick <- r$click[i]
      out$fill <- ink[i]
    }
    out
  }))

  # Squares for studies, or small diamonds for a cumulative analysis.
  in_view <- r$est >= lay$span[1] & r$est <= lay$span[2]
  marks <- if (lay$cumulative) {
    do.call(rbind, lapply(which(in_view), function(i) {
      h <- dims$row_diamond_h
      cx <- X(r$est[i])
      data.frame(x = px(c(cx - h * 1.3, cx, cx + h * 1.3, cx)),
                 y = py(c(r$y[i], r$y[i] - h, r$y[i], r$y[i] + h)),
                 group = r$id[i], id = r$id[i], tooltip = r$tip[i],
                 onclick = r$click[i],
                 fill = if (faded[i]) ghost else accent, stringsAsFactors = FALSE)
    }))
  } else {
    side <- dims$square_min + (dims$square_max - dims$square_min) *
      sqrt(r$weight / max(r$weight))
    do.call(rbind, lapply(which(in_view), function(i) {
      s <- side[i] / 2
      cx <- X(r$est[i])
      data.frame(x = px(c(cx - s, cx + s, cx + s, cx - s)),
                 y = py(c(r$y[i] - s, r$y[i] - s, r$y[i] + s, r$y[i] + s)),
                 group = r$id[i], id = r$id[i], tooltip = r$tip[i],
                 onclick = r$click[i], fill = ink[i], stringsAsFactors = FALSE)
    }))
  }

  # Traffic lights.
  lights <- NULL
  symbols <- empty_texts()
  if (length(lay$rob)) {
    a <- seq(0, 2 * pi, length.out = 25)
    for (j in seq_along(lay$rob)) {
      cls <- rob_class(lay$rob[[j]])
      for (i in which(!is.na(cls))) {
        info <- rob_classes[[cls[i]]]
        fill <- if (faded[i]) ghost else info$color
        tip <- paste0('<div class="ggx-tip-title">', esc(r$label[i]),
                      '</div><div class="ggx-tip-sub">Risk of bias</div>',
                      tip_rows(stats::setNames(capitalize(lay$rob[[j]][i]),
                                               lay$rob_heads[j])))
        lights <- rbind(lights, data.frame(
          x = px(lay$rob_x[j] + dims$rob_r * cos(a)),
          y = py(r$y[i] + dims$rob_r * sin(a)),
          group = paste0(r$id[i], "_", j), id = paste0(r$id[i], "_rob", j), tooltip = tip,
          onclick = r$click[i], fill = fill,
          hover = sprintf("fill:%s;stroke:none;", fill), stringsAsFactors = FALSE
        ))
        symbols <- rbind(symbols, texts(info$symbol, lay$rob_x[j], r$y[i] + 0.4,
                                        dims$small_pt, graph_ink$on_color, hjust = 0.5,
                                        face = "bold"))
      }
    }
  }

  # The pooled diamond and prediction interval.
  dh <- dims$diamond_h
  diamond <- data.frame(
    x = px(X(lay$inside(c(p$lo, p$est, p$hi, p$est)))),
    y = py(c(lay$pooled_y, lay$pooled_y - dh, lay$pooled_y, lay$pooled_y + dh)),
    group = "pooled", id = "pooled", tooltip = p$tip, onclick = "",
    fill = accent, stringsAsFactors = FALSE
  )
  pi_seg <- if (is.null(pooled) && is.finite(p$pi_lo)) {
    segs(X(lay$inside(p$pi_lo)), X(lay$inside(p$pi_hi)), lay$pooled_y, lay$pooled_y,
         "pooled", p$tip, "", accent)
  }

  # Lines, axis and text.
  null_x <- X(0)
  guides <- rbind(
    segs(0, lay$right, lay$header_rule, lay$header_rule, colour = graph_ink$plain_border),
    segs(0, lay$right, lay$rows_end + 4, lay$rows_end + 4, colour = graph_ink$plain_border),
    segs(lay$plot_x0, lay$plot_x1, lay$axis_y, lay$axis_y),
    segs(X(lay$ticks), X(lay$ticks), lay$axis_y, lay$axis_y + 4),
    if (null_x >= lay$plot_x0 && null_x <= lay$plot_x1) {
      segs(null_x, null_x, lay$row_top - 4, lay$axis_y)
    }
  )
  pooled_line <- if (p$est >= lay$span[1] && p$est <= lay$span[2]) {
    segs(X(p$est), X(p$est), lay$row_top - 4, lay$pooled_y - dh, colour = accent)
  }

  header <- rbind(
    texts("Study", lay$label_x, lay$header_y, dims$header_pt, graph_ink$title, face = "bold"),
    texts(lay$column_heads, lay$col_x, rep(lay$header_y, length(lay$col_x)),
          dims$header_pt, graph_ink$title, face = "bold"),
    texts(lay$rob_heads, lay$rob_x, rep(lay$header_y, length(lay$rob_x)),
          dims$small_pt, graph_ink$title, hjust = 0.5, face = "bold"),
    texts(lay$est_head, lay$est_x, lay$header_y, dims$header_pt, graph_ink$title, face = "bold"),
    if (lay$show_weight) texts("Weight", lay$weight_x, lay$header_y, dims$header_pt,
                               graph_ink$title, hjust = 1, face = "bold")
  )
  body <- rbind(
    texts(r$label, rep(lay$label_x, k), r$y, dims$text_pt, ink),
    do.call(rbind, lapply(seq_along(lay$columns), function(j) {
      texts(lay$columns[[j]], rep(lay$col_x[j], k), r$y, dims$text_pt, muted)
    })),
    texts(r$est_text, rep(lay$est_x, k), r$y, dims$text_pt, ink),
    if (lay$show_weight) texts(r$weight_text, rep(lay$weight_x, k), r$y,
                               dims$text_pt, muted, hjust = 1)
  )
  foot <- rbind(
    texts(p$model, lay$label_x, lay$pooled_y, dims$text_pt, graph_ink$title, face = "bold"),
    texts(p$text, lay$est_x, lay$pooled_y, dims$text_pt, graph_ink$title, face = "bold"),
    if (is.null(pooled) && nzchar(p$het)) texts(p$het, lay$label_x, lay$het_y,
                                                dims$small_pt, graph_ink$muted),
    texts(lay$tick_labels, X(lay$ticks), rep(lay$axis_y + 13, length(lay$ticks)),
          dims$small_pt, graph_ink$muted, hjust = 0.5),
    texts(lay$xlab, (lay$plot_x0 + lay$plot_x1) / 2, lay$xlab_y, dims$text_pt,
          graph_ink$text, hjust = 0.5),
    if (!is.null(lay$favors)) rbind(
      texts(paste("\u2190", lay$favors[1]), null_x - 6, lay$favors_y, dims$small_pt,
            graph_ink$muted, hjust = 1),
      texts(paste(lay$favors[2], "\u2192"), null_x + 6, lay$favors_y, dims$small_pt,
            graph_ink$muted, hjust = 0)
    )
  )
  big <- if (!is.null(time_label)) {
    texts(time_label, lay$plot_x1 - 4, lay$row_top + 34, 54, graph_ink$watermark, hjust = 1,
          face = "bold")
  }

  base <- ggplot() + draw_text(big, lay$family)
  if (interactive) {
    base <- base +
      ggiraph::geom_rect_interactive(
        data = hits,
        aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, ymax = .data$ymax,
            data_id = .data$id, tooltip = .data$tooltip, onclick = .data$onclick),
        fill = "#FFFFFF02", colour = NA
      )
  }
  base +
    geom_segment(data = rbind(guides, pooled_line),
                 aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
                 colour = rbind(guides, pooled_line)$colour, linewidth = 0.8 / .pt,
                 linetype = c(rep("solid", nrow(guides)),
                              rep("22", if (is.null(pooled_line)) 0 else nrow(pooled_line)))) +
    meta_segments(pi_seg, 1.3) +
    meta_segments(ci, 1.1) +
    meta_shapes(arrows) +
    meta_shapes(marks) +
    meta_shapes(lights, hover = TRUE) +
    meta_shapes(diamond) +
    draw_text(rbind(header, body, foot, symbols), lay$family) +
    graph_frame(lay$page, lay$family)
}

meta_segments <- function(df, width) {
  if (is.null(df) || !nrow(df)) return(NULL)
  ggiraph::geom_segment_interactive(
    data = df,
    aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend,
        colour = .data$colour, data_id = .data$id, tooltip = .data$tooltip,
        onclick = .data$onclick),
    linewidth = width / .pt, lineend = "butt"
  )
}

meta_shapes <- function(df, hover = FALSE) {
  if (is.null(df) || !nrow(df)) return(NULL)
  mapping <- if (hover) {
    aes(x = .data$x, y = .data$y, group = .data$group, fill = .data$fill,
        data_id = .data$id, tooltip = .data$tooltip, onclick = .data$onclick,
        hover_css = .data$hover)
  } else {
    aes(x = .data$x, y = .data$y, group = .data$group, fill = .data$fill,
        data_id = .data$id, tooltip = .data$tooltip, onclick = .data$onclick)
  }
  ggiraph::geom_polygon_interactive(data = df, mapping = mapping, colour = NA)
}
