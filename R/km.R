#' Draw an interactive Kaplan-Meier plot
#'
#' Draws Kaplan-Meier curves by group with confidence bands, censoring
#' marks and a table of the numbers at risk, followed by the hazard ratios
#' and the proportional hazards tests. Hovering anywhere along the time axis
#' reads every group at that time: survival with its confidence interval,
#' the number at risk, the events so far and the hazard ratio against the
#' reference group at that time, while the matching column of the risk
#' table lights up. Clicking opens the full readout for that time under the
#' plot. Hovering over a curve shows that group's summary, and clicking it
#' opens the group's survival at each break.
#'
#' The hazard ratio at a time comes from `hr_time`. The default,
#' `"schoenfeld"`, smooths the scaled Schoenfeld residuals of the Cox model
#' against time, which estimates the log hazard ratio as a function of time
#' (Grambsch and Therneau 1994), the curve [survival::plot.cox.zph()] draws.
#' `"log"` and `"linear"` take it from a Cox model with an interaction
#' between the group and log time or time, and `"constant"` shows the Cox
#' estimate at every time.
#'
#' With `rmst`, the plot shades the area under each curve up to a horizon
#' \eqn{\tau} and tabulates the restricted mean survival time, the mean time
#' alive (or free of the event) within \eqn{\tau}, for each group, with its
#' difference from the reference group. RMST and its standard error are those
#' [survival::survfit()] reports for each group; the difference assumes the
#' groups are independent, as in a randomized comparison. The widget adds a
#' slider to explore other horizons, up to the end of follow-up in the group
#' followed least, while the prespecified horizon stays marked, so the
#' horizon reported is the one planned rather than the most favorable one.
#'
#' With `ph_tests = TRUE`, a section under the plot, collapsed until the
#' reader opens it, gives the Cox hazard ratios, the log-rank test, the
#' Grambsch and Therneau test for each comparison and overall, and the group
#' by time and group by log time interactions with their joint Wald tests,
#' with a note on what each test asks. The table is also returned as the
#' field `ph`. It belongs to the widget, so static copies from
#' [graph_plot()] and [graph_save()] leave it out.
#'
#' @param formula A formula of the form `Surv(time, status) ~ group`, with
#'   right censored survival times and a single grouping variable.
#' @param data A data frame holding the variables in `formula`.
#' @param type `"survival"` for the survival probability, or `"risk"` for
#'   the cumulative probability of the event.
#' @param hr_time How the hazard ratio at each time is estimated:
#'   `"schoenfeld"`, `"log"`, `"linear"`, `"constant"` or `"none"`.
#' @param ph_tests Add the hazard ratios and proportional hazards tests, in a
#'   collapsed section under the plot. The time interaction models grow with
#'   the number of events, so they are skipped with a message for very large
#'   data.
#' @param rmst Restricted mean survival time: `NULL` for none, the
#'   prespecified horizon \eqn{\tau} on the time scale, or `TRUE` for the
#'   end of follow-up in the group followed least.
#' @param risk_table Show the numbers at risk.
#' @param conf_int Shade the confidence bands.
#' @param breaks Times for the axis ticks and the risk table. Defaults to
#'   about eight evenly spaced times.
#' @param reference The group the hazard ratios compare against. Defaults to
#'   the first level.
#' @param palette Colors for the groups, unnamed in level order or named by
#'   group. The reference group defaults to a neutral gray.
#' @param xlab,ylab Axis labels. `xlab` also names the time in the hover
#'   card, so include its unit, such as `"Years since randomization"`.
#' @param legend Draw a legend of the groups above the plot.
#' @param legend_title Text in front of the legend.
#' @param title,caption Title above the plot and note below it.
#' @param family Font family. The package ships Lato and registers it on load.
#'
#' @return An object of class `ggkm`, which prints as an interactive widget.
#'   Use [graph_widget()], [graph_plot()] or [graph_save()] for the widget, a
#'   static ggplot or a file, and [animate_km()] to draw the curves over
#'   follow-up as an animation. With `ph_tests = TRUE`, the field `ph` holds
#'   the proportional hazards table as a data frame.
#' @export
#'
#' @examples
#' if (requireNamespace("survival", quietly = TRUE)) {
#'   colon <- subset(survival::colon, etype == 2)
#'   colon$years <- colon$time / 365.25
#'   km <- ggkm(survival::Surv(years, status) ~ rx, data = colon,
#'              xlab = "Years since randomization")
#'   km
#' }
#' \donttest{
#' # The proportional hazards tests fit interaction models, which takes a
#' # few seconds.
#' if (requireNamespace("survival", quietly = TRUE)) {
#'   km <- ggkm(survival::Surv(years, status) ~ rx, data = colon,
#'              ph_tests = TRUE, xlab = "Years since randomization")
#'   km$ph
#' }
#' }
ggkm <- function(formula, data,
                 type = c("survival", "risk"),
                 hr_time = c("schoenfeld", "log", "linear", "constant", "none"),
                 ph_tests = FALSE,
                 rmst = NULL,
                 risk_table = TRUE,
                 conf_int = TRUE,
                 breaks = NULL,
                 reference = NULL,
                 palette = NULL,
                 xlab = "Time",
                 ylab = NULL,
                 legend = TRUE,
                 legend_title = NULL,
                 title = NULL,
                 caption = NULL,
                 family = "Lato") {
  rlang::check_installed("survival")
  type <- match.arg(type)
  hr_time <- match.arg(hr_time)
  input <- km_prepare(formula, data, reference, hr_time, ph_tests, breaks)
  input$rmst <- km_rmst(input, rmst)
  if (is.null(ylab)) ylab <- if (type == "survival") "Survival" else "Cumulative incidence"
  lay <- km_layout(input, type, hr_time, ph_tests, risk_table, conf_int, palette,
                   xlab, ylab, legend, legend_title, title, caption, family)
  on_render <- "ggextremeKm(el);"
  if (!is.null(lay$ph_html)) {
    on_render <- paste0(on_render, " ggextremeDetails(el, ",
                        js_string("Hazard ratios and proportional hazards"), ", ",
                        js_string(lay$ph_html), ");")
  }
  if (!is.null(lay$rmst)) on_render <- paste(on_render, "ggextremeRmst(el, data);")
  structure(
    list(plot = km_draw(lay), width = lay$page$width / 72,
         height = lay$page$height / 72, title = title,
         on_render = on_render, hover_inv = "", layout = lay,
         render_data = if (!is.null(lay$rmst)) km_rmst_data(lay),
         ph = lay$ph_table, rmst = if (!is.null(lay$rmst)) lay$rmst$table),
    class = c("ggkm", "ggx_graph")
  )
}

#' Animate a Kaplan-Meier plot
#'
#' Draws the curves of a [ggkm()] plot over follow-up, as though the trial
#' were being watched, and writes the result to a GIF or MP4. The numbers at
#' risk appear as each break is reached and the time is shown large behind
#' the curves.
#'
#' @param x A plot from [ggkm()].
#' @param file Output path, ending in `.gif` or `.mp4`.
#' @param duration Seconds to draw the full follow-up.
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
#' if (requireNamespace("survival", quietly = TRUE)) {
#'   colon <- subset(survival::colon, etype == 2)
#'   km <- ggkm(survival::Surv(time / 365.25, status) ~ rx, data = colon,
#'              xlab = "Years")
#'   animate_km(km, tempfile(fileext = ".gif"), duration = 2, fps = 8,
#'              cores = 1)
#' }
#' }
animate_km <- function(x, file = "km.gif", duration = 5, end_pause = 2,
                       fps = 30, res = 150, loop = TRUE,
                       cores = max(1L, parallel::detectCores() - 1L),
                       quiet = FALSE) {
  stopifnot(inherits(x, "ggkm"))
  lay <- x$layout
  n <- max(2L, round(duration * fps))
  times <- seq(0, lay$tmax, length.out = n)
  plan <- c(seq_len(n), rep(n, round(end_pause * fps)))
  width <- round(x$width * res)
  height <- round(x$height * res)
  dir <- tempfile("ggextreme")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  drawn <- render_frames(file.path(dir, "unique"), n,
                         function(i) km_draw(lay, until = times[i]),
                         width, height, res, graph_ink$page, cores, quiet)
  files <- file.path(dir, sprintf("frame%05d.png", seq_along(plan)))
  if (!all(file.copy(drawn[plan], files))) rlang::abort("Frames could not be assembled.")
  encode_frames(files, file, fps = fps, loop = loop, width = width, height = height)
  invisible(file)
}

km_dims <- utils::modifyList(graph_dims, list(
  plot_w = 440,
  plot_h = 220,
  row_h = 16,
  text_pt = 10,
  small_pt = 9,
  gap = 14,
  slices = 120,
  zph_df = 4,
  tt_limit = 5e6
))

# Fit everything once: the curves, the Cox model, the proportional hazards
# tests and the hazard ratio over time.
km_prepare <- function(formula, data, reference, hr_time, ph_tests, breaks) {
  if (!inherits(formula, "formula") || length(formula) != 3) {
    rlang::abort("`formula` must look like `Surv(time, status) ~ group`.")
  }
  if (!is.data.frame(data)) rlang::abort("`data` must be a data frame.")
  mf <- stats::model.frame(formula, data)
  y <- mf[[1]]
  if (!inherits(y, "Surv") || !identical(attr(y, "type"), "right")) {
    rlang::abort("The left side of `formula` must be a right censored `Surv()` object.")
  }
  if (ncol(mf) != 2) {
    rlang::abort("The right side of `formula` must be a single grouping variable.")
  }
  group <- mf[[2]]
  group <- if (is.factor(group)) droplevels(group) else factor(group)
  if (!is.null(reference)) {
    if (!reference %in% levels(group)) {
      rlang::abort(paste0("`reference` must be one of: ",
                          paste(levels(group), collapse = ", ")))
    }
    group <- stats::relevel(group, ref = reference)
  }
  if (nlevels(group) < 2) rlang::abort("The grouping variable must have at least two groups.")
  d <- data.frame(time = as.numeric(y[, "time"]), status = as.numeric(y[, "status"]),
                  group = group)
  arms <- levels(group)
  k <- length(arms)
  surv <- survival::Surv(d$time, d$status)
  fit <- survival::survfit(surv ~ group, data = d)
  cox <- survival::coxph(surv ~ group, data = d)
  logrank <- survival::survdiff(surv ~ group, data = d)
  tmax <- max(d$time)
  if (is.null(breaks)) {
    breaks <- pretty(c(0, tmax), 8)
    breaks <- breaks[breaks <= tmax]
  }
  breaks <- sort(unique(c(0, breaks[breaks >= 0])))

  events <- d$time[d$status == 1]
  zph <- if (length(events) > k) survival::cox.zph(cox, transform = "km", terms = FALSE)
  size <- nrow(d) * length(unique(events))
  run_tt <- size <= km_dims$tt_limit
  if (!run_tt && (ph_tests || hr_time %in% c("log", "linear"))) {
    rlang::inform("The time interaction models were skipped: the data are too large.")
  }
  tt_model <- function(f) {
    survival::coxph(surv ~ group + tt(group), data = d,
                    tt = function(x, t, ...) stats::model.matrix(~ x)[, -1, drop = FALSE] * f(t))
  }
  lin <- if (run_tt && (ph_tests || hr_time == "linear")) tt_model(identity)
  logt <- if (run_tt && (ph_tests || hr_time == "log") && all(events > 0)) tt_model(log)

  list(d = d, arms = arms, k = k, fit = fit, cox = cox, logrank = logrank,
       zph = zph, lin = lin, logt = logt, tmax = tmax, breaks = breaks)
}

# Restricted mean survival time at the prespecified horizon and on a grid of
# horizons up to the end of follow-up in the group followed least, from
# survfit(), with each group's difference from the reference.
km_rmst <- function(input, rmst) {
  if (is.null(rmst) || isFALSE(rmst)) return(NULL)
  last <- vapply(input$arms, function(a) max(input$d$time[input$d$group == a]), numeric(1))
  limit <- min(last)
  tau <- if (isTRUE(rmst)) limit else rmst
  if (!is.numeric(tau) || length(tau) != 1 || !is.finite(tau) || tau <= 0) {
    rlang::abort("`rmst` must be `TRUE` or a positive horizon on the time scale.")
  }
  if (tau > limit * (1 + 1e-9)) {
    rlang::abort(paste0("`rmst` is past the end of follow-up in the group followed least (",
                        signif(limit, 3), ")."))
  }
  taus <- sort(unique(c(seq(limit / 80, limit, length.out = 80), tau)))
  at <- lapply(taus, function(t) {
    tb <- summary(input$fit, rmean = t)$table
    if (is.null(dim(tb))) tb <- matrix(tb, nrow = 1, dimnames = list(NULL, names(tb)))
    list(m = unname(tb[, "rmean"]), se = unname(tb[, "se(rmean)"]))
  })
  m <- do.call(rbind, lapply(at, `[[`, "m"))
  se <- do.call(rbind, lapply(at, `[[`, "se"))
  d <- m - m[, 1]
  dse <- sqrt(se^2 + se[, 1]^2)
  dse[, 1] <- 0
  i <- match(tau, taus)
  z <- stats::qnorm(0.975)
  table <- data.frame(group = input$arms, rmst = m[i, ], lower = m[i, ] - z * se[i, ],
                      upper = m[i, ] + z * se[i, ], difference = d[i, ],
                      diff_lower = d[i, ] - z * dse[i, ], diff_upper = d[i, ] + z * dse[i, ],
                      stringsAsFactors = FALSE)
  table[1, c("difference", "diff_lower", "diff_upper")] <- NA
  list(tau = tau, prespecified = !isTRUE(rmst), limit = limit, taus = taus,
       m = m, se = se, d = d, dse = dse, index = i, table = table)
}

# The log hazard ratio of each comparison at `times`, with its standard
# error, from the chosen method. Returns a list of data frames, one per
# non-reference group.
km_hr_curves <- function(input, times, method) {
  cox <- input$cox
  b <- stats::coef(cox)
  v <- stats::vcov(cox)
  k1 <- length(b)
  out <- lapply(seq_len(k1), function(j) {
    data.frame(est = rep(NA_real_, length(times)), se = NA_real_)
  })
  if (method == "none") return(out)
  if (method == "constant") {
    for (j in seq_len(k1)) out[[j]] <- data.frame(est = b[j], se = sqrt(v[j, j]))
    return(lapply(out, function(o) o[rep(1, length(times)), ]))
  }
  if (method %in% c("linear", "log")) {
    m <- if (method == "linear") input$lin else input$logt
    if (is.null(m)) return(out)
    f <- if (method == "linear") identity else function(t) ifelse(t > 0, log(t), NA)
    bm <- stats::coef(m)
    vm <- stats::vcov(m)
    ft <- f(times)
    for (j in seq_len(k1)) {
      g <- k1 + j
      est <- bm[j] + bm[g] * ft
      se <- sqrt(vm[j, j] + ft^2 * vm[g, g] + 2 * ft * vm[j, g])
      out[[j]] <- data.frame(est = est, se = se)
    }
    return(out)
  }
  # Smoothed scaled Schoenfeld residuals, as survival::plot.cox.zph draws them.
  zph <- input$zph
  if (is.null(zph)) return(out)
  df <- km_dims$zph_df
  basis <- splines::ns(zph$x, df = df, intercept = TRUE)
  qmat <- qr(basis)
  bk <- backsolve(qmat$qr[seq_len(df), seq_len(df)], diag(df))
  xtx <- bk %*% t(bk)
  at <- stats::approx(zph$time, zph$x, xout = times, ties = mean, rule = 1)$y
  ok <- !is.na(at)
  if (!any(ok)) return(out)
  pmat <- stats::predict(basis, at[ok])
  spread <- rowSums((pmat %*% xtx) * pmat)
  for (j in seq_len(k1)) {
    est <- rep(NA_real_, length(times))
    se <- rep(NA_real_, length(times))
    est[ok] <- as.vector(pmat %*% qr.coef(qmat, zph$y[, j]))
    se[ok] <- sqrt(zph$var[j, j] * spread)
    out[[j]] <- data.frame(est = est, se = se)
  }
  out
}

# The table of hazard ratios and proportional hazards tests.
km_ph_table <- function(input) {
  arms <- input$arms
  ref <- arms[1]
  z <- stats::qnorm(0.975)
  cox <- summary(input$cox)$coefficients
  ci <- exp(stats::confint(input$cox))
  comps <- paste(arms[-1], "vs", ref)
  hr <- function(e, l, u) sprintf("HR %s (%s, %s)", fmt2(e), fmt2(l), fmt2(u))
  rows <- data.frame(
    test = "Cox model", comparison = comps,
    estimate = hr(exp(cox[, "coef"]), ci[, 1], ci[, 2]),
    statistic = sprintf("z = %s", fmt2(cox[, "z"])), p = cox[, "Pr(>|z|)"],
    stringsAsFactors = FALSE
  )
  lr <- input$logrank
  lr_df <- length(lr$n) - 1
  rows <- rbind(rows, data.frame(
    test = "Log-rank test", comparison = "All groups", estimate = "",
    statistic = sprintf("\u03c7\u00b2 = %s, df %d", fmt2(lr$chisq), lr_df),
    p = stats::pchisq(lr$chisq, lr_df, lower.tail = FALSE), stringsAsFactors = FALSE
  ))
  if (!is.null(input$zph)) {
    zt <- input$zph$table
    last <- nrow(zt)
    rows <- rbind(rows, data.frame(
      test = "Schoenfeld residuals",
      comparison = c(comps, "Global"),
      estimate = "",
      statistic = c(sprintf("\u03c7\u00b2 = %s", fmt2(zt[-last, "chisq"])),
                    sprintf("\u03c7\u00b2 = %s, df %d", fmt2(zt[last, "chisq"]), as.integer(zt[last, "df"]))),
      p = zt[, "p"], stringsAsFactors = FALSE
    ))
  }
  interaction <- function(m, name, unit) {
    if (is.null(m)) return(NULL)
    s <- summary(m)$coefficients
    k1 <- length(arms) - 1
    g <- k1 + seq_len(k1)
    est <- s[g, "coef"]
    vg <- stats::vcov(m)[g, g, drop = FALSE]
    wald <- as.numeric(t(est) %*% solve(vg) %*% est)
    data.frame(
      test = name, comparison = c(comps, "Joint"),
      estimate = c(sprintf("%s per %s", formatC(est, format = "f", digits = 3), unit), ""),
      statistic = c(sprintf("z = %s", fmt2(s[g, "z"])),
                    sprintf("\u03c7\u00b2 = %s, df %d", fmt2(wald), k1)),
      p = c(s[g, "Pr(>|z|)"], stats::pchisq(wald, k1, lower.tail = FALSE)),
      stringsAsFactors = FALSE
    )
  }
  out <- rbind(rows, interaction(input$lin, "Group \u00d7 time", "unit of time"),
               interaction(input$logt, "Group \u00d7 log time", "unit of log time"))
  rownames(out) <- NULL
  out
}

fmt2 <- function(v) formatC(v, format = "f", digits = 2)
fmt_p <- function(p) ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3))

km_test_notes <- c(
  "Cox model" = paste(
    "Hazard ratio against the reference group from a Cox model with the",
    "group as its only covariate. It assumes the ratio is the same at every time."),
  "Log-rank test" = "Tests whether survival differs between the groups at all.",
  "Schoenfeld residuals" = paste(
    "Grambsch and Therneau test of whether the scaled Schoenfeld residuals",
    "trend with time, on the Kaplan-Meier time scale. A small p value suggests",
    "the hazard ratio changes over follow-up."),
  "Group \u00d7 time" = paste(
    "Cox model with an interaction between the group and time: the estimate is",
    "how much the log hazard ratio changes per unit of time."),
  "Group \u00d7 log time" = paste(
    "Cox model with an interaction between the group and log time: the estimate",
    "is how much the log hazard ratio changes per unit of log time.")
)

# Positions, strings and cards, in points on a top down canvas.
km_layout <- function(input, type, hr_time, ph_tests, risk_table, conf_int,
                      palette, xlab, ylab, legend, legend_title, title,
                      caption, family) {
  dims <- km_dims
  arms <- input$arms
  k <- input$k
  width <- function(s, pt = dims$text_pt, bold = FALSE) {
    if (!length(s)) return(0)
    max(text_width_card(as.character(s), pt, family, 1, bold = bold))
  }
  colors <- km_colors(arms, palette)
  prob <- function(s) if (type == "survival") s else 1 - s
  pct <- function(v) ifelse(is.na(v), "", paste0(formatC(100 * v, format = "f", digits = 1), "%"))

  # Columns.
  label_w <- max(width(arms) + 14, width("100%", dims$small_pt) + 6)
  plot_x0 <- label_w + dims$gap
  plot_x1 <- plot_x0 + dims$plot_w
  tmax <- input$tmax
  X <- function(t) plot_x0 + pmin(t, tmax) / tmax * dims$plot_w
  plot_top <- 18
  plot_bottom <- plot_top + dims$plot_h
  Y <- function(s) plot_bottom - s * dims$plot_h

  # The curves as steps, with their bands.
  s <- summary(input$fit, censored = TRUE)
  strata <- sub("^group=", "", as.character(s$strata))
  curves <- lapply(arms, function(a) {
    i <- strata == a
    list(t = s$time[i], s = s$surv[i], lo = s$lower[i], hi = s$upper[i],
         cens = s$n.censor[i])
  })

  # Readouts on a grid of slices across the time axis.
  n <- dims$slices
  edges <- seq(0, tmax, length.out = n + 1)
  mids <- (edges[-1] + edges[-(n + 1)]) / 2
  grid <- summary(input$fit, times = mids, extend = TRUE)
  gstrata <- sub("^group=", "", as.character(grid$strata))
  at <- lapply(arms, function(a) {
    i <- gstrata == a
    list(s = grid$surv[i], lo = grid$lower[i], hi = grid$upper[i],
         risk = grid$n.risk[i], events = cumsum(grid$n.event[i]),
         censored = cumsum(grid$n.censor[i]))
  })
  hr_methods <- c(schoenfeld = "smoothed Schoenfeld residuals",
                  log = "group by log time model", linear = "group by time model",
                  constant = "Cox model")
  hrs <- km_hr_curves(input, mids, hr_time)
  z <- stats::qnorm(0.975)
  hr_text <- function(j, i) {
    e <- hrs[[j]]$est[i]
    if (is.na(e)) return("not estimable here")
    se <- hrs[[j]]$se[i]
    sprintf("%s (%s, %s)", fmt2(exp(e)), fmt2(exp(e - z * se)), fmt2(exp(e + z * se)))
  }
  interval <- function(a, i) {
    v <- at[[a]]
    if (type == "survival") paste0(pct(v$s[i]), " (", pct(v$lo[i]), ", ", pct(v$hi[i]), ")")
    else paste0(pct(1 - v$s[i]), " (", pct(1 - v$hi[i]), ", ", pct(1 - v$lo[i]), ")")
  }
  time_text <- function(t) format(signif(t, 3), trim = TRUE)
  nearest <- vapply(mids, function(t) which.min(abs(input$breaks - t)), integer(1))
  slice_ids <- paste0("t", seq_len(n), "_", nearest)
  slice_tip <- vapply(seq_len(n), function(i) {
    arms_html <- paste0(vapply(seq_len(k), function(a) {
      paste0('<div class="ggx-tip-row"><span><i class="ggx-dot" style="background:',
             colors[a], '"></i>', esc(arms[a]), "</span><span>", esc(interval(a, i)),
             "</span></div>",
             '<div class="ggx-tip-row"><span></span><span>', at[[a]]$risk[i], " at risk, ",
             at[[a]]$events[i], if (at[[a]]$events[i] == 1) " event" else " events",
             "</span></div>")
    }, character(1)), collapse = "")
    hr_html <- if (hr_time != "none") {
      paste0('<div class="ggx-tip-sub" style="margin-top:6px">Hazard ratio vs ', esc(arms[1]),
             ", from the ", hr_methods[[hr_time]], "</div>",
             tip_rows(stats::setNames(vapply(seq_len(k - 1), hr_text, character(1), i = i),
                                      arms[-1])))
    }
    paste0('<div class="ggx-tip-title">', esc(xlab), ": ", time_text(mids[i]), "</div>",
           '<div class="ggx-tip-sub">', esc(ylab), " (95% CI), at risk and events so far</div>",
           arms_html, hr_html,
           '<div class="ggx-tip-hint">Click for the full readout at this time.</div>')
  }, character(1))
  slice_click <- pin_js(slice_ids, vapply(seq_len(n), function(i) {
    fitted <- names(hr_methods)[c(!is.null(input$zph), !is.null(input$logt),
                                   !is.null(input$lin), TRUE)]
    all_hr <- lapply(fitted, function(m) {
      h <- km_hr_curves(input, mids[i], m)
      vapply(h, function(o) {
        if (is.na(o$est)) "" else sprintf("%s (%s, %s)", fmt2(exp(o$est)),
                                          fmt2(exp(o$est - z * o$se)), fmt2(exp(o$est + z * o$se)))
      }, character(1))
    })
    hr_rows <- paste0(vapply(seq_along(fitted), function(m) {
      paste0("<tr><th scope=\"row\">", esc(capitalize(hr_methods[[fitted[m]]])), "</th>",
             paste0("<td>", esc(all_hr[[m]]), "</td>", collapse = ""), "</tr>")
    }, character(1)), collapse = "")
    arm_rows <- paste0(vapply(seq_len(k), function(a) {
      paste0("<tr><th scope=\"row\">", esc(arms[a]), "</th><td>", esc(interval(a, i)),
             "</td><td>", at[[a]]$risk[i], "</td><td>", at[[a]]$events[i], "</td><td>",
             at[[a]]$censored[i], "</td></tr>")
    }, character(1)), collapse = "")
    paste0('<div class="ggx-title">', esc(xlab), ": ", time_text(mids[i]), "</div>",
           '<div class="ggx-table"><table><thead><tr><td></td><th scope="col">',
           esc(ylab), ' (95% CI)</th><th scope="col">At risk</th><th scope="col">Events</th>',
           '<th scope="col">Censored</th></tr></thead><tbody class="ggx-num">', arm_rows,
           "</tbody></table></div>",
           '<div class="ggx-refs-head">Hazard ratio against ', esc(arms[1]), " at this time</div>",
           '<div class="ggx-table"><table><thead><tr><td></td>',
           paste0('<th scope="col">', esc(arms[-1]), "</th>", collapse = ""),
           '</tr></thead><tbody class="ggx-num">', hr_rows, "</tbody></table></div>")
  }, character(1)))

  # Group summaries.
  tb <- summary(input$fit)$table
  if (is.null(dim(tb))) tb <- matrix(tb, nrow = 1, dimnames = list(NULL, names(tb)))
  cox_s <- summary(input$cox)$coefficients
  cox_ci <- exp(stats::confint(input$cox))
  at_breaks <- summary(input$fit, times = input$breaks, extend = TRUE)
  bstrata <- sub("^group=", "", as.character(at_breaks$strata))
  median_text <- function(a) {
    m <- tb[a, "median"]
    if (is.na(m)) return("not reached")
    paste0(time_text(m), " (", if (is.na(tb[a, "0.95LCL"])) "?" else time_text(tb[a, "0.95LCL"]),
           ", ", if (is.na(tb[a, "0.95UCL"])) "not reached" else time_text(tb[a, "0.95UCL"]), ")")
  }
  arm_hr <- function(a) {
    if (a == 1) return("reference")
    j <- a - 1
    sprintf("%s (%s, %s)", fmt2(exp(cox_s[j, "coef"])), fmt2(cox_ci[j, 1]), fmt2(cox_ci[j, 2]))
  }
  arm_ids <- paste0("a", seq_len(k))
  arm_tip <- vapply(seq_len(k), function(a) {
    paste0('<div class="ggx-tip-title">', esc(arms[a]), "</div>",
           tip_rows(c(Participants = tb[a, "records"], Events = tb[a, "events"],
                      "Median (95% CI)" = median_text(a),
                      stats::setNames(arm_hr(a), paste("Hazard ratio vs", arms[1])))),
           '<div class="ggx-tip-hint">Click for survival at each time on the axis.</div>')
  }, character(1))
  arm_click <- pin_js(arm_ids, vapply(seq_len(k), function(a) {
    i <- bstrata == arms[a]
    rows <- paste0("<tr><th scope=\"row\">", time_text(at_breaks$time[i]), "</th><td>",
                   pct(prob(at_breaks$surv[i])), "</td><td>", at_breaks$n.risk[i],
                   "</td><td>", cumsum(at_breaks$n.event[i]), "</td></tr>", collapse = "")
    paste0('<div class="ggx-title">', esc(arms[a]), "</div>",
           '<div class="ggx-sub">', tb[a, "records"], " participants, ", tb[a, "events"],
           " events; median ", esc(median_text(a)), "; hazard ratio vs ", esc(arms[1]), " ",
           esc(arm_hr(a)), "</div>",
           '<div class="ggx-table"><table><thead><tr><th scope="col">', esc(xlab),
           '</th><th scope="col">', esc(ylab), '</th><th scope="col">At risk</th>',
           '<th scope="col">Events so far</th></tr></thead><tbody class="ggx-num">', rows,
           "</tbody></table></div>")
  }, character(1)))

  # Risk table.
  y <- plot_bottom + 34
  risk <- NULL
  if (risk_table) {
    risk_head_y <- y + 12
    rows_y <- risk_head_y + seq_len(k) * dims$row_h
    risk <- list(head_y = risk_head_y, y = rows_y, cells = lapply(seq_len(k), function(a) {
      i <- bstrata == arms[a]
      list(n = at_breaks$n.risk[i], events = cumsum(at_breaks$n.event[i]))
    }))
    y <- max(rows_y) + 8
  }
  rmst <- NULL
  if (!is.null(input$rmst)) {
    head_y <- y + 16
    rows_y <- head_y + 4 + seq_len(k) * dims$row_h
    rmst <- c(input$rmst, list(head_y = head_y, y = rows_y, col_rm = plot_x0,
                               col_diff = plot_x0 + 200))
    y <- max(rows_y) + 10
  }

  # Hazard ratios and proportional hazards tests, as an HTML section the
  # widget adds under the plot, collapsed.
  ph_table <- NULL
  ph_html <- NULL
  if (ph_tests) {
    ph_table <- km_ph_table(input)
    body <- paste0(
      "<tr><th scope=\"row\">", esc(ph_table$test), "</th><td>", esc(ph_table$comparison),
      "</td><td>", esc(ph_table$estimate), "</td><td>", esc(ph_table$statistic),
      "</td><td>", fmt_p(ph_table$p), "</td></tr>", collapse = ""
    )
    tests <- unique(ph_table$test)
    ph_html <- paste0(
      '<div class="ggx-table"><table><thead><tr><th scope="col">Test</th>',
      '<th scope="col">Comparison</th><th scope="col">Estimate</th>',
      '<th scope="col">Statistic</th><th scope="col">p</th></tr></thead>',
      '<tbody>', body, "</tbody></table></div>",
      '<dl class="ggx-notes-list">',
      paste0("<dt>", esc(tests), "</dt><dd>", esc(km_test_notes[tests]), "</dd>",
             collapse = ""),
      "</dl>"
    )
  }

  right <- plot_x1 + 8
  page <- graph_canvas(c(0, right), c(0, y), title, caption,
                       if (legend) arms else character(0), if (legend) colors else character(0),
                       legend_title, dims, family)

  list(
    dims = dims, page = page, family = family, type = type, conf_int = conf_int,
    arms = arms, k = k, colors = colors, X = X, Y = Y, prob = prob, tmax = tmax,
    plot_x0 = plot_x0, plot_x1 = plot_x1, plot_top = plot_top, plot_bottom = plot_bottom,
    xlab = xlab, ylab = ylab, breaks = input$breaks, curves = curves,
    edges = edges, slice_ids = slice_ids, slice_tip = slice_tip, slice_click = slice_click,
    arm_ids = arm_ids, arm_tip = arm_tip, arm_click = arm_click,
    risk = risk, ph_table = ph_table, ph_html = ph_html, time_text = time_text,
    rmst = rmst
  )
}

km_colors <- function(arms, palette) {
  base <- c(race_ink$timeline, "#757CC6", "#22928F", "#B66399", "#C28A4A",
            "#568E4F", "#C76253", "#7A5178")
  out <- stats::setNames(rep_len(base, length(arms)), arms)
  if (!is.null(palette)) {
    if (is.null(names(palette))) {
      out[] <- rep_len(palette, length(arms))
    } else {
      hit <- intersect(names(palette), arms)
      out[hit] <- palette[hit]
    }
  }
  unname(out)
}

# Draw a layout. `until` draws the curves only up to that time, which is how
# animate_km() draws each frame.
km_draw <- function(lay, until = NULL) {
  dims <- lay$dims
  px <- lay$page$px
  py <- lay$page$py
  X <- lay$X
  Y <- lay$Y
  k <- lay$k
  interactive <- is.null(until)
  limit <- if (is.null(until)) lay$tmax else until
  texts <- function(label, x, y, pt, color, hjust = 0, face = "plain") {
    if (!length(label)) return(empty_texts())
    data.frame(x = px(x), y = py(y), label = as.character(label), size = pt / .pt,
               colour = color, hjust = hjust, fontface = face, stringsAsFactors = FALSE)
  }
  segs <- function(x0, x1, y0, y1, colour) {
    data.frame(x = px(x0), xend = px(x1), y = py(y0), yend = py(y1), colour = colour,
               stringsAsFactors = FALSE)
  }

  # Steps and bands, cut at `limit`.
  step_xy <- function(t, v, start) {
    keep <- t <= limit
    t <- t[keep]
    v <- v[keep]
    xs <- c(0, rep(t, each = 2), limit)
    ys <- c(start, start, rep(v, each = 2))
    ys <- ys[seq_along(xs)]
    if (length(v)) ys[length(ys)] <- v[length(v)]
    list(x = xs, y = ys)
  }
  lines <- NULL
  bands <- NULL
  ticks <- NULL
  for (a in seq_len(k)) {
    cv <- lay$curves[[a]]
    st <- step_xy(cv$t, lay$prob(cv$s), lay$prob(1))
    lines <- rbind(lines, data.frame(x = px(X(st$x)), y = py(Y(st$y)), id = lay$arm_ids[a],
                                     tooltip = lay$arm_tip[a], onclick = lay$arm_click[a],
                                     colour = lay$colors[a],
                                     hover = sprintf("stroke:%s;stroke-width:3px;", lay$colors[a]),
                                     stringsAsFactors = FALSE))
    if (lay$conf_int) {
      lo <- if (lay$type == "survival") cv$lo else 1 - cv$hi
      hi <- if (lay$type == "survival") cv$hi else 1 - cv$lo
      lo[is.na(lo)] <- lay$prob(cv$s)[is.na(lo)]
      hi[is.na(hi)] <- lay$prob(cv$s)[is.na(hi)]
      up <- step_xy(cv$t, hi, lay$prob(1))
      dn <- step_xy(cv$t, lo, lay$prob(1))
      bands <- rbind(bands, data.frame(x = px(X(c(up$x, rev(dn$x)))),
                                       y = py(Y(c(up$y, rev(dn$y)))),
                                       group = a, fill = lay$colors[a],
                                       stringsAsFactors = FALSE))
    }
    cens <- cv$cens > 0 & cv$t <= limit
    if (any(cens)) {
      yv <- Y(lay$prob(cv$s[cens]))
      ticks <- rbind(ticks, segs(X(cv$t[cens]), X(cv$t[cens]), yv - 3, yv + 3, lay$colors[a]))
    }
  }

  # Axes and grid.
  yt <- c(0, 0.25, 0.5, 0.75, 1)
  grid <- rbind(
    segs(lay$plot_x0, lay$plot_x1, Y(yt), Y(yt), graph_ink$faint),
    segs(lay$plot_x0, lay$plot_x1, lay$plot_bottom, lay$plot_bottom, graph_ink$text),
    segs(X(lay$breaks), X(lay$breaks), lay$plot_bottom, lay$plot_bottom + 4, graph_ink$text)
  )
  labels <- rbind(
    texts(paste0(yt * 100, "%"), rep(lay$plot_x0 - 6, 5), Y(yt), dims$small_pt,
          graph_ink$muted, hjust = 1),
    texts(lay$time_text(lay$breaks), X(lay$breaks), rep(lay$plot_bottom + 13, length(lay$breaks)),
          dims$small_pt, graph_ink$muted, hjust = 0.5),
    texts(lay$xlab, (lay$plot_x0 + lay$plot_x1) / 2, lay$plot_bottom + 27, dims$text_pt,
          graph_ink$text, hjust = 0.5),
    texts(lay$ylab, 0, lay$plot_top - 12, dims$small_pt, graph_ink$muted)
  )
  if (!is.null(until)) {
    labels <- rbind(texts(lay$time_text(round(until, 1)), lay$plot_x1 - 4, lay$plot_top + 30,
                          40, graph_ink$watermark, hjust = 1, face = "bold"), labels)
  }

  # Risk table.
  cells <- NULL
  if (!is.null(lay$risk)) {
    r <- lay$risk
    labels <- rbind(labels, texts("Number at risk", 0, r$head_y, dims$text_pt,
                                  graph_ink$title, face = "bold"))
    a <- seq(0, 2 * pi, length.out = 17)
    for (i in seq_len(k)) {
      labels <- rbind(labels, texts(lay$arms[i], 11, r$y[i], dims$small_pt, graph_ink$text))
      shown <- lay$breaks <= limit + 1e-9
      cells <- rbind(cells, data.frame(
        x = px(X(lay$breaks)), y = py(rep(r$y[i], length(lay$breaks))),
        label = r$cells[[i]]$n, id = paste0("r", seq_along(lay$breaks), "_", i),
        tooltip = paste0('<div class="ggx-tip-title">', esc(lay$arms[i]), "</div>",
                         '<div class="ggx-tip-sub">', esc(lay$xlab), " ",
                         esc(lay$time_text(lay$breaks)), "</div>",
                         tip_rows(stats::setNames(c(r$cells[[i]]$n, r$cells[[i]]$events),
                                                  c("At risk", "Events so far")))),
        colour = ifelse(shown, graph_ink$text, graph_ink$ghost), stringsAsFactors = FALSE
      ))
    }
    dots <- do.call(rbind, lapply(seq_len(k), function(i) {
      data.frame(x = px(4 + 3.5 * cos(a)), y = py(r$y[i] + 3.5 * sin(a)),
                 group = 100 + i, fill = lay$colors[i], stringsAsFactors = FALSE)
    }))
  } else {
    dots <- NULL
  }

  # Restricted mean survival: the area under each curve up to the horizon,
  # the horizon itself and a table of the means under the risk table.
  rm_layers <- NULL
  if (!is.null(lay$rmst) && interactive) {
    rm <- lay$rmst
    # The reference group's mean is the area under its curve; each other
    # group's difference from it is the area between the two curves.
    ref_pts <- km_area(lay$curves[[1]], rm$tau)
    area <- do.call(rbind, lapply(seq_len(k), function(a) {
      if (a == 1) {
        tt <- c(ref_pts$t, rm$tau, 0)
        ss <- c(ref_pts$s, 0, 0)
        fill <- graph_ink$muted
        alpha <- 0.1
      } else {
        pts <- km_area(lay$curves[[a]], rm$tau)
        tt <- c(pts$t, rev(ref_pts$t))
        ss <- c(pts$s, rev(ref_pts$s))
        fill <- lay$colors[a]
        alpha <- 0.3
      }
      data.frame(x = px(X(tt)), y = py(Y(lay$prob(ss))), id = paste0("rma", a), fill = fill,
                 alpha = alpha, stringsAsFactors = FALSE)
    }))
    tau_line <- data.frame(x = px(X(rm$tau)), y = py(lay$plot_top), yend = py(lay$plot_bottom),
                           id = "rmt")
    txt <- km_rmst_text(lay, rm$index)
    words <- rbind(
      data.frame(x = px(X(rm$tau) + 4), y = py(lay$plot_top + 6), label = txt$tau, id = "rml",
                 hjust = 0, colour = graph_ink$title, face = "bold", stringsAsFactors = FALSE),
      data.frame(x = px(0), y = py(rm$head_y), label = txt$head, id = "rmh", hjust = 0,
                 colour = graph_ink$title, face = "bold", stringsAsFactors = FALSE),
      data.frame(x = px(c(rm$col_rm, rm$col_diff)), y = py(rep(rm$head_y + 14, 2)),
                 label = c(txt$col_rm, txt$col_diff), id = c("rmc1", "rmc2"), hjust = 0,
                 colour = graph_ink$muted, face = "plain", stringsAsFactors = FALSE),
      data.frame(x = px(rep(rm$col_rm, k)), y = py(rm$y + 4), label = txt$rm,
                 id = paste0("rmv", seq_len(k)), hjust = 0, colour = graph_ink$text, face = "plain",
                 stringsAsFactors = FALSE),
      data.frame(x = px(rep(rm$col_diff, k)), y = py(rm$y + 4), label = txt$diff,
                 id = paste0("rmd", seq_len(k)), hjust = 0, colour = graph_ink$text, face = "plain",
                 stringsAsFactors = FALSE)
    )
    arm_words <- texts(lay$arms, rep(11, k), rm$y + 4, dims$small_pt, graph_ink$text)
    a <- seq(0, 2 * pi, length.out = 17)
    rm_dots <- do.call(rbind, lapply(seq_len(k), function(i) {
      data.frame(x = px(4 + 3.5 * cos(a)), y = py(rm$y[i] + 4 + 3.5 * sin(a)),
                 group = 200 + i, fill = lay$colors[i], stringsAsFactors = FALSE)
    }))
    pre <- if (rm$prespecified) {
      data.frame(x = px(X(rm$tau)), y = py(lay$plot_top), yend = py(lay$plot_bottom))
    }
    rm_layers <- list(
      ggiraph::geom_polygon_interactive(
        data = area, aes(x = .data$x, y = .data$y, group = .data$id, fill = .data$fill,
                         alpha = .data$alpha, data_id = .data$id),
        colour = NA),
      if (!is.null(pre)) geom_segment(data = pre, aes(x = .data$x, xend = .data$x, y = .data$y, yend = .data$yend),
                                      colour = "#7A5AA6", linewidth = 1.2 / .pt, linetype = "42"),
      if (!is.null(pre)) draw_text(texts("prespecified", X(rm$tau) + 4, lay$plot_bottom - 7,
                                         dims$small_pt, "#7A5AA6"), lay$family),
      ggiraph::geom_segment_interactive(
        data = tau_line, aes(x = .data$x, xend = .data$x, y = .data$y, yend = .data$yend,
                             data_id = .data$id),
        colour = graph_ink$title, linewidth = 1.4 / .pt),
      ggiraph::geom_text_interactive(
        data = words, aes(x = .data$x, y = .data$y, label = .data$label, data_id = .data$id,
                          hjust = .data$hjust, fontface = .data$face),
        size = dims$small_pt / .pt, colour = words$colour, family = lay$family),
      draw_text(arm_words, lay$family),
      draw_shapes(rm_dots)
    )
  }

  slices <- if (interactive) {
    data.frame(xmin = px(X(lay$edges[-length(lay$edges)])), xmax = px(X(lay$edges[-1])),
               ymin = py(lay$plot_bottom), ymax = py(lay$plot_top),
               id = lay$slice_ids, tooltip = lay$slice_tip, onclick = lay$slice_click,
               hover = paste0("fill:", hover_ink, ";fill-opacity:0.06;stroke:none;"),
               stringsAsFactors = FALSE)
  }

  p <- ggplot() +
    geom_segment(data = grid, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
                 colour = grid$colour, linewidth = 0.7 / .pt) +
    (if (!is.null(bands)) geom_polygon(data = bands,
                                       aes(x = .data$x, y = .data$y, group = .data$group),
                                       fill = bands$fill, alpha = 0.14)) +
    rm_layers +
    km_rects(slices) +
    ggiraph::geom_path_interactive(
      data = lines,
      aes(x = .data$x, y = .data$y, group = .data$id, colour = .data$colour,
          data_id = .data$id, tooltip = .data$tooltip, onclick = .data$onclick,
          hover_css = .data$hover),
      linewidth = 1.8 / .pt, lineend = "butt", linejoin = "mitre"
    ) +
    (if (!is.null(ticks)) geom_segment(data = ticks,
                                       aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
                                       colour = ticks$colour, linewidth = 1 / .pt)) +
    draw_shapes(dots) +
    (if (!is.null(cells)) ggiraph::geom_text_interactive(
      data = cells,
      aes(x = .data$x, y = .data$y, label = .data$label, data_id = .data$id,
          tooltip = .data$tooltip),
      size = dims$small_pt / .pt, colour = cells$colour, family = lay$family
    )) +
    draw_text(labels, lay$family) +
    graph_frame(lay$page, lay$family)
  p
}

# A curve's steps up to `tau`, as the upper edge of the area under it.
km_area <- function(cv, tau) {
  keep <- cv$t <= tau
  tt <- cv$t[keep]
  ss <- cv$s[keep]
  prev <- c(1, utils::head(ss, -1))
  list(t = c(0, as.vector(rbind(tt, tt)), tau),
       s = c(1, as.vector(rbind(prev, ss)), if (length(ss)) ss[length(ss)] else 1))
}

km_rmst_fmt <- function(v, limit) formatC(v, format = "f", digits = if (limit >= 100) 0 else if (limit >= 10) 1 else 2)
km_tau_fmt <- function(v, limit) {
  if (abs(v - round(v)) < 1e-9) format(round(v)) else km_rmst_fmt(v, limit)
}

# The words of the RMST table at the horizon with index `i`.
km_rmst_text <- function(lay, i) {
  rm <- lay$rmst
  f <- function(v) km_rmst_fmt(v, rm$limit)
  z <- stats::qnorm(0.975)
  what <- if (lay$type == "survival") "Restricted mean survival time" else "Restricted mean time free of the event"
  list(
    tau = paste0("\u03c4 = ", km_tau_fmt(rm$taus[i], rm$limit)),
    head = paste0(what, " up to \u03c4 = ", km_tau_fmt(rm$taus[i], rm$limit)),
    col_rm = "Mean (95% CI)",
    col_diff = paste("Difference from", lay$arms[1], "(95% CI)"),
    rm = paste0(f(rm$m[i, ]), " (", f(rm$m[i, ] - z * rm$se[i, ]), " to ", f(rm$m[i, ] + z * rm$se[i, ]), ")"),
    diff = c("reference", paste0(f(rm$d[i, -1]), " (", f(rm$d[i, -1] - z * rm$dse[i, -1]), " to ",
                                 f(rm$d[i, -1] + z * rm$dse[i, -1]), ")"))
  )
}

# What the widget needs to move the horizon.
km_rmst_data <- function(lay) {
  rm <- lay$rmst
  list(taus = I(rm$taus), m = rm$m, se = rm$se, d = rm$d, dse = rm$dse,
       start = rm$index - 1L, prespecified = rm$prespecified, limit = rm$limit,
       arms = I(lay$arms), colors = I(lay$colors), type = lay$type,
       curves = lapply(lay$curves, function(cv) list(t = I(cv$t), s = I(cv$s))),
       dx = lay$page$px(0), dy = lay$page$height - lay$page$py(0),
       x0 = lay$plot_x0, x1 = lay$plot_x1, tmax = lay$tmax, top = lay$plot_top,
       bottom = lay$plot_bottom, xlab = lay$xlab)
}

km_rects <- function(df) {
  if (is.null(df) || !nrow(df)) return(NULL)
  ggiraph::geom_rect_interactive(
    data = df,
    aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, ymax = .data$ymax,
        data_id = .data$id, tooltip = .data$tooltip, onclick = .data$onclick,
        hover_css = .data$hover),
    fill = "#FFFFFF02", colour = NA
  )
}
