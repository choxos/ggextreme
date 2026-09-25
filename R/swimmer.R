#' Draw an interactive swimmer plot
#'
#' Draws one lane per patient: a bar for the time on treatment or on study,
#' marks for the events along it, such as responses, progression and death,
#' and an arrow for patients still ongoing. Hovering over a lane shows the
#' patient's group, the columns named in `hover`, the first time of each
#' event and whether they are ongoing, and fades the other lanes. Clicking
#' it opens the patient's events in order under the plot, beside their full
#' record. Buttons under the widget reorder the lanes, which slide into
#' place.
#'
#' With `waterfall`, each lane also gets a bar for the patient's best change
#' from baseline, beside the lanes, so the swimmer plot and the waterfall
#' plot share one row per patient; an order by best change sorts both. With
#' `trajectories`, each patient's course of change over time is drawn under
#' the lanes on the same time axis. Every mark of a patient in the three
#' panels shares one id, so hovering over any of them lights the patient in
#' all three, and the response and progression thresholds are marked on
#' both change panels, with a count of the patients past each.
#'
#' Events are drawn by their wording: a complete response as a star, a
#' partial response or other response as a triangle, progression, relapse or
#' recurrence as a diamond, and death as a cross, with any other event as a
#' circle or square in its own color.
#'
#' @param data A data frame with one row per patient.
#' @param id Column of `data` identifying the patient.
#' @param end Column of `data`, or an expression of its columns, giving the
#'   time each bar ends.
#' @param events Optional data frame with one row per event: the patient's
#'   id in a column named as `id` is in `data`, the time in `time` and what
#'   happened in `event`.
#' @param group Optional column of `data`, such as the arm, that colors the
#'   bars.
#' @param ongoing Optional logical column of `data`, or an expression of its
#'   columns, marking patients still ongoing at `end`, drawn with an arrow.
#' @param start Optional column of `data` giving the time each bar starts.
#'   Defaults to zero.
#' @param hover Names of columns of `data` shown in each lane's hover card.
#' @param sort Order of the lanes in a static copy and when the widget
#'   opens: `"duration"`, the longest first; `"group"`; `"response"`, those
#'   with a complete response first, then a partial one; `"change"`, the
#'   largest decrease in `waterfall` first; or `"data"`, the order of
#'   `data`.
#' @param palette Colors for the groups, unnamed in level order or named by
#'   group.
#' @param ongoing_label What the arrow means, for the legend and the hover
#'   card, such as `"On treatment"` or `"Alive at last follow-up"`.
#' @param xlab Axis label. Include the unit, such as `"Months since first
#'   dose"`.
#' @param legend Draw a legend of the groups and events above the plot.
#' @param title,caption Title above the plot and note below it.
#' @param family Font family. The package ships Lato and registers it on load.
#' @param waterfall Optional column of `data`, or an expression of its
#'   columns, giving each patient's best percent change from baseline, such as
#'   in the sum of target lesion diameters. A missing value is marked not
#'   evaluable.
#' @param trajectories Optional data frame with one row per assessment: the
#'   patient's id in a column named as `id` is in `data`, the time in `time`
#'   on the same scale as `end`, and the percent change from baseline in
#'   `change`. A patient's line starts from no change at the start of the
#'   lane when no assessment comes first.
#' @param thresholds The percent decrease that counts as a response and the
#'   percent increase that counts as progression, marked on the change
#'   panels. The default, `c(-30, 20)`, is the rule for target lesions in
#'   RECIST 1.1, which also counts new lesions and other progression; the
#'   panels show only the measured change.
#' @param change_label Axis label for the percent change.
#'
#' @return An object of class `ggswimmer`, which prints as an interactive
#'   widget. Use [graph_widget()], [graph_plot()] or [graph_save()] for the
#'   widget, a static ggplot or a file.
#' @export
#'
#' @examples
#' if (requireNamespace("survival", quietly = TRUE)) {
#'   aml <- subset(survival::myeloid, id <= 30)
#'   month <- 30.44
#'   events <- rbind(
#'     data.frame(id = aml$id, time = aml$crtime / month, event = "Complete response"),
#'     data.frame(id = aml$id, time = aml$txtime / month, event = "Transplant"),
#'     data.frame(id = aml$id, time = aml$rltime / month, event = "Relapse"),
#'     data.frame(id = aml$id, time = ifelse(aml$death == 1, aml$futime / month, NA),
#'                event = "Death")
#'   )
#'   events <- events[!is.na(events$time), ]
#'   ggswimmer(aml, id, futime / month, events = events, group = trt,
#'             ongoing = death == 0, hover = c("sex", "flt3"),
#'             ongoing_label = "Alive at last follow-up",
#'             xlab = "Months since randomization")
#' }
ggswimmer <- function(data, id, end, events = NULL, group = NULL, ongoing = NULL,
                      start = NULL, hover = NULL,
                      sort = c("duration", "group", "response", "change", "data"),
                      palette = NULL,
                      ongoing_label = "Ongoing",
                      xlab = "Time",
                      legend = TRUE,
                      title = NULL,
                      caption = NULL,
                      family = "Lato",
                      waterfall = NULL,
                      trajectories = NULL,
                      thresholds = c(-30, 20),
                      change_label = "Best change from baseline (%)") {
  sort <- match.arg(sort)
  if (!is.numeric(thresholds) || length(thresholds) != 2 || anyNA(thresholds) ||
      thresholds[1] >= 0 || thresholds[2] <= 0) {
    rlang::abort("`thresholds` must be a negative and a positive percent change, such as c(-30, 20).")
  }
  if (!is.data.frame(data)) rlang::abort("`data` must be a data frame.")
  q_id <- rlang::enquo(id)
  get <- function(q) {
    if (rlang::quo_is_null(q)) return(NULL)
    v <- rlang::eval_tidy(q, data)
    if (length(v) == 1 && nrow(data) != 1) v <- rep(v, nrow(data))
    if (length(v) != nrow(data)) rlang::abort(paste0("`", rlang::as_label(q), "` must give one value per row of `data`."))
    v
  }
  q_group <- rlang::enquo(group)
  input <- swim_prepare(
    data, ids = get(q_id), end = get(rlang::enquo(end)), start = get(rlang::enquo(start)),
    group = get(q_group), ongoing = get(rlang::enquo(ongoing)),
    id_name = if (rlang::quo_is_symbol(q_id)) rlang::as_name(q_id) else "id",
    events = events, hover = hover, change = get(rlang::enquo(waterfall)),
    trajectories = trajectories
  )
  input$group_name <- if (rlang::quo_is_symbol(q_group)) {
    column_labels(data, rlang::as_name(q_group))
  } else "Group"
  lay <- swim_layout(input, sort, palette, ongoing_label, xlab, legend, title, caption, family,
                     thresholds, change_label)
  out <- list(plot = swim_draw(lay), width = lay$page$width / 72, height = lay$page$height / 72,
              title = title, on_render = "ggextremeSwimmer(el, data);",
              render_data = lay$render, hover_inv = "opacity:0.3;", layout = lay)
  if (!is.null(lay$wf)) out$response <- lay$wf$summary
  structure(out, class = c("ggswimmer", "ggx_graph"))
}

swim_dims <- utils::modifyList(graph_dims, list(
  plot_w = 460,
  row_h = 17,
  bar_frac = 0.52,
  text_pt = 10,
  small_pt = 9,
  marker = 5,
  gap = 12,
  key_gap = 16,
  waterfall_w = 170,
  waterfall_gap = 44,
  spider_h = 170
))

swim_change_ink <- c(resp = "#2E7D55", stable = "#9A9A9A", prog = "#C0603F")

# Ticks for percent change, with a full decrease marked when it is in range.
swim_change_breaks <- function(lo, hi) {
  b <- scales::breaks_extended(5)(c(lo, hi))
  b <- b[b >= lo & b <= hi]
  if (lo <= -100 && !(-100 %in% b)) b <- c(-100, b[b > -85])
  b
}

swim_ink <- list(
  groups = c("#22928F", "#7A5AA6", "#3F74B5", "#B0803A", "#8A8A8A", "#A0527A"),
  others = c("#C88A1E", "#3F74B5", "#A0527A", "#6B6B6B")
)

# Event kinds by their wording, checked in this order.
swim_kinds <- list(
  death = list(match = "death|died|dead", shape = "cross", color = "#1F1F1F"),
  cr = list(match = "complete|^cr$", shape = "star", color = "#2E7D55"),
  pd = list(match = "progress|^pd$|relapse|recurr", shape = "diamond", color = "#C0603F"),
  pr = list(match = "partial|^pr$|response|remission", shape = "triangle", color = "#2E7D55")
)

swim_kind <- function(v) {
  v <- tolower(trimws(as.character(v)))
  out <- rep(NA_character_, length(v))
  for (k in names(swim_kinds)) {
    hit <- is.na(out) & grepl(swim_kinds[[k]]$match, v)
    out[hit] <- k
  }
  out
}

swim_prepare <- function(data, ids, end, start, group, ongoing, id_name, events, hover,
                         change = NULL, trajectories = NULL) {
  if (is.null(ids)) rlang::abort("`id` must name a column of `data`.")
  if (anyDuplicated(ids)) rlang::abort("`id` must identify each row of `data` once.")
  if (!is.numeric(end)) rlang::abort("`end` must be numeric.")
  if (is.null(start)) start <- rep(0, length(end))
  if (!is.numeric(start)) rlang::abort("`start` must be numeric.")
  if (any(!is.na(end) & end < start)) rlang::abort("`end` must not come before `start`.")
  if (!is.null(ongoing) && !is.logical(ongoing)) rlang::abort("`ongoing` must be logical.")
  if (is.null(ongoing)) ongoing <- rep(FALSE, length(end))
  ongoing[is.na(ongoing)] <- FALSE
  unknown <- setdiff(hover, names(data))
  if (length(unknown)) rlang::abort(paste0("These columns are not in `data`: ", paste(unknown, collapse = ", ")))
  keep <- !is.na(end)
  if (!any(keep)) rlang::abort("Every `end` is missing.")
  if (!is.null(group)) group <- if (is.factor(group)) droplevels(group[keep]) else factor(group[keep])

  ev <- NULL
  if (!is.null(events)) {
    if (!is.data.frame(events)) rlang::abort("`events` must be a data frame.")
    need <- c(id_name, "time", "event")
    miss <- setdiff(need, names(events))
    if (length(miss)) {
      rlang::abort(paste0("`events` must have the columns ", paste(need, collapse = ", "),
                          "; missing ", paste(miss, collapse = ", "), "."))
    }
    if (!is.numeric(events$time)) rlang::abort("`events$time` must be numeric.")
    lane <- match(as.character(events[[id_name]]), as.character(ids[keep]))
    stray <- unique(events[[id_name]][is.na(lane)])
    if (length(stray)) {
      rlang::abort(paste0("`events` names patients not in `data`: ",
                          paste(utils::head(stray, 5), collapse = ", "), "."))
    }
    ev <- data.frame(lane = lane, time = events$time, event = as.character(events$event),
                     stringsAsFactors = FALSE)
    ev <- ev[!is.na(ev$time) & !is.na(ev$event), ]
    ev <- ev[order(ev$lane, ev$time), ]
  }
  if (!is.null(change) && !is.numeric(change)) rlang::abort("`waterfall` must be numeric.")
  traj <- NULL
  if (!is.null(trajectories)) {
    if (!is.data.frame(trajectories)) rlang::abort("`trajectories` must be a data frame.")
    need <- c(id_name, "time", "change")
    miss <- setdiff(need, names(trajectories))
    if (length(miss)) {
      rlang::abort(paste0("`trajectories` must have the columns ", paste(need, collapse = ", "),
                          "; missing ", paste(miss, collapse = ", "), "."))
    }
    if (!is.numeric(trajectories$time) || !is.numeric(trajectories$change)) {
      rlang::abort("`trajectories$time` and `trajectories$change` must be numeric.")
    }
    lane <- match(as.character(trajectories[[id_name]]), as.character(ids[keep]))
    stray <- unique(trajectories[[id_name]][is.na(lane)])
    if (length(stray)) {
      rlang::abort(paste0("`trajectories` names patients not in `data`: ",
                          paste(utils::head(stray, 5), collapse = ", "), "."))
    }
    traj <- data.frame(lane = lane, time = trajectories$time, change = trajectories$change)
    traj <- traj[!is.na(traj$time) & !is.na(traj$change), ]
    traj <- traj[order(traj$lane, traj$time), ]
  }
  list(data = data[keep, , drop = FALSE], ids = ids[keep], start = start[keep], end = end[keep],
       group = group, ongoing = ongoing[keep], events = ev, hover = hover,
       change = if (!is.null(change)) change[keep], traj = traj)
}

# A marker's outline, centered on (x, y), `r` from center to tip, in points
# on the top down canvas.
swim_shape <- function(shape, x, y, r) {
  a <- function(n, off = -pi / 2) seq(0, 2 * pi, length.out = n + 1)[-1] + off
  pts <- switch(
    shape,
    star = {
      t <- a(10)
      rr <- rep(c(r * 1.2, r * 0.52), 5)
      cbind(rr * cos(t), rr * sin(t))
    },
    triangle = cbind(c(0, r, -r), c(-r * 1.05, r * 0.8, r * 0.8)),
    diamond = cbind(c(0, r, 0, -r), c(-r * 1.1, 0, r * 1.1, 0)),
    square = cbind(c(-1, 1, 1, -1) * r * 0.8, c(-1, -1, 1, 1) * r * 0.8),
    cross = {
      w <- r * 0.32
      q <- r * 0.95
      cbind(c(-q, -q + w, 0, q - w, q, w, q, q - w, 0, -q + w, -q, -w),
            c(-q + w, -q, -w, -q, -q + w, 0, q - w, q, w, q, q - w, 0))
    },
    {
      t <- a(20)
      cbind(r * 0.85 * cos(t), r * 0.85 * sin(t))
    }
  )
  list(x = x + pts[, 1], y = y + pts[, 2])
}

swim_layout <- function(input, sort, palette, ongoing_label, xlab, legend, title,
                        caption, family, thresholds = c(-30, 20),
                        change_label = "Best change from baseline (%)") {
  dims <- swim_dims
  n <- length(input$ids)
  width <- function(s, pt = dims$small_pt, bold = FALSE) {
    if (!length(s)) return(0)
    max(text_width_card(as.character(s), pt, family, 1, bold = bold))
  }
  ev <- input$events
  labels <- as.character(input$ids)

  # Colors for the groups, and a shape and color for every kind of event.
  groups <- if (is.null(input$group)) character(0) else levels(input$group)
  gcol <- if (!length(groups)) rep(swim_ink$groups[1], n) else {
    cols <- swim_colors(groups, palette)
    unname(cols[as.character(input$group)])
  }
  kinds <- NULL
  if (!is.null(ev) && nrow(ev)) {
    types <- unique(ev$event)
    kind <- swim_kind(types)
    other <- which(is.na(kind))
    kinds <- data.frame(event = types, kind = kind, shape = "", color = "",
                        stringsAsFactors = FALSE)
    known <- which(!is.na(kind))
    kinds$shape[known] <- vapply(kind[known], function(k) swim_kinds[[k]]$shape, character(1))
    kinds$color[known] <- vapply(kind[known], function(k) swim_kinds[[k]]$color, character(1))
    kinds$shape[other] <- c("circle", "square")[(seq_along(other) - 1) %% 2 + 1]
    kinds$color[other] <- swim_ink$others[(seq_along(other) - 1) %% length(swim_ink$others) + 1]
    rank <- c(cr = 1, pr = 2, pd = 3, death = 5)
    kinds <- kinds[order(ifelse(is.na(kinds$kind), 4, rank[kinds$kind])), ]
  }

  # Orders of the lanes: position of each lane in each order.
  resp <- rep(3L, n)
  if (!is.null(kinds)) {
    k_of <- kinds$kind[match(ev$event, kinds$event)]
    resp[unique(ev$lane[k_of %in% "pr"])] <- 2L
    resp[unique(ev$lane[k_of %in% "cr"])] <- 1L
  }
  dur <- input$end - input$start
  orders <- list(duration = order(-dur, seq_len(n)))
  if (length(groups)) orders$group <- order(as.integer(input$group), -dur, seq_len(n))
  if (any(resp < 3)) orders$response <- order(resp, -dur, seq_len(n))
  ch <- input$change
  if (!is.null(ch)) orders$change <- order(is.na(ch), ch, -dur, seq_len(n))
  orders$data <- seq_len(n)
  if (!sort %in% names(orders)) {
    rlang::abort(switch(sort, group = "`sort = \"group\"` needs `group`.",
                        change = "`sort = \"change\"` needs `waterfall`.",
                        "`sort = \"response\"` needs response events."))
  }
  row_of <- lapply(orders, function(o) {
    r <- integer(n)
    r[o] <- seq_len(n)
    r
  })
  order_labels <- c(duration = "Longest first", group = input$group_name,
                    response = "Best response", change = "Best change", data = "Data order")

  # Time axis.
  tmin <- min(0, input$start)
  tmax <- max(c(input$end, ev$time), na.rm = TRUE)
  breaks <- scales::breaks_extended(6)(c(tmin, tmax))
  tmax <- max(tmax, breaks)
  breaks <- breaks[breaks >= tmin & breaks <= tmax]
  digits <- if (tmax >= 100) 0 else if (tmax >= 10) 1 else 2
  tfmt <- function(v) formatC(v, format = "f", digits = digits)
  label_w <- width(labels) + dims$gap
  x0 <- label_w
  x1 <- x0 + dims$plot_w
  X <- function(t) x0 + (t - tmin) / (tmax - tmin) * dims$plot_w

  # Percent change: the waterfall beside the lanes and the trajectories
  # under them share one scale, from a full decrease to a capped increase.
  pfmt <- function(v) ifelse(is.na(v), "not evaluable",
                             paste0(ifelse(v > 0, "+", ifelse(v < 0, "\u2212", "")),
                                    formatC(abs(v), format = "f", digits = 0), "%"))
  tr <- input$traj
  wf <- NULL
  change_cat <- function(v) ifelse(is.na(v), "ne", ifelse(v <= thresholds[1], "resp",
                                   ifelse(v >= thresholds[2], "prog", "stable")))
  cat_words <- c(resp = paste0("a decrease of ", -thresholds[1], "% or more"),
                 stable = "in between",
                 prog = paste0("an increase of ", thresholds[2], "% or more"),
                 ne = "not evaluable")
  pool <- c(ch, tr$change)
  if (length(pool) && any(!is.na(pool))) {
    c_lo <- max(-100, min(c(pool, thresholds[1] - 10), na.rm = TRUE))
    c_hi <- min(150, max(c(pool, thresholds[2] + 10), na.rm = TRUE))
  }
  if (!is.null(ch)) {
    wx0 <- x0 + dims$plot_w + dims$waterfall_gap
    W <- function(v) wx0 + (pmin(pmax(v, c_lo), c_hi) - c_lo) / (c_hi - c_lo) * dims$waterfall_w
    wbreaks <- swim_change_breaks(c_lo, c_hi)
    cats <- change_cat(ch)
    ev_n <- sum(!is.na(ch))
    count <- function(k) sum(cats == k)
    share <- function(k) if (ev_n) paste0(" (", round(100 * count(k) / ev_n), "%)") else ""
    levels_of <- c("resp", "stable", "prog", "ne")
    summary <- data.frame(
      category = c("Decrease past the response threshold", "In between",
                   "Increase past the progression threshold", "Not evaluable"),
      patients = vapply(levels_of, count, numeric(1)),
      stringsAsFactors = FALSE, row.names = NULL
    )
    by_group <- ""
    if (length(groups)) {
      for (g in groups) summary[[g]] <- vapply(levels_of, function(k) sum(cats == k & input$group == g), numeric(1))
      by_group <- paste0(" By ", tolower(input$group_name), ": ", paste(vapply(groups, function(g) {
        ev_g <- sum(!is.na(ch) & input$group == g)
        paste0(g, " ", sum(cats == "resp" & input$group == g), " of ", ev_g)
      }, character(1)), collapse = ", "), " fell that far.")
    }
    words <- paste0(
      "Best change: ", count("resp"), " of ", ev_n, " evaluable", share("resp"), " fell by ",
      -thresholds[1], "% or more, and ", count("prog"), share("prog"), " grew by ",
      thresholds[2], "% or more",
      if (count("ne")) paste0("; ", count("ne"), " not evaluable") else "", ".", by_group
    )
    wf <- list(W = W, x0 = wx0, x1 = wx0 + dims$waterfall_w, breaks = wbreaks, cats = cats,
               summary = summary, words = words)
  }

  # A legend of the groups, the events and the arrow, wrapped to the plot.
  key <- NULL
  if (legend) {
    items <- rbind(
      if (length(groups)) data.frame(label = groups, type = "bar", shape = "", color = unname(swim_colors(groups, palette)), stringsAsFactors = FALSE),
      if (!is.null(kinds)) data.frame(label = kinds$event, type = "mark", shape = kinds$shape, color = kinds$color, stringsAsFactors = FALSE),
      if (any(input$ongoing)) data.frame(label = ongoing_label, type = "arrow", shape = "", color = graph_ink$muted, stringsAsFactors = FALSE),
      if (!is.null(wf)) data.frame(
        label = c(paste0("Fell ", -thresholds[1], "% or more"), "In between",
                  paste0("Grew ", thresholds[2], "% or more")),
        type = "bar", shape = "", color = unname(swim_change_ink), stringsAsFactors = FALSE)
    )
    if (!is.null(items)) {
      items$w <- 16 + text_width_card(items$label, dims$small_pt, family, 1)
      x <- 0
      row <- 0
      items$x <- 0
      items$row <- 0
      for (i in seq_len(nrow(items))) {
        if (x > 0 && x + items$w[i] > (if (is.null(wf)) x1 else wf$x1)) {
          row <- row + 1
          x <- 0
        }
        items$x[i] <- x
        items$row[i] <- row
        x <- x + items$w[i] + dims$key_gap
      }
      key <- items
    }
  }
  key_h <- if (is.null(key)) 0 else (max(key$row) + 1) * 16 + 10
  top <- key_h
  lane_y <- function(r) top + (r - 0.5) * dims$row_h
  bottom <- top + n * dims$row_h
  start_row <- row_of[[sort]]

  # Hover cards and panels.
  data <- input$data
  hover_values <- lapply(seq_len(n), function(i) {
    if (!length(input$hover)) return(character(0))
    v <- vapply(input$hover, function(col) format_values(data[[col]])[i], character(1))
    stats::setNames(ifelse(is.na(v), "", v), column_labels(data, input$hover))
  })
  lane_events <- lapply(seq_len(n), function(i) if (is.null(ev)) ev else ev[ev$lane == i, ])
  tips <- vapply(seq_len(n), function(i) {
    e <- lane_events[[i]]
    firsts <- if (is.null(e) || !nrow(e)) character(0) else {
      f <- e[!duplicated(e$event), ]
      stats::setNames(tfmt(f$time), f$event)
    }
    span <- if (any(input$start != 0)) paste(tfmt(input$start[i]), "to", tfmt(input$end[i])) else tfmt(input$end[i])
    paste0(
      '<div class="ggx-tip-title">', esc(labels[i]), "</div>",
      if (length(groups)) paste0('<div class="ggx-tip-sub"><i class="ggx-dot" style="background:',
                                 gcol[i], '"></i>', esc(input$group[i]), "</div>"),
      tip_rows(c(hover_values[[i]], stats::setNames(span, xlab), firsts,
                 if (input$ongoing[i]) c(Status = ongoing_label),
                 if (!is.null(ch)) stats::setNames(
                   paste0(pfmt(ch[i]), if (!is.na(ch[i])) paste0(", ", cat_words[[change_cat(ch[i])]])),
                   "Best change"))),
      '<div class="ggx-tip-hint">Click for the full record.</div>'
    )
  }, character(1))
  clicks <- vapply(seq_len(n), function(i) {
    e <- lane_events[[i]]
    steps <- data.frame(time = c(input$start[i], if (!is.null(e)) e$time, input$end[i]),
                        what = c("Start", if (!is.null(e)) e$event,
                                 if (input$ongoing[i]) paste0("Last time; ", tolower(ongoing_label)) else "End"),
                        stringsAsFactors = FALSE)
    steps <- steps[order(steps$time, seq_len(nrow(steps))), ]
    record <- {
      keep <- names(data)[!vapply(data, is.list, logical(1))]
      v <- vapply(keep, function(col) format_values(data[[col]])[i], character(1))
      stats::setNames(v, column_labels(data, keep))[!is.na(v)]
    }
    paste0('<div class="ggx-title">', esc(labels[i]), "</div>",
           if (length(groups)) paste0('<div class="ggx-sub">', esc(input$group[i]), "</div>"),
           '<div class="ggx-table"><table><thead><tr><th scope="col">', esc(xlab),
           '</th><th scope="col">Event</th></tr></thead><tbody>',
           paste0('<tr><th scope="row">', tfmt(steps$time), "</th><td>", esc(steps$what),
                  "</td></tr>", collapse = ""),
           "</tbody></table></div>",
           if (length(record)) paste0('<div class="ggx-refs-head">Record</div><dl>',
                                      paste0("<dt>", esc(names(record)), "</dt><dd>",
                                             vapply(record, link_reference, character(1)),
                                             "</dd>", collapse = ""), "</dl>"))
  }, character(1))
  ids <- paste0("p", seq_len(n))

  # The trajectories sit under the lanes, on the same time axis.
  spider <- NULL
  below <- bottom + 40
  if (!is.null(wf)) below <- below + 16
  if (!is.null(tr)) {
    s_top <- below + 34
    s_bottom <- s_top + dims$spider_h
    Y <- function(v) s_top + (c_hi - pmin(pmax(v, c_lo), c_hi)) / (c_hi - c_lo) * dims$spider_h
    sbreaks <- swim_change_breaks(c_lo, c_hi)
    # Each line starts from no change at the start of its lane, unless an
    # assessment comes first.
    lines <- do.call(rbind, lapply(sort(unique(tr$lane)), function(i) {
      r <- tr[tr$lane == i, ]
      if (min(r$time) > input$start[i]) r <- rbind(data.frame(lane = i, time = input$start[i], change = 0), r)
      r
    }))
    spider <- list(top = s_top, bottom = s_bottom, Y = Y, breaks = sbreaks, lines = lines)
    below <- s_bottom + 40
  }
  right <- if (is.null(wf)) x1 + 14 else wf$x1 + 14
  page <- graph_canvas(c(0, right), c(0, below), title, caption, character(0),
                       character(0), NULL, dims, family)
  list(
    dims = dims, page = page, family = family, n = n, labels = labels, ids = ids,
    start = input$start, end = input$end, ongoing = input$ongoing, gcol = gcol,
    events = ev, kinds = kinds, key = key, X = X, x0 = x0, x1 = x1, top = top,
    bottom = bottom, lane_y = lane_y, start_row = start_row, breaks = breaks,
    tfmt = tfmt, xlab = xlab, tips = tips, clicks = pin_js(ids, clicks),
    wf = wf, spider = spider, change = ch, thresholds = thresholds, change_label = change_label,
    pfmt = pfmt,
    render = list(
      row_h = dims$row_h,
      start = sort,
      # Marks above this line, in the widget's own units, belong to the
      # lanes and move with them; the trajectories below stay put.
      split = if (!is.null(spider)) (bottom + spider$top) / 2 + page$height - page$py(0),
      orders = unname(lapply(names(orders), function(k) {
        list(key = k, label = order_labels[[k]], row = I(row_of[[k]]))
      }))
    )
  )
}

# Group colors: unnamed in level order, or named by group; the rest from
# the package's defaults.
swim_colors <- function(groups, palette) {
  base <- rep(swim_ink$groups, length.out = length(groups))
  out <- stats::setNames(base, groups)
  if (is.null(palette)) return(out)
  if (!is.null(names(palette))) {
    hit <- intersect(names(palette), groups)
    out[hit] <- palette[hit]
  } else {
    out[seq_len(min(length(palette), length(groups)))] <- palette[seq_len(min(length(palette), length(groups)))]
  }
  out
}

swim_draw <- function(lay) {
  dims <- lay$dims
  px <- lay$page$px
  py <- lay$page$py
  n <- lay$n
  y <- lay$lane_y(lay$start_row)
  half <- dims$row_h * dims$bar_frac / 2
  texts <- function(label, x, y, pt, color, hjust = 0, face = "plain") {
    if (!length(label)) return(empty_texts())
    data.frame(x = px(x), y = py(y), label = as.character(label), size = pt / .pt,
               colour = color, hjust = hjust, fontface = face, stringsAsFactors = FALSE)
  }
  # Every mark in a lane carries the lane's id, so the widget can move the
  # lane and fade the others; only the hit area carries the card.
  # A patient's trajectory is a line, which the stroke must not leave.
  lane_css <- if (is.null(lay$spider)) "stroke:none;" else "stroke-width:2px;"
  right <- if (is.null(lay$wf)) lay$x1 else lay$wf$x1

  bars <- data.frame(xmin = px(lay$X(lay$start)), xmax = px(lay$X(lay$end)),
                     ymin = py(y + half), ymax = py(y - half), fill = lay$gcol,
                     id = lay$ids, hover = lane_css, stringsAsFactors = FALSE)
  hits <- data.frame(xmin = px(0), xmax = px(right + 12), ymin = py(y + dims$row_h / 2),
                     ymax = py(y - dims$row_h / 2), id = lay$ids, tip = lay$tips,
                     click = lay$clicks, hover = lane_css, stringsAsFactors = FALSE)
  shapes <- NULL
  add_shape <- function(s, id, fill) {
    shapes <<- rbind(shapes, data.frame(x = px(s$x), y = py(s$y),
                                        group = paste0(id, "_", if (is.null(shapes)) 0 else nrow(shapes)),
                                        id = id, fill = fill, stringsAsFactors = FALSE))
  }
  for (i in which(lay$ongoing)) {
    xe <- lay$X(lay$end[i])
    add_shape(list(x = xe + c(2, 9, 2, 4.2), y = y[i] + c(-4, 0, 4, 0)), lay$ids[i], graph_ink$muted)
  }
  if (!is.null(lay$events)) {
    ev <- lay$events
    for (j in seq_len(nrow(ev))) {
      k <- lay$kinds[match(ev$event[j], lay$kinds$event), ]
      add_shape(swim_shape(k$shape, lay$X(ev$time[j]), y[ev$lane[j]], dims$marker),
                lay$ids[ev$lane[j]], k$color)
    }
  }
  names_df <- data.frame(x = px(lay$x0 - 8), y = py(y), label = lay$labels, id = lay$ids,
                         hover = lane_css, stringsAsFactors = FALSE)

  grid <- data.frame(x = px(lay$X(lay$breaks)), y = py(lay$top - 2), yend = py(lay$bottom))
  axis <- rbind(
    data.frame(x = px(lay$x0), xend = px(lay$x1), y = py(lay$bottom), yend = py(lay$bottom)),
    data.frame(x = px(lay$X(lay$breaks)), xend = px(lay$X(lay$breaks)), y = py(lay$bottom),
               yend = py(lay$bottom + 4))
  )
  labels <- rbind(
    texts(format(lay$breaks, trim = TRUE, drop0trailing = TRUE), lay$X(lay$breaks),
          rep(lay$bottom + 13, length(lay$breaks)), dims$small_pt, graph_ink$muted, hjust = 0.5),
    texts(lay$xlab, (lay$x0 + lay$x1) / 2, lay$bottom + 29, dims$text_pt, graph_ink$text,
          hjust = 0.5)
  )

  # Legend.
  key_rects <- NULL
  key_shapes <- NULL
  if (!is.null(lay$key)) {
    k <- lay$key
    ky <- 8 + k$row * 16
    labels <- rbind(labels, texts(k$label, k$x + 16, ky, dims$small_pt, graph_ink$text))
    for (i in seq_len(nrow(k))) {
      cx <- k$x[i] + 6
      if (k$type[i] == "bar") {
        key_rects <- rbind(key_rects, data.frame(xmin = px(k$x[i]), xmax = px(k$x[i] + 12),
                                                 ymin = py(ky[i] + half), ymax = py(ky[i] - half),
                                                 fill = k$color[i]))
      } else {
        s <- if (k$type[i] == "arrow") list(x = k$x[i] + c(1, 11, 1, 4.5), y = ky[i] + c(-4.5, 0, 4.5, 0))
             else swim_shape(k$shape[i], cx, ky[i], dims$marker)
        key_shapes <- rbind(key_shapes, data.frame(x = px(s$x), y = py(s$y), group = paste0("k", i),
                                                   fill = k$color[i], stringsAsFactors = FALSE))
      }
    }
  }

  p <- ggplot() +
    geom_segment(data = grid, aes(x = .data$x, xend = .data$x, y = .data$y, yend = .data$yend),
                 colour = graph_ink$faint, linewidth = 0.6 / .pt) +
    geom_segment(data = axis, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
                 colour = graph_ink$text, linewidth = 0.8 / .pt) +
    ggiraph::geom_rect_interactive(
      data = bars,
      aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, ymax = .data$ymax,
          fill = .data$fill, data_id = .data$id, hover_css = .data$hover),
      alpha = 0.55
    )
  if (!is.null(shapes)) {
    p <- p + ggiraph::geom_polygon_interactive(
      data = shapes,
      aes(x = .data$x, y = .data$y, group = .data$group, fill = .data$fill, data_id = .data$id,
          hover_css = lane_css),
      colour = graph_ink$page, linewidth = 0.7 / .pt
    )
  }
  p <- p +
    ggiraph::geom_text_interactive(
      data = names_df,
      aes(x = .data$x, y = .data$y, label = .data$label, data_id = .data$id,
          hover_css = .data$hover),
      hjust = 1, size = dims$small_pt / .pt, colour = graph_ink$text, family = lay$family
    ) +
    ggiraph::geom_rect_interactive(
      data = hits,
      aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, ymax = .data$ymax,
          data_id = .data$id, tooltip = .data$tip, onclick = .data$click,
          hover_css = .data$hover),
      fill = "#FFFFFF02", colour = NA
    )
  if (!is.null(key_rects)) {
    p <- p + geom_rect(data = key_rects, aes(xmin = .data$xmin, xmax = .data$xmax,
                                             ymin = .data$ymin, ymax = .data$ymax, fill = .data$fill),
                       alpha = 0.55)
  }
  if (!is.null(key_shapes)) {
    p <- p + geom_polygon(data = key_shapes, aes(x = .data$x, y = .data$y, group = .data$group,
                                                 fill = .data$fill))
  }
  if (!is.null(lay$wf)) p <- p + swim_waterfall(lay, y, half, texts)
  if (!is.null(lay$spider)) p <- p + swim_spider(lay, texts, lane_css)
  p + draw_text(labels, lay$family) + graph_frame(lay$page, lay$family)
}

# Best change from baseline beside the lanes: a bar from no change for each
# patient, the thresholds, an axis and a count past each threshold.
swim_waterfall <- function(lay, y, half, texts) {
  dims <- lay$dims
  px <- lay$page$px
  py <- lay$page$py
  wf <- lay$wf
  W <- wf$W
  ch <- lay$change
  ok <- !is.na(ch)
  bars <- data.frame(xmin = px(pmin(W(0), W(ch[ok]))), xmax = px(pmax(W(0), W(ch[ok]))),
                     ymin = py(y[ok] + half), ymax = py(y[ok] - half),
                     fill = unname(swim_change_ink[wf$cats[ok]]), id = lay$ids[ok],
                     hover = if (is.null(lay$spider)) "stroke:none;" else "stroke-width:2px;",
                     stringsAsFactors = FALSE)
  ne <- if (any(!ok)) texts("not evaluable", W(0) + 4, y[!ok], dims$small_pt - 1.5, graph_ink$muted)
  if (!is.null(ne)) ne$id <- lay$ids[!ok]
  rules <- data.frame(x = px(W(lay$thresholds)), y = py(lay$top - 2), yend = py(lay$bottom),
                      colour = unname(swim_change_ink[c("resp", "prog")]))
  axis <- rbind(
    data.frame(x = px(wf$x0), xend = px(wf$x1), y = py(lay$bottom), yend = py(lay$bottom)),
    data.frame(x = px(W(wf$breaks)), xend = px(W(wf$breaks)), y = py(lay$bottom), yend = py(lay$bottom + 4))
  )
  labels <- rbind(
    texts(paste0(format(wf$breaks, trim = TRUE)), W(wf$breaks), rep(lay$bottom + 13, length(wf$breaks)),
          dims$small_pt, graph_ink$muted, hjust = 0.5),
    texts(lay$change_label, (wf$x0 + wf$x1) / 2, lay$bottom + 29, dims$text_pt, graph_ink$text, hjust = 0.5),
    texts(wf$words, lay$x0, lay$bottom + 50, dims$small_pt, graph_ink$text)
  )
  list(
    geom_segment(data = data.frame(x = px(W(0)), y = py(lay$top - 2), yend = py(lay$bottom)),
                 aes(x = .data$x, xend = .data$x, y = .data$y, yend = .data$yend),
                 colour = graph_ink$text, linewidth = 0.6 / .pt),
    geom_segment(data = rules, aes(x = .data$x, xend = .data$x, y = .data$y, yend = .data$yend,
                                   colour = .data$colour),
                 linewidth = 0.8 / .pt, linetype = "22"),
    geom_segment(data = axis, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
                 colour = graph_ink$text, linewidth = 0.8 / .pt),
    ggiraph::geom_rect_interactive(
      data = bars,
      aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, ymax = .data$ymax,
          fill = .data$fill, data_id = .data$id, hover_css = .data$hover),
      alpha = 0.8
    ),
    if (!is.null(ne)) ggiraph::geom_text_interactive(
      data = ne, aes(x = .data$x, y = .data$y, label = .data$label, data_id = .data$id),
      hjust = 0, size = ne$size, colour = graph_ink$muted, family = lay$family
    ),
    draw_text(labels, lay$family)
  )
}

# Each patient's percent change over time, under the lanes on their time
# axis, with the thresholds.
swim_spider <- function(lay, texts, lane_css) {
  dims <- lay$dims
  px <- lay$page$px
  py <- lay$page$py
  sp <- lay$spider
  X <- lay$X
  Y <- sp$Y
  lines <- sp$lines
  df <- data.frame(x = px(X(lines$time)), y = py(Y(lines$change)), id = lay$ids[lines$lane],
                   colour = lay$gcol[lines$lane], tip = lay$tips[lines$lane],
                   click = lay$clicks[lines$lane], hover = lane_css, stringsAsFactors = FALSE)
  pts <- df[duplicated(lines$lane) | !(lines$change == 0 & lines$time == lay$start[lines$lane]), ]
  grid <- data.frame(x = px(lay$x0), xend = px(lay$x1), y = py(Y(sp$breaks)))
  rules <- data.frame(x = px(lay$x0), xend = px(lay$x1), y = py(Y(lay$thresholds)),
                      colour = unname(swim_change_ink[c("resp", "prog")]))
  axis <- rbind(
    data.frame(x = px(lay$x0), xend = px(lay$x1), y = py(sp$bottom), yend = py(sp$bottom)),
    data.frame(x = px(X(lay$breaks)), xend = px(X(lay$breaks)), y = py(sp$bottom), yend = py(sp$bottom + 4)),
    data.frame(x = px(lay$x0), xend = px(lay$x0), y = py(sp$top), yend = py(sp$bottom)),
    data.frame(x = px(lay$x0 - 4), xend = px(lay$x0), y = py(Y(sp$breaks)), yend = py(Y(sp$breaks)))
  )
  labels <- rbind(
    texts("Change from baseline over time (%)", lay$x0, sp$top - 14, dims$text_pt, graph_ink$text,
          face = "bold"),
    texts(format(sp$breaks, trim = TRUE), lay$x0 - 7, Y(sp$breaks), dims$small_pt, graph_ink$muted, hjust = 1),
    texts(format(lay$breaks, trim = TRUE, drop0trailing = TRUE), X(lay$breaks),
          rep(sp$bottom + 13, length(lay$breaks)), dims$small_pt, graph_ink$muted, hjust = 0.5),
    texts(lay$xlab, (lay$x0 + lay$x1) / 2, sp$bottom + 29, dims$text_pt, graph_ink$text, hjust = 0.5),
    texts(c(paste0(lay$thresholds[1], "%: response"), paste0("+", lay$thresholds[2], "%: progression")),
          lay$x1 + 4, Y(lay$thresholds), dims$small_pt - 1, unname(swim_change_ink[c("resp", "prog")]))
  )
  list(
    geom_segment(data = grid, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$y),
                 colour = graph_ink$faint, linewidth = 0.6 / .pt),
    geom_segment(data = data.frame(x = px(lay$x0), xend = px(lay$x1), y = py(Y(0))),
                 aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$y),
                 colour = graph_ink$text, linewidth = 0.6 / .pt),
    geom_segment(data = rules, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$y,
                                   colour = .data$colour),
                 linewidth = 0.8 / .pt, linetype = "22"),
    geom_segment(data = axis, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
                 colour = graph_ink$text, linewidth = 0.8 / .pt),
    ggiraph::geom_path_interactive(
      data = df,
      aes(x = .data$x, y = .data$y, group = .data$id, colour = .data$colour, data_id = .data$id,
          hover_css = .data$hover),
      linewidth = 1.1 / .pt, alpha = 0.85
    ),
    ggiraph::geom_point_interactive(
      data = pts,
      aes(x = .data$x, y = .data$y, colour = .data$colour, data_id = .data$id, hover_css = .data$hover),
      size = 1.3
    ),
    # A wide, almost transparent copy of each line catches the pointer.
    ggiraph::geom_path_interactive(
      data = df,
      aes(x = .data$x, y = .data$y, group = .data$id, data_id = .data$id, tooltip = .data$tip,
          onclick = .data$click, hover_css = .data$hover),
      colour = "#FFFFFF02", linewidth = 7 / .pt, lineend = "round"
    ),
    draw_text(labels, lay$family)
  )
}
