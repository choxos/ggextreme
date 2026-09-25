# A bar chart race as an interactive widget. The static copy is the race's
# last frame; the widget is a blank page the same size, which graph.js
# (ggextremeRace) draws the race on. Every frame is sent as its visible bars,
# already laid out and labeled in R, so the widget plays exactly what
# animate_race() would write, with the same formatters.

race_graph <- function(x) {
  size <- race_size(x)
  lay <- x$layout
  page_w <- lay$card_w + 2 * lay$page_pad
  page_h <- lay$card_h + 2 * lay$page_pad
  blank <- ggplot() +
    theme_race(if (x$card) race_ink$page else race_ink$card) +
    coord_cartesian(xlim = c(0, page_w), ylim = c(0, page_h), expand = FALSE)
  structure(
    list(plot = race_frame(x, x$n_frames), widget_plot = blank,
         width = size[["width"]] / x$res, height = size[["height"]] / x$res,
         title = x$title, on_render = "ggextremeRace(el, data);",
         render_data = race_render(x), hover_inv = ""),
    class = c("ggrace_graph", "ggx_graph")
  )
}

race_render <- function(x) {
  lay <- x$layout
  pad <- lay$page_pad
  n <- x$n_frames
  fr <- x$frames
  # Card units, top down, on a page that includes the padding around the
  # card: a card position plus `pad` is a page position.
  card <- function(v) v + pad

  # The axis maximum of each frame, as race_frame() computes it.
  shown <- fr$rank <= x$top_n + 0.5
  top <- tapply(ifelse(shown, fr$value, 0), fr$frame, max)
  top[!is.finite(top) | top <= 0] <- 1
  top <- as.numeric(top)
  breaks <- lapply(top, function(t) {
    b <- if (is.function(x$breaks)) x$breaks(c(0, t)) else x$breaks
    b[b >= 0 & b <= t]
  })
  key <- vapply(breaks, function(b) paste(format(b, digits = 15), collapse = " "), "")
  sets <- unique(key)
  set_values <- breaks[match(sets, key)]

  vis <- fr[fr$rank <= x$top_n + 1, ]
  vis <- vis[order(vis$frame, vis$rank), ]
  unit <- (lay$bar_x1 - lay$bar_x0) / top[vis$frame]
  vis$end <- lay$bar_x0 + vis$value * unit
  starts <- c(0L, cumsum(tabulate(vis$frame, nbins = n)))

  labels <- x$label_time(x$times)
  time_labels <- unique(labels)

  # The parts of the card that do not move, from the same helpers
  # race_frame() uses, in page coordinates.
  rects <- function(x0, x1, y0, y1, fill) {
    if (!length(x0) || !length(x1) || !length(y0) || !length(y1)) return(NULL)
    data.frame(x0 = card(x0), x1 = card(x1), y0 = card(y0), y1 = card(y1),
               fill = fill, stringsAsFactors = FALSE)
  }
  texts <- function(label, x_card, y_card, pt, color, hjust = 0, face = "plain") {
    if (!length(label) || !length(x_card) || !length(y_card)) return(NULL)
    data.frame(x = card(x_card), y = card(y_card), label = as.character(label), pt = pt,
               color = color, hjust = hjust, face = face, stringsAsFactors = FALSE)
  }
  as_rows <- function(df) if (is.null(df) || !nrow(df)) list() else unname(lapply(split(df, seq_len(nrow(df))), as.list))
  timeline_r <- if (x$timeline) timeline_rects(x, lay, rects)
  timeline_t <- if (x$timeline) timeline_text(x, lay, texts)
  legend_t <- legend_text(x$legend, lay, texts)
  legend_s <- legend_shapes(x$legend, lay, card, card)
  legend_s <- if (is.null(legend_s)) list() else
    unname(lapply(split(legend_s, legend_s$group), function(d) list(
      points = paste(round(d$x, 2), round(d$y, 2), sep = ",", collapse = " "), fill = d$fill[1])))

  groups <- NULL
  if (!is.null(x$legend)) {
    by <- x$legend$items
    groups <- by$label[match(x$colors, by$color)]
  }

  list(
    page_w = lay$card_w + 2 * pad, page_h = lay$card_h + 2 * pad, pad = pad, card = x$card,
    lay = lay[c("card_w", "card_h", "content_l", "content_r", "title_mid", "title_pt", "rule_y",
                "rule_h", "axis_mid", "axis_pt", "bars_top", "bars_bottom", "pitch", "bar_h",
                "bar_x0", "bar_x1", "grid_w", "grid_dx", "image_gap", "name_gap", "name_pt",
                "value_gap", "value_pt", "label_dy", "year_right", "year_mid", "year_pt",
                "button_x", "button_y", "button_r", "button_bar_w", "button_bar_h",
                "button_bar_gap", "time_l", "time_r", "time_y", "time_h", "time_label_mid",
                "foot_rule_y", "source_mid", "source_pt")],
    ink = race_ink,
    title = x$title, caption = x$caption, family = x$family,
    timeline = x$timeline, play_button = x$play_button,
    names = x$entities, colors = unname(x$colors), groups = groups,
    images = race_image_uris(x), image_d = x$image_d,
    n = n, fps = x$fps, end_pause = x$end_pause,
    keys = x$keys, times = x$times, date = x$date,
    key_frames = vapply(x$keys, function(k) which.min(abs(x$times - k)), 1L) - 1L,
    time_labels = time_labels, time_index = match(labels, time_labels) - 1L,
    top = signif(top, 8), breaks = lapply(set_values, function(b) list(v = b, s = format_break(b))),
    break_index = match(key, sets) - 1L,
    start = starts, entity = match(vis$name, x$entities) - 1L,
    rank = round(vis$rank, 3), end = round(vis$end, 2), value = x$label_value(vis$value),
    timeline_rects = as_rows(timeline_r), timeline_text = as_rows(timeline_t),
    legend_text = as_rows(legend_t), legend_shapes = legend_s
  )
}

# The images at the ends of the bars, as data URIs the page can show.
race_image_uris <- function(x) {
  files <- x$image_files
  if (is.null(files) || !length(files)) return(NULL)
  uri <- vapply(files, function(f) {
    ext <- tolower(tools::file_ext(f))
    bytes <- readBin(f, "raw", file.info(f)$size)
    type <- switch(ext, svg = "image/svg+xml", png = "image/png", jpg = , jpeg = "image/jpeg",
                   gif = "image/gif", webp = "image/webp", "application/octet-stream")
    paste0("data:", type, ";base64,", jsonlite::base64_enc(bytes))
  }, "")
  as.list(stats::setNames(uri, names(files)))
}
