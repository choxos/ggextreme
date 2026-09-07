# Colouring bars by group, and the legend that explains it.
#
# The legend is laid out once, when the race is built, because the groups and
# the output width are both fixed by then. Positions are card units, so the
# renderer only has to draw what it is handed.

# Entities keep one group each, or the colouring would be ambiguous.
group_key <- function(name, group, entities) {
  if (is.null(group)) return(NULL)
  if (length(group) != length(name)) {
    rlang::abort("`group` must have one value per row of `data`.")
  }
  levels <- if (is.factor(group)) {
    levels(droplevels(group))
  } else {
    sort(unique(as.character(group)))
  }
  group <- as.character(group)
  key <- unique(data.frame(name = name, group = group, stringsAsFactors = FALSE))
  clash <- key$name[duplicated(key$name)]
  if (length(clash)) {
    rlang::abort(paste0(
      "Each `name` must belong to one `group`. These belong to more than one: ",
      paste(unique(clash), collapse = ", ")
    ))
  }
  list(levels = levels,
       of = stats::setNames(key$group, key$name)[entities])
}

# Width of a string in card units, so the legend can be packed without
# opening a graphics device.
text_width_card <- function(labels, pt, family, scale, bold = FALSE) {
  if (!length(labels)) return(numeric(0))
  metrics <- systemfonts::shape_string(
    labels, family = family, weight = if (bold) "bold" else "normal",
    size = pt * scale, res = 72
  )$metrics
  metrics$width / scale
}

# Pack the swatches and labels into rows that fit the card, then report how
# tall the band has to be.
legend_layout <- function(labels, colors, title, lay, family, scale) {
  if (!length(labels)) return(NULL)
  label_w <- text_width_card(labels, lay$legend_pt, family, scale)
  item_w <- lay$legend_swatch_w + lay$legend_gap_swatch + label_w

  start <- lay$content_l
  x <- start
  if (!is.null(title)) {
    x <- x + text_width_card(title, lay$legend_pt, family, scale, bold = TRUE) +
      lay$legend_gap_title
  }

  row <- integer(length(labels))
  at <- numeric(length(labels))
  current <- 0L
  for (i in seq_along(labels)) {
    if (x > start && x + item_w[i] > lay$content_r) {
      current <- current + 1L
      x <- start
    }
    row[i] <- current
    at[i] <- x
    x <- x + item_w[i] + lay$legend_gap_item
  }

  list(
    items = data.frame(label = labels, color = unname(colors), x = at,
                       row = row, stringsAsFactors = FALSE),
    title = title,
    rows = current + 1L,
    height = (current + 1L) * lay$legend_row_h
  )
}

# A swatch is a rounded rectangle, so it needs a traced outline rather than a
# plain rect.
rounded_rect <- function(x0, x1, y0, y1, r, n = 5) {
  r <- min(r, (x1 - x0) / 2, (y1 - y0) / 2)
  arc <- function(cx, cy, from, to) {
    a <- seq(from, to, length.out = n)
    cbind(cx + r * cos(a), cy + r * sin(a))
  }
  pts <- rbind(
    arc(x1 - r, y1 - r, 0, pi / 2),
    arc(x0 + r, y1 - r, pi / 2, pi),
    arc(x0 + r, y0 + r, pi, 3 * pi / 2),
    arc(x1 - r, y0 + r, 3 * pi / 2, 2 * pi)
  )
  data.frame(x = pts[, 1], y = pts[, 2])
}

legend_shapes <- function(legend, lay, fx, fy) {
  if (is.null(legend)) return(NULL)
  items <- legend$items
  mids <- lay$legend_top + (items$row + 0.5) * lay$legend_row_h
  do.call(rbind, lapply(seq_len(nrow(items)), function(i) {
    box <- rounded_rect(
      items$x[i], items$x[i] + lay$legend_swatch_w,
      mids[i] - lay$legend_swatch_h / 2, mids[i] + lay$legend_swatch_h / 2,
      lay$legend_swatch_r
    )
    data.frame(x = fx(box$x), y = fy(box$y), group = 100L + i,
               fill = items$color[i], stringsAsFactors = FALSE)
  }))
}

legend_text <- function(legend, lay, texts) {
  if (is.null(legend)) return(NULL)
  items <- legend$items
  mids <- lay$legend_top + (items$row + 0.5) * lay$legend_row_h
  out <- texts(items$label,
               items$x + lay$legend_swatch_w + lay$legend_gap_swatch,
               mids, lay$legend_pt, race_ink$name, hjust = 0)
  if (!is.null(legend$title)) {
    out <- rbind(out, texts(
      legend$title, lay$content_l,
      lay$legend_top + 0.5 * lay$legend_row_h, lay$legend_pt,
      race_ink$title, hjust = 0, face = "bold"
    ))
  }
  out
}
