# Shared machinery for the interactive graphs: the hover card and click
# panel, node boxes and edge routing, and turning a built graph into a
# widget, a plain ggplot or a file.
#
# A graph is laid out in points on a canvas that runs top down, like the race
# card, and flipped into plot coordinates at the end. The canvas size is the
# output size, so text and boxes are measured in the same units and the
# boxes always fit their labels.

graph_ink <- list(
  page = "#FFFFFF",
  title = "#1F1F1F",
  text = "#2C2C2C",
  muted = "#6B6B6B",
  edge = "#8A8A8A",
  hover = "#1F1F1F",
  plain_fill = "#FFFFFF",
  plain_border = "#9A9A9A"
)

graph_dims <- list(
  margin = 18,
  node_pt = 11,
  line_h = 1.2,
  pad_x = 9,
  pad_y = 6,
  radius = 4,
  title_pt = 15,
  caption_pt = 9,
  gap_title = 10,
  gap_legend = 16,
  gap_caption = 14,
  legend_pt = 10.5,
  legend_row_h = 18,
  legend_swatch_w = 9,
  legend_swatch_h = 11,
  legend_swatch_r = 2,
  legend_gap_swatch = 5,
  legend_gap_item = 14,
  legend_gap_title = 10,
  edge_pt = 1.1,
  hit_pt = 12,
  arrow_pt = 7,
  gap_start = 1.5,
  gap_end = 2.5,
  min_width = 320,
  layer_gap_right = 56,
  row_gap_right = 20,
  column_gap_down = 24,
  layer_gap_down = 44,
  manual_gap_x = 40,
  manual_gap_y = 30
)

# Hover card, in the widget's own tooltip container.
tip_css <- paste0(
  "background:#FFFFFF;color:#2C2C2C;border:1px solid #DADADA;",
  "border-radius:6px;padding:8px 11px;font-family:Lato,sans-serif;",
  "font-size:13px;line-height:1.4;",
  "box-shadow:0 2px 8px rgba(0,0,0,0.12);"
)

esc <- function(x) htmltools::htmlEscape(as.character(x))

# A string literal for an onclick handler. The characters that would need
# escaping inside an HTML attribute are written as JavaScript escapes, so the
# attribute is safe whatever the SVG writer does with it.
js_string <- function(x) {
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub("\"", "\\\"", x, fixed = TRUE)
  x <- gsub("\r", "", x, fixed = TRUE)
  x <- gsub("\n", "\\n", x, fixed = TRUE)
  for (ch in c("&", "'", "<", ">")) {
    x <- gsub(ch, sprintf("\\u%04x", utf8ToInt(ch)), x, fixed = TRUE)
  }
  paste0("\"", x, "\"")
}

# References arrive as a list column or as text with several entries
# separated by `|` or a line break. Vancouver style citations use `;` inside
# a single reference, so that is not a separator.
split_references <- function(x, n) {
  if (is.null(x)) return(rep(list(character(0)), n))
  parts <- if (is.list(x)) {
    lapply(x, function(r) as.character(unlist(r)))
  } else {
    strsplit(as.character(x), "\\s*\\|\\s*|\\s*\n\\s*")
  }
  lapply(parts, function(r) trimws(r[!is.na(r) & nzchar(trimws(r))]))
}

# One reference as HTML, with a link on its URL or, failing that, its DOI.
link_reference <- function(ref) {
  text <- esc(ref)
  link <- function(target, href) {
    target <- sub("[.,;:)]+$", "", target)
    sub(target, sprintf('<a href="%s" target="_blank" rel="noopener">%s</a>',
                        href(target), target), text, fixed = TRUE)
  }
  url <- regmatches(text, regexpr("https?://[^[:space:]]+", text))
  if (length(url)) return(link(url, identity))
  doi <- regmatches(text, regexpr("10\\.[0-9]{4,9}/[^[:space:]]+", text))
  if (length(doi)) return(link(doi, function(d) paste0("https://doi.org/", d)))
  text
}

# Display names for columns: a `label` attribute when there is one, as set
# by the labelled, Hmisc or haven packages, otherwise the name with
# underscores as spaces and a capital first letter.
column_labels <- function(data, cols) {
  vapply(cols, function(col) {
    label <- attr(data[[col]], "label", exact = TRUE)
    if (is.character(label) && length(label) == 1 && nzchar(label)) return(label)
    capitalize(gsub("_", " ", col, fixed = TRUE))
  }, character(1), USE.NAMES = FALSE)
}

# Values as text, with the same number of decimals down a column. Thousands
# are separated only from five digits up, so years read as written.
format_values <- function(v) {
  if (!is.numeric(v)) return(as.character(v))
  out <- rep(NA_character_, length(v))
  ok <- !is.na(v)
  if (any(ok)) {
    out[ok] <- format(v[ok], digits = 4, trim = TRUE, scientific = FALSE,
                      big.mark = if (max(abs(v[ok])) >= 1e4) "," else "")
  }
  out
}

# Extra columns become labeled fields in the card and the panel.
field_values <- function(data, reserved) {
  keep <- setdiff(names(data), reserved)
  keep <- keep[!vapply(data[keep], is.list, logical(1))]
  cols <- lapply(data[keep], format_values)
  labels <- column_labels(data, keep)
  lapply(seq_len(nrow(data)), function(i) {
    v <- vapply(cols, `[`, character(1), i)
    stats::setNames(v[!is.na(v)], labels[!is.na(v)])
  })
}

# Arms side by side, one column each, with the characteristics down the
# side, the way trials report their baseline tables. With `treatment`, the
# arms are grouped under a spanning header for each study. Columns in
# `notes` hold text that is the same for every arm of a study, such as a
# reference, and are listed once per study under the table instead.
arm_table <- function(arms, study, cols, labels, treatment = NULL,
                      notes = character(0)) {
  in_table <- !cols %in% notes
  study_head <- if (is.null(treatment)) {
    paste0('<th scope="col">', esc(study), "</th>", collapse = "")
  } else {
    runs <- rle(study)
    paste0('<th scope="colgroup" colspan="', runs$lengths, '">',
           esc(runs$values), "</th>", collapse = "")
  }
  arm_head <- if (!is.null(treatment)) {
    paste0('<tr><td></td>',
           paste0('<th scope="col" class="ggx-arm">', esc(treatment), "</th>",
                  collapse = ""),
           "</tr>")
  }
  rows <- vapply(which(in_table), function(j) {
    v <- format_values(arms[[cols[j]]])
    cells <- vapply(v, function(x) if (is.na(x)) "" else link_reference(x),
                    character(1))
    paste0('<tr', if (is.numeric(arms[[cols[j]]])) ' class="ggx-num"', '>',
           '<th scope="row">', esc(labels[j]), "</th>",
           paste0("<td>", cells, "</td>", collapse = ""), "</tr>")
  }, character(1))
  lists <- vapply(which(!in_table), function(j) {
    v <- as.character(arms[[cols[j]]])
    first <- !duplicated(study)
    v <- v[first]
    who <- study[first]
    keep <- !is.na(v) & nzchar(v)
    if (!any(keep)) return("")
    same <- split(who[keep], factor(v[keep], levels = unique(v[keep])))
    paste0('<div class="ggx-refs-head">', esc(labels[j]), "</div><ul class=\"ggx-notes\">",
           paste0("<li><b>", vapply(same, function(w) esc(paste(w, collapse = ", ")),
                                     character(1)),
                  "</b>: ", vapply(names(same), link_reference, character(1)),
                  "</li>", collapse = ""),
           "</ul>")
  }, character(1))
  paste0('<div class="ggx-table"><table><thead><tr><td></td>', study_head,
         "</tr>", arm_head, "</thead><tbody>", paste(rows, collapse = ""),
         "</tbody></table></div>", paste(lists, collapse = ""))
}

# Text columns whose value never changes within a study.
study_level <- function(data, study, cols) {
  cols[vapply(cols, function(col) {
    v <- data[[col]]
    (is.character(v) || is.factor(v)) &&
      all(tapply(as.character(v), study, function(x) length(unique(x)) == 1))
  }, logical(1))]
}

capitalize <- function(x) {
  x <- trimws(as.character(x))
  paste0(toupper(substring(x, 1, 1)), substring(x, 2))
}

text_or_empty <- function(x, n) {
  if (is.null(x)) return(rep("", n))
  out <- as.character(x)
  out[is.na(out)] <- ""
  out
}

tip_html <- function(title, sub, body, fields, refs) {
  vapply(seq_along(title), function(i) {
    f <- fields[[i]]
    n <- length(refs[[i]])
    paste0(
      '<div class="ggx-tip-title">', esc(title[i]), "</div>",
      if (nzchar(sub[i])) paste0('<div class="ggx-tip-sub">', esc(sub[i]), "</div>"),
      if (nzchar(body[i])) paste0('<div class="ggx-tip-body">', esc(body[i]), "</div>"),
      if (length(f)) paste0('<div class="ggx-tip-body">',
                            paste0(esc(names(f)), ": ", esc(f), collapse = "<br>"),
                            "</div>"),
      if (n) paste0('<div class="ggx-tip-hint">', n,
                    if (n == 1) " reference" else " references",
                    ". Click to open.</div>")
    )
  }, character(1))
}

panel_html <- function(title, sub, body, fields, refs) {
  vapply(seq_along(title), function(i) {
    f <- fields[[i]]
    r <- refs[[i]]
    paste0(
      '<div class="ggx-title">', esc(title[i]), "</div>",
      if (nzchar(sub[i])) paste0('<div class="ggx-sub">', esc(sub[i]), "</div>"),
      if (nzchar(body[i])) paste0("<p>", esc(body[i]), "</p>"),
      if (length(f)) paste0("<dl>", paste0("<dt>", esc(names(f)), "</dt><dd>",
                                          esc(f), "</dd>", collapse = ""), "</dl>"),
      if (length(r)) paste0(
        '<div class="ggx-refs-head">References</div><ol>',
        paste0("<li>", vapply(r, link_reference, character(1)), "</li>",
               collapse = ""),
        "</ol>"
      )
    )
  }, character(1))
}

pin_js <- function(key, html) {
  sprintf("ggextremePin(this, %s, %s)", js_string(key), js_string(html))
}

# Width and height of each label in points, allowing for line breaks.
label_boxes <- function(labels, dims, family) {
  lines <- strsplit(labels, "\n", fixed = TRUE)
  width <- vapply(lines, function(l) {
    max(text_width_card(l, dims$node_pt, family, 1))
  }, numeric(1))
  list(
    w = width + 2 * dims$pad_x,
    h = lengths(lines) * dims$node_pt * dims$line_h + 2 * dims$pad_y
  )
}

# Lighter version of a color for a box fill.
tint <- function(color, amount = 0.16) {
  rgb <- grDevices::col2rgb(color) / 255
  mixed <- 1 - (1 - rgb) * amount
  grDevices::rgb(mixed[1, ], mixed[2, ], mixed[3, ])
}

# Smooth a route through its bend points with a clamped B-spline, so it
# leaves the first point and reaches the last one exactly.
smooth_route <- function(pts, n = 80) {
  k <- min(3L, nrow(pts) - 1L)
  if (k < 2) {
    t <- seq(0, 1, length.out = n)
    return(cbind(pts[1, 1] + t * (pts[2, 1] - pts[1, 1]),
                 pts[1, 2] + t * (pts[2, 2] - pts[1, 2])))
  }
  inner <- seq(0, 1, length.out = nrow(pts) - k + 1)
  knots <- c(rep(0, k), inner, rep(1, k))
  basis <- splines::splineDesign(knots, seq(0, 1, length.out = n), ord = k + 1)
  basis %*% pts
}

inside_box <- function(pts, box, gap) {
  abs(pts[, 1] - box$x) <= box$w / 2 + gap &
    abs(pts[, 2] - box$y) <= box$h / 2 + gap
}

# Where a segment from a point inside a box to one outside crosses its edge.
box_exit <- function(a, b, box, gap) {
  lo <- 0
  hi <- 1
  for (i in seq_len(30)) {
    mid <- (lo + hi) / 2
    p <- a + mid * (b - a)
    if (inside_box(matrix(p, 1), box, gap)) lo <- mid else hi <- mid
  }
  a + hi * (b - a)
}

# Cut a route back to the borders of the boxes it joins, leaving room for
# the arrowhead. Returns NULL when the boxes overlap and no line is left.
trim_route <- function(pts, from, to, dims) {
  out_from <- which(!inside_box(pts, from, dims$gap_start))
  out_to <- which(!inside_box(pts, to, dims$gap_end))
  if (!length(out_from) || !length(out_to)) return(NULL)
  i <- out_from[1]
  j <- out_to[length(out_to)]
  if (i < 2 || j >= nrow(pts) || i > j) return(NULL)
  start <- box_exit(pts[i - 1, ], pts[i, ], from, dims$gap_start)
  end <- box_exit(pts[j + 1, ], pts[j, ], to, dims$gap_end)
  rbind(start, pts[i:j, , drop = FALSE], end, deparse.level = 0)
}

# Stack the title, legend, graph and caption down the page. `x` and `y` are
# every point the graph occupies, in its own top down coordinates. Returns
# the page size in points, functions that move graph coordinates onto the
# page, and the title, caption and legend as drawable data frames.
graph_canvas <- function(x, y, title, caption, key_labels, key_colors,
                         key_title, dims, family) {
  graph_w <- max(x) - min(x)
  graph_h <- max(y) - min(y)
  title_w <- if (is.null(title)) 0 else
    text_width_card(title, dims$title_pt, family, 1, bold = TRUE)
  caption_w <- if (is.null(caption)) 0 else
    text_width_card(caption, dims$caption_pt, family, 1)
  page_w <- max(graph_w, title_w, caption_w, dims$min_width) + 2 * dims$margin

  lay <- list(
    content_l = dims$margin, content_r = page_w - dims$margin,
    legend_pt = dims$legend_pt, legend_row_h = dims$legend_row_h,
    legend_swatch_w = dims$legend_swatch_w, legend_swatch_h = dims$legend_swatch_h,
    legend_swatch_r = dims$legend_swatch_r, legend_gap_swatch = dims$legend_gap_swatch,
    legend_gap_item = dims$legend_gap_item, legend_gap_title = dims$legend_gap_title
  )
  key <- if (length(key_labels)) {
    legend_layout(key_labels, key_colors, key_title, lay, family, 1)
  }

  top <- dims$margin
  title_y <- top + dims$title_pt * 0.6
  if (!is.null(title)) top <- top + dims$title_pt * 1.2 + dims$gap_title
  if (!is.null(key)) {
    lay$legend_top <- top
    top <- top + key$height + dims$gap_legend
  }
  graph_top <- top
  top <- top + graph_h
  caption_y <- top + dims$gap_caption + dims$caption_pt * 0.6
  if (!is.null(caption)) top <- top + dims$gap_caption + dims$caption_pt * 1.2
  page_h <- top + dims$margin

  dx <- dims$margin + (page_w - 2 * dims$margin - graph_w) / 2 - min(x)
  dy <- graph_top - min(y)
  fy <- function(v) page_h - v
  texts <- function(label, x, y, pt, color, hjust = 0, face = "plain") {
    if (!length(label)) return(empty_texts())
    data.frame(x = x, y = fy(y), label = label, size = pt / .pt,
               colour = color, hjust = hjust, fontface = face,
               stringsAsFactors = FALSE)
  }

  list(
    width = page_w,
    height = page_h,
    px = function(v) v + dx,
    py = function(v) page_h - (v + dy),
    text = rbind(
      if (!is.null(title)) texts(title, dims$margin, title_y, dims$title_pt,
                                 graph_ink$title, face = "bold"),
      if (!is.null(caption)) texts(caption, dims$margin, caption_y,
                                   dims$caption_pt, graph_ink$muted),
      legend_text(key, lay, texts)
    ),
    shapes = legend_shapes(key, lay, function(v) v, fy)
  )
}

# The chrome, scales, coordinates and theme every graph shares.
graph_frame <- function(canvas, family) {
  list(
    draw_shapes(canvas$shapes),
    draw_text(canvas$text, family),
    scale_fill_identity(),
    scale_colour_identity(),
    scale_linetype_identity(),
    coord_cartesian(xlim = c(0, canvas$width), ylim = c(0, canvas$height),
                    expand = FALSE, clip = "off"),
    theme_race(page = graph_ink$page)
  )
}

graph_dependency <- function() {
  htmltools::htmlDependency(
    name = "ggextreme-graph",
    version = as.character(utils::packageVersion("ggextreme")),
    src = system.file("www", package = "ggextreme"),
    script = "graph.js",
    stylesheet = "graph.css",
    attachment = c("lato-regular.woff2", "lato-bold.woff2")
  )
}

#' Use an interactive graph as a widget, a ggplot or a file
#'
#' A graph built by [ggcausal()] or [ggnma()] prints as an interactive
#' widget. These
#' functions give the other forms it can take. `graph_widget()` returns the
#' 'htmlwidgets' object, for use in 'shiny' or to save with
#' [htmlwidgets::saveWidget()]. `graph_plot()` returns the underlying
#' ggplot, which draws as an ordinary static plot. `graph_save()` writes
#' either form to a file chosen by its extension.
#'
#' The graph is laid out at a fixed size, so its boxes always fit their
#' labels. The widget scales to the width of the page it sits in; a static
#' copy should be drawn at `x$width` by `x$height` inches, which is what
#' `graph_save()` does. The widget embeds a web copy of Lato, so it looks the
#' same on machines without the font.
#'
#' @param x A graph from [ggcausal()] or [ggnma()].
#' @param file Output path. `.html` writes the widget as a single file, which
#'   needs 'pandoc'; `.png` writes a static image with 'ragg'.
#' @param res Resolution of a PNG in pixels per inch.
#' @param ... Passed to [knitr::knit_print()].
#'
#' @return `graph_widget()` returns an htmlwidget and `graph_plot()` a
#'   ggplot. `graph_save()` returns `file`, invisibly.
#' @name graph_widget
#' @export
#'
#' @examples
#' dag <- ggcausal(cleft_dag$edges, cleft_dag$nodes)
#' w <- graph_widget(dag)
#' p <- graph_plot(dag)
#' \donttest{
#' graph_save(dag, tempfile(fileext = ".png"))
#' }
graph_widget <- function(x) {
  stopifnot(inherits(x, "ggx_graph"))
  hover <- ggiraph::girafe_css(
    css = "cursor:pointer;",
    # Node labels share their box's id, so they would otherwise pick up the
    # box's fill and border on hover.
    text = paste0("fill:", graph_ink$text, " !important;stroke:none !important;"),
    line = paste0("stroke:", graph_ink$hover, ";"),
    area = paste0("fill:", graph_ink$hover, ";stroke:", graph_ink$hover, ";")
  )
  w <- ggiraph::girafe(
    ggobj = x$plot,
    width_svg = x$width,
    height_svg = x$height,
    font_set = gdtools::font_set(),
    dependencies = list(graph_dependency()),
    options = list(
      ggiraph::opts_tooltip(css = tip_css, opacity = 1, delay_mouseout = 120),
      ggiraph::opts_hover(css = hover),
      ggiraph::opts_hover_inv(css = "opacity:0.4;"),
      ggiraph::opts_selection(type = "none"),
      ggiraph::opts_toolbar(saveaspng = FALSE, hidden = c("selection", "zoom", "misc")),
      ggiraph::opts_sizing(rescale = TRUE, width = 1)
    )
  )
  # Fill the width available up to the natural size, and let the height
  # follow, so the widget never crops the diagram or leaves a gap under it.
  w$width <- "100%"
  w$height <- paste0(round(x$height * 96), "px")
  htmlwidgets::onRender(w, "function(el) { ggextremeFit(el); }")
}

#' @rdname graph_widget
#' @export
graph_plot <- function(x) {
  stopifnot(inherits(x, "ggx_graph"))
  x$plot
}

#' @rdname graph_widget
#' @export
graph_save <- function(x, file, res = 300) {
  stopifnot(inherits(x, "ggx_graph"))
  ext <- tolower(tools::file_ext(file))
  if (ext == "html") {
    file <- file.path(normalizePath(dirname(file)), basename(file))
    htmlwidgets::saveWidget(graph_widget(x), file, selfcontained = TRUE,
                            title = if (is.null(x$title)) "Graph" else x$title)
  } else if (ext == "png") {
    ragg::agg_png(file, width = x$width, height = x$height, units = "in",
                  res = res, background = graph_ink$page)
    on.exit(grDevices::dev.off(), add = TRUE)
    print(x$plot)
  } else {
    rlang::abort("`file` must end in `.html` or `.png`.")
  }
  invisible(file)
}

#' @export
print.ggx_graph <- function(x, ...) {
  print(graph_widget(x))
  invisible(x)
}

#' @rdname graph_widget
#' @exportS3Method knitr::knit_print
knit_print.ggx_graph <- function(x, ...) {
  knitr::knit_print(graph_widget(x), ...)
}
