#' Draw an interactive choropleth map over time
#'
#' Colors every region of a map by a measure, with one map per measure side
#' by side, and a slider and play button under them that step through the
#' years. All the maps show the same year, so a region's measures read
#' together: hovering over a country outlines it on every map and lists its
#' value and rank on each measure that year, and clicking it opens its whole
#' series under the maps. The year can be dragged to or played through, and
#' the shades change in place. Ranks count from the highest value that year,
#' so for a measure where lower is better, such as mortality, rank 1 is the
#' worst.
#'
#' The default map is the world: Natural Earth's 1:50m countries in the Equal
#' Earth projection, bundled with the package. Regions are matched by
#' ISO 3166-1 alpha-3 or alpha-2 code, or by English name, allowing for case,
#' accents and punctuation and for the forms the WHO and the Global Burden of
#' Disease study use, such as "Iran (Islamic Republic of)". Rows for places
#' that are not on the map, such as world regions and income groups, are left
#' out with a message naming them.
#'
#' Any other map can be given as an 'sf' object of polygons, projected as it
#' should be drawn, with `map_id` naming the column that `region` matches. A
#' map in longitude and latitude is scaled so a degree of longitude has its
#' true length at the middle of the map. Detailed boundaries make a heavy
#' page, so simplify them first, for example with [sf::st_simplify()].
#'
#' Each measure has its own color scale, fixed across the years, so a change
#' of shade is a change of value. A measure with values on both sides of zero,
#' such as a change since the first year, gets a diverging scale centered on
#' zero. Shades are a color at increasing strength, drawn with transparency,
#' so a map suits a light or a dark page.
#'
#' @param data A data frame with a row for each region and time.
#' @param region Column of `data` naming the region: a code or an English
#'   name for the world map, or a value of `map_id` for an 'sf' map.
#' @param time Column of `data` giving the time, usually the year.
#' @param values Columns of `data` to map, as a character vector, one map
#'   each. Names label the maps; without them, the columns' `label`
#'   attributes or names are used.
#' @param map `"world"`, or an 'sf' object of polygons.
#' @param map_id For an 'sf' map, the column holding the keys `region`
#'   matches. Defaults to the first character or factor column.
#' @param at The time shown first, and in static copies. Defaults to the
#'   last.
#' @param ncol Maps per row. Defaults to two, or one for a single measure.
#' @param palette Colors for the maps, one entry per measure, in order or
#'   named by map label. One color gives a sequential scale; two give a
#'   diverging scale, the first for values below zero and the second for
#'   values above.
#' @param interval Seconds each time is shown while playing.
#' @param title,caption Title above the maps and note below them.
#' @param family Font family. The package ships Lato and registers it on load.
#'
#' @return An object of class `ggchoropleth`, which prints as an interactive
#'   widget. Use [graph_widget()], [graph_plot()] or [graph_save()] for the
#'   widget, a static ggplot of the year `at` or a file, and
#'   [animate_choropleth()] to play the years as an animation.
#' @export
#'
#' @examples
#' qci <- clefts_qci_world
#' first <- qci$qci[qci$year == 1990][match(qci$iso3, qci$iso3[qci$year == 1990])]
#' qci$change <- qci$qci - first
#' ggchoropleth(qci, iso3, year,
#'              values = c("Quality of Care Index" = "qci",
#'                         "Change since 1990" = "change"),
#'              title = "Quality of care for orofacial clefts")
ggchoropleth <- function(data, region, time, values,
                         map = "world",
                         map_id = NULL,
                         at = NULL,
                         ncol = NULL,
                         palette = NULL,
                         interval = 0.6,
                         title = NULL,
                         caption = NULL,
                         family = "Lato") {
  if (!is.data.frame(data)) rlang::abort("`data` must be a data frame.")
  keys <- rlang::eval_tidy(rlang::enquo(region), data)
  times <- rlang::eval_tidy(rlang::enquo(time), data)
  if (!is.numeric(interval) || length(interval) != 1 || !(interval > 0)) {
    rlang::abort("`interval` must be a positive number of seconds.")
  }
  input <- map_prepare(data, keys, times, values, map, map_id)
  lay <- map_layout(input, at, ncol, palette, title, caption, family)
  structure(
    list(plot = map_draw(lay, map_values(lay, lay$start), lay$time_labels[lay$start]),
         width = lay$page$width / 72, height = lay$page$height / 72, title = title,
         on_render = "ggextremeMap(el, data);", render_data = map_data(lay, interval),
         hover_inv = "", layout = lay),
    class = c("ggchoropleth", "ggx_graph")
  )
}

#' Animate a choropleth map
#'
#' Plays the years of a [ggchoropleth()] map, easing each region from one
#' year's shade to the next, and writes the result to a GIF or MP4.
#'
#' @param x A map from [ggchoropleth()].
#' @param file Output path, ending in `.gif` or `.mp4`.
#' @param step Seconds from one time to the next.
#' @inheritParams animate_km
#'
#' @return `file`, invisibly.
#' @export
#'
#' @examples
#' \donttest{
#' m <- ggchoropleth(subset(clefts_qci_world, year >= 2015), iso3, year,
#'                   values = c(QCI = "qci"))
#' animate_choropleth(m, tempfile(fileext = ".gif"), step = 0.3, fps = 6,
#'                    res = 60, cores = 1)
#' }
animate_choropleth <- function(x, file = "map.gif", step = 0.6, end_pause = 2,
                               fps = 20, res = 150, loop = TRUE,
                               cores = max(1L, parallel::detectCores() - 1L),
                               quiet = FALSE) {
  stopifnot(inherits(x, "ggchoropleth"))
  lay <- x$layout
  n_t <- length(lay$time_labels)
  k <- max(1L, round(step * fps))
  pos <- 1 + (0:((n_t - 1) * k)) / k
  n <- length(pos)
  plan <- c(seq_len(n), rep(n, round(end_pause * fps)))
  # Between two times every region eases from one value to the next; a
  # region with a value at only one of them switches halfway.
  frame <- function(i) {
    a <- floor(pos[i])
    b <- min(a + 1L, n_t)
    f <- pos[i] - a
    values <- lapply(lay$mats, function(m) {
      v <- (1 - f) * m[a, ] + f * m[b, ]
      gap <- is.na(v)
      v[gap] <- if (f < 0.5) m[a, gap] else m[b, gap]
      v
    })
    map_draw(lay, values, lay$time_labels[floor(pos[i] + 0.5)], interactive = FALSE)
  }
  width <- round(x$width * res)
  height <- round(x$height * res)
  dir <- tempfile("ggextreme")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  drawn <- render_frames(file.path(dir, "unique"), n, frame, width, height, res,
                         graph_ink$page, cores, quiet)
  files <- file.path(dir, sprintf("frame%05d.png", seq_along(plan)))
  if (!all(file.copy(drawn[plan], files))) rlang::abort("Frames could not be assembled.")
  encode_frames(files, file, fps = fps, loop = loop, width = width, height = height)
  invisible(file)
}

map_dims <- utils::modifyList(graph_dims, list(
  panel_w = 340,
  panel_w_single = 560,
  max_aspect = 1.1,
  head_pt = 11,
  gap_head = 6,
  gap_x = 24,
  gap_y = 20,
  bar_w = 200,
  bar_h = 7,
  bar_steps = 48,
  gap_bar = 10,
  gap_tick = 3,
  tick_pt = 8.5,
  border_pt = 0.35,
  alpha_low = 0.12,
  alpha_zero = 0.06
))

map_ink <- list(
  sequential = c("#22928F", "#7A5AA6", "#C0603F", "#3F74B5"),
  diverging = c("#C0603F", "#22928F")
)

# A place name reduced to lower case letters, so names match whatever their
# case, accents, spaces or punctuation. The accents are mapped by hand, since
# iconv() transliterates differently from one platform to the next.
normalize_region <- function(x) {
  x <- tolower(enc2utf8(as.character(x)))
  x <- gsub("&", "and", x, fixed = TRUE)
  x <- gsub("\u00df", "ss", x, fixed = TRUE)
  x <- gsub("\u00e6", "ae", x, fixed = TRUE)
  x <- gsub("\u0153", "oe", x, fixed = TRUE)
  x <- chartr("\u00e0\u00e1\u00e2\u00e3\u00e4\u00e5\u0101\u0103\u0105\u00e7\u0107\u010d\u010f\u0111\u00e8\u00e9\u00ea\u00eb\u0113\u0117\u0119\u011b\u011f\u00ec\u00ed\u00ee\u00ef\u012b\u0131\u0142\u00f1\u0144\u0148\u00f2\u00f3\u00f4\u00f5\u00f6\u00f8\u014d\u0151\u0159\u015b\u015f\u0161\u0163\u0165\u00f9\u00fa\u00fb\u00fc\u016b\u016f\u0171\u00fd\u00ff\u017a\u017c\u017e",
              "aaaaaaaaacccddeeeeeeeegiiiiiilnnnoooooooorsssttuuuuuuuyyzzz", x)
  gsub("[^a-z]", "", x)
}

# Map codes for region keys: an ISO 3166-1 alpha-3 or alpha-2 code, a name,
# or a name with a trailing qualifier in brackets dropped, in that order.
world_match <- function(keys) {
  keys <- as.character(keys)
  up <- toupper(trimws(keys))
  ids <- unique(world_geometry$id)
  id <- ifelse(nchar(up) == 3 & up %in% ids, up, NA_character_)
  two <- is.na(id) & nchar(up) == 2 & up %in% names(world_iso2)
  id[two] <- world_iso2[up[two]]
  code <- !is.na(id)
  look <- function(k) world_names$id[match(k, world_names$key)]
  left <- is.na(id) & !is.na(keys)
  id[left] <- look(normalize_region(keys[left]))
  left <- is.na(id) & !is.na(keys)
  id[left] <- look(normalize_region(sub("\\s*\\([^)]*\\)\\s*$", "", keys[left])))
  list(id = unname(id), code = code)
}

# The polygons of a map as rings, with a key and a label for each region.
map_shapes <- function(map, map_id) {
  if (is.character(map) && identical(map, "world")) {
    ids <- unique(world_geometry$id)
    return(list(geo = world_geometry, ids = ids, labels = unname(world_labels[ids]),
                world = TRUE))
  }
  if (!inherits(map, "sf")) rlang::abort("`map` must be \"world\" or an sf object of polygons.")
  rlang::check_installed("sf", "to draw an sf map.")
  fields <- sf::st_drop_geometry(map)
  if (is.null(map_id)) {
    text <- names(fields)[vapply(fields, function(v) is.character(v) || is.factor(v), logical(1))]
    if (!length(text)) rlang::abort("Name the column of `map` that `region` matches with `map_id`.")
    map_id <- text[1]
  }
  if (!is.character(map_id) || length(map_id) != 1 || !map_id %in% names(fields)) {
    rlang::abort("`map_id` must name a column of `map`.")
  }
  g <- sf::st_geometry(map)
  if (!all(as.character(sf::st_geometry_type(g)) %in% c("POLYGON", "MULTIPOLYGON"))) {
    rlang::abort("`map` must hold polygons.")
  }
  xy <- sf::st_coordinates(sf::st_cast(g, "MULTIPOLYGON"))
  x <- xy[, "X"]
  y <- xy[, "Y"]
  if (isTRUE(sf::st_is_longlat(map))) x <- x * cos(mean(range(y)) * pi / 180)
  keys <- as.character(fields[[map_id]])
  geo <- data.frame(
    id = keys[xy[, "L3"]], x = x, y = y,
    piece = as.integer(factor(paste(xy[, "L3"], xy[, "L2"]))),
    ring = as.integer(xy[, "L1"]), stringsAsFactors = FALSE
  )
  geo <- geo[!is.na(geo$id), ]
  ids <- unique(geo$id)
  list(geo = geo, ids = ids, labels = ids, world = FALSE)
}

# Check the input and lay the data out as one matrix per measure, with a row
# for each time and a column for each region on the map.
map_prepare <- function(data, keys, times, values, map, map_id) {
  if (length(keys) != nrow(data) || length(times) != nrow(data)) {
    rlang::abort("`region` and `time` must be columns of `data`.")
  }
  if (!is.character(values) || !length(values) || !all(values %in% names(data))) {
    rlang::abort("`values` must name columns of `data`.")
  }
  if (!all(vapply(data[values], is.numeric, logical(1)))) {
    rlang::abort("Every column in `values` must be numeric.")
  }
  labels <- column_labels(data, values)
  if (!is.null(names(values))) labels <- ifelse(nzchar(names(values)), names(values), labels)

  shape <- map_shapes(map, map_id)
  keys <- as.character(keys)
  found <- if (shape$world) world_match(keys) else
    list(id = ifelse(keys %in% shape$ids, keys, NA_character_), code = rep(FALSE, length(keys)))
  lost <- unique(keys[is.na(found$id) & !is.na(keys)])
  if (length(lost)) {
    rlang::inform(paste0(
      length(lost), if (length(lost) == 1) " region is" else " regions are",
      " not on the map and left out: ", paste(utils::head(lost, 10), collapse = ", "),
      if (length(lost) > 10) ", and more", "."
    ))
  }
  keep <- !is.na(found$id) & !is.na(times)
  if (!any(keep)) rlang::abort("No region in `data` is on the map.")
  time_levels <- sort(unique(times[keep]))
  r <- match(found$id[keep], shape$ids)
  t <- match(times[keep], time_levels)
  twice <- duplicated(cbind(r, t))
  if (any(twice)) {
    i <- which(twice)[1]
    rlang::abort(sprintf("`data` has more than one row for %s at %s.",
                         keys[keep][i], format(time_levels[t[i]])))
  }
  mats <- lapply(values, function(col) {
    m <- matrix(NA_real_, length(time_levels), length(shape$ids))
    m[cbind(t, r)] <- data[[col]][keep]
    m
  })
  empty <- vapply(mats, function(m) all(is.na(m)), logical(1))
  if (any(empty)) rlang::abort(paste0("`", values[empty][1], "` has no values for regions on the map."))

  # Name regions as the data does, unless the data gave codes.
  names_out <- shape$labels
  named <- !found$code[keep]
  names_out[r[named]] <- keys[keep][named]
  list(geo = shape$geo, ids = shape$ids, names = names_out, mats = unname(mats),
       labels = unname(labels), times = time_levels,
       time_labels = trimws(format(time_levels)))
}

# One entry per measure: a color for a sequential scale, or two for a
# diverging one. Measures with values on both sides of zero diverge.
map_palettes <- function(palette, labels, mats) {
  out <- lapply(seq_along(mats), function(j) {
    r <- range(mats[[j]], na.rm = TRUE)
    if (r[1] < 0 && r[2] > 0) map_ink$diverging
    else map_ink$sequential[(j - 1) %% length(map_ink$sequential) + 1]
  })
  if (is.null(palette)) return(out)
  palette <- as.list(palette)
  if (!is.null(names(palette))) {
    bad <- setdiff(names(palette), labels)
    if (length(bad)) rlang::abort(paste0("`palette` names no map called ", bad[1], "."))
    hit <- match(labels, names(palette))
    out[!is.na(hit)] <- palette[hit[!is.na(hit)]]
  } else {
    if (length(palette) != length(mats)) {
      rlang::abort("`palette` must have one entry per measure, or be named by map label.")
    }
    out <- palette
  }
  if (!all(vapply(out, function(p) is.character(p) && length(p) %in% 1:2, logical(1)))) {
    rlang::abort("Each entry of `palette` must be one color or two.")
  }
  out
}

map_scale <- function(m, colors) {
  r <- range(m, na.rm = TRUE)
  if (length(colors) == 2) {
    h <- max(abs(r))
    if (h == 0) h <- 1
    return(list(colors = colors, lo = -h, hi = h, diverging = TRUE))
  }
  if (r[1] == r[2]) r <- r + c(-0.5, 0.5)
  list(colors = colors, lo = r[1], hi = r[2], diverging = FALSE)
}

# Fill and strength for each value. A sequential scale strengthens one color
# from the lowest value to the highest; a diverging one strengthens each of
# its colors away from zero.
map_shade <- function(v, s) {
  if (s$diverging) {
    t <- pmin(abs(v) / s$hi, 1)
    fill <- ifelse(!is.na(v) & v < 0, s$colors[1], s$colors[2])
    alpha <- map_dims$alpha_zero + (1 - map_dims$alpha_zero) * t
  } else {
    t <- pmin(pmax((v - s$lo) / (s$hi - s$lo), 0), 1)
    fill <- rep(s$colors[1], length(v))
    alpha <- map_dims$alpha_low + (1 - map_dims$alpha_low) * t
  }
  none <- is.na(v)
  fill[none] <- graph_ink$ghost
  alpha[none] <- 1
  list(fill = fill, alpha = round(alpha, 3))
}

# Decimals to show a measure with three significant digits at its largest.
map_digits <- function(m) {
  top <- max(abs(m), na.rm = TRUE)
  if (top == 0) return(0L)
  as.integer(max(0, min(6, 3 - ceiling(log10(top)))))
}

map_values <- function(lay, t) lapply(lay$mats, function(m) m[t, ])

map_layout <- function(input, at, ncol, palette, title, caption, family) {
  dims <- map_dims
  k <- length(input$mats)
  start <- if (is.null(at)) length(input$times) else match(at, input$times)
  if (length(start) != 1 || is.na(start)) {
    rlang::abort(paste0("`at` must be one of the times in `data`: ",
                        paste(utils::head(input$time_labels, 6), collapse = ", "),
                        if (length(input$times) > 6) ", ...", "."))
  }
  ncol <- if (is.null(ncol)) min(k, 2L) else as.integer(ncol)
  if (length(ncol) != 1 || is.na(ncol) || ncol < 1) rlang::abort("`ncol` must be a positive whole number.")
  ncol <- min(ncol, k)
  nrow <- ceiling(k / ncol)

  geo <- input$geo
  ext_x <- range(geo$x)
  ext_y <- range(geo$y)
  aspect <- diff(ext_y) / diff(ext_x)
  pw <- if (ncol == 1) dims$panel_w_single else dims$panel_w
  ph <- pw * aspect
  if (ph > dims$max_aspect * pw) {
    ph <- dims$max_aspect * pw
    pw <- ph / aspect
  }
  head_h <- dims$head_pt * dims$line_h
  block_h <- head_h + dims$gap_head + ph + dims$gap_bar + dims$bar_h + dims$gap_tick +
    dims$tick_pt * dims$line_h
  grid_w <- ncol * pw + (ncol - 1) * dims$gap_x
  grid_h <- nrow * block_h + (nrow - 1) * dims$gap_y
  page <- graph_canvas(c(0, grid_w), c(0, grid_h), title, caption, NULL, NULL, NULL,
                       dims, family)
  px <- page$px
  py <- page$py

  pals <- map_palettes(palette, input$labels, input$mats)
  scales <- lapply(seq_len(k), function(j) map_scale(input$mats[[j]], pals[[j]]))
  panels <- lapply(seq_len(k), function(j) {
    x0 <- ((j - 1) %% ncol) * (pw + dims$gap_x)
    y0 <- ((j - 1) %/% ncol) * (block_h + dims$gap_y)
    top <- y0 + head_h + dims$gap_head
    list(x0 = x0, y0 = y0, top = top, bar_y = top + ph + dims$gap_bar,
         polys = data.frame(
           x = px(x0 + (geo$x - ext_x[1]) / diff(ext_x) * pw),
           y = py(top + (ext_y[2] - geo$y) / diff(ext_y) * ph),
           r = match(geo$id, input$ids),
           sub = paste(geo$piece, geo$ring),
           stringsAsFactors = FALSE
         ))
  })
  list(dims = dims, page = page, family = family, pw = pw, panels = panels,
       scales = scales, mats = input$mats, labels = input$labels, names = input$names,
       times = input$times, time_labels = input$time_labels, start = start,
       digits = vapply(input$mats, map_digits, integer(1)))
}

# The maps for one time. `values` holds a vector of region values for each
# measure; `label` names the time.
map_draw <- function(lay, values, label, interactive = TRUE) {
  dims <- lay$dims
  px <- lay$page$px
  py <- lay$page$py
  texts <- function(label, x, y, pt, color, hjust = 0, face = "plain") {
    if (!length(label)) return(empty_texts())
    data.frame(x = px(x), y = py(y), label = as.character(label), size = pt / .pt,
               colour = color, hjust = hjust, fontface = face, stringsAsFactors = FALSE)
  }
  p <- ggplot()
  labels <- NULL
  stamps <- NULL
  bars <- NULL
  for (j in seq_along(lay$panels)) {
    pan <- lay$panels[[j]]
    s <- lay$scales[[j]]
    shade <- map_shade(values[[j]], s)
    d <- pan$polys
    d$fill <- shade$fill[d$r]
    d$alpha <- shade$alpha[d$r]
    p <- p + if (interactive) {
      d$id <- paste0("g", d$r)
      d$tooltip <- esc(lay$names[d$r])
      ggiraph::geom_polygon_interactive(
        data = d,
        aes(x = .data$x, y = .data$y, group = .data$r, subgroup = .data$sub,
            fill = .data$fill, alpha = .data$alpha, data_id = .data$id,
            tooltip = .data$tooltip),
        colour = graph_ink$page, linewidth = dims$border_pt / .pt
      )
    } else {
      geom_polygon(data = d, aes(x = .data$x, y = .data$y, group = .data$r,
                                 subgroup = .data$sub, fill = .data$fill,
                                 alpha = .data$alpha),
                   colour = graph_ink$page, linewidth = dims$border_pt / .pt)
    }

    # Header, then the color bar with its ticks under the map.
    head_y <- pan$y0 + dims$head_pt * 0.6
    labels <- rbind(labels, texts(lay$labels[j], pan$x0, head_y, dims$head_pt,
                                  graph_ink$title, face = "bold"))
    stamps <- rbind(stamps, texts(label, pan$x0 + lay$pw, head_y, dims$head_pt,
                                  graph_ink$muted, hjust = 1, face = "bold"))
    bar_w <- min(dims$bar_w, lay$pw * 0.6)
    steps <- seq(s$lo, s$hi, length.out = dims$bar_steps)
    tone <- map_shade(steps, s)
    edges <- pan$x0 + (0:dims$bar_steps) / dims$bar_steps * bar_w
    bars <- rbind(bars, data.frame(
      xmin = px(edges[-length(edges)]), xmax = px(edges[-1]),
      ymin = py(pan$bar_y + dims$bar_h), ymax = py(pan$bar_y),
      fill = tone$fill, alpha = tone$alpha, stringsAsFactors = FALSE
    ))
    ticks <- pretty(c(s$lo, s$hi), 4)
    ticks <- ticks[ticks >= s$lo - 1e-9 * abs(s$hi - s$lo) & ticks <= s$hi + 1e-9 * abs(s$hi - s$lo)]
    tick_x <- pan$x0 + (ticks - s$lo) / (s$hi - s$lo) * bar_w
    tick_y <- pan$bar_y + dims$bar_h + dims$gap_tick + dims$tick_pt * 0.6
    labels <- rbind(labels, texts(format_values(ticks), tick_x, rep(tick_y, length(ticks)),
                                  dims$tick_pt, graph_ink$muted, hjust = 0.5))
    if (anyNA(lay$mats[[j]])) {
      box_x <- pan$x0 + bar_w + 16
      bars <- rbind(bars, data.frame(
        xmin = px(box_x), xmax = px(box_x + dims$bar_h),
        ymin = py(pan$bar_y + dims$bar_h), ymax = py(pan$bar_y),
        fill = graph_ink$ghost, alpha = 1, stringsAsFactors = FALSE
      ))
      labels <- rbind(labels, texts("No data", box_x + dims$bar_h + 5,
                                    pan$bar_y + dims$bar_h / 2, dims$tick_pt,
                                    graph_ink$muted))
    }
  }
  p <- p + geom_rect(data = bars, aes(xmin = .data$xmin, xmax = .data$xmax,
                                      ymin = .data$ymin, ymax = .data$ymax,
                                      fill = .data$fill, alpha = .data$alpha))
  # The time in each header carries an id, so the widget can change it.
  if (interactive) {
    stamps$id <- "yr"
    p <- p + ggiraph::geom_text_interactive(
      data = stamps, aes(x = .data$x, y = .data$y, label = .data$label, data_id = .data$id),
      hjust = 1, vjust = 0.5, size = stamps$size, colour = stamps$colour,
      fontface = "bold", family = lay$family
    )
  } else {
    labels <- rbind(labels, stamps)
  }
  p + draw_text(labels, lay$family) + graph_frame(lay$page, lay$family)
}

# What the widget needs to redraw the maps at any time: the fill and
# strength of every region at every time, and the values for the hover card
# and the series panel. Matrices have a row for each time.
map_data <- function(lay, interval) {
  n_t <- length(lay$times)
  list(
    times = I(lay$time_labels),
    start = lay$start - 1L,
    interval = round(1000 * interval),
    names = I(unname(lay$names)),
    panels = lapply(seq_along(lay$mats), function(j) {
      m <- lay$mats[[j]]
      s <- lay$scales[[j]]
      shades <- lapply(seq_len(n_t), function(t) map_shade(m[t, ], s))
      fill <- do.call(rbind, lapply(shades, `[[`, "fill"))
      tones <- unique(c(fill))
      list(
        label = lay$labels[j],
        digits = lay$digits[j],
        grouping = max(abs(m), na.rm = TRUE) >= 1e4,
        line = s$colors[length(s$colors)],
        value = round(m, lay$digits[j]),
        # Fills are indices into `tones`, which keeps the page small.
        tones = I(tones),
        fill = matrix(match(fill, tones) - 1L, nrow(fill)),
        alpha = do.call(rbind, lapply(shades, `[[`, "alpha"))
      )
    })
  )
}
