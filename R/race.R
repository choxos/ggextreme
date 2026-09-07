#' Build a bar chart race
#'
#' Turns a long panel of `name`, `time`, `value` observations into a bar chart
#' race that reproduces the 'flourish.studio' design. Values and ranks are
#' both interpolated on a uniform real time grid, so bars glide past each
#' other instead of jumping between ranks, and the returned object holds one
#' row per entity per frame. Draw a single frame with [race_frame()] or write
#' the whole animation with [animate_race()].
#'
#' Ranks are clamped to `top_n + 1` before interpolation. An entity far down
#' the field therefore waits just below the visible window and slides in from
#' the bottom edge instead of flying up from off screen, which is what makes
#' entries and exits read cleanly.
#'
#' The x axis carries no headroom: the longest bar always reaches the right
#' edge of the bar area and the axis maximum is the largest value in the
#' current frame, exactly as the reference does. Gridlines therefore drift as
#' the field grows.
#'
#' @param data A data frame in long format.
#' @param value,name,time Bare column names holding the bar length, the bar
#'   label, and the time point. `time` may be numeric or a `Date`. Each
#'   `name` and `time` pair must be unique.
#' @param top_n Number of bars visible at once.
#' @param duration Length of the animation in seconds, excluding `end_pause`.
#' @param fps Frames per second.
#' @param end_pause Seconds to hold the final frame.
#' @param palette Colors for the bars. Either an unnamed vector, recycled over
#'   the entities in alphabetical order, or a vector named by entity. Defaults
#'   to [race_palette()].
#' @param title,caption Card title and the source note under the timeline.
#' @param label_value A function formatting the number printed after each bar.
#' @param label_time A function formatting the large time label in the corner.
#'   Defaults to the floor of the interpolated time for numeric input and the
#'   year for dates, which is what the reference shows.
#' @param timeline Draw the timeline strip with the moving marker.
#' @param play_button Draw the round pause button next to the timeline, as the
#'   reference player does.
#' @param card Draw the white card and its drop shadow on a light page. Set to
#'   `FALSE` for a plain white background.
#' @param width Output width in pixels. Frames are laid out for this width, so
#'   it also fixes every font size.
#' @param res Output resolution in pixels per inch.
#' @param family Font family. The package ships Lato, the font the reference
#'   uses, and registers it on load.
#'
#' @return An object of class `ggrace`.
#' @export
#'
#' @examples
#' phones <- as.data.frame.table(datasets::WorldPhones, responseName = "phones")
#' names(phones)[1:2] <- c("year", "region")
#' phones$year <- as.numeric(as.character(phones$year))
#'
#' race <- ggrace(phones, phones, region, year, top_n = 7, duration = 5)
#' race
#'
#' # Frames are set in Lato, which only devices that understand registered
#' # fonts can use, so draw them with ragg rather than the default device.
#' file <- tempfile(fileext = ".png")
#' ragg::agg_png(file, width = race$width, height = 500, units = "px",
#'                res = race$res)
#' print(race_frame(race, 60))
#' dev.off()
#' \donttest{
#' animate_race(race, tempfile(fileext = ".gif"), cores = 1)
#' }
ggrace <- function(data, value, name, time,
                   top_n = 10,
                   duration = 25,
                   fps = 60,
                   end_pause = 2,
                   palette = NULL,
                   title = NULL,
                   caption = NULL,
                   label_value = scales::label_comma(accuracy = 1),
                   label_time = NULL,
                   timeline = TRUE,
                   play_button = TRUE,
                   card = TRUE,
                   width = 736,
                   res = 100,
                   family = "Lato") {
  value <- rlang::eval_tidy(rlang::enquo(value), data)
  name <- as.character(rlang::eval_tidy(rlang::enquo(name), data))
  time <- rlang::eval_tidy(rlang::enquo(time), data)

  if (!is.numeric(value)) rlang::abort("`value` must be numeric.")
  if (anyDuplicated(paste(name, time))) {
    rlang::abort("Each `name` and `time` pair must appear at most once.")
  }
  if (top_n < 1) rlang::abort("`top_n` must be at least 1.")

  time_num <- as.numeric(time)
  keys <- sort(unique(time_num))
  if (length(keys) < 2) {
    rlang::abort("`time` must have at least two distinct values.")
  }
  if (is.null(label_time)) label_time <- default_time_label(time)

  entities <- sort(unique(name))
  n_frames <- max(2L, as.integer(round(duration * fps)))
  grid_t <- seq(min(keys), max(keys), length.out = n_frames)

  values <- matrix(NA_real_, length(entities), length(keys),
                   dimnames = list(entities, NULL))
  values[cbind(match(name, entities), match(time_num, keys))] <- value

  ranks <- vapply(seq_along(keys), function(j) {
    column <- values[, j]
    out <- rep(top_n + 1, length(column))
    seen <- !is.na(column)
    out[seen] <- rank(-column[seen], ties.method = "first")
    pmin(out, top_n + 1)
  }, numeric(length(entities)))
  dimnames(ranks) <- dimnames(values)

  values_i <- interpolate_rows(values, keys, grid_t)
  ranks_i <- interpolate_rows(ranks, keys, grid_t)

  frames <- data.frame(
    frame = rep(seq_len(n_frames), each = length(entities)),
    time = rep(grid_t, each = length(entities)),
    name = rep(entities, times = n_frames),
    value = as.vector(values_i),
    rank = as.vector(ranks_i),
    stringsAsFactors = FALSE
  )
  frames$value[is.na(frames$value)] <- 0

  structure(
    list(
      frames = frames,
      entities = entities,
      keys = keys,
      times = grid_t,
      n_frames = n_frames,
      top_n = top_n,
      fps = fps,
      end_pause = end_pause,
      colors = assign_colors(entities, palette),
      title = title,
      caption = caption,
      label_value = label_value,
      label_time = label_time,
      timeline = timeline,
      play_button = play_button,
      card = card,
      width = width,
      res = res,
      family = family,
      layout = race_layout(top_n),
      theme = theme_race(if (card) race_ink$page else race_ink$card)
    ),
    class = "ggrace"
  )
}

#' Draw one frame of a bar chart race
#'
#' The frame is a single [ggplot2::ggplot] laid out in card units, so the
#' title, axis, bars, timeline and footer all live in one coordinate system
#' and land on measured positions rather than wherever a layout engine puts
#' them. That is what keeps the panel from shifting sideways between frames.
#'
#' Frames are set in Lato, which the package registers with 'systemfonts'.
#' Draw them on a device that understands registered fonts, such as
#' [ragg::agg_png()], which is what [animate_race()] uses. On other devices
#' pass `family = ""` to [ggrace()] to fall back to the device default.
#'
#' @param x A `ggrace` object from [ggrace()].
#' @param frame Frame index, between 1 and the number of frames.
#'
#' @return A [ggplot2::ggplot] object.
#' @export
#' @examples
#' phones <- as.data.frame.table(datasets::WorldPhones, responseName = "phones")
#' names(phones)[1:2] <- c("year", "region")
#' phones$year <- as.numeric(as.character(phones$year))
#'
#' race <- ggrace(phones, phones, region, year, top_n = 7, family = "")
#' race_frame(race, 1)
race_frame <- function(x, frame = 1L) {
  stopifnot(inherits(x, "ggrace"))
  frame <- max(1L, min(as.integer(frame), x$n_frames))
  lay <- x$layout
  pad <- lay$page_pad
  page_w <- lay$card_w + 2 * pad
  page_h <- lay$card_h + 2 * pad
  # Card units to points, so text is sized in the same units as everything
  # else in the layout.
  scale <- (x$width / x$res * 72) / page_w

  # Card units to plot units. The card runs top down, the plot bottom up.
  fx <- function(v) v + pad
  fy <- function(v) page_h - (v + pad)
  rects <- function(x0, x1, y0, y1, fill) {
    data.frame(xmin = fx(x0), xmax = fx(x1), ymin = fy(y1), ymax = fy(y0),
               fill = fill, stringsAsFactors = FALSE)
  }
  texts <- function(label, x_card, y_card, pt, color, hjust = 0,
                    face = "plain") {
    data.frame(x = fx(x_card), y = fy(y_card), label = label,
               size = pt * scale / .pt, colour = color, hjust = hjust,
               fontface = face, stringsAsFactors = FALSE)
  }

  d <- x$frames[x$frames$frame == frame & x$frames$rank <= x$top_n + 1, ]
  d$center <- lay$bars_top + (d$rank - 0.5) * lay$pitch
  top <- max(c(d$value[d$rank <= x$top_n + 0.5], 0), na.rm = TRUE)
  if (!is.finite(top) || top <= 0) top <- 1
  unit_w <- (lay$bar_x1 - lay$bar_x0) / top
  d$end <- lay$bar_x0 + d$value * unit_w

  breaks <- scales::breaks_extended(4)(c(0, top))
  breaks <- breaks[breaks >= 0 & breaks <= top]
  break_x <- lay$bar_x0 + lay$grid_dx + breaks * unit_w

  # Everything is collected into a handful of data frames first. Drawing the
  # frame as five layers rather than one per element is what keeps rendering
  # a long animation from crawling.
  page <- if (x$card) {
    shade <- grDevices::colorRampPalette(
      c(race_ink$page, race_ink$shadow)
    )(pad + 1)
    k <- pad:1
    rbind(
      rects(-k, lay$card_w + k, -k, lay$card_h + k, shade[pad - k + 2]),
      rects(0, lay$card_w, 0, lay$card_h, race_ink$card)
    )
  } else {
    rects(0, lay$card_w, 0, lay$card_h, race_ink$card)
  }

  bars <- rects(
    lay$bar_x0, d$end,
    pmax(d$center - lay$bar_h / 2, lay$bars_top),
    pmin(d$center + lay$bar_h / 2, lay$bars_bottom),
    unname(x$colors[d$name])
  )
  bars <- bars[bars$ymax > bars$ymin, ]

  ink <- rbind(
    rects(lay$content_l, lay$content_r, lay$rule_y, lay$rule_y + lay$rule_h,
          race_ink$rule),
    rects(break_x - lay$grid_w / 2, break_x + lay$grid_w / 2,
          lay$bars_top, lay$bars_bottom, race_ink$grid)
  )
  fore <- rbind(
    bars,
    rects(lay$content_l, lay$content_r, lay$foot_rule_y,
          lay$foot_rule_y + lay$rule_h, race_ink$rule)
  )
  if (x$play_button) fore <- rbind(fore, button_bars(lay, rects))
  if (x$timeline) fore <- rbind(fore, timeline_rects(x, lay, rects))

  labelled <- d[d$center > lay$bars_top & d$center < lay$bars_bottom, ]
  label_y <- labelled$center + lay$label_dy
  writing <- rbind(
    texts(format_break(breaks), break_x, lay$axis_mid, lay$axis_pt,
          race_ink$axis, hjust = 0.5),
    texts(labelled$name, lay$bar_x0 - lay$name_gap, label_y, lay$name_pt,
          race_ink$name, hjust = 1),
    texts(x$label_value(labelled$value), labelled$end + lay$value_gap,
          label_y, lay$value_pt, race_ink$value, hjust = 0)
  )
  if (!is.null(x$title)) {
    writing <- rbind(writing, texts(
      x$title, lay$content_l, lay$title_mid, lay$title_pt, race_ink$title,
      hjust = 0, face = "bold"
    ))
  }
  if (!is.null(x$caption)) {
    writing <- rbind(writing, texts(
      x$caption, lay$content_l, lay$source_mid, lay$source_pt, race_ink$source
    ))
  }
  if (x$timeline) writing <- rbind(writing, timeline_text(x, lay, texts))

  behind <- texts(x$label_time(x$times[frame]), lay$year_right, lay$year_mid,
                  lay$year_pt, race_ink$year, hjust = 1, face = "bold")
  shapes <- NULL
  if (x$play_button) shapes <- button_circle(lay, fx, fy)
  if (x$timeline) shapes <- rbind(shapes, timeline_marker(x, frame, lay, fx, fy))

  p <- ggplot() +
    x$theme +
    draw_rects(page) +
    draw_text(behind, x$family) +
    draw_rects(ink) +
    draw_shapes(shapes) +
    draw_rects(fore) +
    draw_text(writing, x$family)
  p + coord_cartesian(xlim = c(0, page_w), ylim = c(0, page_h),
                      expand = FALSE, clip = "off")
}

#' Render a bar chart race to a file
#'
#' Draws every frame with 'ragg' and encodes them. The encoder follows the
#' file extension: `.gif` uses 'gifski' and falls back to 'magick', anything
#' else is treated as video and uses 'av', falling back to an `ffmpeg` binary
#' on the search path.
#'
#' @param x A `ggrace` object from [ggrace()].
#' @param file Output path, ending in `.gif` or `.mp4`.
#' @param loop Loop the GIF. Ignored for video.
#' @param cores Number of cores to draw frames on. Frames are independent, so
#'   this scales close to linearly. Forced to 1 on Windows, where
#'   [parallel::mclapply()] cannot fork.
#' @param quiet Suppress the progress bar. Progress is not reported when
#'   drawing on more than one core.
#'
#' @return `file`, invisibly.
#' @export
animate_race <- function(x, file = "race.mp4", loop = TRUE,
                         cores = max(1L, parallel::detectCores() - 1L),
                         quiet = FALSE) {
  stopifnot(inherits(x, "ggrace"))
  lay <- x$layout
  page_w <- lay$card_w + 2 * lay$page_pad
  page_h <- lay$card_h + 2 * lay$page_pad
  width <- round(x$width)
  height <- round(width * page_h / page_w)

  dir <- tempfile("ggextreme")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  order <- c(seq_len(x$n_frames),
             rep(x$n_frames, round(x$end_pause * x$fps)))
  files <- file.path(dir, sprintf("frame%05d.png", seq_along(order)))

  draw <- function(i) {
    ragg::agg_png(files[i], width = width, height = height, units = "px",
                  res = x$res, background = race_ink$page)
    on.exit(grDevices::dev.off(), add = TRUE)
    print(race_frame(x, order[i]))
  }
  if (cores > 1) {
    parallel::mclapply(seq_along(order), draw, mc.cores = cores)
  } else {
    bar <- if (quiet) NULL else utils::txtProgressBar(max = length(order), style = 3)
    for (i in seq_along(order)) {
      draw(i)
      if (!is.null(bar)) utils::setTxtProgressBar(bar, i)
    }
    if (!is.null(bar)) close(bar)
  }
  missing <- files[!file.exists(files)]
  if (length(missing)) {
    rlang::abort(paste0(length(missing), " frames failed to draw."))
  }

  encode_frames(files, file, fps = x$fps, loop = loop,
                width = width, height = height)
  invisible(file)
}

#' @export
print.ggrace <- function(x, ...) {
  cat("<ggrace>\n")
  cat(" entities: ", length(x$entities), " (top ", x$top_n, " shown)\n", sep = "")
  cat(" keyframes:", length(x$keys), "\n")
  cat(" frames:   ", x$n_frames, " at ", x$fps, " fps\n", sep = "")
  invisible(x)
}

# Internals -------------------------------------------------------------

interpolate_rows <- function(m, keys, grid_t) {
  out <- t(apply(m, 1, function(y) {
    seen <- !is.na(y)
    if (!any(seen)) return(rep(NA_real_, length(grid_t)))
    if (sum(seen) == 1) return(rep(y[seen], length(grid_t)))
    stats::approx(keys[seen], y[seen], xout = grid_t, rule = 2)$y
  }))
  dimnames(out) <- list(rownames(m), NULL)
  out
}

assign_colors <- function(entities, palette) {
  if (is.null(palette)) palette <- race_palette(length(entities))
  if (!is.null(names(palette))) {
    missing <- setdiff(entities, names(palette))
    if (length(missing)) {
      rlang::abort(paste0("`palette` is missing colors for: ",
                          paste(missing, collapse = ", ")))
    }
    return(palette[entities])
  }
  stats::setNames(rep_len(palette, length(entities)), entities)
}

default_time_label <- function(time) {
  if (inherits(time, "Date")) {
    function(t) format(as.Date(t, origin = "1970-01-01"), "%Y")
  } else {
    function(t) format(floor(t))
  }
}

format_break <- function(b) {
  format(b, trim = TRUE, drop0trailing = TRUE, scientific = FALSE)
}

draw_rects <- function(df) {
  if (is.null(df) || !nrow(df)) return(NULL)
  geom_rect(
    data = df,
    aes(xmin = .data$xmin, xmax = .data$xmax,
        ymin = .data$ymin, ymax = .data$ymax),
    fill = df$fill
  )
}

draw_text <- function(df, family) {
  if (is.null(df) || !nrow(df)) return(NULL)
  geom_text(
    data = df,
    aes(x = .data$x, y = .data$y, label = .data$label),
    vjust = 0.5, hjust = df$hjust, size = df$size, colour = df$colour,
    fontface = df$fontface, family = family
  )
}

draw_shapes <- function(df) {
  if (is.null(df) || !nrow(df)) return(NULL)
  geom_polygon(
    data = df,
    aes(x = .data$x, y = .data$y, group = .data$group),
    fill = df$fill
  )
}

button_circle <- function(lay, fx, fy) {
  angle <- seq(0, 2 * pi, length.out = 91)
  data.frame(
    x = fx(lay$button_x + lay$button_r * cos(angle)),
    y = fy(lay$button_y + lay$button_r * sin(angle)),
    group = 1L, fill = race_ink$button, stringsAsFactors = FALSE
  )
}

button_bars <- function(lay, rects) {
  half <- lay$button_bar_gap / 2 + lay$button_bar_w
  rects(
    lay$button_x + c(-half, lay$button_bar_gap / 2),
    lay$button_x + c(-lay$button_bar_gap / 2, half),
    lay$button_y - lay$button_bar_h / 2,
    lay$button_y + lay$button_bar_h / 2,
    race_ink$button_icon
  )
}

timeline_at <- function(x, lay) {
  keys <- x$keys
  span <- diff(range(keys))
  function(t) lay$time_l + (t - min(keys)) / span * (lay$time_r - lay$time_l)
}

timeline_rects <- function(x, lay, rects) {
  at <- timeline_at(x, lay)
  labels <- timeline_breaks(x$keys)
  ends <- range(x$keys)
  tick <- function(ts, len) {
    if (!length(ts)) return(NULL)
    xs <- at(ts)
    rects(xs - lay$tick_w / 2, xs + lay$tick_w / 2,
          lay$time_y + lay$time_h, lay$time_y + lay$time_h + len,
          race_ink$timeline)
  }
  rbind(
    rects(lay$time_l, lay$time_r + 1, lay$time_y, lay$time_y + lay$time_h,
          race_ink$timeline),
    tick(setdiff(x$keys, labels), lay$tick_minor),
    tick(setdiff(labels, ends), lay$tick_major),
    tick(ends, lay$tick_end)
  )
}

# The last label is pulled back inside the axis, as the reference does.
timeline_text <- function(x, lay, texts) {
  at <- timeline_at(x, lay)
  labels <- timeline_breaks(x$keys)
  last <- labels[length(labels)]
  rest <- labels[-length(labels)]
  rbind(
    texts(x$label_time(rest), at(rest), lay$time_label_mid,
          lay$time_label_pt, race_ink$timeline, hjust = 0.5),
    texts(x$label_time(last), at(last), lay$time_label_mid,
          lay$time_label_pt, race_ink$timeline, hjust = 1)
  )
}

timeline_marker <- function(x, frame, lay, fx, fy) {
  at <- timeline_at(x, lay)
  data.frame(
    x = fx(at(x$times[frame]) + c(-10, 10, 0)),
    y = fy(lay$time_y - c(14.5, 14.5, 0.5)),
    group = 2L, fill = race_ink$marker, stringsAsFactors = FALSE
  )
}

# Labelled ticks land on both ends of the timeline, the way the reference
# splits 1990 to 2017 into nine steps of three years.
timeline_breaks <- function(keys) {
  span <- diff(range(keys))
  if (isTRUE(all.equal(span, round(span))) && span >= 2) {
    span <- round(span)
    steps <- seq_len(span)
    steps <- steps[span %% steps == 0]
    n <- steps[which.min(abs(steps - 9))]
    return(seq(min(keys), max(keys), length.out = n + 1))
  }
  breaks <- scales::breaks_pretty(9)(range(keys))
  breaks[breaks >= min(keys) & breaks <= max(keys)]
}

encode_frames <- function(files, file, fps, loop, width, height) {
  ext <- tolower(tools::file_ext(file))
  if (ext == "gif") {
    if (requireNamespace("gifski", quietly = TRUE)) {
      gifski::gifski(files, file, width = width, height = height,
                     delay = 1 / fps, loop = loop, progress = FALSE)
    } else if (requireNamespace("magick", quietly = TRUE)) {
      anim <- magick::image_animate(
        magick::image_read(files),
        fps = magick_fps(fps), loop = if (loop) 0 else 1, optimize = TRUE
      )
      magick::image_write(anim, file)
    } else {
      rlang::abort("Writing a GIF needs the gifski or magick package.")
    }
  } else if (requireNamespace("av", quietly = TRUE)) {
    av::av_encode_video(files, output = file, framerate = fps, verbose = FALSE)
  } else if (nzchar(Sys.which("ffmpeg"))) {
    pattern <- file.path(dirname(files[1]), "frame%05d.png")
    log <- system2("ffmpeg", c(
      "-y", "-loglevel", "error", "-framerate", fps, "-i", shQuote(pattern),
      "-vf", shQuote("scale=trunc(iw/2)*2:trunc(ih/2)*2"), "-c:v", "libx264",
      "-crf", "16", "-pix_fmt", "yuv420p", shQuote(file)
    ), stdout = TRUE, stderr = TRUE)
    if (!is.null(attr(log, "status"))) {
      rlang::abort(c("ffmpeg failed to encode the frames.", utils::tail(log, 5)))
    }
  } else {
    rlang::abort("Writing a video needs the av package or an ffmpeg binary.")
  }
  file
}

# GIF frame delays are whole centiseconds, so magick can only hit rates that
# divide 100.
magick_fps <- function(fps) {
  allowed <- c(1, 2, 4, 5, 10, 20, 25, 50)
  allowed[which.min(abs(allowed - fps))]
}
