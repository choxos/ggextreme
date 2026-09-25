phones <- local({
  d <- as.data.frame.table(datasets::WorldPhones, responseName = "phones")
  names(d)[1:2] <- c("year", "region")
  d$year <- as.numeric(as.character(d$year))
  d
})

test_that("frames cover every entity on a uniform time grid", {
  race <- ggrace(phones, phones, region, year, top_n = 5, duration = 2, fps = 10)
  n_ent <- length(unique(phones$region))

  expect_equal(race$n_frames, 20)
  expect_equal(nrow(race$frames), 20 * n_ent)
  expect_equal(race$times[1], min(phones$year))
  expect_equal(race$times[race$n_frames], max(phones$year))
  expect_equal(diff(race$times), rep(diff(range(phones$year)) / 19, 19))
})

test_that("the first frame reproduces the first keyframe", {
  race <- ggrace(phones, phones, region, year, top_n = 5, duration = 2, fps = 10)
  first <- race$frames[race$frames$frame == 1, ]
  keyed <- phones[phones$year == min(phones$year), ]

  expect_equal(
    first$value[match(keyed$region, first$name)],
    keyed$phones
  )
  expect_equal(min(first$rank), 1)
})

test_that("ranks stay inside the visible window plus one", {
  race <- ggrace(phones, phones, region, year, top_n = 3, duration = 2, fps = 10)
  expect_true(all(race$frames$rank >= 1))
  expect_true(all(race$frames$rank <= 4))
})

test_that("a rank change is a short eased move, not a jump or a drift", {
  race <- ggrace(phones, phones, region, year, top_n = 7, duration = 5,
                 fps = 20, swap = 0.5)
  window <- 0.5 * 20
  moved <- tapply(race$frames$rank, race$frames$name, function(r) {
    busy <- abs(diff(r)) > 1e-8
    if (!any(busy)) return(NULL)
    c(steps = max(abs(diff(r))), run = max(rle(busy)$lengths[rle(busy)$values]))
  })
  moved <- do.call(rbind, moved)
  expect_gt(nrow(moved), 0)
  # No bar teleports, and none takes much longer than the swap window.
  expect_lt(max(moved[, "steps"]), 0.5)
  expect_lte(max(moved[, "run"]), window + 1)
})

test_that("bars rest on whole ranks between swaps", {
  race <- ggrace(phones, phones, region, year, top_n = 7, duration = 5, fps = 20)
  r <- race$frames$rank
  expect_gt(mean(abs(r - round(r)) < 1e-8), 0.8)
})

test_that("bad input is rejected", {
  expect_error(ggrace(phones, region, region, year), "must be numeric")
  expect_error(
    ggrace(rbind(phones, phones), phones, region, year),
    "at most once"
  )
  one <- phones[phones$year == 1951, ]
  expect_error(ggrace(one, phones, region, year), "two distinct values")
})

test_that("a frame is a ggplot and its geometry does not move", {
  race <- ggrace(phones, phones, region, year, top_n = 5, duration = 2, fps = 10)
  p <- race_frame(race, 7)
  expect_s3_class(p, "ggplot")

  left <- function(i) {
    d <- ggplot_build(race_frame(race, i))$data
    rects <- do.call(rbind, Filter(function(l) "xmin" %in% names(l), d))
    bars <- rects[rects$fill %in% race$colors, ]
    expect_equal(nrow(bars), race$top_n)
    min(bars$xmin)
  }
  expect_equal(left(3), left(17))
})

test_that("timeline labels land on both ends", {
  breaks <- ggextreme:::timeline_breaks(1990:2017)
  expect_equal(breaks, seq(1990, 2017, by = 3))
  expect_equal(range(breaks), c(1990, 2017))

  days <- as.numeric(seq(as.Date("2000-01-01"), as.Date("2010-01-01"), "year"))
  dated <- ggextreme:::timeline_breaks(days, use_divisors = FALSE)
  expect_true(all(diff(dated) > 0))
  expect_equal(range(dated), range(days))
  expect_lt(length(dated), 14)
  # Day counts must not be split into a step that repeats a year label.
  expect_gt(min(diff(dated)), 60)
})

test_that("colors are stable across the whole field", {
  race <- ggrace(phones, phones, region, year, top_n = 2, duration = 2, fps = 10)
  expect_named(race$colors, sort(unique(as.character(phones$region))))
  expect_length(unique(race$colors), length(race$colors))
})

test_that("a race becomes a widget that plays the same frames", {
  d <- data.frame(name = rep(c("A", "B", "C"), 3), time = rep(1:3, each = 3),
                  value = c(1, 2, 3, 3, 2, 1, 2, 5, 1))
  race <- ggrace(d, value, name, time, top_n = 2, duration = 1, fps = 10, family = "")
  g <- race_graph(race)
  expect_s3_class(g, "ggx_graph")
  expect_equal(g$on_render, "ggextremeRace(el, data);")
  rd <- g$render_data
  expect_equal(rd$n, race$n_frames)
  expect_length(rd$start, race$n_frames + 1)
  # Each frame lists the bars ranked within the window plus the one waiting.
  vis <- race$frames[race$frames$rank <= race$top_n + 1, ]
  expect_equal(length(rd$entity), nrow(vis))
  expect_equal(diff(rd$start), as.vector(table(factor(vis$frame, levels = seq_len(race$n_frames)))))
  # The labels are the race's own formatter, and the bar ends are laid out
  # against the frame's axis maximum as race_frame() does.
  first <- vis[vis$frame == 1, ]
  first <- first[order(first$rank), ]
  expect_equal(rd$value[1:nrow(first)], race$label_value(first$value))
  top <- max(first$value[first$rank <= race$top_n + 0.5])
  lay <- race$layout
  expect_equal(rd$end[1], round(lay$bar_x0 + first$value[1] * (lay$bar_x1 - lay$bar_x0) / top, 2))
  expect_equal(rd$time_labels[rd$time_index + 1], race$label_time(race$times))
  expect_equal(rd$key_frames + 1, vapply(race$keys, function(k) which.min(abs(race$times - k)), 1L))
  # The widget, the static copy and printing all work like the other graphs.
  expect_s3_class(graph_widget(race), "girafe")
  expect_s3_class(graph_plot(race), "ggplot")
  png <- tempfile(fileext = ".png")
  graph_save(race, png, res = 30)
  expect_true(file.exists(png))
})

test_that("images reach the widget as data URIs", {
  skip_if_not_installed("magick")
  skip_if_not_installed("rsvg")
  key <- unique(clefts_qci[c("country", "iso")])[1:3, ]
  d <- clefts_qci[clefts_qci$country %in% key$country & clefts_qci$year <= 1992, ]
  race <- ggrace(d, qci, country, year, top_n = 3, duration = 1, fps = 5, family = "",
                 images = stats::setNames(race_flags(key$iso), key$country))
  uris <- race_graph(race)$render_data$images
  expect_setequal(names(uris), key$country)
  expect_true(all(startsWith(unlist(uris), "data:image/svg+xml;base64,")))
})
