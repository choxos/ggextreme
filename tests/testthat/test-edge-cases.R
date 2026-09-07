panel <- function(names, times, values) {
  data.frame(name = rep(names, each = length(times)),
             time = rep(times, times = length(names)),
             value = values)
}

test_that("a single entity races on its own", {
  race <- ggrace(panel("A", 1:2, c(10, 20)), value, name, time,
                 duration = 1, fps = 4)
  expect_equal(nrow(race$frames), 4)
  expect_true(all(race$frames$rank == 1))
  expect_s3_class(race_frame(race, 2), "ggplot")
})

test_that("a frame with nothing to label still draws", {
  # A and B swap, so mid swap both sit off the single visible slot.
  race <- ggrace(panel(c("A", "B"), 1:2, c(10, 0, 0, 10)), value, name, time,
                 top_n = 1, duration = 1, fps = 10)
  for (i in seq_len(race$n_frames)) {
    expect_silent(ggplot2::ggplot_build(race_frame(race, i)))
  }
})

test_that("a huge time span does not blow up the timeline", {
  race <- ggrace(panel(c("A", "B"), c(0, 1e12), c(1, 2, 3, 4)), value, name,
                 time, duration = 1, fps = 2)
  expect_silent(ggplot2::ggplot_build(race_frame(race, 1)))
  expect_lt(length(ggextreme:::timeline_breaks(c(0, 1e12))), 20)
})

test_that("negative values are rejected", {
  expect_error(
    ggrace(panel(c("A", "B"), 1:2, c(1, 2, -1, 4)), value, name, time),
    "cannot be negative"
  )
})

test_that("duplicate detection compares values, not their printed form", {
  close <- panel(c("A", "B"), c(1, 1 + 1e-15), c(1, 2, 3, 4))
  expect_s3_class(ggrace(close, value, name, time, duration = 1, fps = 2),
                  "ggrace")
  dup <- data.frame(name = c("A", "A", "B", "B"), time = c(1, 1, 1, 2),
                    value = 1:4)
  expect_error(ggrace(dup, value, name, time), "at most once")
})

test_that("top_n above the field size and all ties still render", {
  race <- ggrace(panel(c("A", "B"), 1:2, rep(1, 4)), value, name, time,
                 top_n = 9, duration = 1, fps = 4)
  expect_silent(ggplot2::ggplot_build(race_frame(race, 2)))
})

test_that("an entity missing at some times slides in and out", {
  d <- data.frame(name = c("A", "A", "A", "B", "B"), time = c(1, 2, 3, 1, 3),
                  value = c(1, 2, 3, 5, 1))
  race <- ggrace(d, value, name, time, top_n = 2, duration = 1, fps = 5)
  expect_silent(ggplot2::ggplot_build(race_frame(race, 3)))
})

test_that("timeline breaks stay sane when the span has no useful divisor", {
  # 29 is prime, so the divisor rule cannot split it.
  breaks <- ggextreme:::timeline_breaks(1990:2019)
  expect_gt(length(breaks), 4)
  expect_equal(max(breaks), 2019)
  expect_equal(min(breaks), 1990)
})

test_that("bundled flags resolve by code, by name and by partial name", {
  flags <- race_flags(c("br", "IRN", "Kenya", "Iran"))
  expect_length(flags, 4)
  expect_true(all(file.exists(flags)))
  expect_equal(unname(flags[2]), unname(flags[4]))
  expect_error(race_flags("Nowhereland"), "No flag for")
})

test_that("images land on the bars and only where an image was given", {
  skip_if_not_installed("magick")
  d <- panel(c("A", "B"), 1:3, c(1, 2, 3, 3, 2, 1))
  flags <- c(A = unname(race_flags("br")))
  race <- ggrace(d, value, name, time, top_n = 2, duration = 1, fps = 5,
                 images = flags)
  expect_named(race$images, "A")
  layers <- race_frame(race, 2)$layers
  custom <- vapply(layers, function(l) inherits(l$geom, "GeomCustomAnn"),
                   logical(1))
  expect_equal(sum(custom), 1)

  plain <- ggrace(d, value, name, time, top_n = 2, duration = 1, fps = 5)
  expect_null(plain$images)
  expect_false(any(vapply(race_frame(plain, 2)$layers,
                          function(l) inherits(l$geom, "GeomCustomAnn"),
                          logical(1))))
})

test_that("the bundled dataset is intact", {
  expect_equal(nrow(clefts_qci), 450)
  expect_equal(length(unique(clefts_qci$country)), 15)
  expect_equal(range(clefts_qci$year), c(1990, 2019))
  expect_false(anyNA(clefts_qci))
  expect_true(all(file.exists(race_flags(unique(clefts_qci$iso)))))
})

test_that("an unknown code is refused rather than guessed", {
  # "aq" is not in the table; a substring match would have found Iraq.
  expect_error(race_flags("aq"), "No flag for")
  expect_error(race_flags("ATA"), "No flag for")
  expect_equal(basename(unname(race_flags("Iran"))), "ir.svg")
  expect_error(race_flags("Korea"), "No flag for")
  expect_equal(basename(unname(race_flags(factor(c("br", "de"))))),
               c("br.svg", "de.svg"))
})

test_that("a named width does not leak into the frame", {
  race <- ggrace(clefts_qci, qci, country, year, top_n = 5, duration = 1,
                 fps = 2, width = c(width = 640))
  expect_named(race_size(race), c("width", "height"))
  expect_equal(unname(race_size(race)[["width"]]), 640)
  expect_silent(ggplot2::ggplot_build(race_frame(race, 1)))
})

test_that("images must be named, and extra names are dropped", {
  d <- data.frame(name = rep(c("A", "B"), each = 2), time = rep(1:2, 2),
                  value = 1:4)
  flags <- race_flags(c("br", "de"))
  expect_error(ggrace(d, value, name, time, images = unname(flags)), "named")
  half <- flags
  names(half)[2] <- ""
  expect_error(ggrace(d, value, name, time, images = half), "named")

  skip_if_not_installed("magick")
  skip_if_not_installed("rsvg")
  race <- ggrace(d, value, name, time,
                 images = c(A = unname(flags[1]), Z = unname(flags[2])))
  expect_named(race$images, "A")
})

test_that("whole number spans keep whole number timeline labels", {
  short <- ggextreme:::timeline_breaks(2000:2005)
  expect_equal(short, round(short))
  expect_equal(anyDuplicated(floor(short)), 0L)
  expect_equal(range(short), c(2000, 2005))
  expect_equal(ggextreme:::timeline_breaks(1990:2017), seq(1990, 2017, by = 3))
  expect_equal(ggextreme:::timeline_breaks(c(5, 5)), 5)
})

test_that("breaks accept a fixed vector or an empty result", {
  d <- data.frame(name = rep(c("A", "B"), each = 2), time = rep(1:2, 2),
                  value = c(1, 9, 2, 8))
  fixed <- ggrace(d, value, name, time, top_n = 2, duration = 1, fps = 3,
                  breaks = c(0, 5))
  expect_silent(ggplot2::ggplot_build(race_frame(fixed, 2)))
  none <- ggrace(d, value, name, time, top_n = 2, duration = 1, fps = 3,
                 breaks = function(range) numeric(0))
  expect_silent(ggplot2::ggplot_build(race_frame(none, 2)))
})

test_that("drawing falls back to one core where it must", {
  expect_equal(ggextreme:::resolve_cores(4, "windows"), 1L)
  expect_equal(ggextreme:::resolve_cores(NA_integer_, "unix"), 1L)
  expect_equal(ggextreme:::resolve_cores(0, "unix"), 1L)
  expect_equal(ggextreme:::resolve_cores(integer(0), "unix"), 1L)
  expect_equal(ggextreme:::resolve_cores(4, "unix"), 4L)
})

test_that("grouping colours bars by category and builds a legend", {
  race <- ggrace(clefts_qci, qci, country, year, group = region,
                 top_n = 15, duration = 1, fps = 4, legend_title = "Region")

  expect_equal(length(unique(race$colors)), nlevels(clefts_qci$region))
  # Level order drives the legend, not alphabetical order.
  expect_equal(race$legend$items$label, levels(clefts_qci$region))
  expect_equal(race$legend$rows, 1L)
  expect_equal(race$legend$title, "Region")

  key <- unique(clefts_qci[c("country", "region")])
  by_country <- race$colors[key$country]
  expect_equal(length(unique(by_country[key$region == "Africa"])), 1)
  expect_false(unname(race$colors["Germany"]) == unname(race$colors["China"]))

  expect_silent(ggplot2::ggplot_build(race_frame(race, 2)))
})

test_that("the legend makes room for itself and can be turned off", {
  with <- ggrace(clefts_qci, qci, country, year, group = region, top_n = 5,
                 duration = 1, fps = 4)
  without <- ggrace(clefts_qci, qci, country, year, group = region, top_n = 5,
                    duration = 1, fps = 4, legend = FALSE)
  none <- ggrace(clefts_qci, qci, country, year, top_n = 5, duration = 1,
                 fps = 4)

  expect_equal(with$layout$card_h - without$layout$card_h, with$legend$height)
  expect_equal(without$layout$card_h, none$layout$card_h)
  expect_null(without$legend)
  # Colours still follow the group even with the legend hidden.
  expect_equal(length(unique(without$colors)), 5)
})

test_that("a long legend wraps onto more rows", {
  d <- clefts_qci
  d$region <- factor(paste("A rather long category name", d$region))
  race <- ggrace(d, qci, country, year, group = region, top_n = 5,
                 duration = 1, fps = 4)
  expect_gt(race$legend$rows, 1L)
  expect_equal(race$legend$height, race$legend$rows * race$layout$legend_row_h)
  expect_equal(max(race$legend$items$row), race$legend$rows - 1L)
  expect_silent(ggplot2::ggplot_build(race_frame(race, 2)))
})

test_that("an entity may only belong to one group", {
  d <- clefts_qci
  d$region <- as.character(d$region)
  d$region[d$country == "Brazil" & d$year > 2000] <- "Asia"
  expect_error(ggrace(d, qci, country, year, group = region),
               "belong to one")
})

test_that("group colours come from the palette by category", {
  race <- ggrace(clefts_qci, qci, country, year, group = region, top_n = 5,
                 duration = 1, fps = 4,
                 palette = c(Asia = "#111111", Africa = "#222222",
                             "Latin America" = "#333333",
                             "North America" = "#444444", Europe = "#555555"))
  expect_equal(unname(race$colors["Germany"]), "#555555")
  expect_equal(race$legend$items$color,
               c("#111111", "#222222", "#333333", "#444444", "#555555"))
})
