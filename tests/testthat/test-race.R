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

test_that("ranks move continuously between frames", {
  race <- ggrace(phones, phones, region, year, top_n = 7, duration = 5, fps = 20)
  steps <- tapply(race$frames$rank, race$frames$name, function(r) max(abs(diff(r))))
  # One frame is a hundredth of the run, so no bar may jump a whole rank.
  expect_lt(max(steps), 1)
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
  breaks <- timeline_breaks(1990:2017)
  expect_equal(breaks, seq(1990, 2017, by = 3))
  expect_equal(range(breaks), c(1990, 2017))
})

test_that("colors are stable across the whole field", {
  race <- ggrace(phones, phones, region, year, top_n = 2, duration = 2, fps = 10)
  expect_named(race$colors, sort(unique(as.character(phones$region))))
  expect_length(unique(race$colors), length(race$colors))
})
