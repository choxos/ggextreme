layer_of <- function(p, geom) {
  hit <- which(vapply(p$layers, function(l) class(l$geom)[1] == geom, logical(1)))
  lapply(hit, function(i) ggplot2::layer_data(p, i))
}

trial <- function() {
  patients <- data.frame(
    patient = c("A1", "A2", "B1", "B2", "B3"),
    arm = c("A", "A", "B", "B", "B"),
    months = c(4, 12, 20, 2.5, 9),
    on_treatment = c(FALSE, TRUE, TRUE, FALSE, FALSE),
    age = c(61, 55, 70, 48, 66)
  )
  events <- data.frame(
    patient = c("A1", "A1", "A2", "B1", "B2", "B3", "B3"),
    time = c(2, 4, 3, 2.5, 2.5, 3, 11),
    event = c("Partial response", "Progression", "Complete response", "Partial response",
              "Progression", "Stable disease", "Death")
  )
  list(patients = patients, events = events)
}

test_that("each lane carries one id on all its marks and one card", {
  d <- trial()
  s <- ggswimmer(d$patients, patient, months, events = d$events, group = arm,
                 ongoing = on_treatment, hover = "age", xlab = "Months")
  expect_s3_class(s, c("ggswimmer", "ggx_graph"))
  hits <- layer_of(s$plot, "GeomInteractiveRect")
  cards <- hits[[length(hits)]]
  expect_equal(sort(cards$data_id), paste0("p", 1:5))
  expect_match(cards$tooltip[cards$data_id == "p2"], "Complete response", fixed = TRUE)
  expect_match(cards$tooltip[cards$data_id == "p2"], "Ongoing", fixed = TRUE)
  expect_match(cards$onclick[cards$data_id == "p5"], "Death", fixed = TRUE)
  marks <- layer_of(s$plot, "GeomInteractivePolygon")[[1]]
  expect_true(all(marks$data_id %in% cards$data_id))
  expect_equal(s$on_render, "ggextremeSwimmer(el, data);")
  expect_s3_class(graph_widget(s), "girafe")
})

test_that("the orders put the right lanes first", {
  d <- trial()
  s <- ggswimmer(d$patients, patient, months, events = d$events, group = arm)
  orders <- s$render_data$orders
  keys <- vapply(orders, `[[`, character(1), "key")
  expect_equal(keys, c("duration", "group", "response", "data"))
  row <- function(k) unclass(orders[[match(k, keys)]]$row)
  expect_equal(which(row("duration") == 1), 3L)
  expect_equal(which(row("response") == 1), 2L)
  expect_equal(row("data"), 1:5)
  expect_true(all(row("group")[1:2] < 3))
  expect_equal(orders[[2]]$label, "Arm")
  # Static copies follow `sort`.
  by_data <- ggswimmer(d$patients, patient, months, sort = "data")
  expect_equal(by_data$layout$start_row, 1:5)
})

test_that("events are drawn by their wording", {
  expect_equal(swim_kind(c("Complete response", "PR", "Relapse", "died", "Transplant")),
               c("cr", "pr", "pd", "death", NA))
  d <- trial()
  s <- ggswimmer(d$patients, patient, months, events = d$events)
  k <- s$layout$kinds
  expect_equal(k$shape[k$event == "Stable disease"], "circle")
  expect_equal(k$shape[k$event == "Death"], "cross")
})

test_that("bad input is refused with a clear message", {
  d <- trial()
  p <- d$patients
  expect_error(ggswimmer(rbind(p, p), patient, months), "once")
  expect_error(ggswimmer(p, patient, months, events = data.frame(patient = "Z", time = 1, event = "x")),
               "not in `data`: Z")
  expect_error(ggswimmer(p, patient, months, events = data.frame(id = "A1", time = 1, event = "x")),
               "missing patient")
  expect_error(ggswimmer(p, patient, months, sort = "group"), "needs `group`")
  expect_error(ggswimmer(p, patient, months, start = months + 1), "before `start`")
})

test_that("a static copy can be drawn in either theme", {
  d <- trial()
  s <- ggswimmer(d$patients, patient, months, events = d$events, group = arm)
  png <- tempfile(fileext = ".png")
  graph_save(s, png, res = 40, theme = "dark")
  expect_true(file.exists(png))
})
