layer_of <- function(p, geom) {
  hit <- which(vapply(p$layers, function(l) class(l$geom)[1] == geom, logical(1)))
  lapply(hit, function(i) ggplot2::layer_data(p, i))
}

lung_data <- function() {
  d <- survival::lung
  d$sex <- factor(d$sex, labels = c("Male", "Female"))
  d$ph <- factor(d$ph.ecog)
  d
}

km_fit <- function(...) {
  ggkm(survival::Surv(time, status) ~ sex, data = lung_data(), ...)
}

test_that("every time slice carries a readout with the hazard ratio", {
  skip_if_not_installed("survival")
  km <- km_fit()
  expect_s3_class(km, c("ggkm", "ggx_graph"))
  slices <- layer_of(km$plot, "GeomInteractiveRect")[[1]]
  expect_equal(nrow(slices), km_dims$slices)
  expect_true(all(grepl("^t\\d+_\\d+$", slices$data_id)))
  expect_match(slices$tooltip[40], "Hazard ratio vs Male", fixed = TRUE)
  expect_match(slices$onclick[40], "Smoothed Schoenfeld residuals", fixed = TRUE)
  # The interaction models are only fitted for the tests.
  expect_false(grepl("Group by log time model", slices$onclick[40], fixed = TRUE))
  expect_equal(km$on_render, "ggextremeKm(el);")
  expect_null(km$ph)
  expect_equal(km$hover_inv, "")
  expect_s3_class(graph_widget(km), "girafe")
})

test_that("the risk table matches survfit", {
  skip_if_not_installed("survival")
  km <- km_fit(breaks = c(0, 250, 500, 750))
  cells <- layer_of(km$plot, "GeomInteractiveText")[[1]]
  fit <- survival::survfit(survival::Surv(time, status) ~ sex, data = lung_data())
  s <- summary(fit, times = c(0, 250, 500, 750), extend = TRUE)
  expect_equal(as.numeric(cells$label[grepl("_1$", cells$data_id)]), s$n.risk[1:4])
  expect_equal(sort(unique(sub("_\\d+$", "", cells$data_id))), paste0("r", 1:4))
})

test_that("the proportional hazards tests go in a collapsed section", {
  skip_if_not_installed("survival")
  km <- km_fit(ph_tests = TRUE)
  expect_match(km$on_render, "ggextremeDetails(el, ", fixed = TRUE)
  expect_match(km$on_render, "Hazard ratios and proportional hazards", fixed = TRUE)
  expect_match(km$on_render, "Grambsch and Therneau", fixed = TRUE)
  slices <- layer_of(km$plot, "GeomInteractiveRect")[[1]]
  expect_match(slices$onclick[40], "Group by log time model", fixed = TRUE)
  # The table is part of the widget, not the plot.
  labels <- layer_of(km$plot, "GeomText")
  expect_false(any(vapply(labels, function(d) any(d$label == "Schoenfeld residuals"),
                          logical(1))))
})

test_that("the proportional hazards table matches survival's own tests", {
  skip_if_not_installed("survival")
  d <- lung_data()
  km <- km_fit(ph_tests = TRUE)
  ph <- km$ph
  cox <- survival::coxph(survival::Surv(time, status) ~ sex, data = d)
  zph <- survival::cox.zph(cox, transform = "km", terms = FALSE)
  lr <- survival::survdiff(survival::Surv(time, status) ~ sex, data = d)
  expect_equal(ph$p[ph$test == "Cox model"], summary(cox)$coefficients[, "Pr(>|z|)"],
               ignore_attr = TRUE)
  expect_equal(ph$p[ph$test == "Log-rank test"], stats::pchisq(lr$chisq, 1, lower.tail = FALSE))
  expect_equal(ph$p[ph$test == "Schoenfeld residuals"], zph$table[, "p"], ignore_attr = TRUE)
  expect_match(ph$estimate[1], sprintf("HR %.2f", exp(stats::coef(cox))), fixed = TRUE)
  expect_true(all(c("Group \u00d7 time", "Group \u00d7 log time") %in% ph$test))
})

test_that("each method gives the hazard ratio it should", {
  skip_if_not_installed("survival")
  d <- lung_data()
  input <- km_prepare(survival::Surv(time, status) ~ sex, d, NULL, "schoenfeld", TRUE, NULL)
  times <- c(100, 300, 600)
  b <- stats::coef(input$cox)
  flat <- km_hr_curves(input, times, "constant")[[1]]
  expect_equal(flat$est, rep(unname(b), 3))
  lin <- km_hr_curves(input, times, "linear")[[1]]
  bl <- stats::coef(input$lin)
  expect_equal(lin$est, unname(bl[1] + bl[2] * times))
  lg <- km_hr_curves(input, times, "log")[[1]]
  bg <- stats::coef(input$logt)
  expect_equal(lg$est, unname(bg[1] + bg[2] * log(times)))
  smooth <- km_hr_curves(input, c(times, 5000), "schoenfeld")[[1]]
  expect_false(any(is.na(smooth$est[1:3])))
  expect_true(is.na(smooth$est[4]))
  expect_true(all(is.na(km_hr_curves(input, times, "none")[[1]]$est)))
})

test_that("the cumulative incidence view starts at zero", {
  skip_if_not_installed("survival")
  km <- km_fit(type = "risk", ph_tests = FALSE)
  paths <- layer_of(km$plot, "GeomInteractivePath")[[1]]
  first <- paths[!duplicated(paths$data_id), ]
  expect_equal(unname(first$y), rep(min(paths$y), nrow(first)))
  expect_null(km$ph)
})

test_that("the reference group and the hazard ratio method can be chosen", {
  skip_if_not_installed("survival")
  km <- km_fit(reference = "Female", hr_time = "none", ph_tests = FALSE)
  slices <- layer_of(km$plot, "GeomInteractiveRect")[[1]]
  expect_false(grepl("Hazard ratio vs", slices$tooltip[10], fixed = TRUE))
  lines <- layer_of(km$plot, "GeomInteractivePath")[[1]]
  expect_match(lines$tooltip[lines$data_id == "a2"][1], "Hazard ratio vs Female", fixed = TRUE)
})

test_that("more than two groups compare each with the reference", {
  skip_if_not_installed("survival")
  d <- lung_data()
  d <- d[!is.na(d$ph) & d$ph %in% c("0", "1", "2"), ]
  d$ph <- droplevels(d$ph)
  km <- ggkm(survival::Surv(time, status) ~ ph, data = d, ph_tests = TRUE)
  expect_equal(sum(km$ph$test == "Cox model"), 2)
  expect_equal(km$ph$comparison[km$ph$test == "Schoenfeld residuals"],
               c("1 vs 0", "2 vs 0", "Global"))
})

test_that("bad input is refused with a clear message", {
  skip_if_not_installed("survival")
  d <- lung_data()
  expect_error(ggkm(time ~ sex, d), "right censored")
  expect_error(ggkm(survival::Surv(time, status) ~ sex + age, d), "single grouping")
  expect_error(ggkm(survival::Surv(time, status) ~ sex, d[d$sex == "Male", ]), "at least two")
  expect_error(km_fit(reference = "Other"), "must be one of: Male, Female")
  expect_error(ggkm(survival::Surv(time, status) ~ sex, as.list(d)), "data frame")
})

test_that("the curves are drawn over follow-up into a file", {
  skip_if_not_installed("survival")
  skip_if_not(requireNamespace("gifski", quietly = TRUE) ||
                requireNamespace("magick", quietly = TRUE))
  skip_on_cran()
  km <- km_fit(ph_tests = FALSE)
  file <- tempfile(fileext = ".gif")
  animate_km(km, file, duration = 0.6, end_pause = 0.2, fps = 5, res = 40,
             cores = 1, quiet = TRUE)
  expect_true(file.exists(file))
})
