layer_of <- function(p, geom) {
  hit <- which(vapply(p$layers, function(l) class(l$geom)[1] == geom, logical(1)))
  lapply(hit, function(i) ggplot2::layer_data(p, i))
}

test_that("E-values follow VanderWeele and Ding", {
  g <- ggsensitivity(1.8, 1.4, 2.31)
  expect_equal(g$evalues$evalue, c(1.8 + sqrt(1.8 * 0.8), 1.4 + sqrt(1.4 * 0.4)))
  # A protective effect uses the reciprocal, and the limit nearer 1.
  p <- ggsensitivity(0.5, 0.3, 0.8)
  expect_equal(p$evalues$evalue, c(2 + sqrt(2), 1.25 + sqrt(1.25 * 0.25)))
  # An interval that holds 1 has nothing to explain away.
  expect_equal(ggsensitivity(1.3, 0.9, 1.8)$evalues$evalue[2], 1)
  # A common outcome's odds ratio is first converted with its square root.
  o <- ggsensitivity(4, 2.25, 9, measure = "OR")
  expect_equal(o$evalues$evalue[1], 2 + sqrt(2))
  expect_equal(ggsensitivity(4, 2.25, 9, measure = "OR", rare = TRUE)$evalues$evalue[1], 4 + sqrt(12))
})

test_that("benchmarks are adjusted by the bounding factor", {
  g <- ggsensitivity(1.8, 1.4, 2.31, benchmarks = data.frame(label = "Age", exposure = 1.6, outcome = 1.9))
  b <- 1.6 * 1.9 / (1.6 + 1.9 - 1)
  expect_equal(g$benchmarks$estimate, 1.8 / b)
  expect_equal(g$benchmarks$lower, 1.4 / b)
  p <- ggsensitivity(0.5, 0.3, 0.8, benchmarks = data.frame(label = "Age", exposure = 1 / 1.6, outcome = 1.9))
  expect_equal(p$benchmarks$estimate, 0.5 * b)
  expect_equal(p$benchmarks$upper, 0.8 * b)
})

test_that("the regions nest and the widget has its parts", {
  g <- ggsensitivity(1.8, 1.4, 2.31, important = 1.25)
  regions <- layer_of(g$plot, "GeomInteractivePolygon")[[1]]
  expect_setequal(unique(regions$data_id), c("rg1", "rg2", "rg3"))
  expect_equal(g$on_render, "ggextremeSensitivity(el, data);")
  ids <- unlist(lapply(layer_of(g$plot, "GeomInteractiveText"), `[[`, "data_id"))
  expect_true(all(c("chv", "chs", "chl") %in% ids))
  expect_equal(sens_bias(3, 3), 1.8)
  # Points on the region's edge have exactly the bias that bounds it.
  m <- sens_curve(1.4, 5)
  expect_equal(sens_bias(m[, 1], m[, 2]), rep(1.4, nrow(m)))
  expect_s3_class(graph_widget(g), "girafe")
})

test_that("inputs are checked", {
  expect_error(ggsensitivity(1.8, 2, 2.31), "hold the estimate")
  expect_error(ggsensitivity(-1, 1, 2), "positive ratio")
  expect_error(ggsensitivity(1.8, 1.4, 2.31, important = 0.8), "same side")
  expect_error(ggsensitivity(1.8, 1.4, 2.31, benchmarks = data.frame(x = 1)), "label, exposure and outcome")
})
