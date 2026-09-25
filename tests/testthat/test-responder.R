layer_of <- function(p, geom) {
  hit <- which(vapply(p$layers, function(l) class(l$geom)[1] == geom, logical(1)))
  lapply(hit, function(i) ggplot2::layer_data(p, i))
}

trial <- function() {
  set.seed(3)
  d <- data.frame(arm = factor(rep(c("Placebo", "Active"), each = 80), levels = c("Placebo", "Active")),
                  change = c(rnorm(80, -1, 2), rnorm(80, -2, 2)))
  d
}

test_that("responders, their difference and the mean difference are right", {
  d <- trial()
  g <- ggresponder(change ~ arm, d, threshold = 2, higher_is_better = FALSE)
  imp <- -d$change
  k1 <- sum(imp[d$arm == "Active"] >= 2)
  k0 <- sum(imp[d$arm == "Placebo"] >= 2)
  r <- g$responders
  expect_equal(r$estimate[1:2], c(k1, k0) / 80)
  w <- suppressWarnings(stats::prop.test(k1, 80, correct = FALSE)$conf.int)
  expect_equal(c(r$lower[1], r$upper[1]), as.numeric(w), tolerance = 1e-8)
  expect_equal(r$estimate[3], (k1 - k0) / 80)
  tt <- stats::t.test(imp[d$arm == "Active"], imp[d$arm == "Placebo"])
  expect_equal(c(r$estimate[4], r$lower[4], r$upper[4]), c(unname(diff(rev(tt$estimate))), tt$conf.int[1:2]),
               tolerance = 1e-8)
  # Newcombe's interval holds the difference and sits inside [-1, 1].
  expect_true(r$lower[3] < r$estimate[3] && r$estimate[3] < r$upper[3])
  expect_equal(nrow(g$curve), 121)
  expect_equal(g$on_render, "ggextremeResponder(el, data);")
  expect_s3_class(graph_widget(g), "girafe")
})

test_that("the number needed to treat follows Altman", {
  expect_equal(resp_nnt(c(0.2, 0.1, 0.3), 0.95)[["v"]], "NNTB 5.0")
  expect_match(resp_nnt(c(0.2, 0.1, 0.3), 0.95)[["u"]], "NNTB 3.3 to 10.0", fixed = TRUE)
  expect_match(resp_nnt(c(0.1, -0.05, 0.25), 0.95)[["u"]], "NNTB 4.0 to \u221e to NNTH 20.0", fixed = TRUE)
  expect_equal(resp_nnt(c(-0.25, -0.4, -0.1), 0.95)[["v"]], "NNTH 4.0")
})

test_that("inputs are checked", {
  d <- trial()
  expect_error(ggresponder(change ~ arm, d), "prespecified")
  d$three <- rep(c("a", "b", "c"), length.out = nrow(d))
  expect_error(ggresponder(change ~ three, d, threshold = 1), "exactly two")
  expect_error(ggresponder(change ~ arm, d, threshold = 1, reference = "Nope"), "one of the arms")
})
