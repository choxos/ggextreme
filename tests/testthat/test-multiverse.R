layer_of <- function(p, geom) {
  hit <- which(vapply(p$layers, function(l) class(l$geom)[1] == geom, logical(1)))
  lapply(hit, function(i) ggplot2::layer_data(p, i))
}

specs <- function() {
  s <- expand.grid(outcome = c("Primary", "Broad"), adjust = c("Minimal", "Standard", "Extended"),
                   stringsAsFactors = FALSE)
  b <- c(-0.4, -0.3, -0.25, -0.1, -0.05, 0.1)
  s$or <- exp(b)
  s$lo <- exp(b - 0.2)
  s$hi <- exp(b + 0.2)
  s
}

test_that("analyses are sorted, marked and summarized", {
  s <- specs()
  g <- ggmultiverse(s, or, lo, hi, decisions = c("outcome", "adjust"),
                    primary = outcome == "Primary" & adjust == "Standard", ylab = "Odds ratio")
  expect_s3_class(g, c("ggmultiverse", "ggx_graph"))
  expect_equal(g$render_data$rank, rank(s$or))
  expect_true(g$render_data$ratio)
  inf <- g$influence
  expect_equal(inf$median[inf$decision == "adjust" & inf$choice == "Minimal"], stats::median(s$or[1:2]))
  expect_equal(inf$analyses, c(3, 3, 2, 2, 2))
  words <- mv_summary(s$or, s$lo, s$hi, g$render_data$primary, 1, TRUE, "Odds ratio")
  expect_match(words, "6 analyses", fixed = TRUE)
  expect_match(words, "3 (50%) have intervals that exclude 1, all below it", fixed = TRUE)
  expect_match(words, "The primary analysis: 0.78 (0.64 to 0.95)", fixed = TRUE)
  pts <- layer_of(g$plot, "GeomInteractivePoint")
  expect_equal(nrow(pts[[2]]), 6 * 5)
  expect_equal(g$on_render, "ggextremeMultiverse(el, data);")
  expect_s3_class(graph_widget(g), "girafe")
})

test_that("differences are drawn around zero", {
  s <- specs()
  s$d <- log(s$or)
  s$dl <- log(s$lo)
  s$dh <- log(s$hi)
  g <- ggmultiverse(s, d, dl, dh, decisions = "adjust", primary = 3)
  expect_false(g$render_data$ratio)
  expect_equal(g$render_data$null, 0)
  expect_equal(which(g$render_data$primary), 3)
})

test_that("inputs are checked", {
  s <- specs()
  expect_error(ggmultiverse(s, or, lo, hi, decisions = "nope"), "name columns")
  expect_error(ggmultiverse(s, or, lo, hi, decisions = "adjust", primary = outcome == "Primary"), "exactly one")
  big <- s[rep(1:6, 90), ]
  expect_error(ggmultiverse(big, or, lo, hi, decisions = "adjust"), "more than 500")
})
