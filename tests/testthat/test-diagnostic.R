layer_of <- function(p, geom) {
  hit <- which(vapply(p$layers, function(l) class(l$geom)[1] == geom, logical(1)))
  lapply(hit, function(i) ggplot2::layer_data(p, i))
}

marker_data <- function() {
  set.seed(11)
  data.frame(sick = rep(c(FALSE, TRUE), c(120, 60)),
             score = c(rnorm(120, 10, 2), rnorm(60, 13, 2.5)))
}

test_that("accuracy at the cutoff matches counts and standard intervals", {
  d <- marker_data()
  g <- ggdiagnostic(sick ~ score, d, cutoff = 12, prevalence = 0.05)
  expect_s3_class(g, c("ggdiagnostic", "ggx_graph"))
  tp <- sum(d$score[d$sick] >= 12)
  tn <- sum(d$score[!d$sick] < 12)
  acc <- g$accuracy
  expect_equal(acc$estimate[1], tp / 60)
  expect_equal(acc$estimate[2], tn / 120)
  wil <- suppressWarnings(stats::prop.test(tp, 60, correct = FALSE)$conf.int)
  expect_equal(c(acc$lower[1], acc$upper[1]), as.numeric(wil), tolerance = 1e-8)
  se <- tp / 60
  sp <- tn / 120
  expect_equal(acc$estimate[3], se * 0.05 / (se * 0.05 + (1 - sp) * 0.95))
  expect_equal(acc$estimate[5], se / (1 - sp))
  # The area under the curve is the Mann-Whitney statistic.
  w <- suppressWarnings(stats::wilcox.test(d$score[d$sick], d$score[!d$sick])$statistic)
  expect_equal(g$auc$auc, unname(w) / (60 * 120))
  expect_equal(g$direction, "higher")
})

test_that("the DeLong interval matches its definition, ties included", {
  d <- marker_data()
  d$score <- round(d$score)
  g <- ggdiagnostic(sick ~ score, d)
  x1 <- d$score[d$sick]
  x0 <- d$score[!d$sick]
  psi <- outer(x1, x0, function(a, b) (a > b) + 0.5 * (a == b))
  v10 <- rowMeans(psi)
  v01 <- colMeans(psi)
  se <- sqrt(stats::var(v10) / length(x1) + stats::var(v01) / length(x0))
  expect_equal(g$auc$auc, mean(psi))
  expect_equal(g$auc$lower, mean(psi) - stats::qnorm(0.975) * se)
})

test_that("the default cutoff maximizes Youden's index and is labeled as chosen", {
  d <- marker_data()
  g <- ggdiagnostic(sick ~ score, d)
  j <- g$roc$sensitivity + g$roc$specificity - 1
  expect_equal(g$cutoff, g$roc$cutoff[which.max(j)])
  expect_false(g$render_data$prespecified)
  cap <- unlist(lapply(layer_of(g$plot, "GeomText"), `[[`, "label"))
  expect_true(any(grepl("Youden", cap, fixed = TRUE)))
  # A marker that falls with the condition is read the other way.
  d$neg <- -d$score
  h <- ggdiagnostic(sick ~ neg, d)
  expect_equal(h$direction, "lower")
  expect_equal(h$auc$auc, g$auc$auc)
})

test_that("the widget gets what it needs and the static plot has its live parts", {
  d <- marker_data()
  d$status <- factor(ifelse(d$sick, "case", "control"), levels = c("control", "case"))
  g <- ggdiagnostic(status ~ score, d, cutoff = 12, marker_label = "Score (points)")
  expect_equal(g$render_data$labels, c("control", "case"))
  expect_equal(g$on_render, "ggextremeDiagnostic(el, data);")
  expect_equal(length(g$render_data$x1) + length(g$render_data$x0), nrow(d))
  expect_equal(g$prevalence, 60 / 180)
  ids <- unlist(lapply(layer_of(g$plot, "GeomInteractiveText"), `[[`, "data_id"))
  expect_true(all(c("dcl", "rpl", "plt", "gl", "gh", paste0("sv", 1:8), paste0("su", 1:8)) %in% ids))
  cells <- Filter(function(x) all(x$data_id == "grid"), layer_of(g$plot, "GeomInteractiveRect"))[[1]]
  expect_equal(nrow(cells), 1000)
  expect_s3_class(graph_widget(g), "girafe")
})

test_that("inputs are checked", {
  d <- marker_data()
  expect_error(ggdiagnostic(score ~ sick, d), "numeric")
  expect_error(ggdiagnostic(sick ~ score, d, prevalence = 1.2), "between 0 and 1")
  expect_error(ggdiagnostic(sick ~ score, d, cutoff = c(1, 2)), "one number")
  d$three <- rep(c("a", "b", "c"), 60)
  expect_error(ggdiagnostic(three ~ score, d), "exactly two")
  expect_error(ggdiagnostic(~ score, d), "outcome ~ marker")
})
