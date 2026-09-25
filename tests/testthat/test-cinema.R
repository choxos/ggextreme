layer_of <- function(p, geom) {
  hit <- which(vapply(p$layers, function(l) class(l$geom)[1] == geom, logical(1)))
  lapply(hit, function(i) ggplot2::layer_data(p, i))
}

# A small network, fast for netcontrib(): three treatments in a loop, with a
# second A vs B study. `worse` makes the events harmful, so a small odds
# ratio is a benefit.
tri_net <- function(first = "A") {
  d <- data.frame(study = rep(c("s1", "s2", "s3", "s4"), each = 2),
                  treatment = c(first, "B", first, "C", "B", "C", first, "B"),
                  r = c(10, 20, 12, 25, 18, 22, 9, 19), n = 100)
  pw <- suppressWarnings(meta::pairwise(treat = treatment, event = r, n = n,
                                        studlab = study, data = d, sm = "OR"))
  list(data = d, fit = netmeta::netmeta(pw, common = FALSE))
}

# Illustrative study judgments, invented for the tests.
tri_rob <- data.frame(study = c("s1", "s2", "s3", "s4"),
                      judgment = c("low", "high", "some concerns", "low"),
                      reason = c("r1", "r2", "r3", "r4"))
tri_ind <- data.frame(study = c("s1", "s2", "s3", "s4"), judgment = c(1, 1, 3, 2))

test_that("the three rules for study judgments follow CINeMA", {
  # Papakonstantinou et al. (2020): 40% low, 25% moderate, 35% high.
  lv <- c(0L, 1L, 2L)
  w <- c(0.40, 0.25, 0.35)
  expect_equal(cinema_rule(lv, w, "average"), 1L)
  expect_equal(cinema_rule(lv, w, "majority"), 0L)
  expect_equal(cinema_rule(lv, w, "highest"), 2L)
  # Nikolakopoulou et al. (2020): 44% low, 32% moderate, 24% high is some concerns.
  expect_equal(cinema_rule(lv, c(0.44, 0.32, 0.24), "average"), 1L)
  # Halves round up, ties go to the more serious level, and a study that
  # contributes nothing does not count for the highest rule.
  expect_equal(cinema_rule(c(0L, 1L), c(0.5, 0.5), "average"), 1L)
  expect_equal(cinema_rule(c(1L, 2L), c(0.5, 0.5), "average"), 2L)
  expect_equal(cinema_rule(c(0L, 2L), c(0.5, 0.5), "majority"), 2L)
  expect_equal(cinema_rule(c(0L, 2L), c(1, 1e-9), "highest"), 0L)
  expect_equal(cinema_rule(c(0L, 0L, 2L), c(0.2, 0.3, 0.5), "average"), 1L)
  expect_true(is.na(cinema_rule(0L, 0, "average")))
})

test_that("imprecision and heterogeneity reproduce every scenario of CINeMA's Figure 3", {
  L <- log(0.8)
  U <- log(1.25)
  step <- function(e, lo, hi) cinema_step(log(e), log(lo), log(hi), L, U)
  # Imprecision scenarios 1 to 4: major, some, no, no.
  expect_equal(c(step(1.08, 0.7, 2.3), step(1.3, 0.9, 2.5), step(2, 1.1, 4), step(1.02, 0.9, 1.15)),
               c(2L, 1L, 0L, 0L))
  # The same rules on the other side of no effect.
  expect_equal(c(step(0.9, 0.4, 1.4), step(0.7, 0.4, 1.1), step(0.5, 0.25, 0.95)), c(2L, 1L, 0L))
  # Heterogeneity scenarios 1 to 8: the prediction interval's step minus the CI's.
  het <- function(e, lo, hi, plo, phi) step(e, plo, phi) - step(e, lo, hi)
  expect_equal(c(het(1.8, 1.41, 2.2, 1.16, 2.66), het(1.2, 0.93, 1.57, 0.85, 1.76),
                 het(1.5, 1.10, 1.95, 0.93, 2.3), het(1.6, 1.32, 2.0, 0.755, 3.7),
                 het(1.1, 0.755, 1.57, 0.55, 1.8), het(1.15, 0.90, 1.4, 0.74, 1.7),
                 het(1.05, 0.97, 1.14, 0.905, 1.23), het(1, 0.84, 1.15, 0.43, 2.1)),
               c(0L, 0L, 1L, 2L, 0L, 1L, 0L, 2L))
  # A range of no effect alone treats any effect as important, and has no
  # inside for an interval to reach.
  expect_equal(cinema_step(0.2, -0.1, 0.5, 0, 0), 2L)
  expect_equal(cinema_step(0.2, 0.1, 0.5, 0, 0), 0L)
  expect_equal(cinema_zones(-0.1, 0.5, 0, 0), 5L)
  expect_equal(cinema_reading(5L, "undesirable"), "harm or benefit")
})

test_that("incoherence with p of 0.10 or less follows CINeMA's Table 5 and Figure 3", {
  L <- log(0.8)
  U <- log(1.25)
  z <- function(lo, hi) cinema_zones(log(lo), log(hi), L, U)
  lvl <- function(d, i) cinema_agreement(z(d[1], d[2]), z(i[1], i[2]))$level
  # Figure 3 of Nikolakopoulou et al. (2020), scenarios 1 to 6.
  expect_equal(c(lvl(c(0.1, 0.3), c(0.2, 0.65)), lvl(c(0.25, 0.6), c(0.36, 1.16)),
                 lvl(c(0.27, 0.7), c(0.69, 2.2)), lvl(c(0.27, 0.7), c(0.9, 3.3)),
                 lvl(c(0.27, 0.7), c(1.41, 4.6)), lvl(c(0.27, 0.7), c(0.86, 1.17))),
               c(0L, 1L, 2L, 2L, 2L, 2L))
  # The worked example of Papakonstantinou et al. (2020): some concerns.
  ex <- cinema_agreement(cinema_zones(log(0.68), log(1.03), log(0.83), log(1.2)),
                         cinema_zones(log(0.49), log(0.75), log(0.83), log(1.2)))
  expect_equal(ex$common, 2L)
  expect_equal(ex$level, 1L)
})

test_that("comparisons without a local test take the global test's thresholds", {
  pairs <- data.frame(a = "A", b = "B", key = pair_key("A", "B"), type = c("direct", "indirect", "mixed"),
                      te = 0, lo = -1, hi = 1, d_te = 0, d_lo = -1, d_hi = 1, i_te = 0, i_lo = -1, i_hi = 1,
                      f_te = 0.1, f_lo = -0.5, f_hi = 0.7, f_p = 0.4, stringsAsFactors = FALSE)
  cn <- list(pairs = pairs, ratio = TRUE, threshold = c(0.8, 1.25), level = 0.95, split_method = "back-calculation")
  at <- function(p) {
    cn$global <- if (is.null(p)) NULL else list(Q = 5, df = 2, p = p)
    cinema_incoherence_rule(cn)$level
  }
  # Major below 0.05, some from 0.05 to 0.10, none above; major without a test.
  expect_equal(at(0.03), c(2L, 2L, 0L))
  expect_equal(at(0.07), c(1L, 1L, 0L))
  expect_equal(at(0.10), c(1L, 1L, 0L))
  expect_equal(at(0.2), c(0L, 0L, 0L))
  expect_equal(at(NULL), c(2L, 2L, 0L))
  expect_match(cinema_incoherence_rule(cn)$reason[1], "no closed loop", fixed = TRUE)
})

test_that("judgments come from the fit, netsplit and netcontrib", {
  skip_if_not_installed("netmeta")
  skip_if_not_installed("meta")
  tri <- tri_net()
  cb <- netmeta::netcontrib(tri$fit, study = TRUE)
  j <- cinema_judge(tri$fit, rob = tri_rob, indirectness = tri_ind, threshold = 1.25,
                    reporting = data.frame(judgment = "undetected"), contributions = cb)
  expect_s3_class(j, "cinema")
  expect_equal(nrow(j$judgments), 3 * 6)
  expect_setequal(unique(j$judgments$domain), unname(cinema_domain_names))

  # Study contributions are netcontrib's, and add up to each estimate.
  st <- cb$study.random
  expect_equal(sum(j$contributions$share), sum(st$contribution), tolerance = 1e-10)
  sums <- tapply(j$contributions$share, paste(j$contributions$treat1, j$contributions$treat2), sum)
  expect_equal(unname(as.vector(sums)), rep(1, 3), tolerance = 1e-8)
  ab <- st[st$comparison == "A:B", ]
  mine <- j$contributions[paste(j$contributions$treat1, j$contributions$treat2) %in% c("A B", "B A"), ]
  expect_equal(mine$share[match(ab$study[ab$contribution > 0], mine$study)], ab$contribution[ab$contribution > 0])

  # Within-study bias by hand, with the average rule.
  pr <- j$pairs
  for (i in seq_len(nrow(pr))) {
    c1 <- j$contributions[j$contributions$treat1 == pr$a[i] & j$contributions$treat2 == pr$b[i], ]
    score <- sum(c1$share * c(1, 3, 2, 1)[match(c1$study, tri_rob$study)]) / sum(c1$share)
    expect_equal(j$domains$bias$level[i], as.integer(floor(score + 0.5)) - 1L)
  }

  # Direct, indirect and the inconsistency factor are netsplit's.
  ns <- netmeta::netsplit(tri$fit)
  row <- ns$compare.random[ns$compare.random$comparison %in% c("A:B", "B:A"), ]
  sign <- if (row$comparison == "A:B") 1 else -1
  cmp <- j$comparisons[j$comparisons$treat1 %in% c("A", "B") & j$comparisons$treat2 %in% c("A", "B"), ]
  if (cmp$treat1 == "B") sign <- -sign
  expect_equal(log(cmp$ifactor), sign * row$TE, tolerance = 1e-10)
  expect_equal(cmp$ifactor_p, row$p, tolerance = 1e-10)
  expect_equal(sort(log(c(cmp$ifactor_lower, cmp$ifactor_upper))), sort(sign * c(row$lower, row$upper)),
               tolerance = 1e-10)
  d <- ns$direct.random[ns$direct.random$comparison == row$comparison, ]
  expect_equal(log(cmp$direct), sign * d$TE, tolerance = 1e-10)
  expect_equal(cmp$evidence, "mixed")

  # Network estimates and prediction intervals are the fit's.
  expect_equal(log(cmp$estimate), tri$fit$TE.random[cmp$treat1, cmp$treat2], tolerance = 1e-10)
  expect_equal(log(cmp$pred_lower), tri$fit$lower.predict[cmp$treat1, cmp$treat2], tolerance = 1e-10)

  # Imprecision by hand against 0.8 to 1.25.
  expect_equal(j$domains$imprecision$level,
               cinema_step(pr$te, pr$lo, pr$hi, log(0.8), log(1.25)))
  expect_true(all(j$judgments$source[j$judgments$domain == "Reporting bias"] == "Yours"))
  expect_true(all(grepl("average rule", j$judgments$source[j$judgments$domain == "Within-study bias"])))
  expect_output(print(j), "never added into a score")
})

test_that("the order, the threshold and small_values set the direction", {
  skip_if_not_installed("netmeta")
  skip_if_not_installed("meta")
  fit <- tri_net()$fit
  # Events are harmful here, so small odds ratios are desirable.
  good <- cinema_judge(fit, threshold = c(0.9, 1.5), small_values = "desirable")
  bad <- cinema_judge(fit, threshold = c(0.9, 1.5), small_values = "undesirable")
  expect_false(identical(good$order, bad$order))
  expect_equal(good$threshold, c(0.9, 1.5))
  expect_equal(cinema_threshold(1.25, TRUE), c(0.8, 1.25))
  expect_equal(cinema_threshold(0.8, TRUE), c(0.8, 1.25))
  expect_equal(cinema_threshold(2, FALSE), c(-2, 2))
  expect_equal(cinema_reading(1L, "desirable"), "benefit only")
  expect_equal(cinema_reading(1L, "undesirable"), "harm only")
  expect_equal(cinema_reading(6L, "undesirable", long = TRUE), "little difference or an important benefit")
  # The first treatment of a protective comparison is favored, and the
  # reason says so after the reciprocal.
  imp <- good$judgments[good$judgments$domain == "Imprecision", ]
  side <- good$pairs$te < 0 & good$pairs$hi < 0
  expect_true(any(side))
  k <- which(side)[1]
  expect_match(imp$reason[k], paste("no value in it favors", good$pairs$b[k]), fixed = TRUE)
})

test_that("a common effect model has no prediction interval to judge heterogeneity by", {
  skip_if_not_installed("netmeta")
  skip_if_not_installed("meta")
  d <- tri_net()$data
  pw <- suppressWarnings(meta::pairwise(treat = treatment, event = r, n = n, studlab = study, data = d, sm = "OR"))
  fit <- netmeta::netmeta(pw, common = TRUE, random = FALSE)
  j <- cinema_judge(fit, threshold = 1.25)
  expect_equal(j$pooled, "common")
  expect_true(all(is.na(j$domains$heterogeneity$level)))
  expect_match(j$domains$heterogeneity$reason[1], "common effect model", fixed = TRUE)
  expect_false(cinema_clinical(j)$render_data$prediction)
  expect_null(cinema_clinical(j)$readings$pi)
})

test_that("your judgments are read, kept and labeled as yours", {
  skip_if_not_installed("netmeta")
  skip_if_not_installed("meta")
  fit <- tri_net()$fit
  # A report in the wide format of the CINeMA web application.
  report <- data.frame(Comparison = c("A:B", "A:C", "B:C"),
                       "Within-study bias" = c("Some concerns", "No concerns", "Major concerns"),
                       "Reporting bias" = c("Suspected", "Undetected", "Suspected"),
                       Indirectness = "No concerns", Imprecision = "Some concerns",
                       Heterogeneity = "No concerns", Incoherence = c("No concerns", "", "Major concerns"),
                       "Confidence rating" = c("Moderate", "High", "Low"),
                       check.names = FALSE, stringsAsFactors = FALSE)
  j <- cinema_judge(fit, judgments = report)
  w <- j$judgments
  ab <- w[paste(w$treat1, w$treat2) %in% c("A B", "B A"), ]
  expect_equal(ab$judgment[ab$domain == "Within-study bias"], "Some concerns")
  expect_equal(ab$judgment[ab$domain == "Reporting bias"], "Suspected")
  expect_equal(ab$source[ab$domain == "Within-study bias"], "Yours")
  expect_match(ab$reason[ab$domain == "Imprecision"], "no reason was given", fixed = TRUE)
  # An empty judgment keeps the computed one.
  ac <- w[paste(w$treat1, w$treat2) %in% c("A C", "C A") & w$domain == "Incoherence", ]
  expect_match(ac$source, "^Computed")
  expect_equal(nrow(j$overall), 3)
  # The long format, with reasons, and comparisons written with "vs".
  long <- data.frame(comparison = "A vs B", domain = "imprecision", judgment = "major concerns", reason = "Mine.")
  jl <- cinema_judge(fit, judgments = long)
  imp <- jl$judgments[jl$judgments$domain == "Imprecision", ]
  expect_equal(imp$reason[paste(imp$treat1, imp$treat2) %in% c("A B", "B A")], "Mine.")
  lg <- cinema_league(j)
  expect_s3_class(lg, c("cinema_league", "ggx_graph"))
  cells <- layer_of(lg$plot, "GeomInteractivePolygon")[[1]]
  expect_true(any(grepl("Your overall rating", cells$onclick, fixed = TRUE)))
})

test_that("inputs are checked", {
  skip_if_not_installed("netmeta")
  skip_if_not_installed("meta")
  fit <- tri_net()$fit
  expect_error(cinema_judge(lm(1 ~ 1)), "netmeta")
  expect_error(cinema_judge(fit, rob = tri_rob[1:3, ]), "no judgment for these studies of the network: s4")
  expect_error(cinema_judge(fit, rob = transform(tri_rob, judgment = c("low", "bad", "high", "low"))), "bad")
  expect_error(cinema_judge(fit, rob = data.frame(id = 1)), "columns `study` and `judgment`")
  expect_error(cinema_judge(fit, rob = rbind(tri_rob, tri_rob[1, ])), "more than once")
  expect_warning(cinema_judge(fit, threshold = 1.25, rob = rbind(tri_rob, data.frame(study = "s9", judgment = "low", reason = ""))),
                 "not in the network")
  expect_error(cinema_judge(fit, threshold = c(1.1, 1.5)), "either side of no effect")
  expect_error(cinema_judge(fit, threshold = -1), "positive")
  expect_error(cinema_judge(fit, threshold = "a"), "one or two numbers")
  expect_error(cinema_judge(fit, rule = "median"), "should be one of")
  expect_error(cinema_judge(fit, reporting = data.frame(judgment = c("suspected", "undetected"))), "single row")
  expect_error(cinema_judge(fit, reporting = data.frame(judgment = "maybe")), "maybe")
  expect_error(cinema_judge(fit, judgments = data.frame(treat1 = "A", treat2 = "Z", Imprecision = "no concerns")),
               "not in the network")
  expect_error(cinema_judge(fit, judgments = data.frame(comparison = "A:B", domain = "Colour", judgment = "no")),
               "not CINeMA's six")
  expect_error(cinema_judge(fit, order = c("A", "B")), "every treatment once")
  expect_error(cinema_judge(fit, contributions = "yes"), "netcontrib")
  expect_error(cinema_judge(fit, contributions = netmeta::netcontrib(fit)), "study = TRUE")
  j <- cinema_judge(fit)
  expect_error(cinema_league(j, threshold = 1.25), "already holds judgments")
  expect_error(cinema_clinical(j), "needs the prespecified limits")
  expect_error(cinema_contribution(j), "needs study judgments")
  expect_error(cinema_network(tri_net()$data, study, treatment), "needs study judgments")
  expect_error(cinema_network(tri_net()$data, study, treatment, rob = tri_rob, color = "indirectness"),
               "no study judgments of it")
})

test_that("every widget builds, with the ids its script needs", {
  skip_if_not_installed("netmeta")
  skip_if_not_installed("meta")
  tri <- tri_net()
  j <- cinema_judge(tri$fit, rob = tri_rob, indirectness = tri_ind, threshold = 1.25)

  lg <- cinema_league(j)
  ids <- unique(layer_of(lg$plot, "GeomInteractivePolygon")[[1]]$data_id)
  expect_equal(sum(grepl("^c\\d+_\\d+$", ids)), 6)
  expect_equal(sum(grepl("^t\\d+$", ids)), 3)
  expect_match(lg$on_render, "ggextremeCinemaKeys", fixed = TRUE)
  marks <- layer_of(lg$plot, "GeomInteractivePolygon")[[2]]
  expect_equal(length(unique(marks$group)), 3 * 6)
  expect_s3_class(graph_widget(lg), "girafe")

  cl <- cinema_clinical(j)
  expect_equal(nrow(cl$readings), 3)
  expect_setequal(names(cl$render_data$rows[[1]]), c("label", "te", "lo", "hi", "plo", "phi"))
  expect_equal(cl$readings$imprecision, cinema_concern_words[j$domains$imprecision$level + 1])
  live <- unlist(lapply(layer_of(cl$plot, "GeomInteractiveText"), `[[`, "data_id"))
  expect_true(all(c("rc1", "jc1", "rp1", "jp1", "st1", "sl2") %in% live))
  expect_length(cinema_clinical(j, reference = "A")$readings$treat1, 2)
  expect_error(cinema_clinical(j, reference = "Z"), "one treatment")
  expect_s3_class(graph_widget(cl, "dark"), "girafe")

  ct <- cinema_contribution(j, color = "indirectness")
  expect_equal(ct$render_data$start, "indirectness")
  seg <- unlist(lapply(layer_of(ct$plot, "GeomInteractiveRect"), `[[`, "data_id"))
  expect_true(any(grepl("^b\\d+_\\d+$", seg)))
  expect_true(any(grepl("^m\\d+_\\d+$", seg)))
  expect_length(ct$render_data$comparisons, 3)
  expect_equal(sum(vapply(ct$render_data$comparisons[[1]]$shares, `[[`, 0, "share")), 1, tolerance = 1e-3)
  expect_s3_class(graph_widget(ct), "girafe")

  ic <- cinema_incoherence(j)
  expect_equal(ic$global$df, j$global$df)
  hit <- layer_of(ic$plot, "GeomInteractiveRect")[[1]]
  expect_true(all(grepl("^ggextremePin", hit$onclick)))
  expect_s3_class(graph_widget(ic), "girafe")

  nw <- cinema_network(tri$data, study, treatment, n = n, rob = tri_rob, indirectness = tri_ind)
  expect_equal(nw$edges$studies, c(2L, 1L, 1L))
  expect_equal(sum(nw$edges$rob_high), 1)
  strands <- layer_of(nw$plot, "GeomInteractiveSegment")[[2]]
  expect_equal(nrow(strands), 4)
  expect_true(all(grepl("^s\\d+_\\d+$", strands$data_id)))
  expect_s3_class(graph_widget(nw), "girafe")
  expect_s3_class(graph_plot(nw, "dark"), "ggplot")
})
