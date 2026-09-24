layer_of <- function(p, geom) {
  hit <- which(vapply(p$layers, function(l) class(l$geom)[1] == geom, logical(1)))
  lapply(hit, function(i) ggplot2::layer_data(p, i))
}

bcg_fit <- function() {
  dat <- metafor::escalc(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg,
                         data = metadat::dat.bcg, slab = paste(author, year))
  metafor::rma(yi, vi, data = dat)
}

test_that("every study is a point with a card and a leave one out panel", {
  skip_if_not_installed("metafor")
  skip_if_not_installed("metadat")
  fit <- bcg_fit()
  f <- ggfunnel(fit, hover = "alloc")
  expect_s3_class(f, c("ggfunnel", "ggx_graph"))
  polys <- layer_of(f$plot, "GeomInteractivePolygon")
  points <- polys[[length(polys)]]
  expect_equal(sort(unique(points$data_id)), sort(paste0("s", 1:13)))
  one <- points[points$data_id == "s1", ][1, ]
  expect_match(one$tooltip, "Against no effect", fixed = TRUE)
  expect_match(one$tooltip, "Alloc", fixed = TRUE)
  loo <- metafor::leave1out(fit)
  expect_match(one$onclick, sprintf("%.2f (%.2f, %.2f)", exp(loo$estimate[1]),
                                    exp(loo$ci.lb[1]), exp(loo$ci.ub[1])), fixed = TRUE)
  # Without trim and fill there is no switch, only the tests.
  expect_false(grepl("ggextremeFunnel", f$on_render, fixed = TRUE))
  expect_match(f$on_render, "Small-study effects", fixed = TRUE)
  expect_s3_class(graph_widget(f), "girafe")
})

test_that("the tests are the ones metafor and meta compute", {
  skip_if_not_installed("metafor")
  skip_if_not_installed("metadat")
  skip_if_not_installed("meta")
  fit <- bcg_fit()
  f <- ggfunnel(fit, trim_fill = TRUE)
  t <- f$tests
  expect_equal(t$p[t$test == "Egger's regression test"], metafor::regtest(fit, model = "lm")$pval)
  expect_equal(t$p[t$test == "Begg's rank correlation"], metafor::ranktest(fit)$pval)
  tf <- metafor::trimfill(fit)
  expect_match(t$statistic[t$test == "Trim and fill"],
               sprintf("%d study imputed on the %s; Risk ratio %.2f", tf$k0, tf$side, exp(tf$b[1])),
               fixed = TRUE)
  hollow <- layer_of(f$plot, "GeomInteractivePolygon")[[1]]
  expect_equal(length(unique(hollow$data_id)), tf$k0)
  expect_match(f$on_render, "ggextremeFunnel(el);", fixed = TRUE)

  m <- meta::metabin(tpos, tpos + tneg, cpos, cpos + cneg, data = metadat::dat.bcg,
                     studlab = paste(author, year), sm = "RR")
  g <- ggfunnel(m)
  eg <- meta::metabias(m, method.bias = "Egger", k.min = 3)
  expect_equal(g$tests$p[1], eg$p.value)
  expect_match(g$tests$statistic[2], "^z = ")
})

test_that("contours are clipped to the axis and can be left out", {
  b <- funnel_band(1.96, 2.576, 1, c(-1, 1), 1)
  expect_true(all(b$x >= -1 & b$x <= 1))
  # Where the inner line leaves the axis, the band has closed to nothing.
  s_out <- 1 / 1.96
  inner_x <- b$x[b$s > s_out + 1e-9 & seq_along(b$x) <= length(b$x) / 2]
  expect_true(all(inner_x == 1))
  skip_if_not_installed("metafor")
  skip_if_not_installed("metadat")
  plain <- ggfunnel(bcg_fit(), contours = NULL, tests = FALSE)
  expect_null(plain$tests)
  expect_null(plain$layout$bands)
  expect_null(plain$on_render)
  expect_error(ggfunnel(bcg_fit(), contours = 5), "between 0 and 1")
})

test_that("risk of bias colors the points", {
  skip_if_not_installed("metafor")
  skip_if_not_installed("metadat")
  dat <- metafor::escalc(measure = "OR", ai = p2y12.mi, n1i = p2y12.total,
                         ci = aspirin.mi, n2i = aspirin.total,
                         data = metadat::dat.chiarito2020, slab = paste(study, year))
  dat <- dat[!is.na(dat$yi), ]
  fit <- metafor::rma(yi, vi, data = dat)
  f <- ggfunnel(fit, rob = "rob.overall")
  cls <- rob_class(dat$rob.overall)
  expect_equal(f$layout$studies$color,
               unname(vapply(cls, function(c) rob_classes[[c]]$color, character(1))))
  expect_error(ggfunnel(fit, rob = c("rob.R", "rob.D")), "one column")
})
