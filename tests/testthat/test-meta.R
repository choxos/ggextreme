layer_of <- function(p, geom) {
  hit <- which(vapply(p$layers, function(l) class(l$geom)[1] == geom, logical(1)))
  lapply(hit, function(i) ggplot2::layer_data(p, i))
}

bcg <- function() {
  dat <- metafor::escalc(measure = "RR", ai = tpos, bi = tneg, ci = cpos,
                         di = cneg, data = metadat::dat.bcg,
                         slab = paste(author, year))
  dat[order(dat$year), ]
}

chiarito <- function() {
  dat <- metafor::escalc(measure = "OR", ai = p2y12.mi, n1i = p2y12.total,
                         ci = aspirin.mi, n2i = aspirin.total,
                         data = metadat::dat.chiarito2020, slab = paste(study, year))
  dat <- dat[!is.na(dat$yi), ]
  dat$p2y12 <- paste0(dat$p2y12.mi, "/", dat$p2y12.total)
  dat
}

rob <- c(R = "rob.R", D = "rob.D", Mi = "rob.Mi", Me = "rob.Me", S = "rob.S",
         Overall = "rob.overall")

test_that("every study row carries a hover card and a click panel", {
  skip_if_not_installed("metafor")
  fit <- metafor::rma(yi, vi, data = bcg())
  g <- ggmeta(fit)
  expect_s3_class(g, c("ggmeta", "ggx_graph"))
  hits <- layer_of(g$plot, "GeomInteractiveRect")[[1]]
  expect_equal(hits$data_id, paste0("s", 1:13))
  expect_true(all(grepl("^ggextremePin\\(this, ", hits$onclick)))
  expect_match(hits$tooltip[1], "Weight", fixed = TRUE)
  expect_s3_class(graph_widget(g), "girafe")
})

test_that("squares follow the weights and the axis is logarithmic for ratios", {
  skip_if_not_installed("metafor")
  fit <- metafor::rma(yi, vi, data = bcg())
  lay <- meta_layout(ggmeta(fit)$input, FALSE)
  expect_equal(sum(lay$rows$weight), 100, tolerance = 1e-6)
  expect_true(lay$exponentiate)
  expect_true("1" %in% lay$tick_labels)
  expect_equal(unname(lay$X(0)), unname(lay$X(log(1))))
  expect_equal(lay$pooled$text, "0.49 (0.34, 0.70)")
})

test_that("the cumulative view shows the pooled estimate after each study", {
  skip_if_not_installed("metafor")
  fit <- metafor::rma(yi, vi, data = bcg())
  cum <- metafor::cumul(fit)
  lay <- meta_layout(ggmeta(fit, cumulative = TRUE)$input, TRUE)
  expect_equal(lay$rows$est, as.numeric(cum$estimate))
  expect_match(lay$rows$label[2], "^\\+ ")
  expect_false(lay$show_weight)
})

test_that("a meta object gives the same forest plot as a metafor fit", {
  skip_if_not_installed("metafor")
  skip_if_not_installed("meta")
  dat <- bcg()
  fit <- metafor::rma(yi, vi, data = dat)
  m <- meta::metabin(tpos, tpos + tneg, cpos, cpos + cneg, studlab = author,
                     data = dat, sm = "RR")
  a <- ggmeta(fit)$input$m
  b <- ggmeta(m)$input$m
  expect_equal(b$pooled$est, a$pooled$est, tolerance = 1e-4)
  expect_equal(b$yi, a$yi, tolerance = 1e-8)
  expect_equal(nrow(b$cumulative), 13)
})

test_that("risk of bias is drawn as traffic lights with a key", {
  skip_if_not_installed("metafor")
  fit <- metafor::rma(yi, vi, data = chiarito())
  g <- ggmeta(fit, columns = c("P2Y12 inhibitor" = "p2y12"), rob = rob)
  shapes <- layer_of(g$plot, "GeomInteractivePolygon")
  lights <- Filter(function(d) any(grepl("_rob", d$data_id)), shapes)[[1]]
  expect_equal(length(unique(lights$group)), 8 * 6)
  expect_setequal(unique(lights$fill), c("#568E4F", "#C28A4A"))
  expect_match(lights$tooltip[1], "Risk of bias", fixed = TRUE)
  key <- layer_of(g$plot, "GeomText")
  expect_true(any(vapply(key, function(d) any(d$label == "Some concerns"), logical(1))))
})

test_that("judgements are classed by their wording", {
  expect_equal(
    rob_class(c("Low risk", "some concerns", "Unclear", "High", "serious",
                "Critical", "No information", "moderate", NA)),
    c("low", "some", "unclear", "high", "high", "critical", "none", "some", NA)
  )
})

test_that("intervals past the axis limits end in an arrow", {
  skip_if_not_installed("metafor")
  fit <- metafor::rma(yi, vi, data = bcg())
  g <- ggmeta(fit, xlim = c(0.2, 2))
  shapes <- layer_of(g$plot, "GeomInteractivePolygon")
  # Arrowheads are the only three point shapes.
  points <- unlist(lapply(shapes, function(d) table(paste(d$data_id, d$group))))
  expect_true(any(points == 3))
  open <- layer_of(ggmeta(fit)$plot, "GeomInteractivePolygon")
  expect_false(any(unlist(lapply(open, function(d) table(paste(d$data_id, d$group)))) == 3))
})

test_that("difference measures are shown on their own scale", {
  skip_if_not_installed("metafor")
  # These trials have standardized differences above 2, which escalc() flags.
  dat <- suppressWarnings(metafor::escalc(measure = "SMD", m1i = m1i, sd1i = sd1i, n1i = n1i,
                                          m2i = m2i, sd2i = sd2i, n2i = n2i,
                                          data = metadat::dat.normand1999))
  lay <- meta_layout(ggmeta(metafor::rma(yi, vi, data = dat))$input, FALSE)
  expect_false(lay$exponentiate)
  expect_equal(lay$xlab, "Standardized mean difference")
})

test_that("bad input is refused with a clear message", {
  skip_if_not_installed("metafor")
  dat <- chiarito()
  fit <- metafor::rma(yi, vi, data = dat)
  expect_error(ggmeta(lm(1 ~ 1)), "rma.uni")
  expect_error(ggmeta(metafor::rma(yi, vi, mods = ~ year, data = dat)), "moderators")
  expect_error(ggmeta(fit, columns = "nope"), "not in `data`: nope")
  expect_error(ggmeta(fit, data = dat[1:3, ]), "one row per study")
  bad <- dat
  bad$rob.R <- as.character(bad$rob.R)
  bad$rob.R[1] <- "probably fine"
  expect_error(ggmeta(fit, data = bad, rob = "rob.R"), "not recognized: probably fine")
  expect_error(ggmeta(fit, favors = "one"), "two labels")
})

test_that("the cumulative replay is written to a file", {
  skip_if_not_installed("metafor")
  skip_if_not(requireNamespace("gifski", quietly = TRUE) ||
                requireNamespace("magick", quietly = TRUE))
  skip_on_cran()
  dat <- bcg()[1:3, ]
  g <- ggmeta(metafor::rma(yi, vi, data = dat))
  file <- tempfile(fileext = ".gif")
  animate_meta(g, file, time = dat$year, hold = 0.2, swap = 0.2, end_pause = 0.2,
               fps = 5, res = 40, cores = 1, quiet = TRUE)
  expect_true(file.exists(file))
  expect_error(animate_meta(g, file, time = 1:2), "one label per study")
})
