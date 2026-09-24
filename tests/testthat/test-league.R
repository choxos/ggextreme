layer_of <- function(p, geom) {
  hit <- which(vapply(p$layers, function(l) class(l$geom)[1] == geom, logical(1)))
  lapply(hit, function(i) ggplot2::layer_data(p, i))
}

psoriasis_fit <- function() {
  pw <- meta::pairwise(treat = treatment, event = pasi75_r, n = pasi75_n,
                       studlab = study, data = psoriasis_nma, sm = "OR")
  netmeta::netmeta(pw, common = FALSE)
}

cell_tone <- function(lg, id) {
  cells <- layer_of(lg$plot, "GeomInteractivePolygon")[[1]]
  unique(cells[cells$data_id == id, c("fill", "alpha")])
}

test_that("every pair has a cell and every treatment a diagonal entry", {
  skip_if_not_installed("netmeta")
  skip_if_not_installed("meta")
  lg <- ggleague(psoriasis_fit(), small_values = "undesirable")
  expect_s3_class(lg, c("ggleague", "ggx_graph"))
  cells <- layer_of(lg$plot, "GeomInteractivePolygon")[[1]]
  ids <- unique(cells$data_id)
  expect_equal(sum(grepl("^c", ids)), 5 * 4)
  expect_equal(sum(grepl("^t", ids)), 5)
  expect_match(lg$on_render, "ggextremeLeague", fixed = TRUE)
  expect_s3_class(graph_widget(lg), "girafe")
})

test_that("treatments are ordered by P-score unless an order is given", {
  skip_if_not_installed("netmeta")
  skip_if_not_installed("meta")
  nma <- psoriasis_fit()
  lg <- ggleague(nma, small_values = "undesirable")
  expect_equal(lg$treatments[1], "Secukinumab 300 mg")
  expect_equal(lg$treatments[5], "Placebo")
  expect_true(all(diff(lg$pscore) <= 0))
  own <- c("Placebo", "Etanercept", "Ustekinumab", "Secukinumab 150 mg",
           "Secukinumab 300 mg")
  expect_equal(ggleague(nma, order = own, small_values = "undesirable")$treatments, own)
  expect_error(ggleague(nma, order = own[1:3]), "every treatment once")
})

test_that("which treatment a cell favors follows small_values", {
  skip_if_not_installed("netmeta")
  skip_if_not_installed("meta")
  nma <- psoriasis_fit()
  own <- c("Secukinumab 300 mg", "Secukinumab 150 mg", "Ustekinumab",
           "Etanercept", "Placebo")
  good <- ggleague(nma, order = own, small_values = "undesirable")
  bad <- ggleague(nma, order = own, small_values = "desirable")
  # Placebo is last, so the cell in its row compares secukinumab 300 mg with it.
  first <- cell_tone(good, "c5_1")
  second <- cell_tone(bad, "c5_1")
  expect_equal(first$fill, league_ink$first)
  expect_equal(second$fill, league_ink$second)
  # Shades are the color with transparency, so they suit a light or dark page.
  expect_equal(first$alpha, 0.2 + 0.5 * min(1, log(81.85) / log(40)), tolerance = 1e-3)
})

test_that("the hover card gives the direct share of the estimate", {
  skip_if_not_installed("netmeta")
  skip_if_not_installed("meta")
  nma <- psoriasis_fit()
  own <- c("Etanercept", "Secukinumab 150 mg", "Placebo", "Ustekinumab",
           "Secukinumab 300 mg")
  lg <- ggleague(nma, order = own, small_values = "undesirable")
  cells <- layer_of(lg$plot, "GeomInteractivePolygon")[[1]]
  tip <- cells$tooltip[cells$data_id == "c2_1"][1]
  share <- round(100 * nma$P.random["Etanercept", "Secukinumab 150 mg"])
  expect_match(tip, paste0(share, "% of the network estimate"), fixed = TRUE)
  expect_match(tip, "Etanercept vs Secukinumab 150 mg", fixed = TRUE)
})

test_that("a pair with no direct trials says so", {
  skip_if_not_installed("netmeta")
  skip_if_not_installed("meta")
  own <- c("Ustekinumab", "Placebo", "Etanercept", "Secukinumab 150 mg",
           "Secukinumab 300 mg")
  lg <- ggleague(psoriasis_fit(), order = own, small_values = "undesirable")
  text <- layer_of(lg$plot, "GeomInteractiveText")[[1]]
  expect_equal(text$label[text$data_id == "c1_2"], "no direct trials")
  cells <- layer_of(lg$plot, "GeomInteractivePolygon")[[1]]
  expect_match(cells$onclick[cells$data_id == "c1_2"][1], "No trial compares", fixed = TRUE)
})

test_that("the click panel shows arms with data and trial estimates without", {
  skip_if_not_installed("netmeta")
  skip_if_not_installed("meta")
  nma <- psoriasis_fit()
  own <- c("Secukinumab 300 mg", "Placebo", "Etanercept", "Ustekinumab",
           "Secukinumab 150 mg")
  with_arms <- ggleague(nma, psoriasis_nma, study, treatment, order = own,
                        small_values = "undesirable")
  cells <- layer_of(with_arms$plot, "GeomInteractivePolygon")[[1]]
  panel <- cells$onclick[cells$data_id == "c2_1"][1]
  expect_match(panel, "JUNCTURE", fixed = TRUE)
  expect_match(panel, "Mean age (years)", fixed = TRUE)

  plain <- ggleague(nma, order = own, small_values = "undesirable")
  cells <- layer_of(plain$plot, "GeomInteractivePolygon")[[1]]
  panel <- cells$onclick[cells$data_id == "c2_1"][1]
  expect_match(panel, "Odds ratio (95% CI)", fixed = TRUE)
  expect_false(grepl("Mean age", panel, fixed = TRUE))

  expect_error(ggleague(nma, psoriasis_nma), "Give `study` and `treatment`")
  expect_error(ggleague(lm(1 ~ 1)), "netmeta")
})
