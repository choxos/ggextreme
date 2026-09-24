layer_of <- function(p, geom) {
  hit <- which(vapply(p$layers, function(l) class(l$geom)[1] == geom, logical(1)))
  lapply(hit, function(i) ggplot2::layer_data(p, i))
}

qci_change <- function() {
  d <- clefts_qci_world
  first <- d$qci[d$year == 1990][match(d$iso3, d$iso3[d$year == 1990])]
  d$change <- d$qci - first
  d
}

test_that("regions match by code, by name and by the forms health agencies use", {
  keys <- c("BRA", "br", "Brazil", "Iran (Islamic Republic of)",
            "C\u00f4te d'Ivoire", "cote d ivoire", "Viet Nam",
            "Bolivia (Plurinational State of)", "Republic of Korea",
            "United States of America", "Congo", "Democratic Republic of the Congo",
            "Global", "High SDI", NA)
  m <- world_match(keys)
  expect_equal(m$id, c("BRA", "BRA", "BRA", "IRN", "CIV", "CIV", "VNM", "BOL", "KOR",
                       "USA", "COG", "COD", NA, NA, NA))
  expect_equal(m$code, c(TRUE, TRUE, rep(FALSE, 13)))
})

test_that("the stored name keys are what the normalizer makes of a name", {
  expect_equal(normalize_region(world_names$key), world_names$key)
  expect_equal(normalize_region("S\u00e3o Tom\u00e9 & Pr\u00edncipe"), "saotomeandprincipe")
  expect_equal(unname(world_labels[c("SOM", "CYP")]), c("Somalia", "Cyprus"))
})

test_that("every map shares its region ids and shows the chosen time", {
  m <- ggchoropleth(qci_change(), iso3, year,
                    values = c(QCI = "qci", "Change since 1990" = "change"))
  expect_s3_class(m, c("ggchoropleth", "ggx_graph"))
  polys <- layer_of(m$plot, "GeomInteractivePolygon")
  expect_length(polys, 2)
  expect_equal(sort(unique(polys[[1]]$data_id)), sort(unique(polys[[2]]$data_id)))
  expect_equal(length(unique(polys[[1]]$data_id)), length(unique(world_geometry$id)))
  stamps <- layer_of(m$plot, "GeomInteractiveText")[[1]]
  expect_equal(unique(stamps$label), "2019")
  expect_equal(unique(stamps$data_id), "yr")
  expect_equal(m$on_render, "ggextremeMap(el, data);")
  expect_equal(m$hover_inv, "")
  w <- graph_widget(m)
  expect_s3_class(w, "girafe")
  expect_identical(w$jsHooks$render[[1]]$data, m$render_data)
})

test_that("the widget's colors for each time are the ones the static map draws", {
  d <- qci_change()
  m <- ggchoropleth(d, iso3, year, values = c(QCI = "qci", Change = "change"), at = 2005)
  rd <- m$render_data
  t <- match(2005, sort(unique(d$year)))
  expect_equal(rd$start, t - 1L)
  expect_equal(dim(rd$panels[[1]]$fill), c(30L, length(rd$names)))
  bra <- match("BRA", unique(world_geometry$id))
  polys <- layer_of(m$plot, "GeomInteractivePolygon")
  for (k in 1:2) {
    p <- rd$panels[[k]]
    drawn <- unique(polys[[k]][polys[[k]]$data_id == paste0("g", bra), c("fill", "alpha")])
    expect_equal(drawn$fill, unclass(p$tones)[p$fill[t, bra] + 1])
    expect_equal(drawn$alpha, p$alpha[t, bra])
  }
  expect_equal(rd$panels[[1]]$value[t, bra], round(d$qci[d$iso3 == "BRA" & d$year == 2005], 1))
  expect_equal(unclass(rd$times)[t], "2005")
})

test_that("a measure on both sides of zero diverges and the others do not", {
  m <- ggchoropleth(qci_change(), iso3, year, values = c(QCI = "qci", Change = "change"))
  s <- m$layout$scales
  expect_false(s[[1]]$diverging)
  expect_true(s[[2]]$diverging)
  expect_equal(s[[2]]$lo, -s[[2]]$hi)
  shade <- map_shade(c(-s[[2]]$hi, 0, s[[2]]$hi, NA), s[[2]])
  expect_equal(shade$fill, c(map_ink$diverging[c(1, 2, 2)], graph_ink$ghost))
  expect_equal(shade$alpha, c(1, map_dims$alpha_zero, 1, 1))
  own <- ggchoropleth(qci_change(), iso3, year, values = c(QCI = "qci", Change = "change"),
                      palette = list(Change = c("#AA0000", "#0000AA")))
  expect_equal(own$layout$scales[[2]]$colors, c("#AA0000", "#0000AA"))
  expect_error(ggchoropleth(qci_change(), iso3, year, values = "qci",
                            palette = list(Other = "#000000")), "no map called Other")
})

test_that("places that are not on the map are named and left out", {
  d <- data.frame(place = c("Brazil", "Global", "High SDI", "Chile"), year = 2000,
                  v = c(1, 2, 3, 4))
  expect_message(m <- ggchoropleth(d, place, year, values = "v"),
                 "2 regions are not on the map and left out: Global, High SDI.")
  # Names given by the data label the regions, codes take the map's names.
  expect_true("Chile" %in% m$render_data$names)
  expect_error(suppressMessages(ggchoropleth(rbind(d, d), place, year, values = "v")),
               "more than one row for Brazil at 2000")
  expect_error(ggchoropleth(d, place, year, values = "missing"), "must name columns")
  expect_error(suppressMessages(ggchoropleth(d, place, year, values = "v", at = 1999)),
               "`at` must be one of")
})

test_that("any sf map of polygons can be drawn", {
  skip_if_not_installed("sf")
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  d <- rbind(data.frame(county = nc$NAME, year = 1974, rate = 1000 * nc$SID74 / nc$BIR74),
             data.frame(county = nc$NAME, year = 1979, rate = 1000 * nc$SID79 / nc$BIR79))
  m <- ggchoropleth(d, county, year, values = c("SIDS per 1,000 births" = "rate"),
                    map = nc, map_id = "NAME")
  expect_equal(length(m$render_data$names), length(unique(nc$NAME)))
  expect_equal(unclass(m$render_data$times), c("1974", "1979"))
  polys <- layer_of(m$plot, "GeomInteractivePolygon")[[1]]
  expect_equal(length(unique(polys$data_id)), length(unique(nc$NAME)))
  expect_error(ggchoropleth(d, county, year, values = "rate", map = nc, map_id = "none"),
               "must name a column")
  expect_error(ggchoropleth(d, county, year, values = "rate", map = "moon"), "sf object")
})

test_that("static copies and the animation draw every time", {
  d <- subset(clefts_qci_world, year >= 2017)
  m <- ggchoropleth(d, iso3, year, values = c(QCI = "qci"))
  expect_equal(m$layout$time_labels, c("2017", "2018", "2019"))
  png <- tempfile(fileext = ".png")
  graph_save(m, png, res = 40, theme = "dark")
  expect_true(file.exists(png))
  skip_if_not(requireNamespace("gifski", quietly = TRUE) ||
                requireNamespace("magick", quietly = TRUE))
  skip_on_cran()
  gif <- tempfile(fileext = ".gif")
  animate_choropleth(m, gif, step = 0.2, end_pause = 0.2, fps = 5, res = 30,
                     cores = 1, quiet = TRUE)
  expect_true(file.exists(gif))
})
