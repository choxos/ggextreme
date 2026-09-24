layer_of <- function(p, geom) {
  hit <- which(vapply(p$layers, function(l) class(l$geom)[1] == geom, logical(1)))
  lapply(hit, function(i) ggplot2::layer_data(p, i))
}

# The nodes are the last polygon layer; multi-arm shading, when drawn, sits
# underneath them.
nodes_of <- function(net) {
  layers <- layer_of(net$plot, "GeomInteractivePolygon")
  layers[[length(layers)]]
}

arms <- data.frame(
  trial = c("s1", "s1", "s2", "s2", "s3", "s3", "s3"),
  drug = c("A", "B", "A", "B", "A", "B", "C"),
  size = c(100, 100, 50, 60, 30, 30, 40),
  events = c(10, 20, 5, 9, 3, 4, 8)
)

test_that("nodes are treatments and edges are direct comparisons", {
  net <- ggnma(arms, trial, drug, n = size)
  expect_s3_class(net, c("ggnma", "ggx_graph"))
  expect_equal(net$nodes$treatment, c("A", "B", "C"))
  expect_equal(net$nodes$studies, c(3, 3, 1))
  expect_equal(net$nodes$n, c(180, 190, 40))
  expect_equal(paste(net$edges$from, net$edges$to), c("A B", "A C", "B C"))
  expect_equal(net$edges$studies, c(3, 1, 1))
  expect_equal(net$edges$n, c(370, 70, 70))
})

test_that("every node and edge carries a tooltip and a click handler", {
  net <- ggnma(arms, trial, drug, n = size)
  nodes <- nodes_of(net)
  labels <- layer_of(net$plot, "GeomInteractiveText")[[1]]
  hits <- layer_of(net$plot, "GeomInteractivePath")[[2]]
  expect_setequal(unique(nodes$data_id), c("n1", "n2", "n3"))
  expect_setequal(labels$data_id, c("n1", "n2", "n3"))
  expect_setequal(unique(hits$data_id), c("e1", "e2", "e3"))
  expect_true(all(grepl("^ggextremePin\\(this, ", nodes$onclick)))
  tip <- hits$tooltip[hits$data_id == "e1"][1]
  expect_match(tip, '<th scope="colgroup" colspan="2">s3</th>', fixed = TRUE)
  expect_match(tip, "Events", fixed = TRUE)
})

test_that("the hover card shows the chosen columns, or lists the studies", {
  hits <- function(net) layer_of(net$plot, "GeomInteractivePath")[[2]]
  tip <- hits(ggnma(arms, trial, drug, hover = "events"))$tooltip[1]
  expect_match(tip, "Events", fixed = TRUE)
  expect_false(grepl("Size", tip, fixed = TRUE))

  plain <- hits(ggnma(arms, trial, drug, hover = character(0)))$tooltip[1]
  expect_match(plain, "s1, s2, s3", fixed = TRUE)
  expect_false(grepl("<table>", plain, fixed = TRUE))

  expect_error(ggnma(arms, trial, drug, hover = "trial"), "not in the arm table: trial")
})

test_that("a class named like its treatment is not repeated", {
  d <- arms
  d$class <- c("A", "y", "A", "y", "A", "y", "y")
  nodes <- nodes_of(ggnma(d, trial, drug, group = class))
  expect_match(nodes$tooltip[nodes$data_id == "n1"][1],
               '<div class="ggx-tip-sub">3 studies</div>', fixed = TRUE)
})

test_that("line width follows the number of studies", {
  net <- ggnma(arms, trial, drug)
  path <- layer_of(net$plot, "GeomInteractivePath")[[1]]
  w <- vapply(split(path$linewidth, path$data_id), unique, numeric(1))
  expect_gt(w[["e1"]], w[["e2"]])
  expect_equal(w[["e2"]], w[["e3"]])
})

test_that("node area follows the number of participants", {
  net <- ggnma(arms, trial, drug, n = size)
  nodes <- nodes_of(net)
  r <- tapply(nodes$x, nodes$data_id, function(x) diff(range(x)) / 2)
  expect_equal(unname((r["n3"] / r["n2"])^2), 40 / 190, tolerance = 0.01)
})

test_that("treatments start at the top and run clockwise in level order", {
  d <- arms
  d$drug <- factor(d$drug, levels = c("C", "A", "B"))
  net <- ggnma(d, trial, drug)
  expect_equal(net$nodes$treatment, c("C", "A", "B"))
  labels <- layer_of(net$plot, "GeomInteractiveText")[[1]]
  first <- labels[labels$data_id == "n1", ]
  expect_equal(first$y, max(labels$y))
  second <- labels[labels$data_id == "n2", ]
  expect_gt(second$x, first$x)
})

test_that("the click panel shows each arm with every other column", {
  net <- ggnma(arms, trial, drug, n = size)
  nodes <- nodes_of(net)
  panel <- nodes$onclick[nodes$data_id == "n3"][1]
  expect_match(panel, "Events", fixed = TRUE)
  expect_match(panel, "Size", fixed = TRUE)
  expect_false(grepl("Trial", panel, fixed = TRUE))
})

test_that("labels come from the label attribute", {
  expect_equal(column_labels(psoriasis_nma, c("age", "study")),
               c("Mean age (years)", "Study"))
})

test_that("text that is the same across a study is listed once", {
  html <- arm_table(
    data.frame(ref = c("R1", "R1"), x = c(1, 2)), c("s1", "s1"),
    c("ref", "x"), c("Reference", "X"), treatment = c("A", "B"),
    notes = "ref"
  )
  expect_equal(lengths(regmatches(html, gregexpr("R1", html, fixed = TRUE))), 1)
  expect_match(html, 'colspan="2"', fixed = TRUE)
  expect_equal(study_level(data.frame(a = c("x", "x", "y"), b = c("p", "q", "r")),
                           c("s1", "s1", "s2"), c("a", "b")), "a")
})

test_that("values in a row share their decimals", {
  expect_equal(format_values(c(68, 64.4, NA)), c("68.0", "64.4", NA))
  expect_equal(format_values(c(2014, 2019)), c("2014", "2019"))
  expect_equal(format_values(c(12000, 5)), c("12,000", "5"))
})

test_that("classes color the nodes and add a legend", {
  d <- arms
  d$class <- c("x", "y", "x", "y", "x", "y", "y")
  net <- ggnma(d, trial, drug, group = class, palette = c(y = "#000000"))
  nodes <- nodes_of(net)
  fill <- vapply(split(nodes$fill, nodes$data_id), unique, character(1))
  expect_equal(unname(fill[c("n2", "n3")]), c("#000000", "#000000"))
  expect_false(fill[["n1"]] == "#000000")
  expect_length(layer_of(net$plot, "GeomPolygon"), 1)

  d$class[2] <- "x"
  expect_error(ggnma(d, trial, drug, group = class), "Each treatment")
})

test_that("bad input is refused with a clear message", {
  expect_error(ggnma(arms[c(1, 1, 2), ], trial, drug), "once per study")
  bad <- arms
  bad$drug[1] <- NA
  expect_error(ggnma(bad, trial, drug), "missing")
  expect_error(ggnma(arms, trial, drug, n = -size), "negative")
  expect_warning(ggnma(rbind(arms, data.frame(trial = "s4", drug = "C",
                                              size = 10, events = 1)),
                       trial, drug), "no comparisons: s4")
})

test_that("the bundled psoriasis network builds", {
  net <- ggnma(psoriasis_nma, study, treatment, n = n, group = class)
  expect_equal(nrow(net$nodes), 5)
  expect_equal(nrow(net$edges), 7)
  expect_s3_class(graph_widget(net), "girafe")
})

test_that("multi-arm studies are shaded, one polygon per set of treatments", {
  d <- rbind(arms, data.frame(trial = "s4", drug = c("A", "B", "C"),
                              size = 20, events = 2))
  net <- ggnma(d, trial, drug, n = size)
  expect_equal(net$multiarm$treatments, "A, B, C")
  expect_equal(net$multiarm$studies, "s3, s4")
  expect_equal(net$multiarm$arms, 3)
  shade <- layer_of(net$plot, "GeomInteractivePolygon")[[1]]
  expect_equal(unique(shade$data_id), "m1")
  expect_equal(nrow(shade), 3)
  expect_match(shade$tooltip[1], "3-arm studies of A, B and C", fixed = TRUE)
  expect_true(all(shade$alpha < 1))

  off <- ggnma(d, trial, drug, multiarm = FALSE)
  expect_equal(nrow(off$multiarm), 0)
  first <- layer_of(off$plot, "GeomInteractivePolygon")[[1]]
  expect_false("m1" %in% first$data_id)
})

test_that("nodes can be placed by hand, keeping the layout's shape", {
  pos <- data.frame(treatment = c("C", "A", "B"), x = c(1, 0, 2), y = c(2, 0, 0))
  net <- ggnma(arms, trial, drug, positions = pos, multiarm = FALSE)
  nodes <- nodes_of(net)
  mid <- stats::aggregate(cbind(x, y) ~ data_id, nodes, mean)
  a <- mid[mid$data_id == "n1", ]
  b <- mid[mid$data_id == "n2", ]
  c <- mid[mid$data_id == "n3", ]
  expect_equal(a$y, b$y, tolerance = 1e-6)
  expect_gt(c$y, a$y)
  expect_equal(c$x, (a$x + b$x) / 2, tolerance = 1e-6)
  expect_equal((c$y - a$y) / (b$x - a$x), 1, tolerance = 1e-6)

  expect_error(ggnma(arms, trial, drug, positions = pos[1:2, ]), "no row for: B")
  expect_error(ggnma(arms, trial, drug, positions = rbind(pos, pos[1, ])),
               "more than once: C")
  expect_error(ggnma(arms, trial, drug, positions = transform(pos, x = 0, y = 0)),
               "same place")
  expect_error(ggnma(arms, trial, drug, positions = pos[c("x", "y")]), "treatment")
})

test_that("labels point away from the middle of the layout", {
  pos <- data.frame(treatment = c("A", "B", "C"), x = c(0, 2, 1), y = c(0, 0, 2))
  net <- ggnma(arms, trial, drug, positions = pos, multiarm = FALSE)
  labels <- layer_of(net$plot, "GeomInteractiveText")[[1]]
  nodes <- nodes_of(net)
  mid <- stats::aggregate(cbind(x, y) ~ data_id, nodes, mean)
  top <- labels[labels$data_id == "n3", ]
  expect_gt(top$y, mid$y[mid$data_id == "n3"])
  expect_equal(top$vjust, 0, tolerance = 0.2)
})
