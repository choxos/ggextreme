layer_of <- function(p, geom) {
  hit <- which(vapply(p$layers, function(l) class(l$geom)[1] == geom, logical(1)))
  lapply(hit, function(i) ggplot2::layer_data(p, i))
}

test_that("every node and arrow carries a tooltip and a click handler", {
  dag <- ggcausal(cleft_dag$edges, cleft_dag$nodes, legend_title = "Role")
  expect_s3_class(dag, c("ggcausal", "ggx_graph"))
  expect_gt(dag$width, 0)
  expect_gt(dag$height, 0)

  boxes <- layer_of(dag$plot, "GeomInteractivePolygon")[[1]]
  labels <- layer_of(dag$plot, "GeomInteractiveText")[[1]]
  hits <- layer_of(dag$plot, "GeomInteractivePath")[[2]]
  expect_setequal(unique(boxes$data_id), paste0("n", 1:6))
  expect_setequal(unique(labels$data_id), paste0("n", 1:6))
  expect_setequal(unique(hits$data_id), paste0("e", 1:8))
  expect_false(any(is.na(boxes$tooltip) | !nzchar(boxes$tooltip)))
  expect_true(all(grepl("^ggextremePin\\(this, ", hits$onclick)))
})

test_that("a label with a line break is drawn as one text row per line", {
  dag <- ggcausal(cleft_dag$edges, cleft_dag$nodes)
  labels <- layer_of(dag$plot, "GeomInteractiveText")[[1]]
  expect_equal(sum(labels$data_id == "n1"), 2)
  expect_false(any(grepl("\n", labels$label, fixed = TRUE)))
})

test_that("arrows stop at the border of the box they point to", {
  dag <- ggcausal(cleft_dag$edges, cleft_dag$nodes)
  boxes <- layer_of(dag$plot, "GeomInteractivePolygon")[[1]]
  path <- layer_of(dag$plot, "GeomInteractivePath")[[1]]
  to <- match(cleft_dag$edges$to, cleft_dag$nodes$name)
  for (k in seq_len(nrow(cleft_dag$edges))) {
    end <- utils::tail(path[path$data_id == paste0("e", k), ], 1)
    box <- boxes[boxes$data_id == paste0("n", to[k]), ]
    outside <- end$x < min(box$x) | end$x > max(box$x) |
      end$y < min(box$y) | end$y > max(box$y)
    expect_true(outside)
    gap <- max(min(box$x) - end$x, end$x - max(box$x),
               min(box$y) - end$y, end$y - max(box$y))
    expect_lt(gap, 4)
  }
})

test_that("arrows run left to right, or top down", {
  e <- data.frame(from = c("a", "b"), to = c("b", "c"))
  centers <- function(dag) {
    b <- layer_of(dag$plot, "GeomInteractivePolygon")[[1]]
    stats::aggregate(cbind(x, y) ~ data_id, b, mean)
  }
  right <- centers(ggcausal(e))
  expect_true(all(diff(right$x) > 0))
  down <- centers(ggcausal(e, direction = "down"))
  expect_true(all(diff(down$y) < 0))
})

test_that("an arrow that skips a layer bends around the box in between", {
  e <- data.frame(from = c("c", "c", "x"), to = c("x", "y", "y"))
  dag <- ggcausal(e)
  path <- layer_of(dag$plot, "GeomInteractivePath")[[1]]
  expect_gt(sum(path$data_id == "e2"), 2)
  expect_equal(sum(path$data_id == "e1"), 2)
})

test_that("boxes placed by hand keep their order", {
  n <- data.frame(name = c("a", "b", "c"), x = c(0, 2, 1), y = c(0, 0, 1))
  e <- data.frame(from = c("a", "c"), to = c("b", "b"))
  b <- layer_of(ggcausal(e, n)$plot, "GeomInteractivePolygon")[[1]]
  mid <- stats::aggregate(cbind(x, y) ~ data_id, b, mean)
  expect_equal(order(mid$x), c(1, 3, 2))
  expect_equal(which.max(mid$y), 3)
})

test_that("an arrow between overlapping boxes is dropped with a warning", {
  n <- data.frame(name = c("a", "b"), x = c(0, 0.1), y = c(0, 0))
  e <- data.frame(from = "a", to = "b")
  expect_warning(dag <- ggcausal(e, n), "a to b")
  expect_length(layer_of(dag$plot, "GeomInteractivePath"), 0)
})

test_that("bad input is refused with a clear message", {
  e <- data.frame(from = c("a", "b"), to = c("b", "c"))
  expect_error(ggcausal(data.frame(a = 1)), "`from` and `to`")
  expect_error(ggcausal(data.frame(from = "a", to = "a")), "itself")
  expect_error(ggcausal(rbind(e, e[1, ])), "once")
  expect_error(ggcausal(rbind(e, data.frame(from = "c", to = "a"))), "cycle")
  expect_error(ggcausal(e, data.frame(name = c("a", "b"))), "Missing: c")
  expect_error(ggcausal(e, data.frame(name = c("a", "b", "c", "a"))), "Repeated: a")
  expect_error(ggcausal(e, data.frame(name = c("a", "b", "c"), x = 1:3)), "both")
  expect_error(ggcausal(e, palette = "red",
                        nodes = data.frame(name = c("a", "b", "c"), role = "exposure")),
               "named")
})

test_that("roles are ordered, colored and overridable", {
  roles <- causal_roles(c("Outcome", "mood", "exposure", NA), NULL, 4)
  expect_equal(roles$levels, c("exposure", "Outcome", "mood"))
  expect_equal(unname(roles$colors["exposure"]), known_roles[["exposure"]])
  expect_false(roles$colors["mood"] %in% known_roles)
  expect_true(is.na(roles$color_of[4]))

  roles <- causal_roles(c("exposure", "outcome"), c(outcome = "#000000"), 2)
  expect_equal(unname(roles$colors["outcome"]), "#000000")

  f <- factor(c("b", "a"), levels = c("b", "a"))
  expect_equal(causal_roles(f, NULL, 2)$levels, c("b", "a"))

  none <- causal_roles(NULL, NULL, 3)
  expect_length(none$role, 3)
  expect_length(none$levels, 0)
})

test_that("a diagram without roles has no legend", {
  e <- data.frame(from = "a", to = "b")
  dag <- ggcausal(e)
  expect_length(layer_of(dag$plot, "GeomPolygon"), 0)
})

test_that("user text is escaped and references are linked", {
  tip <- tip_html("A & B", "", "<script>x</script>", list(character(0)),
                  list(character(0)))
  expect_match(tip, "A &amp; B", fixed = TRUE)
  expect_match(tip, "&lt;script&gt;", fixed = TRUE)
  expect_false(grepl("<script>", tip, fixed = TRUE))

  expect_match(link_reference("Smith. BMJ. 2007;334:464. doi:10.1136/bmj.39079.618287.0B."),
               'href="https://doi.org/10.1136/bmj.39079.618287.0B"', fixed = TRUE)
  expect_match(link_reference("See https://example.org/a?b=1&c=2."),
               'href="https://example.org/a?b=1&amp;c=2"', fixed = TRUE)
  expect_equal(link_reference("No link here"), "No link here")

  js <- js_string("<a href='x'>&</a>\n")
  expect_false(grepl("[<>&']", js))
  expect_match(js, "\\n", fixed = TRUE)
})

test_that("references split on bars and line breaks but not semicolons", {
  refs <- split_references(c("A. 2004;82(3):213-8. | B", "C\nD", NA), 3)
  expect_equal(refs, list(c("A. 2004;82(3):213-8.", "B"), c("C", "D"), character(0)))
  expect_equal(split_references(list(c("x", "y"), NULL), 2),
               list(c("x", "y"), character(0)))
  expect_equal(split_references(NULL, 2), list(character(0), character(0)))
})

test_that("extra columns become labeled fields", {
  d <- data.frame(name = c("a", "b"), sample_size = c(10, NA), note = c(NA, "x"))
  f <- field_values(d, "name")
  expect_equal(f[[1]], c("Sample size" = "10"))
  expect_equal(f[[2]], c(Note = "x"))
})

test_that("the graph becomes a widget, a ggplot and a file", {
  dag <- ggcausal(cleft_dag$edges, cleft_dag$nodes)
  w <- graph_widget(dag)
  expect_s3_class(w, c("girafe", "htmlwidget"))
  expect_true(any(vapply(w$dependencies, `[[`, "", "name") == "ggextreme-graph"))
  expect_s3_class(graph_plot(dag), "ggplot")

  png <- tempfile(fileext = ".png")
  graph_save(dag, png, res = 72)
  expect_true(file.exists(png))
  expect_error(graph_save(dag, tempfile(fileext = ".pdf")), "html")

  skip_if_not(rmarkdown::pandoc_available())
  html <- tempfile(fileext = ".html")
  graph_save(dag, html)
  expect_match(paste(readLines(html, warn = FALSE), collapse = ""), "ggextremePin")
})

test_that("role fills are colors with transparency", {
  dag <- ggcausal(cleft_dag$edges, cleft_dag$nodes)
  boxes <- layer_of(dag$plot, "GeomInteractivePolygon")[[1]]
  exposure <- boxes[boxes$data_id == "n2", ]
  expect_equal(unique(exposure$fill), known_roles[["exposure"]])
  expect_equal(unique(exposure$alpha), 0.16)
})

test_that("a graph takes a dark theme as a widget and as a static copy", {
  dag <- ggcausal(cleft_dag$edges, cleft_dag$nodes, title = "A title")
  expect_match(graph_widget(dag, "dark")$jsHooks$render[[1]]$code, '"dark"', fixed = TRUE)
  expect_match(graph_widget(dag)$jsHooks$render[[1]]$code, '"auto"', fixed = TRUE)

  dark <- graph_plot(dag, theme = "dark")
  text <- layer_of(dark, "GeomText")
  title <- Filter(function(d) any(d$label == "A title"), text)[[1]]
  expect_equal(title$colour[title$label == "A title"], dark_ink[["#1F1F1F"]])
  boxes <- layer_of(dark, "GeomInteractivePolygon")[[1]]
  expect_true(known_roles[["exposure"]] %in% boxes$fill)
  # The light plot is left as it was.
  light <- Filter(function(d) any(d$label == "A title"), layer_of(graph_plot(dag), "GeomText"))[[1]]
  expect_equal(light$colour[light$label == "A title"], graph_ink$title)

  png <- tempfile(fileext = ".png")
  graph_save(dag, png, res = 40, theme = "dark")
  expect_true(file.exists(png))
})

test_that("every neutral has a dark counterpart in R and in the stylesheet", {
  neutrals <- unlist(graph_ink[setdiff(names(graph_ink), "on_color")])
  expect_true(all(toupper(unique(neutrals)) %in% names(dark_ink)))
  css <- paste(readLines(system.file("www", "graph.css", package = "ggextreme")),
               collapse = "\n")
  for (hex in names(dark_ink)) {
    expect_match(css, sprintf('[fill="%s"] { fill: %s; }', hex, dark_ink[[hex]]), fixed = TRUE)
  }
})

test_that("adjustment paths follow the backdoor criterion", {
  e <- cleft_dag$edges
  n <- cleft_dag$nodes
  none <- ggcausal(e, n, paths = TRUE)
  expect_equal(none$adjustment$exposure, "smoking")
  expect_equal(none$adjustment$outcome, "cleft")
  expect_false(none$adjustment$sufficient)
  expect_equal(nrow(none$paths), 4)
  # Two backdoor paths through socioeconomic status are open; the path
  # through live birth is blocked by the collider.
  open_bias <- none$paths$kind == "biasing" & none$paths$status == "open"
  expect_equal(sum(open_bias), 2)
  expect_true(all(grepl("Socioeconomic status", none$paths$path[open_bias])))
  birth <- grepl("Live birth", none$paths$path)
  expect_equal(none$paths$status[birth], "blocked")
  expect_match(none$paths$reason[birth], "collider")
  expect_equal(none$adjustment$minimal_sets, list("ses"))

  ses <- ggcausal(e, n, paths = TRUE, adjust = "ses")
  expect_true(ses$adjustment$sufficient)
  expect_equal(sum(ses$paths$status == "open"), 1)
  expect_match(ses$adjustment$verdict, "is sufficient")

  # Adjusting for the collider opens its path, and live birth is also a
  # consequence of the exposure.
  both <- ggcausal(e, n, paths = TRUE, adjust = c("ses", "birth"))
  expect_false(both$adjustment$sufficient)
  expect_equal(both$paths$status[grepl("Live birth", both$paths$path)], "open")
  expect_match(both$adjustment$verdict, "consequence of the exposure")

  expect_error(ggcausal(e, n, paths = TRUE, adjust = "genes"), "Unobserved")
  expect_error(ggcausal(e, paths = TRUE), "exposure")
  expect_error(ggcausal(e, n, paths = TRUE, exposure = "cleft"), "different")
  expect_null(ggcausal(e, n)$paths)
})

test_that("a descendant of a collider opens it, and a mediator blocks the effect", {
  e <- data.frame(from = c("X", "M", "X", "Y", "C", "U", "U"),
                  to   = c("M", "Y", "C", "C", "D", "X", "Y"))
  n <- data.frame(name = c("X", "M", "Y", "C", "D", "U"),
                  role = c("exposure", "mediator", "outcome", "collider", "", "unobserved"))
  g <- ggcausal(e, n, paths = TRUE, adjust = c("D", "M"))
  through <- function(v) grepl(v, g$paths$path, fixed = TRUE)
  expect_equal(g$paths$status[through("C")], "open")
  expect_match(g$paths$reason[through("C")], "consequence of the collider")
  expect_equal(g$paths$status[through("M")], "blocked")
  expect_false(g$adjustment$sufficient)
  # The confounder is unobserved, so no set of observed variables works.
  expect_length(g$adjustment$minimal_sets, 0)
})

test_that("the widget gets the paths and the static plot colors them", {
  g <- ggcausal(cleft_dag$edges, cleft_dag$nodes, paths = TRUE, adjust = "ses",
                caption = "Source: a review.")
  expect_match(g$on_render, "ggextremeCausalPaths", fixed = TRUE)
  expect_equal(g$render_data$exposure, "smoking")
  # The verdict wraps under the diagram instead of widening the page.
  wide <- ggcausal(cleft_dag$edges, cleft_dag$nodes, paths = TRUE, adjust = c("ses", "birth"))
  plain <- ggcausal(cleft_dag$edges, cleft_dag$nodes)
  expect_equal(wide$width, plain$width)
  verdict <- Filter(function(d) any(d$data_id == "cv"), layer_of(wide$plot, "GeomInteractiveText"))[[1]]
  expect_gt(nrow(verdict), 1)
  expect_match(paste(verdict$label, collapse = " "), wide$adjustment$verdict, fixed = TRUE)
  expect_true("Source: a review." %in% unlist(lapply(layer_of(g$plot, "GeomText"), `[[`, "label")))
  hidden <- vapply(g$render_data$nodes, function(v) v$hidden, logical(1))
  expect_equal(cleft_dag$nodes$name[hidden], "genes")
  arrows <- layer_of(g$plot, "GeomInteractivePath")[[1]]
  state <- causal_edge_state(causal_setup(cleft_dag$nodes, cleft_dag$edges, NULL, NULL, "ses"),
                             cleft_dag$edges)
  expect_equal(state[cleft_dag$edges$from == "smoking" & cleft_dag$edges$to == "cleft"], "causal")
  expect_equal(state[cleft_dag$edges$from == "ses" & cleft_dag$edges$to == "cleft"], "blocked")
  expect_equal(state[cleft_dag$edges$from == "genes"], "none")
  expect_true(causal_ink$causal %in% arrows$colour)
  boxes <- Filter(function(d) any(grepl("^adj", d$data_id)), layer_of(g$plot, "GeomInteractivePolygon"))[[1]]
  expect_equal(unique(boxes$data_id[!is.na(boxes$colour)]), paste0("adj", match("ses", cleft_dag$nodes$name)))
  expect_s3_class(graph_widget(g), "htmlwidget")
})
