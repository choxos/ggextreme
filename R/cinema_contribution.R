#' Draw where each network estimate's evidence comes from, study by study
#'
#' Draws the contribution of every study to every estimate of a network
#' meta-analysis, from `netmeta::netcontrib(x, study = TRUE)`: one bar per
#' comparison, split into studies as wide as their share of the estimate and
#' colored by each study's risk of bias or indirectness, with the studies
#' grouped low, moderate and high as CINeMA draws them; a comparison by
#' study matrix of the same numbers, whose squares mark with an outline the
#' studies that compare that pair head to head; and a small network.
#'
#' In the widget, a switch colors the studies by risk of bias or by
#' indirectness. Selecting a comparison, from its label, the matrix or the
#' menu, lights its bar and matrix row and widens each line of the small
#' network by the share of the estimate that flows through it. Selecting a
#' study, from the matrix, its button or the panel, dims the others and
#' marks the comparisons it randomized. Clicking a part of a bar lists the
#' studies with that judgment in that estimate, with their shares and the
#' reasons for their judgments. The labels and matrix headers can be reached
#' with the keyboard, and a sentence under the controls says what the
#' selection shows.
#'
#' Contributions say where an estimate's information comes from, not how
#' trustworthy it is. Dropping a study would change the whole fit, so there
#' is deliberately no switch that removes one and rescales the bars.
#'
#' @param x Judgments from [cinema_judge()] made with study judgments, or a
#'   network meta-analysis from [netmeta::netmeta()], judged with the
#'   arguments in `...`.
#' @param ... When `x` is a netmeta fit, arguments for [cinema_judge()]: the
#'   study judgments `rob` and `indirectness`, at least one of them, and
#'   optionally `contributions`, `small_values`, `order` and `pooled`.
#' @param color Which judgment colors the studies at first, `"rob"` or
#'   `"indirectness"`. Defaults to risk of bias when it was given.
#' @param positions Optional data frame placing the treatments of the small
#'   network by hand, with columns `treatment`, `x` and `y` and `y` pointing
#'   up, as in [ggnma()]. Defaults to a circle.
#' @param title,caption Title above the plot and note below it. A caption
#'   you give is added above the default notes.
#' @param family Font family. The package ships Lato and registers it on load.
#'
#' @inheritSection cinema_judge Sources
#'
#' @return An object of class `cinema_contribution`, which prints as an
#'   interactive widget. Use [graph_widget()], [graph_plot()] or
#'   [graph_save()] for the widget, a static ggplot or a file. The field
#'   `contributions` holds the share of each estimate from each study.
#' @export
#'
#' @examples
#' \donttest{
#' if (requireNamespace("netmeta", quietly = TRUE) &&
#'     requireNamespace("meta", quietly = TRUE)) {
#'   pw <- meta::pairwise(treat = treatment, event = pasi75_r,
#'                        n = pasi75_n, studlab = study,
#'                        data = psoriasis_nma, sm = "OR")
#'   nma <- netmeta::netmeta(pw, common = FALSE)
#'   # Illustrative judgments, invented for this example.
#'   rob <- data.frame(
#'     study = c("CLEAR", "ERASURE", "FEATURE", "FIXTURE", "JUNCTURE"),
#'     judgment = c("high", "low", "some concerns", "low", "some concerns")
#'   )
#'   cinema_contribution(nma, rob = rob, small_values = "undesirable",
#'                       caption = "Illustrative judgments, not published assessments.")
#' }
#' }
cinema_contribution <- function(x, ..., color = NULL, positions = NULL, title = NULL, caption = NULL,
                                family = "Lato") {
  cn <- cinema_input(x, ..., fn = "cinema_contribution")
  has <- c(rob = !is.null(cn$studies$rob), indirectness = !is.null(cn$studies$indirectness))
  if (!any(has) || is.null(cn$contrib)) {
    rlang::abort("cinema_contribution() needs study judgments: give `rob` or `indirectness`.")
  }
  if (is.null(color)) color <- names(has)[has][1]
  color <- match.arg(color, c("rob", "indirectness"))
  if (!has[[color]]) rlang::abort(paste0("`color` is \"", color, "\", but no study judgments of it were given."))
  lay <- contrib_layout(cn, color, positions, title, caption, family)
  structure(
    list(plot = contrib_draw(lay), width = lay$page$width / 72, height = lay$page$height / 72,
         title = title, on_render = "ggextremeCinemaContribution(el, data);",
         render_data = contrib_render(lay), hover_inv = "opacity:1;",
         contributions = cn$contributions, judgments = cn),
    class = c("cinema_contribution", "ggx_graph")
  )
}

# Circle or hand placed positions for a small network, in points, centered on
# 0 and spanning about `size`.
cinema_ring <- function(trts, positions, size) {
  k <- length(trts)
  if (is.null(positions)) {
    angle <- -pi / 2 + 2 * pi * (seq_len(k) - 1) / k
    return(list(x = size / 2 * cos(angle), y = size / 2 * sin(angle)))
  }
  network_positions(positions, trts, size / 2)
}

# Diagonal hatching inside a rectangle, as segments, for the static copy.
cinema_hatch <- function(x0, x1, y0, y1, gap = 4) {
  out <- NULL
  for (c0 in seq(x0 - y1, x1 - y0, by = gap)) {
    ya <- max(y0, x0 - c0)
    yb <- min(y1, x1 - c0)
    if (yb > ya) out <- rbind(out, data.frame(x = ya + c0, xend = yb + c0, y = ya, yend = yb))
  }
  out
}

contrib_layout <- function(cn, color, positions, title, caption, family) {
  dims <- utils::modifyList(graph_dims, list(label_pt = 9, small_pt = 8.5, row_h = 24, bar_h = 16, bar_w = 400,
                                             cell = 18, net = 150))
  pr <- cn$pairs
  n <- nrow(pr)
  studies <- cn$study_names
  ns <- length(studies)
  width <- function(s, pt, bold = FALSE) max(text_width_card(s, pt, family, 1, bold = bold))
  labels <- paste(pr$a, "vs", pr$b)
  lab_w <- width(labels, dims$label_pt) + 4
  b0 <- lab_w + 10
  b1 <- b0 + dims$bar_w
  ev_x <- b1 + 8
  total_w <- ev_x + width(c("indirect only", "Evidence"), dims$small_pt)
  key_top <- 0
  head_y <- key_top + 30
  rows_top <- head_y + 10
  row_y <- rows_top + (seq_len(n) - 1) * dims$row_h
  bars_bottom <- rows_top + n * dims$row_h
  axis_y <- bars_bottom + 2

  # Matrix and small network, side by side when they fit.
  pitch <- max(dims$cell + 8, min(40, width(studies, dims$small_pt) + 8))
  rotate <- width(studies, dims$small_pt) > pitch - 6
  head_h <- if (rotate) width(studies, dims$small_pt) * 0.72 + 18 else 30
  mat_top <- axis_y + 52
  m0 <- lab_w + 10
  col_x <- m0 + (seq_len(ns) - 0.5) * pitch
  mat_right <- m0 + ns * pitch
  mrow_top <- mat_top + head_h
  mrow_y <- mrow_top + (seq_len(n) - 1) * (dims$cell + 4)
  mat_bottom <- mrow_top + n * (dims$cell + 4)
  size_key_y <- mat_bottom + 16

  # Node labels sit outside their node, pointing away from the middle, so
  # the network's width is measured to their far ends.
  ring <- cinema_ring(cn$order, positions, dims$net)
  node_w <- text_width_card(cn$order, dims$small_pt, family, 1)
  ux <- ring$x - mean(ring$x)
  uy <- ring$y - mean(ring$y)
  away <- sqrt(ux^2 + uy^2)
  away[away < 1e-6] <- 1
  ux <- ux / away
  uy <- uy / away
  hjust <- ifelse(abs(ux) < 0.3, 0.5, (1 - sign(ux)) / 2)
  lx <- ring$x + ux * 12
  ly <- ring$y + uy * 12 + ifelse(abs(uy) > 0.5, 3 * sign(uy), 0)
  left <- min(lx - hjust * node_w, ring$x - 8)
  net_w <- max(lx + (1 - hjust) * node_w, ring$x + 8) - left
  beside <- mat_right + 40 + net_w <= max(total_w + 90, 560)
  net_cx <- if (beside) mat_right + 40 - left else 10 - left
  net_top <- if (beside) mat_top + 22 else size_key_y + 40
  net_cy <- net_top + dims$net / 2 + 36
  nodes <- data.frame(t = cn$order, x = net_cx + ring$x, y = net_cy + ring$y, lx = net_cx + lx, ly = net_cy + ly,
                      hjust = hjust, stringsAsFactors = FALSE)
  bottom <- max(size_key_y + 12, net_cy + dims$net / 2 + 36)
  right <- max(total_w, mat_right, if (beside) mat_right + 40 + net_w else net_w)

  cap <- c(caption,
           paste0("Each bar splits a network estimate into the studies it draws on, as wide as their share of it, ",
                  "grouped low, moderate and high as CINeMA draws them. ", cn$contribution_method),
           paste0("An outlined square marks a study that compares that pair head to head; its evidence reaches the ",
                  "other comparisons through them. Dropping a study would change the whole fit, so no switch removes one."),
           paste0("Study judgments: ", paste(c(if (!is.null(cn$studies$rob)) "risk of bias",
                                                if (!is.null(cn$studies$indirectness)) "indirectness"), collapse = " and "),
                  ", as given to cinema_judge()."))
  cap <- cinema_wrap(cap, right - 10, graph_dims$caption_pt, family)
  page <- graph_canvas(c(0, right), c(0, bottom), title, cap, character(0), character(0), NULL, dims, family)

  # Every comparison's studies with a share, and every line of the network.
  ct <- cn$contrib[cn$contrib$share > 5e-4, , drop = FALSE]
  edges <- unique(data.frame(a = pmin(cn$fit$treat1, cn$fit$treat2), b = pmax(cn$fit$treat1, cn$fit$treat2),
                             stringsAsFactors = FALSE))
  edges$key <- pair_key(edges$a, edges$b)
  edges$studies <- lapply(edges$key, function(k) which(studies %in% cn$direct[[match(k, pr$key)]]))
  list(cn = cn, color = color, dims = dims, page = page, family = family, pr = pr, n = n, studies = studies,
       ns = ns, labels = labels, lab_w = lab_w, b0 = b0, b1 = b1, ev_x = ev_x, key_top = key_top,
       head_y = head_y, rows_top = rows_top, row_y = row_y, bars_bottom = bars_bottom, axis_y = axis_y,
       pitch = pitch, rotate = rotate, mat_top = mat_top, m0 = m0, col_x = col_x, mat_right = mat_right,
       mrow_top = mrow_top, mrow_y = mrow_y, mat_bottom = mat_bottom, size_key_y = size_key_y,
       nodes = nodes, net_top = net_top, beside = beside, ct = ct, edges = edges, right = right)
}

contrib_levels <- function(cn, what, studies) {
  sj <- cn$studies[[what]]
  if (is.null(sj)) return(rep(NA_integer_, length(studies)))
  sj$level[match(studies, sj$study)]
}

# The order of a bar's studies: grouped by judgment, low first, and by share
# within a group.
contrib_segments <- function(lay, i, what) {
  ct <- lay$ct[lay$ct$key == lay$pr$key[i], , drop = FALSE]
  j <- match(ct$study, lay$studies)
  lv <- contrib_levels(lay$cn, what, lay$studies)[j]
  o <- order(lv, -ct$share)
  data.frame(j = j[o], share = ct$share[o], level = lv[o], from = c(0, cumsum(ct$share[o]))[seq_along(o)],
             stringsAsFactors = FALSE)
}

contrib_draw <- function(lay) {
  cn <- lay$cn
  pr <- lay$pr
  dims <- lay$dims
  px <- lay$page$px
  py <- lay$page$py
  n <- lay$n
  what <- lay$color
  fam <- lay$family
  texts <- function(label, x, y, pt, color, hjust = 0, face = "plain") {
    if (!length(label)) return(empty_texts())
    data.frame(x = px(x), y = py(y), label = as.character(label), size = pt / .pt, colour = color,
               hjust = hjust, fontface = face, stringsAsFactors = FALSE)
  }
  rect <- function(x0, x1, y0, y1) data.frame(xmin = px(x0), xmax = px(x1), ymin = py(y1), ymax = py(y0))
  seg <- function(x, xend, y, yend) data.frame(x = px(x), xend = px(xend), y = py(y), yend = py(yend))
  BX <- function(v) lay$b0 + v * (lay$b1 - lay$b0)
  lv_all <- contrib_levels(cn, what, lay$studies)
  study_tip <- function(j) {
    lv <- c(rob = contrib_levels(cn, "rob", lay$studies)[j], indirectness = contrib_levels(cn, "indirectness", lay$studies)[j])
    tip_rows(c(if (!is.na(lv[["rob"]])) c("Risk of bias" = cinema_study_short$rob[lv[["rob"]] + 1]),
               if (!is.na(lv[["indirectness"]])) c("Indirectness" = cinema_study_short$indirectness[lv[["indirectness"]] + 1])))
  }

  # The key.
  words <- cinema_study_short[[what]]
  key_x <- c(0, cumsum(text_width_card(words, dims$small_pt, fam, 1) + 28))[1:3]
  title_w <- text_width_card(paste0(cinema_study_title[[what]], " of each study:"), dims$small_pt, fam, 1, bold = TRUE)
  key_x <- key_x + title_w + 12
  key_rects <- data.frame(rect(key_x, key_x + 10, 5, 15), fill = cinema_ink$level, id = paste0("kq", 1:3))
  live <- rbind(
    data.frame(texts(paste0(cinema_study_title[[what]], " of each study:"), 0, 10, dims$small_pt, graph_ink$title,
                     face = "bold"), id = "kt"),
    data.frame(texts(paste0(words, c("", "", " (hatched)")), key_x + 14, 10, dims$small_pt, graph_ink$text),
               id = paste0("kl", 1:3))
  )
  labels <- rbind(
    texts("Network comparison", lay$lab_w, lay$head_y, dims$small_pt, graph_ink$muted, hjust = 1),
    texts("Evidence", lay$ev_x, lay$head_y, dims$small_pt, graph_ink$muted),
    texts(c(mixed = "mixed", direct = "direct only", indirect = "indirect only")[pr$type], lay$ev_x,
          lay$row_y + dims$row_h / 2, dims$small_pt, graph_ink$muted)
  )

  # Rows: a background to light, the label to click, and the segments.
  row_bg <- data.frame(rect(0, lay$right, lay$row_y + 1, lay$row_y + dims$row_h - 1), id = paste0("r", seq_len(n)))
  row_tip <- vapply(seq_len(n), function(i) {
    paste0('<div class="ggx-tip-title">', esc(lay$labels[i]), "</div>",
           tip_rows(c("Network estimate" = cinema_estimate(pr$te[i], pr$lo[i], pr$hi[i], cn$ratio),
                      "Evidence" = c(mixed = "direct and indirect", direct = "direct only", indirect = "indirect only")[[pr$type[i]]],
                      "Studies that contribute" = sum(lay$ct$key == pr$key[i]))),
           '<div class="ggx-tip-hint">Click or press Enter to see the studies behind this estimate.</div>')
  }, character(1))
  row_hit <- data.frame(rect(0, lay$lab_w + 4, lay$row_y + 2, lay$row_y + dims$row_h - 2),
                        id = paste0("c", seq_len(n)), tooltip = row_tip)
  row_labels <- data.frame(texts(lay$labels, lay$lab_w, lay$row_y + dims$row_h / 2, dims$label_pt, graph_ink$title,
                                 hjust = 1), id = paste0("c", seq_len(n)), tooltip = row_tip)
  segs <- NULL
  seg_labels <- NULL
  hatch <- NULL
  for (i in seq_len(n)) {
    s <- contrib_segments(lay, i, what)
    y0 <- lay$row_y[i] + (dims$row_h - dims$bar_h) / 2
    y1 <- y0 + dims$bar_h
    x0 <- BX(s$from)
    x1 <- BX(s$from + s$share)
    tip <- vapply(seq_len(nrow(s)), function(k) {
      j <- s$j[k]
      h2h <- lay$studies[j] %in% cn$direct[[i]]
      paste0('<div class="ggx-tip-title">', esc(lay$studies[j]), "</div>",
             '<div class="ggx-tip-sub">in ', esc(lay$labels[i]), "</div>",
             tip_rows(c("Share of the estimate" = cinema_pct(s$share[k]),
                        "Head to head" = if (h2h) "yes" else "no, through other treatments")),
             study_tip(j), '<div class="ggx-tip-hint">Click to list the studies with this judgment in this estimate.</div>')
    }, character(1))
    ids <- paste0("b", i, "_", s$j)
    segs <- rbind(segs, data.frame(rect(x0, x1, y0, y1), fill = cinema_ink$level[s$level + 1], id = ids,
                                   tooltip = tip, stringsAsFactors = FALSE))
    w <- x1 - x0
    lab <- ifelse(w > text_width_card(paste0(lay$studies[s$j], " ", round(100 * s$share), "%"), 8, fam, 1, bold = TRUE) + 8,
                  paste0(lay$studies[s$j], " ", round(100 * s$share), "%"),
                  ifelse(w > text_width_card(lay$studies[s$j], 8, fam, 1, bold = TRUE) + 8, lay$studies[s$j],
                         ifelse(w > 26, paste0(round(100 * s$share), "%"), "")))
    keep <- nzchar(lab)
    if (any(keep)) {
      seg_labels <- rbind(seg_labels, data.frame(texts(lab[keep], x0[keep] + 4, (y0 + y1) / 2, 8, graph_ink$on_color,
                                                       face = "bold"), id = ids[keep], tooltip = tip[keep]))
    }
    for (k in which(s$level == 2)) hatch <- rbind(hatch, cinema_hatch(x0[k], x1[k], y0, y1))
  }
  ticks <- c(0, 0.25, 0.5, 0.75, 1)
  grid <- seg(BX(ticks), BX(ticks), lay$rows_top - 4, lay$bars_bottom)
  axis <- rbind(seg(lay$b0, lay$b1, lay$axis_y, lay$axis_y), seg(BX(ticks), BX(ticks), lay$axis_y, lay$axis_y + 4))
  labels <- rbind(labels,
    texts(paste0(100 * ticks, "%"), BX(ticks), lay$axis_y + 12, dims$small_pt, graph_ink$muted, hjust = 0.5),
    texts("Share of each network estimate contributed by each study", (lay$b0 + lay$b1) / 2, lay$axis_y + 26,
          dims$small_pt, graph_ink$text, hjust = 0.5))

  # The matrix.
  cell <- dims$cell
  pitch_y <- cell + 4
  mat_bg <- rbind(
    data.frame(rect(0, lay$mat_right, lay$mrow_y - 2, lay$mrow_y + cell + 2), id = paste0("mr", seq_len(n))),
    data.frame(rect(lay$col_x - lay$pitch / 2, lay$col_x + lay$pitch / 2, lay$mat_top - 2, lay$mat_bottom),
               id = paste0("mk", seq_len(lay$ns)))
  )
  mat_labels <- data.frame(texts(lay$labels, lay$lab_w, lay$mrow_y + cell / 2, dims$small_pt, graph_ink$title,
                                 hjust = 1), id = paste0("c", seq_len(n)), tooltip = row_tip)
  head_tip <- vapply(seq_len(lay$ns), function(j) {
    informs <- sum(lay$ct$study == lay$studies[j])
    paste0('<div class="ggx-tip-title">', esc(lay$studies[j]), "</div>",
           '<div class="ggx-tip-sub">Informs ', informs, " of ", n, " network estimates</div>", study_tip(j),
           '<div class="ggx-tip-hint">Click or press Enter to select this study.</div>')
  }, character(1))
  head_hit <- data.frame(rect(lay$col_x - lay$pitch / 2 + 1, lay$col_x + lay$pitch / 2 - 1, lay$mat_top - 2,
                              lay$mrow_top - 2), id = paste0("s", seq_len(lay$ns)), tooltip = head_tip)
  head_text <- data.frame(texts(lay$studies, lay$col_x + if (lay$rotate) -2 else 0, lay$mrow_top - 12, dims$small_pt,
                                graph_ink$title, hjust = if (lay$rotate) 0 else 0.5),
                          id = paste0("s", seq_len(lay$ns)), tooltip = head_tip)
  head_text$angle <- if (lay$rotate) 40 else 0
  dots <- data.frame(x = px(lay$col_x), y = py(lay$mrow_top - 4), fill = cinema_ink$level[lv_all + 1],
                     id = paste0("d", seq_len(lay$ns)), stringsAsFactors = FALSE)
  outline <- NULL
  squares <- NULL
  for (i in seq_len(n)) {
    cy <- lay$mrow_y[i] + cell / 2
    direct <- which(lay$studies %in% cn$direct[[i]])
    if (length(direct)) outline <- rbind(outline, rect(lay$col_x[direct] - cell / 2 - 1, lay$col_x[direct] + cell / 2 + 1,
                                                       cy - cell / 2 - 1, cy + cell / 2 + 1))
    ct <- lay$ct[lay$ct$key == pr$key[i], , drop = FALSE]
    j <- match(ct$study, lay$studies)
    side <- pmax(2.5, cell * sqrt(ct$share))
    tip <- vapply(seq_along(j), function(k) {
      paste0('<div class="ggx-tip-title">', esc(lay$studies[j[k]]), " in ", esc(lay$labels[i]), "</div>",
             tip_rows(c("Share of the estimate" = cinema_pct(ct$share[k]),
                        "Head to head" = if (lay$studies[j[k]] %in% cn$direct[[i]]) "yes" else "no, through other treatments")),
             study_tip(j[k]), '<div class="ggx-tip-hint">Click to select this comparison and this study.</div>')
    }, character(1))
    squares <- rbind(squares, data.frame(rect(lay$col_x[j] - side / 2, lay$col_x[j] + side / 2, cy - side / 2, cy + side / 2),
                                         fill = cinema_ink$level[lv_all[j] + 1], id = paste0("m", i, "_", j), tooltip = tip,
                                         stringsAsFactors = FALSE))
  }
  sk <- c(0.1, 0.25, 0.5, 1)
  sk_x <- lay$m0 + c(62, 100, 142, 190)
  sk_side <- cell * sqrt(sk)
  size_key <- rect(sk_x, sk_x + sk_side, lay$size_key_y - sk_side / 2, lay$size_key_y + sk_side / 2)
  labels <- rbind(labels,
    texts("Matrix: square area is the study's share of the estimate", 0, lay$mat_top - 14, dims$small_pt, graph_ink$title,
          face = "bold"),
    texts("Square area:", lay$m0, lay$size_key_y, dims$small_pt, graph_ink$muted),
    texts(paste0(100 * sk, "%"), sk_x + sk_side + 3, lay$size_key_y, dims$small_pt, graph_ink$muted),
    texts("head to head", lay$m0 + 262, lay$size_key_y, dims$small_pt, graph_ink$muted))
  outline <- rbind(outline, rect(lay$m0 + 238, lay$m0 + 238 + cell + 2, lay$size_key_y - cell / 2 - 1,
                                 lay$size_key_y + cell / 2 + 1))

  # The small network.
  nd <- lay$nodes
  e <- lay$edges
  ea <- match(e$a, nd$t)
  eb <- match(e$b, nd$t)
  edge_df <- data.frame(seg(nd$x[ea], nd$x[eb], nd$y[ea], nd$y[eb]), id = paste0("e", seq_len(nrow(e))))
  node_labels <- data.frame(texts(nd$t, nd$lx, nd$ly, dims$small_pt, graph_ink$title, hjust = nd$hjust),
                            id = paste0("nl", seq_len(nrow(nd))))
  node_df <- data.frame(x = px(nd$x), y = py(nd$y), id = paste0("n", seq_len(nrow(nd))), stringsAsFactors = FALSE)
  live <- rbind(live, data.frame(texts("Network: select a comparison or a study",
                                       min(nd$lx - nd$hjust * text_width_card(nd$t, dims$small_pt, fam, 1)), lay$net_top,
                                       dims$small_pt, graph_ink$title, face = "bold"),
                                 id = "nh"))
  live <- rbind(live, node_labels)

  ggplot() +
    ggiraph::geom_rect_interactive(data = row_bg, aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin,
                                                      ymax = .data$ymax, data_id = .data$id),
                                   fill = graph_ink$faint, alpha = 0, colour = NA) +
    ggiraph::geom_rect_interactive(data = mat_bg, aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin,
                                                      ymax = .data$ymax, data_id = .data$id),
                                   fill = graph_ink$faint, alpha = 0, colour = NA) +
    geom_segment(data = grid, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
                 colour = graph_ink$faint, linewidth = 0.6 / .pt) +
    ggiraph::geom_rect_interactive(data = row_hit, aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin,
                                                       ymax = .data$ymax, data_id = .data$id, tooltip = .data$tooltip),
                                   fill = graph_ink$page, alpha = 0.01, colour = NA) +
    ggiraph::geom_rect_interactive(data = segs, aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin,
                                                    ymax = .data$ymax, fill = .data$fill, data_id = .data$id,
                                                    tooltip = .data$tooltip),
                                   alpha = 0.55, colour = graph_ink$page, linewidth = 1.2 / .pt) +
    (if (!is.null(hatch)) ggiraph::geom_segment_interactive(
      data = seg(hatch$x, hatch$xend, hatch$y, hatch$yend),
      aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend, data_id = "h"),
      colour = cinema_ink$level[3], linewidth = 1 / .pt)) +
    (if (!is.null(seg_labels)) ggiraph::geom_text_interactive(
      data = seg_labels, aes(x = .data$x, y = .data$y, label = .data$label, data_id = .data$id, tooltip = .data$tooltip),
      hjust = 0, size = seg_labels$size, colour = graph_ink$title, fontface = "bold", family = fam)) +
    geom_segment(data = axis, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
                 colour = graph_ink$text, linewidth = 0.8 / .pt) +
    geom_rect(data = outline, aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, ymax = .data$ymax),
              fill = NA, colour = graph_ink$text, linewidth = 1 / .pt) +
    ggiraph::geom_rect_interactive(data = squares, aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin,
                                                       ymax = .data$ymax, fill = .data$fill, data_id = .data$id,
                                                       tooltip = .data$tooltip), alpha = 0.85, colour = NA) +
    geom_rect(data = size_key, aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, ymax = .data$ymax),
              fill = graph_ink$muted) +
    ggiraph::geom_rect_interactive(data = head_hit, aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin,
                                                        ymax = .data$ymax, data_id = .data$id, tooltip = .data$tooltip),
                                   fill = graph_ink$page, alpha = 0.01, colour = NA) +
    ggiraph::geom_text_interactive(data = head_text, aes(x = .data$x, y = .data$y, label = .data$label, data_id = .data$id,
                                                         tooltip = .data$tooltip, angle = .data$angle),
                                   hjust = head_text$hjust, size = dims$small_pt / .pt, colour = graph_ink$title,
                                   family = fam) +
    ggiraph::geom_point_interactive(data = dots, aes(x = .data$x, y = .data$y, fill = .data$fill, data_id = .data$id),
                                    shape = 21, size = 2.2, colour = graph_ink$page, stroke = 0) +
    ggiraph::geom_rect_interactive(data = key_rects, aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin,
                                                         ymax = .data$ymax, fill = .data$fill, data_id = .data$id),
                                   alpha = 0.55, colour = NA) +
    ggiraph::geom_segment_interactive(data = edge_df, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend,
                                                          data_id = .data$id),
                                      colour = graph_ink$ghost, linewidth = 3 / .pt, lineend = "round") +
    ggiraph::geom_point_interactive(data = node_df, aes(x = .data$x, y = .data$y, data_id = .data$id),
                                    shape = 21, size = 3.4, fill = graph_ink$page, colour = graph_ink$muted, stroke = 1.2) +
    draw_text(rbind(labels), fam) +
    ggiraph::geom_text_interactive(data = rbind(row_labels, mat_labels),
                                   aes(x = .data$x, y = .data$y, label = .data$label, data_id = .data$id,
                                       tooltip = .data$tooltip), hjust = 1, size = dims$label_pt / .pt,
                                   colour = graph_ink$title, family = fam) +
    ggiraph::geom_text_interactive(data = live, aes(x = .data$x, y = .data$y, label = .data$label, data_id = .data$id),
                                   hjust = live$hjust, size = live$size, colour = live$colour, fontface = live$fontface,
                                   family = fam) +
    graph_frame(lay$page, fam)
}

contrib_render <- function(lay) {
  cn <- lay$cn
  pr <- lay$pr
  page <- lay$page
  BX <- function(v) lay$b0 + v * (lay$b1 - lay$b0)
  lv <- list(rob = contrib_levels(cn, "rob", lay$studies), indirectness = contrib_levels(cn, "indirectness", lay$studies))
  reason <- function(what) {
    sj <- cn$studies[[what]]
    if (is.null(sj)) return(rep("", lay$ns))
    sj$reason[match(lay$studies, sj$study)]
  }
  list(
    dx = page$px(0), dy = page$height - page$py(0),
    b0 = lay$b0, b1 = lay$b1, row_y = I(lay$row_y), row_h = lay$dims$row_h, bar_h = lay$dims$bar_h,
    start = lay$color, has = list(rob = !is.null(cn$studies$rob), indirectness = !is.null(cn$studies$indirectness)),
    method = cn$contribution_method, measure = cn$measure,
    studies = lapply(seq_len(lay$ns), function(j) list(
      name = lay$studies[j], rob = if (is.na(lv$rob[j])) NULL else lv$rob[j],
      indirectness = if (is.na(lv$indirectness[j])) NULL else lv$indirectness[j],
      rob_reason = reason("rob")[j], indirectness_reason = reason("indirectness")[j])),
    comparisons = lapply(seq_len(lay$n), function(i) {
      ct <- lay$ct[lay$ct$key == pr$key[i], , drop = FALSE]
      fl <- cn$flow[cn$flow$net_key == pr$key[i], , drop = FALSE]
      list(label = lay$labels[i], a = pr$a[i], b = pr$b[i], type = pr$type[i],
           estimate = cinema_estimate(pr$te[i], pr$lo[i], pr$hi[i], cn$ratio),
           direct = I(match(cn$direct[[i]], lay$studies) - 1L),
           shares = lapply(seq_len(nrow(ct)), function(k) list(j = match(ct$study[k], lay$studies) - 1L, share = ct$share[k])),
           edges = lapply(seq_len(nrow(fl)), function(k) list(e = match(fl$dir_key[k], lay$edges$key) - 1L, share = fl$share[k])))
    }),
    edges = lapply(seq_len(nrow(lay$edges)), function(k) list(a = lay$edges$a[k], b = lay$edges$b[k],
                                                               studies = I(lay$edges$studies[[k]] - 1L))),
    nodes = lapply(seq_len(nrow(lay$nodes)), function(k) list(t = lay$nodes$t[k], x = lay$nodes$x[k], y = lay$nodes$y[k])),
    arms = lapply(lay$studies, function(s) unique(c(cn$fit$treat1[cn$fit$studlab == s], cn$fit$treat2[cn$fit$studlab == s]))),
    title = as.list(cinema_study_title), short = cinema_study_short, words = cinema_study_words,
    phrase = cinema_study_phrase, levels = cinema_ink$level
  )
}
