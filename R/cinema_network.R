#' Draw a network plot with a strand for every study, colored by its judgment
#'
#' Draws the network of a network meta-analysis from arm level data, with
#' every line drawn as one strand per study, so a comparison's width counts
#' its studies and its colors show each study's risk of bias or indirectness
#' instead of an average that would hide a mix of judgments. Node area
#' follows the participants randomized to each treatment when `n` is given.
#'
#' In the widget, a switch colors the strands by risk of bias or by
#' indirectness, and a sentence under the plot counts the comparisons that
#' rest only on studies with the most serious judgment, those that mix
#' judgments and those that rest only on studies without concerns.
#' Hovering over a strand names its study and judgments. Clicking a line or
#' a treatment, or pressing Enter on it, opens a panel that lists the
#' studies behind it with their judgments and the reasons for them.
#'
#' @param data A data frame with one row per study arm.
#' @param study,treatment Bare column names identifying the study and the
#'   treatment of each arm.
#' @param n Optional bare column with the number of participants in each
#'   arm; it sets node area and the counts in the panels.
#' @param rob,indirectness Study level judgments of risk of bias and of
#'   indirectness, at least one of them: data frames with columns `study`,
#'   `judgment` and optionally `reason`, as in [cinema_judge()], or the
#'   `studies` field of its result.
#' @param color Which judgment colors the strands at first, `"rob"` or
#'   `"indirectness"`. Defaults to risk of bias when it was given.
#' @param positions Optional data frame placing the treatments by hand, with
#'   columns `treatment`, `x` and `y` and `y` pointing up, as in [ggnma()].
#'   Defaults to a circle.
#' @param title,caption Title above the plot and note below it. A caption
#'   you give is added above the default notes.
#' @param family Font family. The package ships Lato and registers it on load.
#'
#' @inheritSection cinema_judge Sources
#'
#' @return An object of class `cinema_network`, which prints as an
#'   interactive widget. Use [graph_widget()], [graph_plot()] or
#'   [graph_save()] for the widget, a static ggplot or a file. The field
#'   `edges` lists each comparison with its studies and how many of them
#'   have each judgment.
#' @export
#'
#' @examples
#' # Illustrative judgments, invented for this example.
#' rob <- data.frame(
#'   study = c("CLEAR", "ERASURE", "FEATURE", "FIXTURE", "JUNCTURE"),
#'   judgment = c("high", "low", "some concerns", "low", "some concerns"),
#'   reason = "Illustrative only."
#' )
#' cinema_network(psoriasis_nma, study, treatment, n = n, rob = rob,
#'                caption = "Illustrative judgments, not published assessments.")
cinema_network <- function(data, study, treatment, n = NULL, rob = NULL, indirectness = NULL, color = NULL,
                           positions = NULL, title = NULL, caption = NULL, family = "Lato") {
  if (!is.data.frame(data)) rlang::abort("`data` must be a data frame.")
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  study <- rlang::eval_tidy(rlang::enquo(study), data)
  treatment <- rlang::eval_tidy(rlang::enquo(treatment), data)
  size <- rlang::eval_tidy(rlang::enquo(n), data)
  if (is.null(study) || is.null(treatment)) rlang::abort("Give the `study` and `treatment` columns of `data`.")
  if (anyNA(study) || anyNA(treatment)) rlang::abort("`study` and `treatment` must not be missing.")
  levels <- if (is.factor(treatment)) levels(droplevels(treatment)) else unique(as.character(treatment))
  study <- as.character(study)
  treatment <- as.character(treatment)
  if (anyDuplicated(data.frame(study, treatment))) rlang::abort("Each treatment may appear once per study.")
  if (!is.null(size) && (!is.numeric(size) || anyNA(size) || any(size < 0))) {
    rlang::abort("`n` must be numeric, not missing, and not negative.")
  }
  studies <- unique(study)
  judged <- list(rob = cinema_studies(rob, "rob", studies),
                 indirectness = cinema_studies(indirectness, "indirectness", studies))
  has <- !vapply(judged, is.null, logical(1))
  if (!any(has)) rlang::abort("cinema_network() needs study judgments: give `rob` or `indirectness`.")
  if (is.null(color)) color <- names(has)[has][1]
  color <- match.arg(color, c("rob", "indirectness"))
  if (!has[[color]]) rlang::abort(paste0("`color` is \"", color, "\", but no study judgments of it were given."))
  lay <- cinet_layout(study, treatment, size, levels, studies, judged, color, positions, title, caption, family)
  structure(
    list(plot = cinet_draw(lay), width = lay$page$width / 72, height = lay$page$height / 72, title = title,
         on_render = "ggextremeCinemaNetwork(el, data); ggextremeCinemaKeys(el, 'e'); ggextremeCinemaKeys(el, 'n');",
         render_data = cinet_render(lay), hover_inv = "opacity:1;", edges = cinet_edges_table(lay)),
    class = c("cinema_network", "ggx_graph")
  )
}

# How each comparison's studies were judged, in a sentence, for one domain.
cinet_sentence <- function(lay, what) {
  lv <- lay$judged[[what]]$level[match(lay$studies, lay$judged[[what]]$study)]
  per <- lapply(lay$edges$study_list, function(s) lv[match(s, lay$studies)])
  worst <- vapply(per, function(v) all(v == 2), logical(1))
  mixed <- vapply(per, function(v) length(unique(v)) > 1, logical(1))
  clean <- vapply(per, function(v) all(v == 0), logical(1))
  names <- paste(lay$levels[lay$edges$from_i], "vs", lay$levels[lay$edges$to_i])
  e <- nrow(lay$edges)
  ph <- cinema_study_phrase[[what]]
  paste0("Judged by ", tolower(cinema_study_title[[what]]), ", ",
         if (any(worst)) paste0(sum(worst), " of ", e, " direct comparisons ", if (sum(worst) == 1) "rests" else "rest",
                                " only on studies ", ph[3], " (", cinema_names(names[worst], 3), "), ") else
           paste0("no direct comparison rests only on studies ", ph[3], ", "),
         sum(mixed), " ", if (sum(mixed) == 1) "mixes" else "mix", " studies with different judgments, which one averaged color would hide, and ",
         sum(clean), " ", if (sum(clean) == 1) "rests" else "rest", " only on studies ", ph[1], ".")
}

cinet_layout <- function(study, treatment, size, levels, studies, judged, color, positions, title, caption, family) {
  dims <- utils::modifyList(graph_dims, list(label_pt = 10, small_pt = 8.5, node_r = 12, node_r_min = 8, node_r_max = 24,
                                             ring_min = 150, ring_gap = 70, gap = 5.5, strand = 4))
  node_of <- match(treatment, levels)
  k <- length(levels)
  node_n <- if (is.null(size)) NULL else vapply(seq_len(k), function(i) sum(size[node_of == i]), numeric(1))
  edges <- network_comparisons(study, node_of, size)
  radius <- if (is.null(node_n) || max(node_n) <= 0) rep(dims$node_r, k) else
    pmax(dims$node_r_min, dims$node_r_max * sqrt(node_n / max(node_n)))
  ring <- max(dims$ring_min, k * (2 * max(radius) + dims$ring_gap) / (2 * pi))
  if (is.null(positions)) {
    angle <- -pi / 2 + 2 * pi * (seq_len(k) - 1) / k
    cx <- ring * cos(angle)
    cy <- ring * sin(angle)
  } else {
    placed <- network_positions(positions, levels, ring)
    cx <- placed$x
    cy <- placed$y
  }
  ux <- cx - mean(cx)
  uy <- cy - mean(cy)
  away <- sqrt(ux^2 + uy^2)
  # A node near the middle has no side to point away from; its label goes
  # below it.
  central <- away < 0.3 * max(away)
  away[away < 1e-6] <- 1
  ux <- ifelse(central, 0, ux / away)
  uy <- ifelse(central, 1, uy / away)
  sub <- if (is.null(node_n)) rep("", k) else paste(format(node_n, big.mark = ",", trim = TRUE), "randomized")
  label_w <- pmax(text_width_card(levels, dims$label_pt, family, 1, bold = TRUE),
                  if (is.null(node_n)) 0 else text_width_card(sub, dims$small_pt, family, 1))
  hjust <- ifelse(abs(ux) < 0.35, 0.5, (1 - sign(ux)) / 2)
  lx <- cx + ux * (radius + 7)
  ly <- cy + uy * (radius + 7) + ifelse(uy > 0.35, 8, ifelse(uy < -0.35, -14, -3))
  lh <- 30
  xs <- c(cx - radius, cx + radius, lx - hjust * label_w, lx + (1 - hjust) * label_w)
  ys <- c(cy - radius, cy + radius, ly - 10, ly + lh)
  words <- cinema_study_short[[color]]
  key_w <- sum(text_width_card(c(paste0(cinema_study_title[[color]], " of each study:"), words, "Each strand is one study"),
                               dims$small_pt, family, 1)) + 90
  canvas_x <- c(xs, min(xs) + key_w)
  lay <- list(dims = dims, levels = levels, studies = studies, judged = judged, color = color, study = study,
              treatment = treatment, size = size, node_n = node_n, edges = edges, radius = radius,
              cx = cx, cy = cy, ux = ux, uy = uy, lx = lx, ly = ly, hjust = hjust, sub = sub, family = family,
              key_top = min(ys) - 40, left = min(xs), label_w = label_w)
  # The sentence about the judgments sits under the network, where the
  # widget hides it and writes it again for the judgment switched to.
  lay$sentence <- cinema_wrap(cinet_sentence(lay, color), max(diff(range(canvas_x)), 420), dims$small_pt, family)
  lay$sentence_top <- max(ys) + 16
  ys <- c(ys, lay$sentence_top + length(lay$sentence) * dims$small_pt * 1.35)
  cap <- c(caption,
           paste0("Each strand is one study, so a line's width counts its studies and no color is averaged",
                  if (!is.null(node_n)) "; node area follows the participants randomized to each treatment" else "",
                  ". Study judgments are as given; the colors change with the switch in the widget."))
  cap <- cinema_wrap(cap, max(diff(range(canvas_x)), 420), graph_dims$caption_pt, family)
  lay$page <- graph_canvas(canvas_x, c(ys, lay$key_top), title, cap, character(0), character(0), NULL, dims, family)
  lay
}

# Distance from points to the segment from (ax, ay) to (bx, by).
cinema_seg_dist <- function(px, py, ax, ay, bx, by) {
  l2 <- (bx - ax)^2 + (by - ay)^2
  t <- if (l2 == 0) 0 else pmax(0, pmin(1, ((px - ax) * (bx - ax) + (py - ay) * (by - ay)) / l2))
  sqrt((px - ax - t * (bx - ax))^2 + (py - ay - t * (by - ay))^2)
}

cinet_levels <- function(lay, what) {
  sj <- lay$judged[[what]]
  if (is.null(sj)) return(rep(NA_integer_, length(lay$studies)))
  sj$level[match(lay$studies, sj$study)]
}

# The study table and composition bars that a panel shows.
cinet_panel <- function(lay, title, sub, rows, cols) {
  bars <- vapply(c("rob", "indirectness"), function(what) {
    if (is.null(lay$judged[[what]])) return("")
    lv <- cinet_levels(lay, what)[match(rows$study, lay$studies)]
    counts <- table(factor(lv, levels = 0:2))
    words <- paste(counts[counts > 0], tolower(cinema_study_short[[what]][counts > 0]), collapse = ", ")
    o <- order(lv)
    paste0('<div class="ggx-refs-head">', esc(cinema_study_title[[what]]), ": ", esc(words), "</div>",
           '<div class="ggx-cn-bar" role="img" aria-label="', esc(words), '">',
           paste0('<span class="ggx-cn-seg', ifelse(lv[o] == 2, " ggx-cn-hatch", ""), '" style="flex:1;background:',
                  cinema_ink$level[lv[o] + 1], '" title="', esc(paste0(rows$study[o], ": ", cinema_study_short[[what]][lv[o] + 1])),
                  '">', esc(rows$study[o]), "</span>", collapse = ""), "</div>")
  }, character(1))
  judg <- function(what, s) {
    sj <- lay$judged[[what]]
    if (is.null(sj)) return(NULL)
    i <- match(s, sj$study)
    cinema_chip(sj$level[i], cinema_study_short[[what]][sj$level[i] + 1])
  }
  reason <- function(s) {
    parts <- vapply(c("rob", "indirectness"), function(what) {
      sj <- lay$judged[[what]]
      if (is.null(sj)) return("")
      r <- sj$reason[match(s, sj$study)]
      if (nzchar(r)) paste0("<b>", esc(cinema_study_title[[what]]), ":</b> ", esc(r)) else ""
    }, character(1))
    paste(parts[nzchar(parts)], collapse = "<br>")
  }
  heads <- c("Study", names(cols), if (!is.null(lay$judged$rob)) "Risk of bias",
             if (!is.null(lay$judged$indirectness)) "Indirectness", "Reasons")
  body <- vapply(seq_len(nrow(rows)), function(r) {
    s <- rows$study[r]
    paste0('<tr><th scope="row">', esc(s), "</th>",
           paste0("<td>", vapply(cols, function(f) esc(f(s)), ""), "</td>", collapse = ""),
           if (!is.null(lay$judged$rob)) paste0("<td>", judg("rob", s), "</td>"),
           if (!is.null(lay$judged$indirectness)) paste0("<td>", judg("indirectness", s), "</td>"),
           "<td>", reason(s), "</td></tr>")
  }, character(1))
  paste0('<div class="ggx-title">', esc(title), '</div><div class="ggx-sub">', esc(sub), "</div>",
         paste(bars, collapse = ""),
         '<div class="ggx-table ggx-cn-studies"><table><thead><tr>',
         paste0('<th scope="col">', esc(heads), "</th>", collapse = ""), "</tr></thead><tbody>",
         paste(body, collapse = ""), "</tbody></table></div>")
}

cinet_draw <- function(lay) {
  dims <- lay$dims
  px <- lay$page$px
  py <- lay$page$py
  fam <- lay$family
  e <- lay$edges
  what <- lay$color
  lv <- cinet_levels(lay, what)
  texts <- function(label, x, y, pt, color, hjust = 0, face = "plain") {
    if (!length(label)) return(empty_texts())
    data.frame(x = px(x), y = py(y), label = as.character(label), size = pt / .pt, colour = color,
               hjust = hjust, fontface = face, stringsAsFactors = FALSE)
  }
  count <- function(m, one, many) paste(format(m, big.mark = ",", trim = TRUE), ifelse(m == 1, one, many))
  study_tip <- function(s, sub) {
    rows <- c(if (!is.null(lay$judged$rob)) c("Risk of bias" = cinema_study_short$rob[cinet_levels(lay, "rob")[match(s, lay$studies)] + 1]),
              if (!is.null(lay$judged$indirectness)) c("Indirectness" = cinema_study_short$indirectness[cinet_levels(lay, "indirectness")[match(s, lay$studies)] + 1]))
    paste0('<div class="ggx-tip-title">', esc(s), '</div><div class="ggx-tip-sub">', esc(sub), "</div>", tip_rows(rows),
           '<div class="ggx-tip-hint">Click for every study on this line.</div>')
  }
  arm_n <- function(s, t) if (is.null(lay$size)) NA else lay$size[lay$study == s & lay$treatment == t]

  strands <- NULL
  hits <- NULL
  edge_labels <- NULL
  for (k in seq_len(nrow(e))) {
    a <- e$from_i[k]
    b <- e$to_i[k]
    ta <- lay$levels[a]
    tb <- lay$levels[b]
    s <- e$study_list[[k]]
    m <- length(s)
    gap <- min(dims$gap, 36 / m)
    x1 <- lay$cx[a]; y1 <- lay$cy[a]; x2 <- lay$cx[b]; y2 <- lay$cy[b]
    len <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
    nx <- -(y2 - y1) / len
    ny <- (x2 - x1) / len
    off <- (seq_len(m) - (m + 1) / 2) * gap
    title <- paste(ta, "vs", tb)
    sub <- paste0(count(m, "study", "studies"),
                  if (!is.null(lay$size)) paste0(", ", format(sum(vapply(s, function(z) arm_n(z, ta) + arm_n(z, tb), 0)),
                                                              big.mark = ","), " randomized to these two arms") else "")
    panel <- cinet_panel(lay, title, sub, data.frame(study = s, stringsAsFactors = FALSE),
                         c(if (!is.null(lay$size)) list("Randomized, these arms" = function(z) format(arm_n(z, ta) + arm_n(z, tb), big.mark = ",")),
                           list(Arms = function(z) paste(lay$treatment[lay$study == z], collapse = ", "))))
    click <- pin_js(paste0("e", k), panel)
    sj <- match(s, lay$studies)
    strands <- rbind(strands, data.frame(
      x = px(x1 + nx * off), xend = px(x2 + nx * off), y = py(y1 + ny * off), yend = py(y2 + ny * off),
      colour = cinema_ink$level[lv[sj] + 1], id = paste0("s", k, "_", sj),
      tooltip = vapply(s, study_tip, "", sub = title), onclick = click, width = gap * 0.72, stringsAsFactors = FALSE))
    tip <- paste0('<div class="ggx-tip-title">', esc(title), '</div><div class="ggx-tip-sub">', esc(sub), "</div>",
                  '<div class="ggx-tip-body">', esc(cinema_names(s, 8)), "</div>",
                  '<div class="ggx-tip-hint">Click or press Enter for the studies and their judgments.</div>')
    hits <- rbind(hits, data.frame(x = px(x1), xend = px(x2), y = py(y1), yend = py(y2), id = paste0("e", k),
                                   tooltip = tip, onclick = click, width = m * gap + 10, stringsAsFactors = FALSE))
    # The count sits beside the line where it is farthest from the other
    # lines and the nodes.
    word <- count(m, "study", "studies")
    ww <- text_width_card(word, dims$small_pt, fam, 1) / 2
    d <- m * gap / 2 + 8 + abs(nx) * ww
    cand <- expand.grid(f = c(0.5, 0.38, 0.62, 0.3, 0.7), side = c(1, -1))
    cand$x <- x1 + cand$f * (x2 - x1) + cand$side * nx * d
    cand$y <- y1 + cand$f * (y2 - y1) + cand$side * ny * d
    score <- vapply(seq_len(nrow(cand)), function(r) {
      bx <- cand$x[r] + c(0, -ww, ww, -ww, ww)
      by <- cand$y[r] + c(0, -5, -5, 5, 5)
      others <- setdiff(seq_len(nrow(e)), k)
      dl <- vapply(others, function(o) {
        min(cinema_seg_dist(bx, by, lay$cx[e$from_i[o]], lay$cy[e$from_i[o]], lay$cx[e$to_i[o]], lay$cy[e$to_i[o]])) -
          e$studies[o] * dims$gap / 2
      }, numeric(1))
      dn <- vapply(seq_along(lay$cx), function(i) min(sqrt((bx - lay$cx[i])^2 + (by - lay$cy[i])^2)) - lay$radius[i], numeric(1))
      # A count must never sit on a treatment's label.
      hit_label <- any(vapply(seq_along(lay$cx), function(i) {
        l0 <- lay$lx[i] - lay$hjust[i] * lay$label_w[i] - 3
        l1 <- l0 + lay$label_w[i] + 6
        cand$x[r] + ww > l0 && cand$x[r] - ww < l1 && cand$y[r] + 6 > lay$ly[i] - 9 &&
          cand$y[r] - 6 < lay$ly[i] + if (is.null(lay$node_n)) 5 else 18
      }, logical(1)))
      min(c(dl, dn, Inf)) - 0.02 * abs(cand$f[r] - 0.5) * 100 - if (hit_label) 1000 else 0
    }, numeric(1))
    best <- which.max(score)
    edge_labels <- rbind(edge_labels, data.frame(texts(word, cand$x[best], cand$y[best] + 3, dims$small_pt, graph_ink$muted,
                                                       hjust = 0.5), id = paste0("el", k)))
  }

  # Nodes, with a panel of every study of the treatment.
  node_tip <- character(length(lay$levels))
  node_click <- character(length(lay$levels))
  for (i in seq_along(lay$levels)) {
    t <- lay$levels[i]
    s <- unique(lay$study[lay$treatment == t])
    sub <- paste0(count(length(s), "study", "studies"), if (!is.null(lay$node_n)) paste0(", ", lay$sub[i]) else "")
    node_tip[i] <- paste0('<div class="ggx-tip-title">', esc(t), '</div><div class="ggx-tip-sub">', esc(sub), "</div>",
                          '<div class="ggx-tip-body">', esc(cinema_names(s, 8)), "</div>",
                          '<div class="ggx-tip-hint">Click or press Enter for its studies and their judgments.</div>')
    panel <- cinet_panel(lay, t, sub, data.frame(study = s, stringsAsFactors = FALSE),
                         c(if (!is.null(lay$size)) list("Randomized to this arm" = function(z) format(arm_n(z, t), big.mark = ",")),
                           list("Other arms" = function(z) paste(setdiff(lay$treatment[lay$study == z], t), collapse = ", "))))
    node_click[i] <- pin_js(paste0("n", i), panel)
  }
  a <- seq(0, 2 * pi, length.out = 49)
  nodes <- do.call(rbind, lapply(seq_along(lay$levels), function(i) {
    data.frame(x = px(lay$cx[i] + lay$radius[i] * cos(a)), y = py(lay$cy[i] + lay$radius[i] * sin(a)),
               id = paste0("n", i), tooltip = node_tip[i], onclick = node_click[i], stringsAsFactors = FALSE)
  }))
  node_labels <- rbind(
    data.frame(texts(lay$levels, lay$lx, lay$ly, dims$label_pt, graph_ink$title, hjust = lay$hjust, face = "bold"),
               id = paste0("n", seq_along(lay$levels)), tooltip = node_tip, onclick = node_click),
    if (!is.null(lay$node_n)) data.frame(texts(lay$sub, lay$lx, lay$ly + 13, dims$small_pt, graph_ink$muted, hjust = lay$hjust),
                                         id = paste0("n", seq_along(lay$levels)), tooltip = node_tip, onclick = node_click)
  )

  # The key, whose words the widget changes with the switch.
  kx0 <- lay$left
  words <- cinema_study_short[[what]]
  head <- paste0(cinema_study_title[[what]], " of each study:")
  hw <- text_width_card(c(head, words), dims$small_pt, fam, 1, bold = FALSE)
  kx <- kx0 + hw[1] + 10 + c(0, cumsum(hw[2:3] + 26))
  ky <- lay$key_top + 6
  key_rects <- data.frame(xmin = px(kx), xmax = px(kx + 10), ymin = py(ky + 5), ymax = py(ky - 5),
                          fill = cinema_ink$level, id = paste0("kq", 1:3))
  live <- rbind(
    data.frame(texts(head, kx0, ky, dims$small_pt, graph_ink$title, face = "bold"), id = "kt"),
    data.frame(texts(words, kx + 14, ky, dims$small_pt, graph_ink$text), id = paste0("kl", 1:3))
  )
  other <- texts("Each strand is one study", kx[3] + 14 + hw[4] + 24, ky, dims$small_pt, graph_ink$muted)
  live <- rbind(live, data.frame(texts(lay$sentence, lay$left, lay$sentence_top + (seq_along(lay$sentence) - 1) *
                                         dims$small_pt * 1.35, dims$small_pt, graph_ink$text), id = "cs"))

  ggplot() +
    ggiraph::geom_segment_interactive(data = hits, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend,
                                                       data_id = .data$id, tooltip = .data$tooltip, onclick = .data$onclick,
                                                       linewidth = .data$width / .pt),
                                      colour = "#FFFFFF02", lineend = "butt") +
    ggiraph::geom_segment_interactive(data = strands, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend,
                                                          colour = .data$colour, data_id = .data$id, tooltip = .data$tooltip,
                                                          onclick = .data$onclick, linewidth = .data$width / .pt),
                                      lineend = "butt") +
    ggiraph::geom_polygon_interactive(data = nodes, aes(x = .data$x, y = .data$y, group = .data$id, data_id = .data$id,
                                                        tooltip = .data$tooltip, onclick = .data$onclick),
                                      fill = graph_ink$page, colour = cinema_ink$first, linewidth = 2 / .pt) +
    ggiraph::geom_text_interactive(data = node_labels, aes(x = .data$x, y = .data$y, label = .data$label, data_id = .data$id,
                                                           tooltip = .data$tooltip, onclick = .data$onclick),
                                   hjust = node_labels$hjust, size = node_labels$size, colour = node_labels$colour,
                                   fontface = node_labels$fontface, family = fam) +
    ggiraph::geom_rect_interactive(data = key_rects, aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin,
                                                         ymax = .data$ymax, fill = .data$fill, data_id = .data$id), colour = NA) +
    ggiraph::geom_text_interactive(data = live, aes(x = .data$x, y = .data$y, label = .data$label, data_id = .data$id),
                                   hjust = live$hjust, size = live$size, colour = live$colour, fontface = live$fontface,
                                   family = fam) +
    ggiraph::geom_text_interactive(data = edge_labels, aes(x = .data$x, y = .data$y, label = .data$label, data_id = .data$id),
                                   hjust = 0.5, size = dims$small_pt / .pt, colour = graph_ink$muted, family = fam) +
    draw_text(other, fam) +
    scale_linewidth_identity() +
    graph_frame(lay$page, fam)
}

cinet_render <- function(lay) {
  e <- lay$edges
  list(
    start = lay$color,
    has = list(rob = !is.null(lay$judged$rob), indirectness = !is.null(lay$judged$indirectness)),
    rob = I(cinet_levels(lay, "rob")), indirectness = I(cinet_levels(lay, "indirectness")),
    edges = lapply(seq_len(nrow(e)), function(k) list(label = paste(lay$levels[e$from_i[k]], "vs", lay$levels[e$to_i[k]]),
                                                     studies = I(match(e$study_list[[k]], lay$studies) - 1L))),
    title = as.list(cinema_study_title), short = cinema_study_short, phrase = cinema_study_phrase,
    levels = cinema_ink$level
  )
}

cinet_edges_table <- function(lay) {
  e <- lay$edges
  out <- data.frame(treat1 = lay$levels[e$from_i], treat2 = lay$levels[e$to_i], studies = e$studies,
                    stringsAsFactors = FALSE)
  out$study_list <- I(lapply(e$study_list, as.character))
  for (what in c("rob", "indirectness")) {
    if (is.null(lay$judged[[what]])) next
    lv <- cinet_levels(lay, what)
    for (k in 0:2) {
      out[[paste0(what, "_", c("low", "moderate", "high")[k + 1])]] <-
        vapply(e$study_list, function(s) sum(lv[match(s, lay$studies)] == k), integer(1))
    }
  }
  out
}
