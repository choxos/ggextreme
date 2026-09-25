#' Draw an interactive league table for a network meta-analysis
#'
#' Draws every pairwise estimate of a network meta-analysis as a grid, with
#' the treatments on the diagonal and a ranking beside it. Following
#' [netmeta::netleague()], each cell compares the treatment that comes first
#' in the table with the one that comes second: network estimates sit below
#' the diagonal and direct estimates, from the trials that compare the pair
#' head to head, above it.
#'
#' Cells are shaded by the size of the effect, in one color when it favors
#' the first treatment and another when it favors the second, and faded
#' when the confidence interval includes the null. Hovering over a cell
#' shows its network, direct and indirect estimates and the share of the
#' network estimate that comes from direct trials. Clicking a cell opens the
#' direct trials under the table: their arm level data when `data` is given,
#' and otherwise each trial's own estimate. Hovering over a treatment on the
#' diagonal or in the ranking lights its row and column.
#'
#' @param x A network meta-analysis from [netmeta::netmeta()].
#' @param data Optional arm level data, one row per study arm, for the click
#'   panels, such as the data given to [ggnma()]. Every column other than
#'   `study` and `treatment` becomes a row of the arm table.
#' @param study,treatment Bare column names in `data` identifying the study
#'   and treatment of each arm. Treatment names must match those in `x`.
#' @param pooled Which model to show, `"random"` or `"common"`. Defaults to
#'   the random effects model when `x` has one.
#' @param small_values Whether small values of the effect are
#'   `"desirable"`, as for mortality, or `"undesirable"`, as for a response.
#'   Sets which treatment a cell favors and the direction of the ranking.
#'   Defaults to the setting stored in `x`, which is worth checking.
#' @param order Order of the treatments along the diagonal. Defaults to the
#'   ranking, best first.
#' @param ranking Draw the P-score ranking beside the table.
#' @param contributions Show where each network estimate comes from: `TRUE`
#'   to compute the share of it that flows through each direct comparison
#'   with [netmeta::netcontrib()], or an object that function returned, which
#'   saves recomputing it for a large network. Hovering or tapping a network
#'   estimate then marks the direct comparisons it draws on with their
#'   shares, and its panel lists them.
#' @param title,caption Title above the table and note below it. The default
#'   caption explains which estimates sit on each side of the diagonal.
#' @param family Font family. The package ships Lato and registers it on load.
#'
#' @return An object of class `ggleague`, which prints as an interactive
#'   widget. Use [graph_widget()], [graph_plot()] or [graph_save()] for the
#'   widget, a static ggplot or a file. The fields `treatments` and `pscore`
#'   give the order used and the P-scores.
#' @export
#'
#' @examples
#' if (requireNamespace("netmeta", quietly = TRUE) &&
#'     requireNamespace("meta", quietly = TRUE)) {
#'   pw <- meta::pairwise(treat = treatment, event = pasi75_r,
#'                        n = pasi75_n, studlab = study,
#'                        data = psoriasis_nma, sm = "OR")
#'   nma <- netmeta::netmeta(pw, common = FALSE)
#'   ggleague(nma, psoriasis_nma, study, treatment,
#'            small_values = "undesirable")
#' }
ggleague <- function(x, data = NULL, study = NULL, treatment = NULL,
                     pooled = NULL, small_values = NULL, order = NULL,
                     ranking = TRUE, contributions = NULL, title = NULL,
                     caption = NULL, family = "Lato") {
  if (!inherits(x, "netmeta")) {
    rlang::abort("`x` must be a network meta-analysis from netmeta::netmeta().")
  }
  rlang::check_installed("netmeta")
  if (is.null(pooled)) pooled <- if (isTRUE(x$random)) "random" else "common"
  pooled <- match.arg(pooled, c("random", "common"))
  if (is.null(small_values)) small_values <- x$small.values
  if (is.null(small_values)) small_values <- "desirable"
  small_values <- match.arg(small_values, c("desirable", "undesirable"))
  dims <- league_dims
  get <- function(stem) x[[paste0(stem, ".", pooled)]]
  flow <- NULL
  if (!is.null(contributions) && !isFALSE(contributions)) {
    source <- if (isTRUE(contributions)) x else contributions
    flow <- nma_contributions(source, pooled)
    if (!setequal(attr(flow, "trts"), x$trts)) {
      rlang::abort("`contributions` must come from the same network as `x`.")
    }
  }

  trts <- x$trts
  pscore <- netmeta::netrank(x, small.values = small_values)[[paste0("ranking.", pooled)]]
  pscore <- pscore[trts]
  if (is.null(order)) {
    order <- trts[base::order(-pscore)]
  } else {
    order <- as.character(order)
    if (!setequal(order, trts) || anyDuplicated(order)) {
      rlang::abort(paste0("`order` must name every treatment once: ",
                          paste(trts, collapse = ", ")))
    }
  }
  k <- length(order)
  ratio <- x$sm %in% ratio_measures
  shown <- if (ratio) exp else identity
  measure <- if (x$sm %in% names(measure_names)) measure_names[[x$sm]] else "Effect"
  fmt <- function(v) {
    v <- shown(v)
    if (!is.finite(v)) return("")
    formatC(v, format = "f", digits = if (!ratio) 2 else if (v >= 10) 1 else if (v < 0.1) 3 else 2)
  }
  interval <- function(e, l, h) paste0(fmt(e), " (", fmt(l), ", ", fmt(h), ")")

  te <- get("TE")
  lower <- get("lower")
  upper <- get("upper")
  te_d <- get("TE.direct")
  lower_d <- get("lower.direct")
  upper_d <- get("upper.direct")
  te_i <- get("TE.indirect")
  share <- x[[paste0("P.", pooled)]]
  n_direct <- x$A.matrix
  z <- stats::qnorm(1 - (1 - x$level) / 2)
  pw <- data.frame(study = x$studlab, t1 = x$treat1, t2 = x$treat2, te = x$TE,
                   se = x$seTE, stringsAsFactors = FALSE)
  favors_first <- function(e) if (small_values == "desirable") e < 0 else e > 0
  top <- max(abs(te[is.finite(te)]))
  magnitude <- function(e) {
    if (ratio) min(1, abs(e) / log(40)) else if (top > 0) min(1, abs(e) / top) else 0
  }
  shade <- function(e, l, h) {
    sig <- l > 0 || h < 0
    amount <- if (sig) 0.2 + 0.5 * magnitude(e) else 0.08 + 0.12 * magnitude(e)
    list(color = if (favors_first(e)) league_ink$first else league_ink$second,
         alpha = amount)
  }

  # Arm level data for the click panels, when given.
  arms <- NULL
  if (!is.null(data)) {
    data <- as.data.frame(data, stringsAsFactors = FALSE)
    q_study <- rlang::enquo(study)
    q_treat <- rlang::enquo(treatment)
    if (rlang::quo_is_null(q_study) || rlang::quo_is_null(q_treat)) {
      rlang::abort("Give `study` and `treatment` with `data`.")
    }
    arm_study <- as.character(rlang::eval_tidy(q_study, data))
    arm_treat <- as.character(rlang::eval_tidy(q_treat, data))
    placing <- vapply(list(q_study, q_treat), function(q) {
      if (rlang::quo_is_symbol(q)) rlang::as_name(q) else NA_character_
    }, character(1))
    cols <- setdiff(names(data), placing)
    cols <- cols[!vapply(data[cols], is.list, logical(1))]
    arms <- list(data = data, study = arm_study, treatment = arm_treat,
                 cols = cols, labels = column_labels(data, cols),
                 notes = study_level(data, arm_study, cols))
  }

  # Cell size from the widest estimate and the treatment names.
  width <- function(s, pt, bold = FALSE) max(text_width_card(s, pt, family, 1, bold = bold))
  sample_text <- c(vapply(seq_len(k), function(i) interval(te[order[1], order[i]],
                                                             lower[order[1], order[i]],
                                                             upper[order[1], order[i]]),
                          character(1)), "no direct trials")
  cell_w <- max(dims$cell_min, width(sample_text, dims$small_pt) + 2 * dims$cell_pad,
                max(vapply(strsplit(order, " ", fixed = TRUE), function(words) {
                  width(words, dims$name_pt, TRUE)
                }, numeric(1))) + 2 * dims$cell_pad)
  names_wrapped <- lapply(order, wrap_words, width = cell_w - 2 * dims$cell_pad,
                          pt = dims$name_pt, family = family, bold = TRUE)
  lines_max <- max(lengths(names_wrapped))
  cell_h <- max(dims$cell_h, lines_max * dims$name_pt * 1.2 + dims$small_pt * 1.4 + 10)
  pitch_x <- cell_w + dims$cell_gap
  pitch_y <- cell_h + dims$cell_gap
  cell_x <- function(b) (b - 1) * pitch_x
  cell_y <- function(a) (a - 1) * pitch_y
  grid_w <- k * pitch_x - dims$cell_gap
  grid_h <- k * pitch_y - dims$cell_gap

  rank_x <- grid_w + dims$rank_gap
  rank_w <- if (ranking) max(dims$rank_w, width(order, dims$small_pt) + 40) else 0
  right <- if (ranking) rank_x + rank_w else grid_w

  if (is.null(caption)) {
    caption <- paste("Below the diagonal: network estimates; above it: direct",
                     "estimates. Each compares the first treatment with the second.")
  }
  page <- graph_canvas(c(0, right), c(0, grid_h), title, caption,
                       c("Favors the first treatment", "Favors the second"),
                       c(league_ink$first, league_ink$second), measure, dims, family)
  px <- page$px
  py <- page$py
  texts <- function(label, x, y, pt, color, hjust = 0.5, face = "plain") {
    if (!length(label)) return(empty_texts())
    data.frame(x = px(x), y = py(y), label = label, size = pt / .pt,
               colour = color, hjust = hjust, fontface = face,
               stringsAsFactors = FALSE)
  }

  studies_with <- function(t) unique(pw$study[pw$t1 == t | pw$t2 == t])
  direct_studies <- function(a, b) {
    unique(pw$study[(pw$t1 == a & pw$t2 == b) | (pw$t1 == b & pw$t2 == a)])
  }
  pair_tip <- function(a, b) {
    st <- direct_studies(a, b)
    net <- interval(te[a, b], lower[a, b], upper[a, b])
    direct <- if (n_direct[a, b] > 0) interval(te_d[a, b], lower_d[a, b], upper_d[a, b]) else "none"
    indirect <- if (is.finite(te_i[a, b])) fmt(te_i[a, b]) else "none"
    p <- if (is.finite(share[a, b])) share[a, b] else 0
    sig <- lower[a, b] > 0 || upper[a, b] < 0
    verdict <- if (!sig) "The interval includes no difference." else
      paste("Favors", if (favors_first(te[a, b])) a else b)
    paste0(
      '<div class="ggx-tip-title">', esc(a), " vs ", esc(b), "</div>",
      '<div class="ggx-tip-sub">', esc(measure), ", ", pooled, " effects</div>",
      tip_rows(c("Network estimate" = net,
                 stats::setNames(direct, paste0("Direct, ", n_direct[a, b],
                                                if (n_direct[a, b] == 1) " trial" else " trials")),
                 "Indirect" = indirect)),
      '<div class="ggx-share"><i style="width:', round(100 * p), '%"></i></div>',
      '<div class="ggx-tip-sub">', round(100 * p), "% of the network estimate comes from direct trials",
      if (length(st)) paste0(": ", esc(paste(st, collapse = ", "))), "</div>",
      '<div class="ggx-tip-body">', esc(verdict), "</div>",
      if (!is.null(flow)) flow_tip(a, b),
      if (length(st) || !is.null(flow)) paste0('<div class="ggx-tip-hint">Click for ',
        if (length(st)) "the direct trials" else "", if (length(st) && !is.null(flow)) " and " else "",
        if (!is.null(flow)) "where the estimate comes from" else "", ".</div>")
    )
  }
  flow_of <- function(a, b) flow[flow$net_key == pair_key(a, b), , drop = FALSE]
  # Direct comparisons are named in table order, like the cells.
  flow_name <- function(f) {
    swap <- match(f$dir_a, order) > match(f$dir_b, order)
    paste(ifelse(swap, f$dir_b, f$dir_a), "vs", ifelse(swap, f$dir_a, f$dir_b))
  }
  pct <- function(v) paste0(formatC(100 * v, format = "f", digits = if (v < 0.01) 1 else 0), "%")
  flow_tip <- function(a, b) {
    f <- utils::head(flow_of(a, b), 3)
    paste0('<div class="ggx-tip-sub">Draws most on: ',
           esc(paste0(flow_name(f), " (", vapply(f$share, pct, ""), ")", collapse = ", ")), "</div>")
  }
  flow_panel <- function(a, b) {
    f <- flow_of(a, b)
    own <- f$dir_key == pair_key(a, b)
    lead <- if (any(own)) {
      paste0(pct(f$share[own]), " of the network estimate flows through the direct ", esc(a), " vs ",
             esc(b), " trials, and ", pct(1 - f$share[own]), " through other comparisons.")
    } else {
      "All of it flows through other comparisons:" 
    }
    rows <- vapply(seq_len(nrow(f)), function(i) {
      n <- length(direct_studies(f$dir_a[i], f$dir_b[i]))
      paste0("<tr", if (own[i]) ' class="ggx-own"', "><th scope=\"row\">", esc(flow_name(f[i, ])),
             "</th><td>", n, "</td><td><span class=\"ggx-flow-bar\"><i style=\"width:",
             round(100 * f$share[i], 1), "%\"></i></span> ", pct(f$share[i]), "</td></tr>")
    }, character(1))
    paste0('<div class="ggx-title ggx-flow-head">Where the network estimate comes from</div><p>', lead, "</p>",
           '<div class="ggx-table"><table><thead><tr><td></td><th scope="col">Trials</th>',
           '<th scope="col">Share of the estimate</th></tr></thead><tbody class="ggx-num">',
           paste(rows, collapse = ""), "</tbody></table></div>",
           '<p class="ggx-note">', esc(contribution_method(flow)), "</p>")
  }
  pair_panel <- function(a, b) {
    out <- pair_trials(a, b)
    if (is.null(flow)) out else paste0(out, flow_panel(a, b))
  }
  pair_trials <- function(a, b) {
    st <- direct_studies(a, b)
    head <- paste0('<div class="ggx-title">', esc(a), " vs ", esc(b), "</div>")
    if (!length(st)) {
      return(paste0(head, "<p>No trial compares these two directly. The network estimate of ",
                    esc(interval(te[a, b], lower[a, b], upper[a, b])),
                    " comes from their comparisons with common comparators.</p>"))
    }
    sub <- paste0('<div class="ggx-sub">', length(st),
                  if (length(st) == 1) " trial compares" else " trials compare",
                  " these directly</div>")
    if (!is.null(arms)) {
      rows <- which(arms$study %in% st & arms$treatment %in% c(a, b))
      rows <- rows[base::order(match(arms$study[rows], st),
                               match(arms$treatment[rows], c(a, b)))]
      return(paste0(head, sub, arm_table(arms$data[rows, , drop = FALSE],
                                         arms$study[rows], arms$cols, arms$labels,
                                         treatment = arms$treatment[rows],
                                         notes = arms$notes)))
    }
    rows <- vapply(st, function(s) {
      hit <- which(pw$study == s & ((pw$t1 == a & pw$t2 == b) | (pw$t1 == b & pw$t2 == a)))[1]
      sign <- if (pw$t1[hit] == a) 1 else -1
      e <- sign * pw$te[hit]
      paste0("<tr><th scope=\"row\">", esc(s), "</th><td>",
             esc(interval(e, e - z * pw$se[hit], e + z * pw$se[hit])), "</td></tr>")
    }, character(1))
    paste0(head, sub, '<div class="ggx-table"><table><thead><tr><td></td><th scope="col">',
           esc(measure), " (95% CI)</th></tr></thead><tbody class=\"ggx-num\">",
           paste(rows, collapse = ""), "</tbody></table></div>")
  }
  treatment_tip <- function(p) {
    t <- order[p]
    rank_of <- match(t, trts[base::order(-pscore)])
    paste0('<div class="ggx-tip-title">', esc(t), "</div>",
           '<div class="ggx-tip-sub">Ranked ', rank_of, " of ", k, "</div>",
           tip_rows(c("P-score" = formatC(pscore[[t]], format = "f", digits = 2),
                      "Studies" = length(studies_with(t)))),
           '<div class="ggx-tip-body">The P-score is the mean certainty that this ',
           "treatment is better than each of the others. Its row and column are lit.</div>")
  }

  # Cells.
  treatment_hover <- paste0("stroke:", hover_ink, ";stroke-width:1.5px;")
  shapes <- NULL
  labels <- empty_texts()
  cell_text <- NULL
  for (a in seq_len(k)) {
    for (b in seq_len(k)) {
      x0 <- cell_x(b)
      y0 <- cell_y(a)
      box <- rounded_rect(x0, x0 + cell_w, y0, y0 + cell_h, dims$radius, n = 5)
      cx <- x0 + cell_w / 2
      cy <- y0 + cell_h / 2
      if (a == b) {
        id <- paste0("t", a)
        tip <- treatment_tip(a)
        shapes <- rbind(shapes, data.frame(
          x = px(box$x), y = py(box$y), group = paste0("g", a, "_", b), id = id,
          tooltip = tip, onclick = "", fill = league_ink$diagonal, alpha = 1,
          border = league_ink$diagonal, linetype = "solid",
          hover = treatment_hover,
          stringsAsFactors = FALSE))
        lines <- names_wrapped[[a]]
        block <- length(lines) * dims$name_pt * 1.2 + dims$small_pt * 1.4
        first_y <- cy - block / 2 + dims$name_pt * 0.6
        ys <- first_y + (seq_along(lines) - 1) * dims$name_pt * 1.2
        cell_text <- rbind(cell_text, data.frame(
          x = px(cx), y = py(c(ys, max(ys) + dims$name_pt * 0.6 + dims$small_pt * 0.9)),
          label = c(lines, paste("P-score", formatC(pscore[[order[a]]], format = "f", digits = 2))),
          size = c(rep(dims$name_pt, length(lines)), dims$small_pt) / .pt,
          colour = c(rep(graph_ink$title, length(lines)), graph_ink$muted),
          fontface = c(rep("bold", length(lines)), "plain"),
          id = id, tooltip = tip, onclick = "", stringsAsFactors = FALSE))
        next
      }
      first <- order[min(a, b)]
      second <- order[max(a, b)]
      id <- paste0("c", a, "_", b)
      tip <- pair_tip(first, second)
      click <- pin_js(id, pair_panel(first, second))
      network <- a > b
      if (network) {
        e <- te[first, second]
        l <- lower[first, second]
        h <- upper[first, second]
      } else {
        e <- te_d[first, second]
        l <- lower_d[first, second]
        h <- upper_d[first, second]
      }
      empty <- !network && !(n_direct[first, second] > 0 && is.finite(e))
      tone <- if (empty) list(color = graph_ink$page, alpha = 1) else shade(e, l, h)
      fill <- tone$color
      border <- if (empty) graph_ink$plain_border else NA
      shapes <- rbind(shapes, data.frame(
        x = px(box$x), y = py(box$y), group = paste0("g", a, "_", b), id = id,
        tooltip = tip, onclick = click, fill = fill, alpha = tone$alpha, border = border,
        linetype = if (empty) "22" else "solid",
        hover = paste0("stroke:", hover_ink, ";stroke-width:1.5px;"),
        stringsAsFactors = FALSE))
      cell_text <- rbind(cell_text, if (empty) {
        data.frame(x = px(cx), y = py(cy), label = "no direct trials",
                   size = dims$small_pt / .pt, colour = graph_ink$muted,
                   fontface = "plain", id = id, tooltip = tip, onclick = click,
                   stringsAsFactors = FALSE)
      } else {
        data.frame(x = px(cx), y = py(c(cy - 6, cy + 8)),
                   label = c(fmt(e), paste0("(", fmt(l), ", ", fmt(h), ")")),
                   size = c(dims$value_pt, dims$small_pt) / .pt,
                   colour = c(graph_ink$title, graph_ink$text),
                   fontface = c("bold", "plain"), id = id, tooltip = tip,
                   onclick = click, stringsAsFactors = FALSE)
      })
    }
  }

  # Ranking bars. The hover target on top of each row shares the diagonal
  # cell's id, so hovering either lights both; they also share one hover
  # style, since ggiraph keys hover styles by id.
  bars <- NULL
  targets <- NULL
  if (ranking) {
    ranked <- order[base::order(-pscore[order])]
    labels <- texts("Ranking by P-score", rank_x, 6, dims$small_pt, graph_ink$title,
                    hjust = 0, face = "bold")
    for (r in seq_along(ranked)) {
      p <- match(ranked[r], order)
      y <- 22 + (r - 1) * dims$rank_row
      track <- rounded_rect(rank_x, rank_x + rank_w, y + 8, y + 14, 3, n = 4)
      value <- pscore[[ranked[r]]]
      bar <- rounded_rect(rank_x, rank_x + max(6, rank_w * value), y + 8, y + 14, 3, n = 4)
      hit <- rounded_rect(rank_x - 4, rank_x + rank_w + 4, y - 7, y + 19, 4, n = 4)
      bars <- rbind(
        bars,
        data.frame(x = px(track$x), y = py(track$y), group = sprintf("r%03da", r),
                   fill = league_ink$diagonal, stringsAsFactors = FALSE),
        if (value > 0) data.frame(x = px(bar$x), y = py(bar$y),
                                  group = sprintf("r%03db", r),
                                  fill = league_ink$first, stringsAsFactors = FALSE)
      )
      targets <- rbind(targets, data.frame(
        x = px(hit$x), y = py(hit$y), group = sprintf("r%03d", r),
        id = paste0("t", p), tooltip = treatment_tip(p), onclick = "",
        fill = "#FFFFFF02", alpha = 1, border = NA, linetype = "solid", hover = treatment_hover,
        stringsAsFactors = FALSE
      ))
      labels <- rbind(
        labels,
        texts(ranked[r], rank_x, y, dims$small_pt, graph_ink$text, hjust = 0),
        texts(formatC(value, format = "f", digits = 2), rank_x + rank_w, y,
              dims$small_pt, graph_ink$muted, hjust = 1)
      )
    }
  }

  p <- ggplot() +
    ggiraph::geom_polygon_interactive(
      data = shapes,
      aes(x = .data$x, y = .data$y, group = .data$group, fill = .data$fill,
          alpha = .data$alpha, colour = .data$border, linetype = .data$linetype,
          data_id = .data$id,
          tooltip = .data$tooltip, onclick = .data$onclick, hover_css = .data$hover),
      linewidth = 0.8 / .pt
    ) +
    ggiraph::geom_text_interactive(
      data = cell_text,
      aes(x = .data$x, y = .data$y, label = .data$label, data_id = .data$id,
          tooltip = .data$tooltip, onclick = .data$onclick),
      size = cell_text$size, colour = cell_text$colour, fontface = cell_text$fontface,
      family = family
    ) +
    draw_shapes(bars) +
    draw_text(labels, family) +
    (if (!is.null(targets)) ggiraph::geom_polygon_interactive(
      data = targets,
      aes(x = .data$x, y = .data$y, group = .data$group, fill = .data$fill,
          data_id = .data$id, tooltip = .data$tooltip, onclick = .data$onclick,
          hover_css = .data$hover),
      colour = NA
    )) +
    graph_frame(page, family)

  out <- list(plot = p, width = page$width / 72, height = page$height / 72,
              title = title, on_render = "ggextremeLeague(el, data);",
              treatments = order, pscore = pscore[order])
  if (!is.null(flow)) {
    # Each pair's two cells, and the direct cell of every comparison it
    # draws on, which sits above the diagonal.
    cell_of <- function(u, v) {
      i <- match(u, order)
      j <- match(v, order)
      paste0("c", pmin(i, j), "_", pmax(i, j))
    }
    net_cell <- function(u, v) {
      i <- match(u, order)
      j <- match(v, order)
      paste0("c", pmax(i, j), "_", pmin(i, j))
    }
    pairs <- unique(flow[c("net_a", "net_b", "net_key")])
    out$render_data <- list(flow = lapply(seq_len(nrow(pairs)), function(r) {
      f <- flow_of(pairs$net_a[r], pairs$net_b[r])
      first <- order[min(match(c(pairs$net_a[r], pairs$net_b[r]), order))]
      second <- setdiff(c(pairs$net_a[r], pairs$net_b[r]), first)
      list(cells = c(net_cell(first, second), cell_of(first, second)),
           label = paste(first, "vs", second),
           direct = sum(f$share[f$dir_key == pairs$net_key[r]]),
           sources = lapply(seq_len(nrow(f)), function(i) list(
             cell = cell_of(f$dir_a[i], f$dir_b[i]), share = f$share[i],
             label = flow_name(f[i, ]))))
    }))
    out$contributions <- flow[c("net_a", "net_b", "dir_a", "dir_b", "share")]
  }
  structure(out, class = c("ggleague", "ggx_graph"))
}

league_ink <- list(first = "#22928F", second = "#B66399", diagonal = graph_ink$faint)

league_dims <- utils::modifyList(graph_dims, list(
  cell_min = 78,
  cell_h = 42,
  cell_pad = 7,
  cell_gap = 3,
  name_pt = 9.5,
  value_pt = 10.5,
  small_pt = 8.5,
  rank_gap = 28,
  rank_w = 150,
  rank_row = 30
))

# Break a name into lines that fit `width` points, at spaces.
wrap_words <- function(text, width, pt, family, bold = FALSE) {
  words <- strsplit(text, " ", fixed = TRUE)[[1]]
  lines <- character(0)
  line <- ""
  for (w in words) {
    candidate <- if (nzchar(line)) paste(line, w) else w
    if (nzchar(line) && text_width_card(candidate, pt, family, 1, bold = bold) > width) {
      lines <- c(lines, line)
      line <- w
    } else {
      line <- candidate
    }
  }
  c(lines, line)
}

# Where each network estimate comes from: the share of it that flows through
# each direct comparison, from netmeta::netcontrib() (Papakonstantinou et al.
# 2018). `x` is a netmeta fit, which is then computed, or a netcontrib
# object. Returns one row per network comparison and direct comparison with a
# share above zero, keyed by unordered treatment pairs.
nma_contributions <- function(x, pooled = NULL) {
  rlang::check_installed("netmeta")
  if (inherits(x, "netmeta")) {
    x <- netmeta::netcontrib(x)
  } else if (!inherits(x, "netcontrib")) {
    rlang::abort("Give contributions as a fit from netmeta::netmeta() or an object from netmeta::netcontrib().")
  }
  fit <- x$x
  if (is.null(pooled)) pooled <- if (isTRUE(fit$random)) "random" else "common"
  m <- x[[pooled]]
  if (is.null(m)) rlang::abort(paste0("The contributions have no ", pooled, " effects results."))
  # netmeta names each comparison by its two treatments joined by sep.trts,
  # so every ordered pair is matched against the names it would get.
  trts <- fit$trts
  grid <- expand.grid(a = trts, b = trts, stringsAsFactors = FALSE)
  grid <- grid[grid$a != grid$b, ]
  grid$name <- paste(grid$a, grid$b, sep = fit$sep.trts)
  side <- function(names, what) {
    hit <- match(names, grid$name)
    if (anyNA(hit)) rlang::abort(paste0("Could not match these ", what, " to treatments: ",
                                        paste(names[is.na(hit)], collapse = ", ")))
    grid[hit, c("a", "b")]
  }
  rows <- side(rownames(m), "network comparisons")
  cols <- side(colnames(m), "direct comparisons")
  long <- expand.grid(r = seq_len(nrow(m)), k = seq_len(ncol(m)))
  long$share <- m[cbind(long$r, long$k)]
  long <- long[long$share > 1e-6, ]
  out <- data.frame(net_a = rows$a[long$r], net_b = rows$b[long$r],
                    dir_a = cols$a[long$k], dir_b = cols$b[long$k],
                    share = long$share, stringsAsFactors = FALSE)
  out$net_key <- pair_key(out$net_a, out$net_b)
  out$dir_key <- pair_key(out$dir_a, out$dir_b)
  out <- out[order(out$net_key, -out$share), ]
  rownames(out) <- NULL
  attr(out, "method") <- x$method
  attr(out, "pooled") <- pooled
  attr(out, "trts") <- trts
  out
}

pair_key <- function(a, b) ifelse(a < b, paste(a, b, sep = "\r"), paste(b, a, sep = "\r"))

# What the method is, in words, for the panels.
contribution_method <- function(flow) {
  m <- attr(flow, "method")
  name <- switch(m, shortestpath = "the shortest path method of Papakonstantinou et al. (2018)",
                 randomwalk = "the random walk method of Davies et al. (2022)",
                 cccp = "the method of Rucker et al.", paste0("the ", m, " method"))
  paste0("Shares come from netmeta::netcontrib(), by ", name, ", for the ",
         attr(flow, "pooled"), " effects model. They say how much of an estimate's ",
         "information flows through each direct comparison, not how trustworthy it is.")
}
