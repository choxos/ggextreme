#' Draw an interactive network plot for a network meta-analysis
#'
#' Draws the network of treatment comparisons from arm level data. Each node
#' is a treatment and each line joins two treatments compared directly in at
#' least one study. Hovering over a node shows a compact table of every arm
#' on that treatment; hovering over a line shows the arms of each study that
#' makes that comparison. The card holds the columns named in `hover`.
#' Clicking either opens a panel under the plot with the full arm level data
#' side by side, one column per arm, in the manner of a trial's baseline
#' table: every column of `data` other than `study`, `treatment` and `group`
#' becomes a row, so baseline characteristics and outcomes are shown as they
#' were given.
#'
#' Treatments sit on a circle, starting at the top and running clockwise in
#' the order of the levels of `treatment` when it is a factor, or in order of
#' first appearance otherwise. Line width follows the number of studies that
#' make the comparison. When `n` is given, node area follows the total number
#' of participants on that treatment. A study with more than two arms adds a
#' line for every pair of its treatments.
#'
#' Row labels come from each column's `label` attribute when it has one, as
#' set by the 'labelled', 'Hmisc' or 'haven' packages, and otherwise from its
#' name. Text that contains a URL or a DOI, such as a reference column, is
#' linked in the panel.
#'
#' @param data A data frame with one row per study arm.
#' @param study,treatment Bare column names identifying the study and the
#'   treatment of each arm. Each treatment may appear once per study.
#' @param n Optional bare column giving the number of participants in each
#'   arm. Sets node area and the participant counts in the hover cards.
#' @param group Optional bare column naming a class for each treatment, such
#'   as a drug class. Nodes are then colored by class and a legend is drawn.
#'   Each treatment must belong to exactly one class.
#' @param hover Names of the columns shown in the hover card, one row each,
#'   such as the sample size, an outcome and a key baseline characteristic.
#'   Defaults to the first four columns of the arm table. Use
#'   `character(0)` for a card that only lists the studies. The click panel
#'   always shows every column.
#' @param palette Node colors. With `group`, a vector named by class, or an
#'   unnamed vector recycled over the classes; without it, a single color.
#'   Defaults to [race_palette()].
#' @param legend Draw the legend when `group` is given.
#' @param legend_title Text in front of the legend, such as `"Class"`.
#' @param title,caption Title above the plot and note below it.
#' @param family Font family. The package ships Lato and registers it on load.
#'
#' @return An object of class `ggnma`, which prints as an interactive widget.
#'   Use [graph_widget()], [graph_plot()] or [graph_save()] for the widget, a
#'   static ggplot or a file. The fields `nodes` and `edges` hold the
#'   treatments and comparisons with their study counts, and `width` and
#'   `height` the natural size in inches.
#' @export
#'
#' @examples
#' net <- ggnma(psoriasis_nma, study, treatment, n = n, group = class,
#'              legend_title = "Class")
#' net
#' net$edges
ggnma <- function(data, study, treatment, n = NULL, group = NULL,
                  hover = NULL,
                  palette = NULL,
                  legend = TRUE,
                  legend_title = NULL,
                  title = NULL,
                  caption = NULL,
                  family = "Lato") {
  if (!is.data.frame(data)) rlang::abort("`data` must be a data frame.")
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  quos <- list(study = rlang::enquo(study), treatment = rlang::enquo(treatment),
               n = rlang::enquo(n), group = rlang::enquo(group))
  study <- rlang::eval_tidy(quos$study, data)
  treatment <- rlang::eval_tidy(quos$treatment, data)
  size <- rlang::eval_tidy(quos$n, data)
  group <- rlang::eval_tidy(quos$group, data)
  dims <- network_dims

  if (anyNA(study) || anyNA(treatment)) {
    rlang::abort("`study` and `treatment` must not be missing.")
  }
  levels <- if (is.factor(treatment)) {
    levels(droplevels(treatment))
  } else {
    unique(as.character(treatment))
  }
  study <- as.character(study)
  treatment <- as.character(treatment)
  twice <- duplicated(data.frame(study, treatment))
  if (any(twice)) {
    rlang::abort(paste0(
      "Each treatment may appear once per study. Repeated: ",
      paste(study[twice], treatment[twice], sep = ": ", collapse = ", ")
    ))
  }
  if (!is.null(size) && (!is.numeric(size) || anyNA(size) || any(size < 0))) {
    rlang::abort("`n` must be numeric, not missing, and not negative.")
  }
  arms_per_study <- table(study)
  lonely <- names(arms_per_study)[arms_per_study < 2]
  if (length(lonely)) {
    rlang::warn(paste0("Studies with a single arm add no comparisons: ",
                       paste(lonely, collapse = ", ")))
  }

  # Rows of the panel tables: every column except the ones that place the
  # arm, in the order given.
  placing <- vapply(quos[c("study", "treatment", "group")], function(q) {
    if (rlang::quo_is_symbol(q)) rlang::as_name(q) else NA_character_
  }, character(1))
  shown <- setdiff(names(data), placing)
  shown <- shown[!vapply(data[shown], is.list, logical(1))]
  shown_labels <- column_labels(data, shown)
  notes <- study_level(data, study, shown)
  in_table <- setdiff(shown, notes)
  if (is.null(hover)) hover <- utils::head(in_table, 4)
  if (!is.character(hover)) {
    rlang::abort("`hover` must be a character vector of column names.")
  }
  unknown <- setdiff(hover, in_table)
  if (length(unknown)) {
    rlang::abort(paste0("`hover` names columns that are not in the arm table: ",
                        paste(unknown, collapse = ", ")))
  }
  hover_labels <- shown_labels[match(hover, shown)]

  node_of <- match(treatment, levels)
  k <- length(levels)
  node_studies <- lapply(seq_len(k), function(i) unique(study[node_of == i]))
  node_n <- if (is.null(size)) NULL else
    vapply(seq_len(k), function(i) sum(size[node_of == i]), numeric(1))

  comparisons <- network_comparisons(study, node_of, size)

  # Colors, by class when there is one.
  key <- group_key(treatment, group, levels, what = "treatment")
  colors <- if (is.null(key)) {
    fill <- if (is.null(palette)) race_palette(1) else palette[[1]]
    list(node = rep(fill, k), labels = character(0), values = character(0))
  } else {
    values <- if (is.null(palette)) {
      stats::setNames(race_palette(length(key$levels)), key$levels)
    } else if (is.null(names(palette))) {
      stats::setNames(rep_len(palette, length(key$levels)), key$levels)
    } else {
      out <- stats::setNames(race_palette(length(key$levels)), key$levels)
      hit <- intersect(names(palette), key$levels)
      out[hit] <- palette[hit]
      out
    }
    list(node = unname(values[key$of]),
         labels = if (legend) key$levels else character(0),
         values = if (legend) unname(values) else character(0))
  }

  # Nodes on a circle, starting at the top and running clockwise down the
  # page. Node area follows the number of participants.
  radius <- if (is.null(node_n)) {
    rep(dims$node_r, k)
  } else {
    top <- max(node_n)
    if (top <= 0) rep(dims$node_r, k) else
      pmax(dims$node_r_min, dims$node_r_max * sqrt(node_n / top))
  }
  ring <- max(dims$ring_min,
              k * (2 * max(radius) + dims$ring_gap) / (2 * pi))
  angle <- -pi / 2 + 2 * pi * (seq_len(k) - 1) / k
  if (k == 1) ring <- 0
  cx <- ring * cos(angle)
  cy <- ring * sin(angle)

  # Labels sit outside their node, pointing away from the center.
  ux <- if (k == 1) 0 else cos(angle)
  uy <- if (k == 1) 1 else sin(angle)
  label_w <- text_width_card(levels, dims$label_pt, family, 1)
  label_h <- dims$label_pt * 1.2
  lx <- cx + ux * (radius + dims$label_gap)
  ly <- cy + uy * (radius + dims$label_gap)
  hjust <- (1 - ux) / 2
  vjust <- (1 + uy) / 2
  label_x0 <- lx - hjust * label_w
  label_y0 <- ly - (1 - vjust) * label_h

  canvas <- graph_canvas(
    c(cx - radius, cx + radius, label_x0, label_x0 + label_w),
    c(cy - radius, cy + radius, label_y0, label_y0 + label_h),
    title, caption, colors$labels, colors$values, legend_title, dims, family
  )
  px <- canvas$px
  py <- canvas$py

  # What the hover card and the click panel say.
  count <- function(m, one, many) {
    paste(format(m, big.mark = ",", scientific = FALSE, trim = TRUE),
          ifelse(m == 1, one, many))
  }
  node_sub <- vapply(seq_len(k), function(i) {
    paste(c(
      if (!is.null(key) && key$of[[i]] != levels[i]) key$of[[i]],
      count(length(node_studies[[i]]), "study", "studies"),
      if (!is.null(node_n)) count(node_n[i], "participant", "participants")
    ), collapse = "; ")
  }, character(1))
  node_ids <- paste0("n", seq_len(k))
  hover_table <- function(rows, by_treatment) {
    if (!length(hover)) return("")
    arm_table(data[rows, , drop = FALSE], study[rows], hover, hover_labels,
              treatment = if (by_treatment) treatment[rows])
  }
  node_rows <- lapply(seq_len(k), function(i) which(node_of == i))
  node_tip <- network_tip(
    levels, node_sub, node_studies,
    vapply(node_rows, hover_table, character(1), by_treatment = FALSE),
    "Click for every column."
  )
  node_panel <- vapply(seq_len(k), function(i) {
    rows <- node_rows[[i]]
    network_panel(levels[i], node_sub[i],
                  arm_table(data[rows, , drop = FALSE], study[rows], shown,
                            shown_labels, notes = notes))
  }, character(1))
  node_click <- pin_js(node_ids, node_panel)

  edges <- comparisons
  edge_ids <- paste0("e", seq_len(nrow(edges)))
  edge_title <- paste(levels[edges$from_i], "vs", levels[edges$to_i])
  edge_sub <- vapply(seq_len(nrow(edges)), function(j) {
    paste(c(count(edges$studies[j], "study", "studies"),
            if (!is.null(size)) count(edges$n[j], "participant in these arms",
                                      "participants in these arms")),
          collapse = "; ")
  }, character(1))
  edge_rows <- lapply(seq_len(nrow(edges)), function(j) {
    rows <- which(study %in% edges$study_list[[j]] &
                    node_of %in% c(edges$from_i[j], edges$to_i[j]))
    rows[order(match(study[rows], unique(study)), node_of[rows])]
  })
  edge_tip <- network_tip(
    edge_title, edge_sub, edges$study_list,
    vapply(edge_rows, hover_table, character(1), by_treatment = TRUE),
    "Click for every column."
  )
  edge_panel <- vapply(seq_len(nrow(edges)), function(j) {
    rows <- edge_rows[[j]]
    network_panel(edge_title[j], edge_sub[j],
                  arm_table(data[rows, , drop = FALSE], study[rows], shown,
                            shown_labels, treatment = treatment[rows],
                            notes = notes))
  }, character(1))
  edge_click <- pin_js(edge_ids, edge_panel)

  widths <- if (!nrow(edges)) numeric(0) else if (max(edges$studies) == 1) {
    rep(dims$edge_min, nrow(edges))
  } else {
    dims$edge_min + (dims$edge_max - dims$edge_min) *
      (edges$studies - 1) / (max(edges$studies) - 1)
  }
  edge_df <- do.call(rbind, lapply(seq_len(nrow(edges)), function(j) {
    a <- edges$from_i[j]
    b <- edges$to_i[j]
    data.frame(x = px(c(cx[a], cx[b])), y = py(c(cy[a], cy[b])),
               id = edge_ids[j], width = widths[j] / .pt,
               hit = (widths[j] + dims$hit_extra) / .pt,
               tooltip = edge_tip[j], onclick = edge_click[j],
               stringsAsFactors = FALSE)
  }))
  node_df <- do.call(rbind, lapply(seq_len(k), function(i) {
    a <- seq(0, 2 * pi, length.out = 61)
    data.frame(x = px(cx[i] + radius[i] * cos(a)),
               y = py(cy[i] + radius[i] * sin(a)),
               id = node_ids[i], fill = colors$node[i],
               tooltip = node_tip[i], onclick = node_click[i],
               hover = sprintf("fill:%s;stroke:%s;stroke-width:2px;",
                               colors$node[i], graph_ink$hover),
               stringsAsFactors = FALSE)
  }))
  label_df <- data.frame(x = px(lx), y = py(ly), label = levels,
                         hjust = hjust, vjust = vjust, id = node_ids,
                         tooltip = node_tip, onclick = node_click,
                         stringsAsFactors = FALSE)

  p <- ggplot() +
    network_edge_layers(edge_df) +
    ggiraph::geom_polygon_interactive(
      data = node_df,
      aes(x = .data$x, y = .data$y, group = .data$id, fill = .data$fill,
          data_id = .data$id, tooltip = .data$tooltip,
          onclick = .data$onclick, hover_css = .data$hover),
      colour = graph_ink$page, linewidth = dims$node_border / .pt
    ) +
    ggiraph::geom_text_interactive(
      data = label_df,
      aes(x = .data$x, y = .data$y, label = .data$label,
          hjust = .data$hjust, vjust = .data$vjust,
          data_id = .data$id, tooltip = .data$tooltip,
          onclick = .data$onclick),
      size = dims$label_pt / .pt, colour = graph_ink$text, family = family
    ) +
    scale_linewidth_identity() +
    graph_frame(canvas, family)

  structure(
    list(
      plot = p, width = canvas$width / 72, height = canvas$height / 72,
      title = title,
      nodes = data.frame(
        treatment = levels,
        studies = lengths(node_studies),
        n = if (is.null(node_n)) NA_real_ else node_n,
        stringsAsFactors = FALSE
      ),
      edges = data.frame(
        from = levels[edges$from_i], to = levels[edges$to_i],
        studies = edges$studies,
        n = if (is.null(size)) rep(NA_real_, nrow(edges)) else edges$n,
        stringsAsFactors = FALSE
      )
    ),
    class = c("ggnma", "ggx_graph")
  )
}

network_dims <- utils::modifyList(graph_dims, list(
  node_r = 10,
  node_r_min = 5,
  node_r_max = 18,
  node_border = 1.5,
  ring_min = 110,
  ring_gap = 40,
  label_pt = 11,
  label_gap = 6,
  edge_min = 1.5,
  edge_max = 7,
  hit_extra = 9
))

# Every pair of treatments compared within a study, with the studies that
# compare them and the participants in those arms.
network_comparisons <- function(study, node_of, size) {
  pairs <- do.call(rbind, lapply(unique(study), function(s) {
    rows <- which(study == s)
    if (length(rows) < 2) return(NULL)
    rows <- rows[order(node_of[rows])]
    idx <- utils::combn(length(rows), 2)
    data.frame(
      study = s,
      from_i = node_of[rows[idx[1, ]]],
      to_i = node_of[rows[idx[2, ]]],
      n = if (is.null(size)) NA_real_ else size[rows[idx[1, ]]] + size[rows[idx[2, ]]],
      stringsAsFactors = FALSE
    )
  }))
  if (is.null(pairs)) {
    return(data.frame(from_i = integer(0), to_i = integer(0),
                      studies = integer(0), n = numeric(0),
                      study_list = I(list())))
  }
  key <- paste(pairs$from_i, pairs$to_i)
  first <- !duplicated(key)
  out <- pairs[first, c("from_i", "to_i")]
  groups <- split(seq_len(nrow(pairs)), factor(key, levels = key[first]))
  out$study_list <- I(lapply(groups, function(g) pairs$study[g]))
  out$studies <- lengths(out$study_list)
  out$n <- vapply(groups, function(g) sum(pairs$n[g]), numeric(1))
  rownames(out) <- NULL
  out[order(out$from_i, out$to_i), ]
}

# The hover card: a compact arm table when there are columns to show, and
# otherwise the list of studies.
network_tip <- function(title, sub, studies, tables, hint) {
  vapply(seq_along(title), function(i) {
    s <- studies[[i]]
    listed <- if (length(s) > 10) {
      paste0(paste(s[1:10], collapse = ", "), " and ", length(s) - 10, " more")
    } else {
      paste(s, collapse = ", ")
    }
    body <- if (nzchar(tables[i])) tables[i] else
      paste0('<div class="ggx-tip-body">', esc(listed), "</div>")
    paste0(
      '<div class="ggx-tip">',
      '<div class="ggx-tip-title">', esc(title[i]), "</div>",
      '<div class="ggx-tip-sub">', esc(sub[i]), "</div>",
      body,
      '<div class="ggx-tip-hint">', esc(hint), "</div>",
      "</div>"
    )
  }, character(1))
}

network_panel <- function(title, sub, table) {
  paste0('<div class="ggx-title">', esc(title), "</div>",
         '<div class="ggx-sub">', esc(sub), "</div>", table)
}

network_edge_layers <- function(df) {
  if (is.null(df) || !nrow(df)) return(NULL)
  list(
    ggiraph::geom_path_interactive(
      data = df,
      aes(x = .data$x, y = .data$y, group = .data$id, data_id = .data$id,
          linewidth = .data$width),
      colour = graph_ink$edge, lineend = "round"
    ),
    # See edge_layers(): an almost transparent copy on top is the target.
    ggiraph::geom_path_interactive(
      data = df,
      aes(x = .data$x, y = .data$y, group = .data$id, data_id = .data$id,
          linewidth = .data$hit, tooltip = .data$tooltip,
          onclick = .data$onclick),
      colour = "#FFFFFF02", lineend = "round"
    )
  )
}
