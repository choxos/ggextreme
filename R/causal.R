#' Draw an interactive causal diagram
#'
#' Draws a directed acyclic graph (DAG) in which every node and every arrow
#' carries its own justification. Hovering over a node shows its role and the
#' reason it is in the diagram; hovering over an arrow shows the assumed
#' direction of effect and the reason for it. Clicking either opens a panel
#' under the diagram with the full rationale and clickable references, which
#' also makes the diagram usable on touch screens.
#'
#' The layout is layered, so every arrow points the same way, left to right
#' or top down. An arrow that skips a layer is routed around the boxes in
#' between on a smooth curve. Give `x` and `y` columns in `nodes` to place the
#' boxes yourself; arrows are then drawn straight.
#'
#' Nodes are colored by `role`. The roles `exposure`, `outcome`,
#' `confounder`, `mediator`, `collider`, `instrument`, `unobserved` and
#' `latent` have fixed colors, and the last two are drawn with a dashed
#' border. Any other role is allowed and takes the next color from
#' [race_palette()].
#'
#' @param edges A data frame with one row per arrow and columns `from` and
#'   `to` naming its ends. Optional columns: `rationale`, the reason for the
#'   arrow and its direction, and `references`. Any other column is shown as a
#'   labeled field.
#' @param nodes A data frame with one row per node and a `name` column
#'   matching `from` and `to`. Optional columns: `label`, the text in the box,
#'   which may contain line breaks and defaults to `name`; `role`;
#'   `rationale`; `references`; and `x` and `y` to place the box by hand, in
#'   grid units with `y` pointing up. Any other column is shown as a labeled
#'   field. When `NULL`, the nodes are taken from `edges`.
#' @param direction Which way the arrows point: `"right"` or `"down"`.
#' @param palette Colors for the roles, as a vector named by role. Roles left
#'   out keep their default color.
#' @param legend Draw a legend of the roles above the diagram.
#' @param legend_title Text in front of the legend, such as `"Role"`.
#' @param title,caption Title above the diagram and note below it.
#' @param family Font family. The package ships Lato and registers it on load.
#'
#' @param paths Show which paths between the exposure and the outcome are
#'   open or blocked for an adjustment set. The widget then lets the reader
#'   choose the exposure and outcome, click variables to adjust for them, and
#'   read why each path is open or blocked, whether the set is sufficient and
#'   which minimal sets would be.
#' @param exposure,outcome Names of the exposure and the outcome. Default to
#'   the nodes whose role is `exposure` and `outcome`.
#' @param adjust Names of the nodes adjusted for at the start.
#'
#' @section Adjustment paths:
#' A path between the exposure and the outcome is causal when every arrow on
#' it points from the exposure toward the outcome, and biasing otherwise. A
#' path is blocked when it passes through a variable that is adjusted for,
#' unless that variable is a collider, where two arrows meet head to head;
#' a collider blocks a path until it, or one of its descendants, is adjusted
#' for, which opens it. An adjustment set is sufficient by the backdoor
#' criterion when it blocks every biasing path and contains no descendant of
#' the exposure (Pearl 2009). Unobserved and latent variables cannot be
#' adjusted for. Adjustment is conditioning, not intervention, and a diagram
#' gives no size of effect.
#'
#' @details `references` may be a list column with one character vector per
#'   row, or text with several references separated by `|` or a line break.
#'   A reference that contains a URL is linked to it; otherwise one that
#'   contains a DOI is linked to `https://doi.org/`.
#'
#' @return An object of class `ggcausal`, which prints as an interactive
#'   widget. Use [graph_widget()], [graph_plot()] or [graph_save()] for the
#'   widget, a static ggplot or a file. The fields `width` and `height` give
#'   the natural size in inches.
#' @export
#'
#' @examples
#' dag <- ggcausal(cleft_dag$edges, cleft_dag$nodes, legend_title = "Role")
#' dag
#'
#' # Only the arrows are required.
#' ggcausal(data.frame(from = c("A", "A", "B"), to = c("B", "C", "C")))
ggcausal <- function(edges, nodes = NULL,
                     direction = c("right", "down"),
                     palette = NULL,
                     legend = TRUE,
                     legend_title = NULL,
                     title = NULL,
                     caption = NULL,
                     family = "Lato",
                     paths = FALSE,
                     exposure = NULL,
                     outcome = NULL,
                     adjust = character(0)) {
  direction <- match.arg(direction)
  dims <- graph_dims
  edges <- check_edges(edges)
  nodes <- check_nodes(nodes, edges)
  graph <- igraph::graph_from_data_frame(
    edges[c("from", "to")], directed = TRUE,
    vertices = data.frame(name = nodes$name, stringsAsFactors = FALSE)
  )
  check_acyclic(graph)

  labels <- if (is.null(nodes[["label"]])) nodes$name else as.character(nodes$label)
  labels[is.na(labels) | !nzchar(labels)] <- nodes$name[is.na(labels) | !nzchar(labels)]
  flat <- gsub("\n", " ", labels, fixed = TRUE)
  label_of <- function(v) flat[match(v, nodes$name)]
  study <- if (isTRUE(paths)) causal_setup(nodes, edges, exposure, outcome, adjust, label_of)
  boxes <- label_boxes(labels, dims, family)
  roles <- causal_roles(nodes[["role"]], palette, nrow(nodes))

  from_i <- match(edges$from, nodes$name)
  to_i <- match(edges$to, nodes$name)
  placed <- if (is.null(nodes[["x"]])) {
    layered_layout(graph, from_i, to_i, boxes, direction, dims)
  } else {
    manual_layout(nodes, from_i, to_i, boxes, dims)
  }

  # Routes join box centers through any bend points, and are then cut back
  # to the box borders.
  node_box <- function(i) list(x = placed$x[i], y = placed$y[i],
                               w = boxes$w[i], h = boxes$h[i])
  routes <- lapply(seq_len(nrow(edges)), function(k) {
    pts <- rbind(c(placed$x[from_i[k]], placed$y[from_i[k]]),
                 placed$bends[[k]],
                 c(placed$x[to_i[k]], placed$y[to_i[k]]),
                 deparse.level = 0)
    path <- trim_route(smooth_route(pts), node_box(from_i[k]), node_box(to_i[k]), dims)
    if (!is.null(path) && nrow(pts) == 2) path <- path[c(1, nrow(path)), , drop = FALSE]
    path
  })

  # Everything the diagram occupies, so the page can be sized around it.
  all_x <- c(placed$x - boxes$w / 2, placed$x + boxes$w / 2,
             unlist(lapply(routes, function(r) if (!is.null(r)) r[, 1])))
  all_y <- c(placed$y - boxes$h / 2, placed$y + boxes$h / 2,
             unlist(lapply(routes, function(r) if (!is.null(r)) r[, 2])))
  keys <- if (legend) roles$levels else character(0)
  # The verdict sits under the diagram, wrapped to its width so it does not
  # widen the page. The widget replaces it with its panel.
  verdict_lines <- character(0)
  if (!is.null(study)) {
    study$verdict <- causal_verdict(study, label_of)
    key_text <- paste("Teal arrows lie on the open causal path, red ones on an open biasing path,",
                      "and dashed ones on blocked paths. Adjusted variables are boxed.")
    wrap_at <- max(diff(range(all_x)), dims$min_width)
    verdict_lines <- c(wrap_words(study$verdict, wrap_at, dims$caption_pt, family),
                       wrap_words(key_text, wrap_at, dims$caption_pt, family))
  }
  canvas <- graph_canvas(all_x, all_y, title, c(caption, verdict_lines), capitalize(keys),
                         roles$colors[keys], legend_title, dims, family)
  verdict_df <- empty_texts()
  if (length(verdict_lines)) {
    said <- canvas$text$label %in% verdict_lines & canvas$text$colour == graph_ink$muted
    verdict_df <- canvas$text[said, , drop = FALSE]
    canvas$text <- canvas$text[!said, , drop = FALSE]
  }
  px <- canvas$px
  py <- canvas$py

  # What the hover card and the click panel say about each node and arrow.
  node_refs <- split_references(nodes[["references"]], nrow(nodes))
  node_fields <- field_values(nodes, c("name", "label", "role", "rationale",
                                       "references", "x", "y"))
  node_sub <- ifelse(is.na(roles$role), "", capitalize(roles$role))
  node_body <- text_or_empty(nodes[["rationale"]], nrow(nodes))
  node_ids <- paste0("n", seq_len(nrow(nodes)))

  edge_refs <- split_references(edges[["references"]], nrow(edges))
  edge_fields <- field_values(edges, c("from", "to", "rationale", "references"))
  edge_title <- paste(flat[from_i], "\u2192", flat[to_i])
  edge_sub <- rep("Assumed direct effect", nrow(edges))
  edge_body <- text_or_empty(edges[["rationale"]], nrow(edges))
  edge_ids <- paste0("e", seq_len(nrow(edges)))

  node_tip <- tip_html(flat, node_sub, node_body, node_fields, node_refs)
  node_click <- pin_js(node_ids, panel_html(flat, node_sub, node_body,
                                            node_fields, node_refs))
  edge_tip <- tip_html(edge_title, edge_sub, edge_body, edge_fields, edge_refs)
  edge_click <- pin_js(edge_ids, panel_html(edge_title, edge_sub, edge_body,
                                            edge_fields, edge_refs))

  # Role fills are the role's color with transparency, so they sit right on
  # a light or a dark page.
  fill <- ifelse(is.na(roles$role), graph_ink$plain_fill, roles$color_of)
  fill_alpha <- ifelse(is.na(roles$role), 1, 0.16)
  border <- ifelse(is.na(roles$role), graph_ink$plain_border, roles$color_of)
  dashed <- tolower(trimws(roles$role)) %in% c("unobserved", "latent")

  node_df <- do.call(rbind, lapply(seq_len(nrow(nodes)), function(i) {
    cx <- placed$x[i]
    cy <- placed$y[i]
    shape <- rounded_rect(cx - boxes$w[i] / 2, cx + boxes$w[i] / 2,
                          cy - boxes$h[i] / 2, cy + boxes$h[i] / 2,
                          dims$radius, n = 6)
    data.frame(
      x = px(shape$x), y = py(shape$y), id = node_ids[i],
      fill = fill[i], alpha = fill_alpha[i], border = border[i],
      linetype = if (dashed[i]) "22" else "solid",
      tooltip = node_tip[i], onclick = node_click[i],
      hover = sprintf("stroke:%s;stroke-width:2px;", border[i]),
      stringsAsFactors = FALSE
    )
  }))
  # One row per line of a label. The SVG writer draws each line of a
  # multi-line string as its own element, which would leave the later lines
  # without the node's hover and click handlers.
  label_df <- do.call(rbind, lapply(seq_len(nrow(nodes)), function(i) {
    lines <- strsplit(labels[i], "\n", fixed = TRUE)[[1]]
    shift <- (seq_along(lines) - (length(lines) + 1) / 2) *
      dims$node_pt * dims$line_h
    data.frame(x = px(placed$x[i]), y = py(placed$y[i] + shift),
               label = lines, id = node_ids[i], tooltip = node_tip[i],
               onclick = node_click[i], stringsAsFactors = FALSE)
  }))
  drawn <- !vapply(routes, is.null, logical(1))
  if (!all(drawn)) {
    rlang::warn(paste0(
      "Some arrows join boxes that overlap and were not drawn: ",
      paste(edges$from[!drawn], "to", edges$to[!drawn], collapse = ", "),
      ". Move the boxes apart with `x` and `y`."
    ))
  }
  state <- if (!is.null(study)) causal_edge_state(study, edges)
  edge_df <- do.call(rbind, lapply(which(drawn), function(k) {
    data.frame(x = px(routes[[k]][, 1]), y = py(routes[[k]][, 2]),
               id = edge_ids[k], tooltip = edge_tip[k], onclick = edge_click[k],
               colour = if (is.null(state)) graph_ink$edge else causal_ink[[state[k]]],
               linetype = if (!is.null(state) && state[k] == "blocked") "22" else "solid",
               stringsAsFactors = FALSE)
  }))
  # Adjusted variables are boxed, as is the convention in causal diagrams.
  # Every node gets an outline so the widget can turn it on and off.
  adj_df <- if (!is.null(study)) do.call(rbind, lapply(seq_len(nrow(nodes)), function(i) {
    g <- 3.5
    shape <- rounded_rect(placed$x[i] - boxes$w[i] / 2 - g, placed$x[i] + boxes$w[i] / 2 + g,
                          placed$y[i] - boxes$h[i] / 2 - g, placed$y[i] + boxes$h[i] / 2 + g,
                          dims$radius + g, n = 6)
    data.frame(x = px(shape$x), y = py(shape$y), id = paste0("adj", i),
               colour = if (nodes$name[i] %in% study$adjust) graph_ink$title else NA_character_,
               stringsAsFactors = FALSE)
  }))

  p <- ggplot() +
    edge_layers(edge_df, dims) +
    if (!is.null(adj_df)) ggiraph::geom_polygon_interactive(
      data = adj_df,
      aes(x = .data$x, y = .data$y, group = .data$id, colour = .data$colour,
          data_id = .data$id),
      fill = NA, linewidth = 1.6 / .pt
    )
  p <- p +
    ggiraph::geom_polygon_interactive(
      data = node_df,
      aes(x = .data$x, y = .data$y, group = .data$id, fill = .data$fill,
          alpha = .data$alpha, colour = .data$border, linetype = .data$linetype,
          data_id = .data$id, tooltip = .data$tooltip,
          onclick = .data$onclick, hover_css = .data$hover),
      linewidth = 0.9 / .pt
    ) +
    ggiraph::geom_text_interactive(
      data = label_df,
      aes(x = .data$x, y = .data$y, label = .data$label,
          data_id = .data$id, tooltip = .data$tooltip,
          onclick = .data$onclick),
      size = dims$node_pt / .pt, colour = graph_ink$text, family = family
    ) +
    (if (nrow(verdict_df)) ggiraph::geom_text_interactive(
      data = verdict_df,
      aes(x = .data$x, y = .data$y, label = .data$label, data_id = "cv"),
      hjust = 0, size = dims$caption_pt / .pt, colour = graph_ink$muted, family = family
    )) +
    graph_frame(canvas, family)

  out <- list(plot = p, width = canvas$width / 72, height = canvas$height / 72,
              title = title, nodes = nodes, edges = edges)
  if (!is.null(study)) {
    out$paths <- data.frame(
      path = vapply(study$paths$path, function(v) causal_path_text(v, edges, label_of), ""),
      kind = ifelse(study$paths$causal, "causal", "biasing"),
      status = ifelse(study$paths$open, "open", "blocked"),
      reason = study$paths$reason, stringsAsFactors = FALSE
    )
    out$adjustment <- list(exposure = study$exposure, outcome = study$outcome,
                           adjust = study$adjust, sufficient = study$sufficient,
                           verdict = study$verdict, minimal_sets = study$sets)
    out$on_render <- "ggextremeCausalPaths(el, data);"
    out$render_data <- list(
      nodes = lapply(seq_len(nrow(nodes)), function(i) list(
        name = nodes$name[i], label = flat[i], id = node_ids[i],
        hidden = nodes$name[i] %in% study$hidden)),
      edges = lapply(seq_len(nrow(edges)), function(k) list(
        from = edges$from[k], to = edges$to[k], id = edge_ids[k], drawn = drawn[k])),
      exposure = study$exposure, outcome = study$outcome, adjust = as.list(study$adjust),
      ink = causal_ink
    )
  }
  structure(out, class = c("ggcausal", "ggx_graph"))
}

# Arrow colors for paths between the exposure and the outcome.
# The neutrals are graph_ink's plain_border and edge, written out because
# graph_ink is defined in a file collated later.
causal_ink <- list(causal = "#22928F", biasing = "#C0603F", blocked = "#9A9A9A",
                   none = "#8A8A8A")

# Which state colors each arrow: an open biasing path wins over an open
# causal path, which wins over a blocked path.
causal_edge_state <- function(study, edges) {
  rank <- c(none = 0, blocked = 1, causal = 2, biasing = 3)
  state <- rep("none", nrow(edges))
  for (r in seq_len(nrow(study$paths))) {
    p <- study$paths$path[[r]]
    s <- if (!study$paths$open[r]) "blocked" else if (study$paths$causal[r]) "causal" else "biasing"
    for (i in seq_along(p)[-1]) {
      k <- which((edges$from == p[i - 1] & edges$to == p[i]) | (edges$from == p[i] & edges$to == p[i - 1]))
      if (rank[s] > rank[state[k]]) state[k] <- s
    }
  }
  state
}

# A path as text, with each arrow pointing the way it does in the diagram.
causal_path_text <- function(p, edges, label_of) {
  out <- label_of(p[1])
  for (i in seq_along(p)[-1]) {
    fwd <- any(edges$from == p[i - 1] & edges$to == p[i])
    out <- paste(out, if (fwd) "\u2192" else "\u2190", label_of(p[i]))
  }
  out
}

edge_layers <- function(df, dims) {
  if (is.null(df) || !nrow(df)) return(NULL)
  list(
    # Arrowheads take the color of their line.
    ggiraph::geom_path_interactive(
      data = df,
      aes(x = .data$x, y = .data$y, group = .data$id, data_id = .data$id,
          colour = .data$colour, linetype = .data$linetype),
      linewidth = dims$edge_pt / .pt,
      linejoin = "mitre", lineend = "butt",
      arrow = grid::arrow(angle = 20, length = grid::unit(dims$arrow_pt, "bigpts"),
                          type = "closed")
    ),
    # A wide, almost transparent copy on top gives the thin line a hover
    # target that is easy to hit. Fully transparent strokes are not painted,
    # so they would not catch the pointer at all.
    ggiraph::geom_path_interactive(
      data = df,
      aes(x = .data$x, y = .data$y, group = .data$id, data_id = .data$id,
          tooltip = .data$tooltip, onclick = .data$onclick),
      colour = "#FFFFFF02", linewidth = dims$hit_pt / .pt, lineend = "round"
    )
  )
}

check_edges <- function(edges) {
  if (!is.data.frame(edges) || !all(c("from", "to") %in% names(edges))) {
    rlang::abort("`edges` must be a data frame with `from` and `to` columns.")
  }
  edges <- as.data.frame(edges, stringsAsFactors = FALSE)
  edges$from <- as.character(edges$from)
  edges$to <- as.character(edges$to)
  if (anyNA(edges$from) || anyNA(edges$to)) {
    rlang::abort("`from` and `to` must not be missing.")
  }
  loops <- edges$from == edges$to
  if (any(loops)) {
    rlang::abort(paste0("An arrow cannot join a node to itself: ",
                        paste(unique(edges$from[loops]), collapse = ", ")))
  }
  dup <- duplicated(edges[c("from", "to")])
  if (any(dup)) {
    rlang::abort(paste0("Each arrow must appear once. Repeated: ",
                        paste(edges$from[dup], "to", edges$to[dup], collapse = ", ")))
  }
  edges
}

check_nodes <- function(nodes, edges) {
  used <- unique(c(edges$from, edges$to))
  if (is.null(nodes)) return(data.frame(name = used, stringsAsFactors = FALSE))
  if (!is.data.frame(nodes) || !"name" %in% names(nodes)) {
    rlang::abort("`nodes` must be a data frame with a `name` column.")
  }
  nodes <- as.data.frame(nodes, stringsAsFactors = FALSE)
  nodes$name <- as.character(nodes$name)
  if (anyNA(nodes$name)) rlang::abort("`name` must not be missing.")
  dup <- unique(nodes$name[duplicated(nodes$name)])
  if (length(dup)) {
    rlang::abort(paste0("Each node must appear once in `nodes`. Repeated: ",
                        paste(dup, collapse = ", ")))
  }
  missing <- setdiff(used, nodes$name)
  if (length(missing)) {
    rlang::abort(paste0("Every node named in `edges` must be in `nodes`. ",
                        "Missing: ", paste(missing, collapse = ", ")))
  }
  has_x <- !is.null(nodes[["x"]])
  has_y <- !is.null(nodes[["y"]])
  if (has_x != has_y) rlang::abort("Give both `x` and `y` in `nodes`, or neither.")
  if (has_x && (!is.numeric(nodes$x) || !is.numeric(nodes$y) ||
                anyNA(nodes$x) || anyNA(nodes$y))) {
    rlang::abort("`x` and `y` must be numeric with no missing values.")
  }
  nodes
}

check_acyclic <- function(graph) {
  if (igraph::is_dag(graph)) return(invisible())
  comp <- igraph::components(graph, mode = "strong")
  on_cycle <- names(comp$membership)[comp$csize[comp$membership] > 1]
  rlang::abort(paste0("The arrows form a cycle, so this is not a DAG. ",
                      "Nodes on a cycle: ", paste(on_cycle, collapse = ", ")))
}

known_roles <- c(
  exposure = "#22928F", outcome = "#757CC6", confounder = "#C76253",
  mediator = "#C28A4A", collider = "#7A5178", instrument = "#568E4F",
  unobserved = "#8C8C8C", latent = "#8C8C8C"
)

# Roles in legend order, their colors, and the color of every node.
causal_roles <- function(role, palette, n) {
  if (is.null(role)) {
    return(list(role = rep(NA_character_, n), levels = character(0),
                colors = character(0), color_of = rep(NA_character_, n)))
  }
  levels <- if (is.factor(role)) {
    levels(droplevels(role))
  } else {
    seen <- unique(as.character(role[!is.na(role)]))
    rank <- match(tolower(trimws(seen)), names(known_roles))
    c(seen[!is.na(rank)][order(rank[!is.na(rank)])], sort(seen[is.na(rank)]))
  }
  role <- as.character(role)
  colors <- unname(known_roles[tolower(trimws(levels))])
  spare <- setdiff(race_palette(26), known_roles)
  colors[is.na(colors)] <- rep_len(spare, sum(is.na(colors)))
  names(colors) <- levels
  if (!is.null(palette)) {
    if (is.null(names(palette))) rlang::abort("`palette` must be named by role.")
    hit <- intersect(names(palette), levels)
    colors[hit] <- palette[hit]
  }
  list(role = role, levels = levels, colors = colors,
       color_of = unname(colors[role]))
}

# Layers from the Sugiyama method. Arrows that skip layers get dummy points
# in the layers they cross, and those become the bends of their route.
layered_layout <- function(graph, from_i, to_i, boxes, direction, dims) {
  n <- length(boxes$w)
  s <- igraph::layout_with_sugiyama(graph)
  coords <- rbind(s$layout, s$layout.dummy)
  layer <- max(coords[, 2]) - coords[, 2]
  along <- coords[, 1]
  w <- c(boxes$w, rep(0, nrow(coords) - n))
  h <- c(boxes$h, rep(0, nrow(coords) - n))

  if (direction == "right") {
    slot <- vapply(split(w, factor(layer, levels = sort(unique(layer)))), max,
                   numeric(1))
    ends <- cumsum(slot) + (seq_along(slot) - 1) * dims$layer_gap_right
    centers <- ends - slot / 2
    x <- centers[match(layer, sort(unique(layer)))]
    y <- along * (max(boxes$h) + dims$row_gap_right)
  } else {
    x <- along * (max(boxes$w) + dims$column_gap_down)
    y <- layer * (max(boxes$h) + dims$layer_gap_down)
  }

  edges <- igraph::as_edgelist(s$extd_graph, names = FALSE)
  bends <- rep(list(NULL), length(from_i))
  for (k in seq_len(nrow(edges))) {
    if (edges[k, 1] > n) next
    chain <- integer(0)
    v <- edges[k, 2]
    while (v > n) {
      chain <- c(chain, v)
      v <- edges[edges[, 1] == v, 2][1]
    }
    hit <- which(from_i == edges[k, 1] & to_i == v)
    if (length(hit) && length(chain)) bends[[hit]] <- cbind(x[chain], y[chain])
  }
  list(x = unname(x[seq_len(n)]), y = unname(y[seq_len(n)]), bends = bends)
}

# Boxes placed by hand, in grid units with y pointing up.
manual_layout <- function(nodes, from_i, to_i, boxes, dims) {
  list(
    x = nodes$x * (max(boxes$w) + dims$manual_gap_x),
    y = -nodes$y * (max(boxes$h) + dims$manual_gap_y),
    bends = rep(list(NULL), length(from_i))
  )
}

# Adjustment paths --------------------------------------------------------

# Which role names mean a node cannot be adjusted for.
causal_hidden <- c("unobserved", "latent")

causal_setup <- function(nodes, edges, exposure, outcome, adjust, label_of = identity) {
  role <- tolower(trimws(if (is.null(nodes[["role"]])) rep("", nrow(nodes)) else as.character(nodes$role)))
  pick <- function(given, r, what) {
    if (!is.null(given)) {
      if (!given %in% nodes$name) rlang::abort(paste0("`", what, "` must name a node."))
      return(given)
    }
    hit <- nodes$name[role == r]
    if (length(hit) != 1) {
      rlang::abort(paste0("Name the ", what, " with `", what, "`, or give one node the role \"", r, "\"."))
    }
    hit
  }
  x <- pick(exposure, "exposure", "exposure")
  y <- pick(outcome, "outcome", "outcome")
  if (x == y) rlang::abort("The exposure and the outcome must be different nodes.")
  adjust <- as.character(adjust)
  if (length(setdiff(adjust, nodes$name))) rlang::abort("`adjust` must name nodes.")
  hidden <- nodes$name[role %in% causal_hidden]
  if (length(intersect(adjust, hidden))) rlang::abort("Unobserved variables cannot be adjusted for.")
  adjust <- setdiff(adjust, c(x, y))
  res <- causal_assess(edges$from, edges$to, x, y, adjust, label_of = label_of)
  sets <- causal_minimal_sets(edges$from, edges$to, x, y, setdiff(nodes$name, hidden))
  c(res, list(exposure = x, outcome = y, adjust = adjust, hidden = hidden, sets = sets))
}

causal_descendants <- function(from, to, v) {
  out <- character(0)
  stack <- v
  while (length(stack)) {
    ch <- setdiff(to[from == stack[1]], out)
    out <- c(out, ch)
    stack <- c(stack[-1], ch)
  }
  out
}

# Every simple path between the exposure and the outcome, and whether each
# is open given the adjustment set.
causal_assess <- function(from, to, x, y, adjust, limit = 500, label_of = identity) {
  nb <- function(v) unique(c(to[from == v], from[to == v]))
  arrow <- function(a, b) any(from == a & to == b)
  found <- list()
  walk <- function(path) {
    if (length(found) >= limit) return()
    v <- path[length(path)]
    if (v == y) { found[[length(found) + 1]] <<- path; return() }
    for (w in nb(v)) if (!w %in% path) walk(c(path, w))
  }
  walk(x)
  desc <- lapply(stats::setNames(nm = unique(c(from, to))), function(v) causal_descendants(from, to, v))
  rows <- lapply(found, function(p) {
    causal <- all(vapply(seq_along(p)[-1], function(i) arrow(p[i - 1], p[i]), logical(1)))
    open <- TRUE
    why <- character(0)
    for (i in seq_along(p)[-c(1, length(p))]) {
      a <- p[i - 1]
      m <- p[i]
      b <- p[i + 1]
      if (arrow(a, m) && arrow(b, m)) {
        opener <- intersect(c(m, desc[[m]]), adjust)
        if (length(opener)) {
          why <- c(why, if (opener[1] == m) {
            paste0("adjusting for the collider ", label_of(m), " opens it")
          } else {
            paste0("adjusting for ", label_of(opener[1]), ", a consequence of the collider ",
                   label_of(m), ", opens it")
          })
        } else {
          open <- FALSE
          why <- c(why, paste0(label_of(m), " is a collider, which blocks the path"))
        }
      } else if (m %in% adjust) {
        open <- FALSE
        why <- c(why, paste0("adjusting for ", label_of(m), " blocks it"))
      }
    }
    if (!open) why <- why[!grepl("opens it", why, fixed = TRUE)]
    if (open && !length(why)) {
      why <- if (causal) "the effect to estimate; nothing on it is adjusted for" else "nothing on it is adjusted for"
    }
    data.frame(path = I(list(p)), causal = causal, backdoor = arrow(p[2], p[1]), open = open,
               reason = paste(why, collapse = "; "), stringsAsFactors = FALSE)
  })
  tab <- if (length(rows)) do.call(rbind, rows) else
    data.frame(path = I(list()), causal = logical(0), backdoor = logical(0), open = logical(0), reason = character(0))
  mediators <- intersect(adjust, desc[[x]])
  open_bias <- sum(!tab$causal & tab$open)
  list(paths = tab, mediators = mediators, open_biasing = open_bias,
       sufficient = open_bias == 0 && !length(mediators), truncated = length(found) >= limit)
}

# The smallest adjustment sets that satisfy the backdoor criterion, among
# observed variables that are not descendants of the exposure.
causal_minimal_sets <- function(from, to, x, y, observed, most = 12) {
  cand <- setdiff(observed, c(x, y, causal_descendants(from, to, x)))
  if (length(cand) > most) return(NULL)
  out <- list()
  for (size in 0:length(cand)) {
    combos <- if (size == 0) list(character(0)) else utils::combn(cand, size, simplify = FALSE)
    for (z in combos) {
      if (any(vapply(out, function(o) all(o %in% z), logical(1)))) next
      if (causal_assess(from, to, x, y, z)$sufficient) out[[length(out) + 1]] <- z
    }
  }
  out
}

# One sentence on whether an adjustment set works.
causal_verdict <- function(study, label_of) {
  set <- if (length(study$adjust)) paste(label_of(study$adjust), collapse = ", ") else "nothing"
  if (study$sufficient) {
    return(paste0("Adjusting for ", set, " is sufficient: every biasing path from ",
                  label_of(study$exposure), " to ", label_of(study$outcome), " is blocked."))
  }
  bits <- c(
    if (study$open_biasing) paste0(study$open_biasing, if (study$open_biasing == 1) " biasing path stays open" else " biasing paths stay open"),
    if (length(study$mediators)) paste0(paste(label_of(study$mediators), collapse = ", "),
                                        " is a consequence of the exposure and should not be adjusted for")
  )
  paste0("Adjusting for ", set, " is not sufficient: ", paste(bits, collapse = ", and "), ".")
}
