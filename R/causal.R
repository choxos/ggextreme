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
                     family = "Lato") {
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
  canvas <- graph_canvas(all_x, all_y, title, caption, capitalize(keys),
                         roles$colors[keys], legend_title, dims, family)
  px <- canvas$px
  py <- canvas$py

  # What the hover card and the click panel say about each node and arrow.
  node_refs <- split_references(nodes[["references"]], nrow(nodes))
  node_fields <- field_values(nodes, c("name", "label", "role", "rationale",
                                       "references", "x", "y"))
  flat <- gsub("\n", " ", labels, fixed = TRUE)
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
  edge_df <- do.call(rbind, lapply(which(drawn), function(k) {
    data.frame(x = px(routes[[k]][, 1]), y = py(routes[[k]][, 2]),
               id = edge_ids[k], tooltip = edge_tip[k], onclick = edge_click[k],
               stringsAsFactors = FALSE)
  }))

  p <- ggplot() +
    edge_layers(edge_df, dims) +
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
    graph_frame(canvas, family)

  structure(
    list(plot = p, width = canvas$width / 72, height = canvas$height / 72,
         title = title,
         nodes = nodes, edges = edges),
    class = c("ggcausal", "ggx_graph")
  )
}

edge_layers <- function(df, dims) {
  if (is.null(df) || !nrow(df)) return(NULL)
  list(
    ggiraph::geom_path_interactive(
      data = df,
      aes(x = .data$x, y = .data$y, group = .data$id, data_id = .data$id),
      colour = graph_ink$edge, linewidth = dims$edge_pt / .pt,
      linejoin = "mitre", lineend = "butt",
      arrow = grid::arrow(angle = 20, length = grid::unit(dims$arrow_pt, "bigpts"),
                          type = "closed"),
      arrow.fill = graph_ink$edge
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
