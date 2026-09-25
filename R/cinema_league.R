#' Draw a league table with a CINeMA confidence profile in every cell
#'
#' Draws every pairwise estimate of a network meta-analysis as a grid laid out
#' like [ggleague()] and [netmeta::netleague()]: each cell compares the
#' treatment that comes first on the diagonal with the one that comes later,
#' network estimates sit below the diagonal and direct estimates above it.
#' Under each network estimate six small marks give its judgment in each
#' CINeMA domain, left to right within-study bias, reporting bias,
#' indirectness, imprecision, heterogeneity and incoherence, colored by no,
#' some or major concerns. The marks are never added into a score.
#'
#' Hovering over a cell lists its judgments; clicking it opens a panel under
#' the table with every domain's judgment, the reason for it and whether it
#' was computed by a rule or given by you, the estimate against the range of
#' little difference, and a bar of the contribution of each study to the
#' estimate, colored by its risk of bias. The cells can also be reached with
#' the keyboard: Tab moves between them and Enter or Space opens one.
#' Hovering over a treatment on the diagonal lights its row and column.
#'
#' A mark drawn hollow is a domain not judged, such as reporting bias when you
#' gave no judgment for it. A round mark in the incoherence place means there
#' was no local test for that comparison, only the global test of the whole
#' network, as CINeMA prescribes.
#'
#' @param x Judgments from [cinema_judge()], or a network meta-analysis from
#'   [netmeta::netmeta()], which is then judged with the arguments in `...`.
#' @param ... When `x` is a netmeta fit, arguments for [cinema_judge()]: the
#'   study judgments `rob` and `indirectness`, `reporting`, `threshold`,
#'   `rule`, your own `judgments`, `small_values` and `order`. Giving only
#'   `judgments`, such as a report exported from the CINeMA web application,
#'   draws your judgments as they are.
#' @param title,caption Title above the table and note below it. The default
#'   caption says which estimates sit on each side of the diagonal and, domain
#'   by domain, which judgments were computed by which rule and which are
#'   yours. A caption you give is added above it.
#' @param family Font family. The package ships Lato and registers it on load.
#'
#' @inheritSection cinema_judge Rules
#' @inheritSection cinema_judge Sources
#'
#' @return An object of class `cinema_league`, which prints as an interactive
#'   widget. Use [graph_widget()], [graph_plot()] or [graph_save()] for the
#'   widget, a static ggplot or a file. The field `judgments` holds the
#'   result of [cinema_judge()] the table was drawn from.
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
#'   cinema_league(nma, rob = rob, threshold = 1.25,
#'                 small_values = "undesirable",
#'                 caption = "Illustrative judgments, not published assessments.")
#' }
#' }
cinema_league <- function(x, ..., title = NULL, caption = NULL, family = "Lato") {
  cn <- cinema_input(x, ..., fn = "cinema_league")
  dims <- utils::modifyList(league_dims, list(cell_h = 60, mark = 7.5, mark_gap = 3.5, key_pt = 9,
                                              tiny_pt = 7.5))
  order <- cn$order
  k <- length(order)
  ratio <- cn$ratio
  pr <- cn$pairs
  pct <- paste0(format(100 * cn$level), "%")
  width <- function(s, pt, bold = FALSE) max(text_width_card(s, pt, family, 1, bold = bold))
  intervals <- c(paste0("(", cinema_interval(pr$lo, pr$hi, ratio), ")"),
                 paste0("(", cinema_interval(pr$d_lo, pr$d_hi, ratio), ")")[pr$k > 0],
                 "no direct studies")
  marks_w <- 6 * dims$mark + 5 * dims$mark_gap
  cell_w <- max(dims$cell_min, width(intervals, dims$small_pt) + 2 * dims$cell_pad,
                marks_w + 2 * dims$cell_pad,
                max(vapply(strsplit(order, " ", fixed = TRUE), function(w) width(w, dims$name_pt, TRUE),
                           numeric(1))) + 2 * dims$cell_pad)
  names_wrapped <- lapply(order, wrap_words, width = cell_w - 2 * dims$cell_pad,
                          pt = dims$name_pt, family = family, bold = TRUE)
  cell_h <- max(dims$cell_h, max(lengths(names_wrapped)) * dims$name_pt * 1.2 + dims$small_pt * 1.4 + 12)
  pitch_x <- cell_w + dims$cell_gap
  pitch_y <- cell_h + dims$cell_gap
  grid_w <- k * pitch_x - dims$cell_gap
  grid_w_min <- max(grid_w, 420)

  # The key above the grid: what the marks are and what their colors mean.
  key_lines <- cinema_wrap(paste0("Six marks under each network estimate give its judgment in each CINeMA domain, ",
                                  "left to right: 1 within-study bias, 2 reporting bias (undetected or suspected, drawn as no or some concerns), 3 indirectness, ",
                                  "4 imprecision, 5 heterogeneity, 6 incoherence. They are never added into a score."),
                           grid_w_min, dims$key_pt, family)
  key_items <- c("No concerns", "Some concerns", "Major concerns", "Not judged",
                 "Round: incoherence from the global test only")
  item_w <- text_width_card(key_items, dims$key_pt, family, 1) + 13
  key_x <- key_row <- numeric(length(key_items))
  at <- 0
  row <- 0
  for (j in seq_along(key_items)) {
    if (at > 0 && at + item_w[j] > grid_w_min) {
      row <- row + 1
      at <- 0
    }
    key_x[j] <- at
    key_row[j] <- row
    at <- at + item_w[j] + 14
  }
  line_h <- dims$key_pt * 1.35
  swatch_top <- length(key_lines) * line_h + 6
  key_h <- swatch_top + (max(key_row) + 1) * 16 + 10
  cell_x <- function(b) (b - 1) * pitch_x
  cell_y <- function(a) key_h + (a - 1) * pitch_y
  grid_h <- key_h + k * pitch_y - dims$cell_gap

  cap <- c(caption,
           paste0("Below the diagonal: network estimates with their judgments; above it: direct estimates. ",
                  "Each compares the treatment that comes first with the one that comes later, as ",
                  cinema_article(tolower(cn$measure)), " with its ", pct, " CI. ",
                  if (!is.null(cn$threshold)) paste0("Range of little difference: ",
                                                     paste(format(cn$threshold, digits = 3), collapse = " to "), ". ") else ""),
           cinema_sources_text(cn))
  cap <- cinema_wrap(cap, grid_w_min, graph_dims$caption_pt, family)
  page <- graph_canvas(c(0, grid_w_min), c(0, grid_h), title, cap, character(0), character(0), NULL,
                       dims, family)
  px <- page$px
  py <- page$py
  texts <- function(label, x, y, pt, color, hjust = 0.5, face = "plain") {
    if (!length(label)) return(empty_texts())
    data.frame(x = px(x), y = py(y), label = label, size = pt / .pt, colour = color, hjust = hjust,
               fontface = face, stringsAsFactors = FALSE)
  }

  domain_words <- function(i) {
    vapply(names(cinema_domain_names), function(d) {
      lv <- cn$domains[[d]]$level[i]
      if (is.na(lv)) "Not judged" else if (d == "reporting") cinema_reporting_words[lv + 1] else
        cinema_concern_words[lv + 1]
    }, character(1))
  }
  estimate <- function(i) cinema_estimate(pr$te[i], pr$lo[i], pr$hi[i], ratio)
  direct <- function(i) if (pr$k[i] > 0) cinema_estimate(pr$d_te[i], pr$d_lo[i], pr$d_hi[i], ratio) else "none"
  tip_network <- function(i) {
    paste0('<div class="ggx-tip-title">', esc(pr$a[i]), " vs ", esc(pr$b[i]), "</div>",
           '<div class="ggx-tip-sub">Network estimate, ', esc(tolower(cn$measure)), " (", pct, " CI)</div>",
           tip_rows(c("Estimate" = estimate(i), stats::setNames(domain_words(i), unname(cinema_domain_names)))),
           '<div class="ggx-tip-hint">Click or press Enter for the reason behind each judgment.</div>')
  }
  tip_direct <- function(i) {
    st <- cn$direct[[i]]
    paste0('<div class="ggx-tip-title">', esc(pr$a[i]), " vs ", esc(pr$b[i]), "</div>",
           '<div class="ggx-tip-sub">Direct estimate, ', esc(tolower(cn$measure)), " (", pct, " CI)</div>",
           if (length(st)) tip_rows(c("Direct" = direct(i),
                                      stats::setNames(cinema_names(st), if (length(st) == 1) "Study" else "Studies"))) else
             '<div class="ggx-tip-body">No study compares these treatments directly.</div>',
           '<div class="ggx-tip-hint">Click or press Enter for the judgments of the network estimate.</div>')
  }

  shapes <- NULL
  marks <- NULL
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
        tip <- paste0('<div class="ggx-tip-title">', esc(order[a]), "</div>",
                      tip_rows(c("P-score" = formatC(cn$pscore[[order[a]]], format = "f", digits = 2))),
                      '<div class="ggx-tip-body">The P-score is the mean certainty that this treatment is better ',
                      "than each of the others. Its row and column are lit.</div>")
        shapes <- rbind(shapes, data.frame(x = px(box$x), y = py(box$y), group = paste0("g", a, "_", b), id = id,
                                           tooltip = tip, onclick = "", fill = graph_ink$faint, alpha = 1,
                                           border = graph_ink$faint, stringsAsFactors = FALSE))
        lines <- names_wrapped[[a]]
        block <- length(lines) * dims$name_pt * 1.2 + dims$small_pt * 1.4
        ys <- cy - block / 2 + dims$name_pt * 0.6 + (seq_along(lines) - 1) * dims$name_pt * 1.2
        cell_text <- rbind(cell_text, data.frame(
          x = px(cx), y = py(c(ys, max(ys) + dims$name_pt * 0.6 + dims$small_pt * 0.9)),
          label = c(lines, paste("P-score", formatC(cn$pscore[[order[a]]], format = "f", digits = 2))),
          size = c(rep(dims$name_pt, length(lines)), dims$small_pt) / .pt,
          colour = c(rep(graph_ink$title, length(lines)), graph_ink$muted),
          fontface = c(rep("bold", length(lines)), "plain"), id = id, tooltip = tip, onclick = "",
          stringsAsFactors = FALSE))
        next
      }
      i <- match(pair_key(order[min(a, b)], order[max(a, b)]), pr$key)
      id <- paste0("c", a, "_", b)
      network <- a > b
      tip <- if (network) tip_network(i) else tip_direct(i)
      click <- pin_js(id, cinema_league_panel(cn, i, network))
      shapes <- rbind(shapes, data.frame(x = px(box$x), y = py(box$y), group = paste0("g", a, "_", b), id = id,
                                         tooltip = tip, onclick = click, fill = graph_ink$page, alpha = 1,
                                         border = if (network) graph_ink$plain_border else graph_ink$ghost,
                                         stringsAsFactors = FALSE))
      if (network) {
        cell_text <- rbind(cell_text, data.frame(
          x = px(cx), y = py(c(cy - 16, cy - 2)),
          label = c(cinema_num(pr$te[i], ratio), paste0("(", cinema_interval(pr$lo[i], pr$hi[i], ratio), ")")),
          size = c(dims$value_pt, dims$small_pt) / .pt, colour = c(graph_ink$title, graph_ink$text),
          fontface = c("bold", "plain"), id = id, tooltip = tip, onclick = click, stringsAsFactors = FALSE))
        sx <- cx - marks_w / 2
        my <- cy + 13
        for (j in seq_along(cinema_domain_names)) {
          d <- names(cinema_domain_names)[j]
          lv <- cn$domains[[d]]$level[i]
          x1 <- sx + (j - 1) * (dims$mark + dims$mark_gap)
          round_mark <- d == "incoherence" && pr$type[i] != "mixed" && !is.na(lv) &&
            cn$domains[[d]]$source[i] == "Computed: global design by treatment test"
          if (round_mark) {
            t <- seq(0, 2 * pi, length.out = 17)
            m <- data.frame(x = x1 + dims$mark / 2 + dims$mark / 2 * cos(t), y = my + dims$mark / 2 * sin(t))
          } else {
            m <- data.frame(x = c(x1, x1 + dims$mark, x1 + dims$mark, x1),
                            y = my + c(-1, -1, 1, 1) * dims$mark / 2)
          }
          marks <- rbind(marks, data.frame(
            x = px(m$x), y = py(m$y), group = paste0(id, "m", j), id = id, tooltip = tip, onclick = click,
            fill = if (is.na(lv)) graph_ink$page else cinema_ink$level[lv + 1],
            border = if (is.na(lv)) graph_ink$muted else NA, stringsAsFactors = FALSE))
        }
      } else if (pr$k[i] > 0) {
        cell_text <- rbind(cell_text, data.frame(
          x = px(cx), y = py(c(cy - 19, cy - 6, cy + 7, cy + 19)),
          label = c("direct", cinema_num(pr$d_te[i], ratio),
                    paste0("(", cinema_interval(pr$d_lo[i], pr$d_hi[i], ratio), ")"),
                    paste(pr$k[i], if (pr$k[i] == 1) "study" else "studies")),
          size = c(dims$tiny_pt, dims$value_pt, dims$small_pt, dims$tiny_pt) / .pt,
          colour = c(graph_ink$muted, graph_ink$text, graph_ink$muted, graph_ink$muted),
          fontface = "plain", id = id, tooltip = tip, onclick = click, stringsAsFactors = FALSE))
      } else {
        cell_text <- rbind(cell_text, data.frame(
          x = px(cx), y = py(cy), label = "no direct studies", size = dims$small_pt / .pt,
          colour = graph_ink$muted, fontface = "italic", id = id, tooltip = tip, onclick = click,
          stringsAsFactors = FALSE))
      }
    }
  }

  # The key.
  key_text <- rbind(
    texts(key_lines, 0, (seq_along(key_lines) - 0.5) * line_h, dims$key_pt, graph_ink$text, hjust = 0),
    texts(key_items, key_x + 13, swatch_top + key_row * 16 + 5, dims$key_pt, graph_ink$text, hjust = 0)
  )
  key_marks <- do.call(rbind, lapply(seq_along(key_items), function(j) {
    x1 <- key_x[j]
    y1 <- swatch_top + key_row[j] * 16 + 5
    if (j == 5) {
      t <- seq(0, 2 * pi, length.out = 17)
      m <- data.frame(x = x1 + 4.5 + 4.5 * cos(t), y = y1 + 4.5 * sin(t))
    } else {
      m <- data.frame(x = c(x1, x1 + 9, x1 + 9, x1), y = y1 + c(-4.5, -4.5, 4.5, 4.5))
    }
    data.frame(x = px(m$x), y = py(m$y), group = paste0("key", j),
               fill = c(cinema_ink$level, graph_ink$page, graph_ink$muted)[j],
               border = c(NA, NA, NA, graph_ink$muted, NA)[j], stringsAsFactors = FALSE)
  }))

  shapes$hover <- paste0("stroke:", hover_ink, ";stroke-width:1.5px;")
  p <- ggplot() +
    ggiraph::geom_polygon_interactive(
      data = shapes,
      aes(x = .data$x, y = .data$y, group = .data$group, fill = .data$fill, colour = .data$border,
          data_id = .data$id, tooltip = .data$tooltip, onclick = .data$onclick, hover_css = .data$hover),
      linewidth = 0.8 / .pt) +
    ggiraph::geom_polygon_interactive(
      data = marks,
      aes(x = .data$x, y = .data$y, group = .data$group, fill = .data$fill, colour = .data$border,
          data_id = .data$id, tooltip = .data$tooltip, onclick = .data$onclick),
      linewidth = 1 / .pt) +
    ggiraph::geom_text_interactive(
      data = cell_text,
      aes(x = .data$x, y = .data$y, label = .data$label, data_id = .data$id, tooltip = .data$tooltip,
          onclick = .data$onclick),
      size = cell_text$size, colour = cell_text$colour, fontface = cell_text$fontface, family = family) +
    geom_polygon(data = key_marks, aes(x = .data$x, y = .data$y, group = .data$group, fill = .data$fill,
                                       colour = .data$border), linewidth = 1 / .pt) +
    draw_text(key_text, family) +
    graph_frame(page, family)

  structure(
    list(plot = p, width = page$width / 72, height = page$height / 72, title = title,
         on_render = "ggextremeLeague(el, null); ggextremeCinemaKeys(el, 'c');",
         judgments = cn),
    class = c("cinema_league", "ggx_graph")
  )
}

# The panel a league cell opens: the estimate, its judgments with reasons
# and sources, and where the estimate's evidence comes from.
cinema_league_panel <- function(cn, i, network) {
  pr <- cn$pairs
  ratio <- cn$ratio
  pct <- paste0(format(100 * cn$level), "%")
  evidence <- switch(pr$type[i],
                     mixed = paste0("direct and indirect evidence; ", pr$k[i], if (pr$k[i] == 1) " study compares" else
                       " studies compare", " them directly"),
                     direct = paste0("direct evidence only, from ", pr$k[i], if (pr$k[i] == 1) " study" else " studies"),
                     indirect = "indirect evidence only; no study compares them directly")
  kv <- c(paste0("Network estimate (", pct, " CI)"), cinema_estimate(pr$te[i], pr$lo[i], pr$hi[i], ratio))
  kv <- rbind(kv, c("Direct estimate", if (pr$k[i] > 0) cinema_estimate(pr$d_te[i], pr$d_lo[i], pr$d_hi[i], ratio) else
    "none: no study compares them directly"))
  if (is.finite(pr$plo[i])) kv <- rbind(kv, c(paste0(pct, " prediction interval"),
                                             cinema_interval(pr$plo[i], pr$phi[i], ratio)))
  reading <- ""
  if (!is.null(cn$threshold)) {
    lim <- log_or(cn$threshold, ratio)
    m <- cinema_zones(pr$lo[i], pr$hi[i], lim[1], lim[2])
    reading <- paste0("<p>Against a range of little difference from ", esc(paste(format(cn$threshold, digits = 3), collapse = " to ")),
                      ", the ", pct, " CI is compatible with ", esc(cinema_reading(m, cn$small_values, long = TRUE)),
                      " for ", esc(pr$a[i]), " against ", esc(pr$b[i]), ".</p>")
  }
  rows <- vapply(seq_along(cinema_domain_names), function(j) {
    d <- names(cinema_domain_names)[j]
    lv <- cn$domains[[d]]$level[i]
    words <- if (is.na(lv)) "Not judged" else if (d == "reporting") cinema_reporting_words[lv + 1] else
      cinema_concern_words[lv + 1]
    src <- cinema_source_short(cn$domains[[d]]$source[i])
    paste0('<tr><th scope="row">', j, " ", esc(cinema_domain_names[[j]]), "</th><td>", cinema_chip(lv, words),
           '<div class="ggx-cn-src">', esc(src), "</div></td><td>", esc(cn$domains[[d]]$reason[i]), "</td></tr>")
  }, character(1))
  overall <- ""
  if (!is.null(cn$overall)) {
    o <- cn$overall[cn$overall$key == pr$key[i], , drop = FALSE]
    if (nrow(o) && (!is.na(o$confidence[1]) || !is.na(o$note[1]))) {
      overall <- paste0("<p><b>Your overall rating:</b> ", esc(if (is.na(o$confidence[1])) "not given" else o$confidence[1]),
                        if (!is.na(o$note[1]) && nzchar(o$note[1])) paste0(". ", esc(o$note[1])) else "", "</p>")
    }
  }
  bar <- cinema_bar_html(cn, pr$key[i], "rob")
  paste0('<div class="ggx-title">', esc(pr$a[i]), " vs ", esc(pr$b[i]), "</div>",
         '<div class="ggx-sub">', if (network) "Network estimate" else
           "Direct estimate selected; the judgments belong to the network estimate", ", from ", esc(evidence), "</div>",
         '<dl class="ggx-cn-kv">', paste0("<div><dt>", esc(kv[, 1]), "</dt><dd>", esc(kv[, 2]), "</dd></div>", collapse = ""),
         "</dl>", reading, overall,
         '<div class="ggx-table ggx-cn-domains"><table><thead><tr><th scope="col">Domain</th>',
         '<th scope="col">Judgment</th><th scope="col">Why</th></tr></thead><tbody>',
         paste(rows, collapse = ""), "</tbody></table></div>",
         if (nzchar(bar)) paste0('<div class="ggx-refs-head">Where the estimate\'s evidence comes from, by risk of bias</div>',
                                 bar, '<p class="ggx-note">Each block is one study, as wide as its share of the estimate; ',
                                 "high risk of bias is hatched. ", esc(cn$contribution_method), "</p>") else "",
         '<p class="ggx-note">The six domains are related: one study at high risk of bias can raise concerns in ',
         "more than one, so weigh them together and do not count the same concern twice.</p>")
}
