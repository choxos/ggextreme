#' Colors used by the bar chart race
#'
#' A qualitative palette of muted teals, greens, blues, purples and warm
#' earth tones, chosen to stay readable side by side in a dense stack of bars.
#' Consecutive colours are far apart in hue, so the first few remain easy to
#' tell apart when only a handful are used, as with `group` in [ggrace()].
#'
#' @param n Number of colors to return. The palette is recycled when `n` is
#'   larger than the number of base colors.
#'
#' @return A character vector of `n` hex colors.
#' @export
#'
#' @examples
#' race_palette(5)
race_palette <- function(n = 26) {
  # Ordered so that colours taken in sequence stay far apart in hue, which
  # matters when a handful of groups share the top of the palette.
  base <- c(
    "#22928F", "#B66399", "#757CC6", "#568E4F", "#C76253",
    "#C28A4A", "#7A5178", "#4BAAA9", "#C16275", "#2588C6",
    "#89AC70", "#B16F51", "#717171", "#9B4979", "#249195",
    "#5D7FCA", "#398965", "#BF6662", "#957D3C", "#9887BD",
    "#598390", "#C36284", "#2B8BB5", "#6A8B8B", "#A87546",
    "#238FB2"
  )
  rep_len(base, n)
}

#' The bare theme a race frame is drawn on
#'
#' A race frame paints its own title, axis, timeline and footer as layers in
#' one canvas coordinate system, so the theme only has to supply the page
#' color and get everything else out of the way.
#'
#' @param page Background color of the page behind the card.
#'
#' @return A [ggplot2::theme] object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_race()
theme_race <- function(page = race_ink$page) {
  theme_void() %+replace%
    theme(
      plot.background = element_rect(fill = page, color = NA),
      panel.background = element_rect(fill = page, color = NA),
      plot.margin = margin(0, 0, 0, 0),
      legend.position = "none",
      complete = TRUE
    )
}
