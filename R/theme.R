#' Colors used by the bar chart race
#'
#' A qualitative palette sampled straight off the reference recording: muted
#' teals, greens, blues, purples and warm earth tones that stay readable side
#' by side in a dense stack of bars.
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
  base <- c(
    "#22928F", "#B66399", "#BF6662", "#757CC6", "#4BAAA9",
    "#C36284", "#568E4F", "#238FB2", "#5D7FCA", "#7A5178",
    "#9B4979", "#249195", "#C16275", "#398965", "#B16F51",
    "#C28A4A", "#89AC70", "#6A8B8B", "#9887BD", "#598390",
    "#C76253", "#2B8BB5", "#A87546", "#2588C6", "#717171",
    "#957D3C"
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
