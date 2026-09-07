# The chart layout, in card units.
#
# Positions are given on a card 710 units wide. Distances stay in card units
# so the design scales with the output width, and anything below the bar block
# is an offset from the bottom of that block so the card grows with `top_n`.
# Font sizes are in the same units and are converted to points at draw time.
#
# Text is positioned by the vertical center of its ink, which is what
# `vjust = 0.5` gives, so no baseline arithmetic is needed anywhere.

CARD_W <- 710
BAR_PITCH <- 569 / 31

race_layout <- function(top_n) {
  bars_top <- 112
  bars_bottom <- bars_top + top_n * BAR_PITCH

  list(
    card_w = CARD_W,
    card_h = bars_bottom + 138,
    page_pad = 12,

    content_l = 25,
    content_r = 684,

    title_mid = 43.5,
    title_pt = 31,
    rule_y = 72,
    rule_h = 2,

    axis_mid = 92,
    axis_pt = 16,

    bars_top = bars_top,
    bars_bottom = bars_bottom,
    pitch = BAR_PITCH,
    bar_h = 17,
    bar_x0 = 125.5,
    bar_x1 = 545,
    grid_w = 1,
    grid_dx = 1,

    image_gap = 1,
    name_gap = 11.5,
    name_pt = 10.5,
    value_gap = 12,
    value_pt = 10,
    label_dy = -1.5,

    year_right = 683,
    year_mid = bars_bottom - 37,
    year_pt = 72,

    button_x = 65.5,
    button_y = bars_bottom + 30.5,
    button_r = 25.25,
    button_bar_w = 8,
    button_bar_h = 22,
    button_bar_gap = 5,

    time_l = 115.5,
    time_r = 684,
    time_y = bars_bottom + 41,
    time_h = 2,
    tick_w = 2,
    tick_minor = 4,
    tick_major = 10,
    tick_end = 13,
    time_label_mid = bars_bottom + 63.5,
    time_label_pt = 13.5,

    foot_rule_y = bars_bottom + 74.5,
    source_mid = bars_bottom + 100,
    source_pt = 18.5
  )
}

# Ink colors.
race_ink <- list(
  page = "#F7F7F7",
  shadow = "#E4E4E4",
  card = "#FFFFFF",
  title = "#1F1F1F",
  rule = "#D9D9D9",
  axis = "#262626",
  grid = "#DADADA",
  name = "#2C2C2C",
  value = "#494949",
  year = "#C6C6C6",
  button = "#2C2C2C",
  button_icon = "#FFFFFF",
  timeline = "#A1A1A1",
  marker = "#A3A3A3",
  source = "#242424"
)
