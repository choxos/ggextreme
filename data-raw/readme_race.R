# The animation shown in README.md and on the package website.
#
# Run with: source("data-raw/readme_race.R")

library(ggextreme)

key <- unique(clefts_qci[c("country", "iso")])
flags <- stats::setNames(race_flags(key$iso), key$country)

race <- ggrace(
  clefts_qci, qci, country, year,
  top_n = 15,
  duration = 15,
  fps = 20,
  end_pause = 1.5,
  images = flags,
  group = region,
  legend_title = "Region",
  breaks = scales::breaks_extended(6),
  title = "Quality of care for orofacial clefts",
  caption = "Source: Sofi-Mahmudi et al. 2025, PLOS ONE 20(1): e0317267",
  width = 640
)

dir.create("man/figures", recursive = TRUE, showWarnings = FALSE)
animate_race(race, "man/figures/README-race.gif")

cat(round(file.size("man/figures/README-race.gif") / 1024^2, 2), "MB\n")
