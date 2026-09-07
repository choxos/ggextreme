# Quality of Care Index for orofacial clefts, fifteen countries, 1990 to 2019.
#
# Source: Sofi-Mahmudi A, Shamsoddin E, Khademioore S, Khazaei Y, Vahdati A,
# Tovani-Palone MR (2025). Global, regional, and national survey on burden and
# Quality of Care Index (QCI) of orofacial clefts: Global burden of disease
# systematic analysis 1990-2019. PLOS ONE 20(1): e0317267.
#
# The full country panel lives in documentation/data, which is not tracked.
# Fifteen countries were chosen for spread across continents, a wide range of
# scores and several rank changes over the period.
#
# Run with: source("data-raw/clefts_qci.R")

raw <- read.csv("documentation/data/pca_in_years_all_countries.csv")

keep <- c(
  "Brazil" = "br", "China" = "cn", "Turkey" = "tr", "Mexico" = "mx",
  "Indonesia" = "id", "Nigeria" = "ng", "India" = "in",
  "Iran (Islamic Republic of)" = "ir", "Kenya" = "ke", "South Africa" = "za",
  "Egypt" = "eg", "Republic of Korea" = "kr",
  "United States of America" = "us", "Chile" = "cl", "Germany" = "de"
)
short <- c(
  "Iran (Islamic Republic of)" = "Iran",
  "Republic of Korea" = "South Korea",
  "United States of America" = "United States"
)

d <- raw[raw$location_name %in% names(keep), ]
clefts_qci <- data.frame(
  country = ifelse(d$location_name %in% names(short),
                   short[d$location_name], d$location_name),
  iso = unname(keep[d$location_name]),
  year = as.integer(d$year),
  qci = d$pca_score,
  stringsAsFactors = FALSE
)
clefts_qci <- clefts_qci[order(clefts_qci$country, clefts_qci$year), ]
rownames(clefts_qci) <- NULL

stopifnot(
  nrow(clefts_qci) == 15 * 30,
  !anyNA(clefts_qci),
  length(unique(clefts_qci$country)) == 15,
  identical(range(clefts_qci$year), c(1990L, 2019L))
)

save(clefts_qci, file = "data/clefts_qci.rda", compress = "xz", version = 3)
