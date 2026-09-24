# Quality of Care Index for orofacial clefts, every country, 1990 to 2019.
#
# Source: Sofi-Mahmudi A, Shamsoddin E, Khademioore S, Khazaei Y, Vahdati A,
# Tovani-Palone MR (2025). Global, regional, and national survey on burden and
# Quality of Care Index (QCI) of orofacial clefts: Global burden of disease
# systematic analysis 1990-2019. PLOS ONE 20(1): e0317267.
#
# The full panel lives in documentation/data, which is not tracked. It also
# holds world regions, WHO regions, World Bank income groups and SDI groups,
# which are dropped here, leaving the countries and territories. Each keeps
# the GBD name, with its ISO 3166-1 alpha-3 code from the map ggchoropleth()
# draws.
#
# Run with: source("data-raw/clefts_qci_world.R")

pkgload::load_all(quiet = TRUE)

raw <- read.csv("documentation/data/pca_in_years_all_countries.csv",
                stringsAsFactors = FALSE)
# One name was saved with its UTF-8 bytes encoded a second time.
twice <- grepl("Ã", raw$location_name)
raw$location_name[twice] <- iconv(raw$location_name[twice], "UTF-8", "latin1")
Encoding(raw$location_name) <- "UTF-8"

found <- world_match(raw$location_name)
d <- raw[!is.na(found$id), ]
clefts_qci_world <- data.frame(
  country = d$location_name,
  iso3 = found$id[!is.na(found$id)],
  year = as.integer(d$year),
  qci = d$pca_score,
  stringsAsFactors = FALSE
)
clefts_qci_world <- clefts_qci_world[order(clefts_qci_world$country, clefts_qci_world$year), ]
rownames(clefts_qci_world) <- NULL

left_out <- sort(unique(raw$location_name[is.na(found$id)]))
stopifnot(
  "Côte d'Ivoire" %in% clefts_qci_world$country,
  all(c("Bolivia (Plurinational State of)", "Iran (Islamic Republic of)",
        "Taiwan (Province of China)", "Venezuela (Bolivarian Republic of)") %in%
        clefts_qci_world$country),
  length(unique(clefts_qci_world$iso3)) == length(unique(clefts_qci_world$country)),
  !anyDuplicated(clefts_qci_world[c("iso3", "year")]),
  !anyNA(clefts_qci_world),
  identical(range(clefts_qci_world$year), c(1990L, 2019L)),
  !any(c("Global", "North America", "High SDI", "European Region") %in% clefts_qci_world$country)
)
cat(length(unique(clefts_qci_world$iso3)), "countries; left out:",
    paste(left_out, collapse = "; "), "\n")

save(clefts_qci_world, file = "data/clefts_qci_world.rda", compress = "xz", version = 3)
