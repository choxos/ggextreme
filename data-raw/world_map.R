# The world map ggchoropleth() draws by default, and the table it matches
# country names against.
#
# Source: Natural Earth 1:50m admin 0 countries, which are in the public
# domain (https://www.naturalearthdata.com/about/terms-of-use/), as
# distributed in the rnaturalearthdata package. Antarctica is dropped, the
# outlines are projected to the Equal Earth projection and simplified to
# about 20 km, finer than a point at the sizes the maps are drawn. The result
# is stored as plain polygon rings, so drawing a world map needs neither sf
# nor rnaturalearthdata.
#
# Somaliland and Northern Cyprus take the codes of Somalia and Cyprus, as
# country level health statistics report them; Kosovo takes XKX.
#
# Run with: source("data-raw/world_map.R")

library(sf)
sf_use_s2(FALSE)
pkgload::load_all(quiet = TRUE)

w <- rnaturalearthdata::countries50
w <- w[w$adm0_a3 != "ATA", ]
w$iso3 <- ifelse(w$iso_a3_eh == "-99", w$adm0_a3, w$iso_a3_eh)
w$iso3[w$adm0_a3 == "SOL"] <- "SOM"
w$iso3[w$adm0_a3 == "CYN"] <- "CYP"
w$iso3[w$adm0_a3 == "KOS"] <- "XKX"
stopifnot(!any(w$iso3 == "-99"))

p <- st_transform(w, "+proj=eqearth +datum=WGS84")
p <- st_simplify(p, dTolerance = 20000, preserveTopology = TRUE)
p <- st_cast(st_make_valid(p), "MULTIPOLYGON")

xy <- st_coordinates(p)
world_geometry <- data.frame(
  id = p$iso3[xy[, "L3"]],
  x = round(xy[, "X"] / 1000, 1),
  y = round(xy[, "Y"] / 1000, 1),
  piece = paste(xy[, "L3"], xy[, "L2"], sep = "_"),
  ring = as.integer(xy[, "L1"]),
  stringsAsFactors = FALSE
)
world_geometry$piece <- as.integer(factor(world_geometry$piece,
                                          levels = unique(world_geometry$piece)))

# Every English name Natural Earth records for a country, normalized as
# ggchoropleth() normalizes the names it is given, with the code it belongs
# to. Countries come before the territories that share their code, and the
# sovereign's name is left out, so Denmark does not also mean Greenland.
w <- w[order(w$adm0_a3 != w$iso3), ]
norm <- normalize_region
fields <- c("name_long", "name", "admin", "formal_en", "name_sort", "brk_name",
            "geounit", "subunit", "name_ciawf", "name_alt")
world_names <- do.call(rbind, lapply(fields, function(f) {
  data.frame(key = norm(w[[f]]), id = w$iso3, stringsAsFactors = FALSE)
}))
world_names <- world_names[!is.na(world_names$key) & nzchar(world_names$key), ]
world_names <- world_names[!duplicated(world_names$key), ]
aliases <- c(
  turkiye = "TUR", cotedivoire = "CIV", ivorycoast = "CIV", czechrepublic = "CZE",
  swaziland = "SWZ", macedonia = "MKD", burma = "MMR", capeverde = "CPV",
  easttimor = "TLS", vatican = "VAT", holysee = "VAT", unitedstates = "USA",
  usa = "USA", uk = "GBR", greatbritain = "GBR", russia = "RUS", korea = "KOR",
  southkorea = "KOR", northkorea = "PRK", laos = "LAO", syria = "SYR",
  vietnam = "VNM", palestine = "PSE", micronesia = "FSM", tanzania = "TZA"
)
world_names <- rbind(
  world_names[!world_names$key %in% names(aliases), ],
  data.frame(key = names(aliases), id = unname(aliases), stringsAsFactors = FALSE)
)
world_labels <- stats::setNames(w$name_long, w$iso3)
world_labels <- world_labels[!duplicated(names(world_labels))]
two <- ifelse(w$adm0_a3 == "NAM", "NA", w$iso_a2_eh)
world_iso2 <- stats::setNames(w$iso3, two)[!is.na(two) & two != "-99"]
world_iso2 <- world_iso2[!duplicated(names(world_iso2))]
stopifnot(world_labels[["SOM"]] == "Somalia", world_labels[["CYP"]] == "Cyprus",
          world_names$id[world_names$key == "congo"] == "COG",
          world_iso2[["NA"]] == "NAM", world_iso2[["GB"]] == "GBR")

cat(nrow(world_geometry), "vertices,", length(unique(world_geometry$id)), "countries\n")
usethis::use_data(world_geometry, world_names, world_labels, world_iso2, internal = TRUE,
                  overwrite = TRUE, compress = "xz")
