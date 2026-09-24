# The choropleth maps and animation shown in README.md and the vignette.
# GitHub cannot run the widget, so the README shows these and links to the
# live article.
#
# Run with: source("data-raw/readme_map.R")

library(ggextreme)

qci <- clefts_qci_world
first <- qci$qci[qci$year == 1990][match(qci$iso3, qci$iso3[qci$year == 1990])]
qci$change <- qci$qci - first

m <- ggchoropleth(qci, iso3, year,
                  values = c("Quality of Care Index" = "qci",
                             "Change since 1990" = "change"),
                  title = "Quality of care for orofacial clefts",
                  caption = "Source: Sofi-Mahmudi et al. (2025), PLOS ONE.")
graph_save(m, "man/figures/README-map.png", res = 200)
animate_choropleth(m, "man/figures/README-map.gif", step = 0.4, end_pause = 2,
                   fps = 15, res = 90)
