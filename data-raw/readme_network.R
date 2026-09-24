# The static network plot shown in README.md. GitHub cannot run the widget,
# so the README shows this image and links to the live article.
#
# Run with: source("data-raw/readme_network.R")

library(ggextreme)

net <- ggnma(
  psoriasis_nma, study, treatment, n = n, group = class,
  legend_title = "Class",
  title = "Treatments for plaque psoriasis"
)
graph_save(net, "man/figures/README-network.png", res = 200)
