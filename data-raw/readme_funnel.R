# The funnel plot shown in README.md. GitHub cannot run the widget, so the
# README shows this and links to the live article.
#
# Run with: source("data-raw/readme_funnel.R")

library(ggextreme)

dat <- metafor::escalc(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg,
                       data = metadat::dat.bcg, slab = paste(author, year))
fit <- metafor::rma(yi, vi, data = dat)
f <- ggfunnel(fit, hover = c("alloc", "ablat"), trim_fill = TRUE,
              title = "BCG vaccine and tuberculosis")
graph_save(f, "man/figures/README-funnel.png", res = 200)
