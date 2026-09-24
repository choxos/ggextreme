# The forest plot, cumulative replay and league table shown in README.md
# and the vignettes. GitHub cannot run the widgets, so the README shows
# these images and links to the live articles.
#
# Run with: source("data-raw/readme_meta.R")

library(ggextreme)

dat <- metafor::escalc(
  measure = "OR", ai = p2y12.mi, n1i = p2y12.total,
  ci = aspirin.mi, n2i = aspirin.total,
  data = metadat::dat.chiarito2020, slab = paste(study, year)
)
dat <- dat[!is.na(dat$yi), ]
dat$p2y12 <- paste0(dat$p2y12.mi, "/", dat$p2y12.total)
dat$aspirin <- paste0(dat$aspirin.mi, "/", dat$aspirin.total)
fit <- metafor::rma(yi, vi, data = dat)
forest <- ggmeta(
  fit,
  columns = c("P2Y12 inhibitor" = "p2y12", Aspirin = "aspirin"),
  rob = c(R = "rob.R", D = "rob.D", Mi = "rob.Mi", Me = "rob.Me",
          S = "rob.S", Overall = "rob.overall"),
  favors = c("Favors P2Y12 inhibitor", "Favors aspirin"),
  title = "Myocardial infarction"
)
graph_save(forest, "man/figures/README-forest.png", res = 200)

bcg <- metafor::escalc(
  measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg,
  data = metadat::dat.bcg, slab = paste(author, year)
)
bcg <- bcg[order(bcg$year), ]
bcg_fit <- metafor::rma(yi, vi, data = bcg)
animate_meta(
  ggmeta(bcg_fit, favors = c("Favors BCG", "Favors control"),
         title = "BCG vaccine against tuberculosis"),
  "man/figures/README-meta-cumulative.gif",
  time = bcg$year, fps = 20, res = 100
)

pw <- meta::pairwise(treat = treatment, event = pasi75_r, n = pasi75_n,
                     studlab = study, data = psoriasis_nma, sm = "OR")
nma <- netmeta::netmeta(pw, common = FALSE)
league <- ggleague(nma, psoriasis_nma, study, treatment,
                   small_values = "undesirable", title = "PASI 75 response")
graph_save(league, "man/figures/README-league.png", res = 200)
