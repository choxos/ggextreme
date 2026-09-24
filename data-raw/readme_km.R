# The Kaplan-Meier plot and animation shown in README.md and the vignette.
# GitHub cannot run the widget, so the README shows these and links to the
# live article.
#
# Run with: source("data-raw/readme_km.R")

library(ggextreme)

colon <- subset(survival::colon, etype == 2)
colon$years <- colon$time / 365.25
colon$arm <- factor(colon$rx, labels = c("Observation", "Levamisole",
                                         "Levamisole + fluorouracil"))

km <- ggkm(survival::Surv(years, status) ~ arm, data = colon,
           xlab = "Years since randomization",
           title = "Overall survival, stage C colon cancer")
graph_save(km, "man/figures/README-km.png", res = 200)

drawn <- ggkm(survival::Surv(years, status) ~ arm, data = colon,
              ph_tests = FALSE, xlab = "Years since randomization",
              title = "Overall survival, stage C colon cancer")
animate_km(drawn, "man/figures/README-km.gif", duration = 5, fps = 20,
           res = 100)
