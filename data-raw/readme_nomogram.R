# The nomogram shown in README.md. GitHub cannot run the widget, so the
# README shows this static copy and links to the live article.
#
# Run with: source("data-raw/readme_nomogram.R")

library(ggextreme)

bw <- MASS::birthwt
bw$race <- factor(bw$race, labels = c("White", "Black", "Other"))
bw$smoke <- factor(bw$smoke, labels = c("No", "Yes"))
bw$ht <- factor(bw$ht, labels = c("No", "Yes"))
fit <- glm(low ~ splines::ns(age, 3) + lwt + race + smoke * ht,
           family = binomial, data = bw)
n <- ggnomogram(
  fit,
  outcome = "Risk of low birth weight",
  labels = c(age = "Mother's age (years)", lwt = "Weight at last period (lb)",
             race = "Race", smoke = "Smoked in pregnancy", ht = "Hypertension"),
  title = "Low birth weight"
)
graph_save(n, "man/figures/README-nomogram.png", res = 200)
