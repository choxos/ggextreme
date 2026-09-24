# The swimmer plot shown in README.md. GitHub cannot run the widget, so the
# README shows this and links to the live article.
#
# Run with: source("data-raw/readme_swimmer.R")

library(ggextreme)

aml <- subset(survival::myeloid, id <= 30)
aml$arm <- paste("Arm", aml$trt)
month <- 30.44
events <- rbind(
  data.frame(id = aml$id, time = aml$crtime / month, event = "Complete response"),
  data.frame(id = aml$id, time = aml$txtime / month, event = "Transplant"),
  data.frame(id = aml$id, time = aml$rltime / month, event = "Relapse"),
  data.frame(id = aml$id, time = ifelse(aml$death == 1, aml$futime / month, NA),
             event = "Death")
)
events <- events[!is.na(events$time), ]
s <- ggswimmer(aml, id, futime / month, events = events, group = arm,
               ongoing = death == 0, hover = c("sex", "flt3"),
               ongoing_label = "Alive at last follow-up",
               xlab = "Months since randomization",
               title = "Acute myeloid leukemia, 30 patients")
graph_save(s, "man/figures/README-swimmer.png", res = 200)
