# Arm level data from five randomized trials in moderate to severe plaque
# psoriasis, for the network plot example.
#
# The aggregate data are those compiled by Phillippo (2019) and analyzed in
# Phillippo et al. (2020), as distributed in the multinma package
# (plaque_psoriasis_agd). The response rates match those published for
# ERASURE, FIXTURE and JUNCTURE. Trial references were checked against
# PubMed.
#
# Run with: source("data-raw/psoriasis_nma.R")

agd <- multinma::plaque_psoriasis_agd

refs <- c(
  CLEAR = paste(
    "Thaçi D, Blauvelt A, Reich K, et al. Secukinumab is superior to",
    "ustekinumab in clearing skin of subjects with moderate to severe plaque",
    "psoriasis: CLEAR, a randomized controlled trial. J Am Acad Dermatol.",
    "2015;73(3):400-9. doi:10.1016/j.jaad.2015.05.013"
  ),
  ERASURE = paste(
    "Langley RG, Elewski BE, Lebwohl M, et al. Secukinumab in plaque",
    "psoriasis: results of two phase 3 trials. N Engl J Med.",
    "2014;371(4):326-38. doi:10.1056/NEJMoa1314258"
  ),
  FEATURE = paste(
    "Blauvelt A, Prinz JC, Gottlieb AB, et al. Secukinumab administration by",
    "pre-filled syringe: efficacy, safety and usability results from a",
    "randomized controlled trial in psoriasis (FEATURE). Br J Dermatol.",
    "2015;172(2):484-93. doi:10.1111/bjd.13348"
  ),
  FIXTURE = paste(
    "Langley RG, Elewski BE, Lebwohl M, et al. Secukinumab in plaque",
    "psoriasis: results of two phase 3 trials. N Engl J Med.",
    "2014;371(4):326-38. doi:10.1056/NEJMoa1314258"
  ),
  JUNCTURE = paste(
    "Paul C, Lacour JP, Tedremets L, et al. Efficacy, safety and usability of",
    "secukinumab administration by autoinjector/pen in psoriasis: a",
    "randomized, controlled trial (JUNCTURE). J Eur Acad Dermatol Venereol.",
    "2015;29(6):1082-90. doi:10.1111/jdv.12751"
  )
)

treatments <- c("Placebo", "Etanercept", "Ustekinumab",
                "Secukinumab 150 mg", "Secukinumab 300 mg")
classes <- c(
  "Placebo" = "Placebo",
  "Etanercept" = "TNF inhibitor",
  "Ustekinumab" = "IL-12/23 inhibitor",
  "Secukinumab 150 mg" = "IL-17A inhibitor",
  "Secukinumab 300 mg" = "IL-17A inhibitor"
)

with_label <- function(x, label) structure(x, label = label)

psoriasis_nma <- data.frame(
  study = agd$studyc,
  treatment = factor(agd$trtc_long, levels = treatments),
  class = factor(unname(classes[agd$trtc_long]),
                 levels = c("Placebo", "TNF inhibitor", "IL-12/23 inhibitor",
                            "IL-17A inhibitor")),
  n = agd$sample_size_w0,
  pasi75_r = agd$pasi75_r,
  pasi75_n = agd$pasi75_n,
  age = agd$age_mean,
  male = agd$male,
  weight = agd$weight_mean,
  bmi = agd$bmi_mean,
  pasi_w0 = agd$pasi_w0_mean,
  duration = agd$durnpso_mean,
  prior_systemic = agd$prevsys,
  psa = agd$psa,
  reference = unname(refs[agd$studyc]),
  stringsAsFactors = FALSE
)
psoriasis_nma <- psoriasis_nma[order(psoriasis_nma$study,
                                     as.integer(psoriasis_nma$treatment)), ]
rownames(psoriasis_nma) <- NULL

labels <- c(
  n = "Randomized",
  pasi75_r = "PASI 75 responders",
  pasi75_n = "PASI 75 analyzed",
  age = "Mean age (years)",
  male = "Male (%)",
  weight = "Mean weight (kg)",
  bmi = "Mean BMI",
  pasi_w0 = "Mean baseline PASI",
  duration = "Mean duration of psoriasis (years)",
  prior_systemic = "Previous systemic treatment (%)",
  psa = "Psoriatic arthritis (%)",
  reference = "Reference"
)
for (col in names(labels)) {
  psoriasis_nma[[col]] <- with_label(psoriasis_nma[[col]], labels[[col]])
}

# PASI 75 response rates published in the trial reports, in treatment order.
published <- list(
  ERASURE = c(4.5, 71.6, 81.6),
  FIXTURE = c(4.9, 44.0, 67.0, 77.1),
  JUNCTURE = c(3.3, 71.7, 86.7)
)
rate <- function(s) {
  d <- psoriasis_nma[psoriasis_nma$study == s, ]
  round(100 * unclass(d$pasi75_r) / unclass(d$pasi75_n), 1)
}

stopifnot(
  nrow(psoriasis_nma) == 15,
  !anyNA(psoriasis_nma[c("study", "treatment", "class", "n", "pasi75_r",
                         "pasi75_n", "reference")]),
  all(vapply(names(published), function(s) {
    isTRUE(all.equal(rate(s), published[[s]]))
  }, logical(1)))
)

save(psoriasis_nma, file = "data/psoriasis_nma.rda", compress = "xz",
     version = 3)
