# An illustrative causal diagram for maternal smoking and orofacial clefts.
#
# The structure is a teaching example rather than the result of a formal
# review. Every reference was checked against PubMed.
#
# Run with: source("data-raw/cleft_dag.R")

little <- paste(
  "Little J, Cardy A, Munger RG. Tobacco smoking and oral clefts: a",
  "meta-analysis. Bull World Health Organ. 2004;82(3):213-8.",
  "https://pmc.ncbi.nlm.nih.gov/articles/PMC2585921/"
)
wilcox <- paste(
  "Wilcox AJ, Lie RT, Solvoll K, et al. Folic acid supplements and risk of",
  "facial clefts: national population based case-control study. BMJ.",
  "2007;334(7591):464. doi:10.1136/bmj.39079.618287.0B"
)
sivertsen <- paste(
  "Sivertsen A, Wilcox AJ, Skjaerven R, et al. Familial risk of oral clefts",
  "by morphological type and severity: population based cohort study of",
  "first degree relatives. BMJ. 2008;336(7641):432-4.",
  "doi:10.1136/bmj.39458.563611.AE"
)
hernan <- paste(
  "Hernán MA, Hernández-Díaz S, Robins JM. A structural approach",
  "to selection bias. Epidemiology. 2004;15(5):615-25.",
  "doi:10.1097/01.ede.0000135174.63482.43"
)
sofi <- paste(
  "Sofi-Mahmudi A, Shamsoddin E, Khademioore S, Khazaei Y, Vahdati A,",
  "Tovani-Palone MR. Global, regional, and national survey on burden and",
  "Quality of Care Index (QCI) of orofacial clefts: Global burden of disease",
  "systematic analysis 1990-2019. PLOS ONE. 2025;20(1):e0317267.",
  "doi:10.1371/journal.pone.0317267"
)

nodes <- data.frame(
  name = c("ses", "smoking", "folate", "genes", "cleft", "birth"),
  label = c("Socioeconomic\nstatus", "Maternal smoking", "Folic acid\nsupplements",
            "Genetic\nsusceptibility", "Orofacial cleft", "Live birth"),
  role = c("confounder", "exposure", "protective factor", "unobserved", "outcome",
           "collider"),
  rationale = c(
    paste("A common cause of smoking in pregnancy and of cleft risk, acting",
          "partly through diet and supplement use. Adjusting for it closes",
          "the back door paths from smoking to clefts."),
    paste("Smoking in early pregnancy, when the lip and palate form, is the",
          "exposure of interest."),
    paste("Folic acid in early pregnancy lowers the risk of cleft lip. It",
          "lies on a back door path through socioeconomic status, which",
          "adjusting for socioeconomic status already closes."),
    paste("Clefts recur strongly within families, which points to inherited",
          "risk that is rarely measured. It affects the outcome only, so",
          "leaving it out does not bias the estimate."),
    paste("Cleft lip with or without cleft palate in the child, one of the",
          "most common congenital anomalies."),
    paste("Many registries count only live births. Smoking and clefts both",
          "affect whether a pregnancy ends in a live birth, so restricting to",
          "live births conditions on a collider and can bias the estimate.")
  ),
  references = c(NA, little, wilcox, sivertsen, sofi, hernan),
  timing = c("Before pregnancy", "First trimester", "Periconception",
             "At conception", "At birth", "End of pregnancy"),
  stringsAsFactors = FALSE
)

edges <- data.frame(
  from = c("ses", "ses", "ses", "smoking", "folate", "genes", "smoking", "cleft"),
  to = c("smoking", "folate", "cleft", "cleft", "cleft", "cleft", "birth", "birth"),
  rationale = c(
    "Smoking in pregnancy is more common at lower socioeconomic position.",
    paste("Taking supplements before and early in pregnancy is less common",
          "at lower socioeconomic position."),
    paste("Pathways other than supplement use, such as diet and access to",
          "care, are allowed for with a direct arrow."),
    paste("A meta-analysis of 24 studies found a relative risk of 1.34 for",
          "cleft lip with or without cleft palate, with a modest dose",
          "response."),
    paste("At least 400 micrograms a day in early pregnancy was associated",
          "with about a third lower risk of isolated cleft lip, adjusted odds",
          "ratio 0.61."),
    paste("The relative risk of recurrence in first degree relatives is",
          "about 32 for any cleft lip."),
    "Smoking in pregnancy raises the risk of miscarriage and stillbirth.",
    paste("Pregnancies with a cleft, especially alongside other anomalies,",
          "are more often lost or terminated.")
  ),
  references = c(NA, NA, NA, little, wilcox, sivertsen, NA, NA),
  stringsAsFactors = FALSE
)

cleft_dag <- list(nodes = nodes, edges = edges)
save(cleft_dag, file = "data/cleft_dag.rda", compress = "xz", version = 3)
