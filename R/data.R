#' Quality of Care Index for orofacial clefts, 1990 to 2019
#'
#' Yearly Quality of Care Index (QCI) scores for orofacial clefts in fifteen
#' countries. The QCI is a composite of four secondary indices derived from
#' Global Burden of Disease estimates and summarized by principal component
#' analysis, rescaled to run from 0 to 100, where higher is better care.
#'
#' The published analysis covers every country; the fifteen here were chosen
#' to span continents, to cover a wide range of scores and to include several
#' changes of rank over the period, which makes the set a useful example for
#' [ggrace()]. Country names are shortened for plotting, and `iso` matches the
#' codes [race_flags()] uses.
#'
#' @format A data frame with 450 rows and 5 columns:
#' \describe{
#'   \item{country}{Country name.}
#'   \item{iso}{ISO 3166-1 alpha-2 country code, lower case.}
#'   \item{region}{World region, a factor with five levels.}
#'   \item{year}{Year, 1990 to 2019.}
#'   \item{qci}{Quality of Care Index, 0 to 100.}
#' }
#'
#' @source Sofi-Mahmudi A, Shamsoddin E, Khademioore S, Khazaei Y, Vahdati A,
#'   Tovani-Palone MR (2025). Global, regional, and national survey on burden
#'   and Quality of Care Index (QCI) of orofacial clefts: Global burden of
#'   disease systematic analysis 1990-2019. PLOS ONE 20(1): e0317267.
#'   \doi{10.1371/journal.pone.0317267}
#'
#' @examples
#' head(clefts_qci)
#' subset(clefts_qci, year == 2019)[order(-subset(clefts_qci, year == 2019)$qci), ]
"clefts_qci"

#' Quality of Care Index for orofacial clefts in every country, 1990 to 2019
#'
#' Yearly Quality of Care Index (QCI) scores for orofacial clefts in 195
#' countries and territories, the full country panel of the analysis that
#' [clefts_qci] samples. The QCI is a composite of four secondary indices
#' derived from Global Burden of Disease estimates and summarized by principal
#' component analysis, rescaled to run from 0 to 100, where higher is better
#' care. World regions, WHO regions, World Bank income groups and SDI groups
#' in the published panel are left out.
#'
#' Countries keep the names the Global Burden of Disease study gives them,
#' such as "Iran (Islamic Republic of)", and carry the ISO 3166-1 alpha-3
#' code of the map [ggchoropleth()] draws, so either column can name the
#' region.
#'
#' @format A data frame with 5,850 rows and 4 columns:
#' \describe{
#'   \item{country}{Country or territory, as named by the Global Burden of
#'     Disease study.}
#'   \item{iso3}{ISO 3166-1 alpha-3 code.}
#'   \item{year}{Year, 1990 to 2019.}
#'   \item{qci}{Quality of Care Index, 0 to 100.}
#' }
#'
#' @source Sofi-Mahmudi A, Shamsoddin E, Khademioore S, Khazaei Y, Vahdati A,
#'   Tovani-Palone MR (2025). Global, regional, and national survey on burden
#'   and Quality of Care Index (QCI) of orofacial clefts: Global burden of
#'   disease systematic analysis 1990-2019. PLOS ONE 20(1): e0317267.
#'   \doi{10.1371/journal.pone.0317267}
#'
#' @examples
#' head(clefts_qci_world)
#' ggchoropleth(clefts_qci_world, iso3, year, values = c(QCI = "qci"))
"clefts_qci_world"

#' An illustrative causal diagram for maternal smoking and orofacial clefts
#'
#' A small directed acyclic graph for a study of maternal smoking and
#' orofacial clefts in the child, with a rationale for every node and arrow
#' and published references for several of them. It shows each kind of role
#' [ggcausal()] colors: an exposure, an outcome, a confounder, an unobserved
#' cause of the outcome, a collider created by counting only live births,
#' and a free text role. The structure is a teaching example rather than the
#' result of a formal review.
#'
#' @format A list of two data frames.
#' \describe{
#'   \item{nodes}{Six nodes with columns `name`, `label`, `role`,
#'     `rationale`, `references` and `timing`. `timing` is not a reserved
#'     column, so it appears as a field in the hover card and panel.}
#'   \item{edges}{Eight arrows with columns `from`, `to`, `rationale` and
#'     `references`.}
#' }
#'
#' @source Rationales written for this package. References were checked
#'   against PubMed and are listed in full in the `references` columns.
#'
#' @examples
#' cleft_dag$nodes[c("name", "role")]
#' ggcausal(cleft_dag$edges, cleft_dag$nodes)
"cleft_dag"

#' Arm level data from five trials in plaque psoriasis
#'
#' Baseline characteristics and PASI 75 response for the 15 arms of five
#' randomized trials in moderate to severe plaque psoriasis: CLEAR, ERASURE,
#' FEATURE, FIXTURE and JUNCTURE. The trials compare secukinumab at two
#' doses with placebo, etanercept and ustekinumab, and most have more than
#' two arms, which makes a small but well connected network for [ggnma()].
#'
#' Every column except `study`, `treatment` and `class` carries a `label`
#' attribute, which [ggnma()] uses to name the rows of its arm tables.
#'
#' @format A data frame with 15 rows, one per arm, and 15 columns:
#' \describe{
#'   \item{study}{Trial name.}
#'   \item{treatment}{Treatment, a factor with placebo first.}
#'   \item{class}{Drug class, a factor with four levels.}
#'   \item{n}{Participants randomized.}
#'   \item{pasi75_r, pasi75_n}{Participants with a PASI 75 response, and the
#'     number analyzed.}
#'   \item{age}{Mean age in years.}
#'   \item{male}{Percentage of men.}
#'   \item{weight}{Mean weight in kilograms.}
#'   \item{bmi}{Mean body mass index.}
#'   \item{pasi_w0}{Mean PASI score at baseline.}
#'   \item{duration}{Mean duration of psoriasis in years.}
#'   \item{prior_systemic}{Percentage with previous systemic treatment.}
#'   \item{psa}{Percentage with psoriatic arthritis.}
#'   \item{reference}{The trial's main publication, with its DOI.}
#' }
#'
#' @source Aggregate data compiled by Phillippo (2019) and distributed as
#'   `plaque_psoriasis_agd` in the 'multinma' package. The analysis that
#'   uses them is Phillippo DM, Dias S, Ades AE, et al. (2020). Multilevel
#'   network meta-regression for population-adjusted treatment comparisons.
#'   Journal of the Royal Statistical Society Series A 183(3): 1189-1210.
#'   \doi{10.1111/rssa.12579}. Trial references were checked against PubMed.
#'
#' @examples
#' psoriasis_nma[c("study", "treatment", "n", "pasi75_r")]
#' attr(psoriasis_nma$age, "label")
"psoriasis_nma"
