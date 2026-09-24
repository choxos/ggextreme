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
