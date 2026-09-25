# CINeMA: confidence in the results of a network meta-analysis. One shared
# preparation of a netmeta fit feeds cinema_judge() and the five plots, so
# every plot reads the same estimates, contributions and judgments.
#
# This file is collated before graph.R, so nothing at the top level may use
# graph_ink, graph_dims or other objects defined there.

cinema_domain_names <- c(bias = "Within-study bias", reporting = "Reporting bias",
                         indirectness = "Indirectness", imprecision = "Imprecision",
                         heterogeneity = "Heterogeneity", incoherence = "Incoherence")
cinema_concern_words <- c("No concerns", "Some concerns", "Major concerns")
cinema_reporting_words <- c("Undetected", "Suspected", "Strongly suspected")
cinema_study_words <- list(
  rob = c("Low risk of bias", "Some concerns", "High risk of bias"),
  indirectness = c("Low indirectness", "Moderate indirectness", "High indirectness")
)
cinema_study_short <- list(rob = c("Low", "Some concerns", "High"),
                           indirectness = c("Low", "Moderate", "High"))
cinema_study_phrase <- list(
  rob = c("at low risk of bias", "with some concerns about risk of bias", "at high risk of bias"),
  indirectness = c("of low indirectness", "of moderate indirectness", "of high indirectness")
)
cinema_study_title <- c(rob = "Risk of bias", indirectness = "Indirectness")

# Colors that carry meaning, written as literals because this file is
# collated before graph.R. They keep their hue on a dark page. The level
# colors are no, some and major concerns; the reading colors follow
# ggleague(): teal favors the first treatment, pink the second.
cinema_ink <- list(
  level = c("#2E7D55", "#C88A1E", "#C0603F"),
  first = "#22928F", second = "#B66399", little = "#8A8A8A", wide = "#757CC6",
  prespecified = "#7A5AA6"
)

#' Judge confidence in the results of a network meta-analysis
#'
#' Applies the rules of CINeMA, Confidence in Network Meta-Analysis
#' (Nikolakopoulou et al. 2020; Papakonstantinou et al. 2020), to every
#' comparison of a network meta-analysis. Each comparison gets a judgment of
#' no concerns, some concerns or major concerns in each of six domains:
#' within-study bias, reporting bias, indirectness, imprecision, heterogeneity
#' and incoherence, each with its reason in words and a note of whether it
#' was computed by a rule or given by you. The domains are kept side by side
#' and never added into a score.
#'
#' The result feeds the plots of the family, so they share one set of
#' estimates, contributions and judgments: [cinema_contribution()],
#' [cinema_clinical()], [cinema_incoherence()] and [cinema_league()]. Each of
#' them also accepts a netmeta fit and the arguments of this function.
#'
#' @param x A network meta-analysis from [netmeta::netmeta()].
#' @param rob,indirectness Study level judgments of risk of bias and of
#'   indirectness: data frames with a column `study`, naming every study in
#'   the network once, a column `judgment`, and optionally a column `reason`
#'   in words. A judgment is low, moderate or high, written as `"low"`,
#'   `"some concerns"` (or `"moderate"` or `"unclear"`) and `"high"`, as
#'   `"l"`, `"m"` and `"h"`, or as 1, 2 and 3, the codes the CINeMA web
#'   application reads.
#' @param reporting Your judgment of reporting bias, which cannot be computed
#'   from the data: a data frame with a column `judgment`, `"undetected"` or
#'   `"suspected"` (`"strongly suspected"` is also read), an optional column
#'   `reason`, and the comparison it applies to, either as `treat1` and
#'   `treat2` or as `comparison` (such as `"A:B"` or `"A vs B"`). A data frame
#'   with a single row and no comparison applies to every comparison.
#' @param threshold The limits of the range of little difference, on the
#'   scale of the effect (an odds ratio, say, not its logarithm), for the
#'   first treatment of each comparison against the second: one number, such
#'   as `1.25`, for a range symmetric about no effect (0.8 to 1.25 here), or
#'   two numbers for independent lower and upper limits, which must lie on
#'   either side of no effect. A threshold of no effect itself, 1 for a ratio
#'   or 0 for a difference, treats any effect as important. Imprecision and
#'   heterogeneity, and incoherence when its test gives p of 0.10 or less,
#'   need it.
#' @param rule How the study judgments are summarized for each comparison:
#'   `"average"`, `"majority"` or `"highest"`. Give two, such as
#'   `c("average", "highest")`, for different rules for within-study bias and
#'   for indirectness. See the section on rules.
#' @param judgments Domain judgments you made yourself, such as those
#'   exported from the CINeMA web application, which replace the computed
#'   ones. Either wide, with the comparison (`treat1` and `treat2`, or
#'   `comparison`) and one column per domain, named like the domains
#'   (`"Within-study bias"`, `"Reporting bias"`, `"Indirectness"`,
#'   `"Imprecision"`, `"Heterogeneity"`, `"Incoherence"`), optional columns
#'   such as `"Imprecision reason"` and optional `"Confidence rating"` and
#'   `"Reason(s) for downgrading"` columns; or long, with the comparison and
#'   columns `domain`, `judgment` and optionally `reason`. A missing or empty
#'   judgment leaves the computed one in place.
#' @param small_values Whether small values of the effect are `"desirable"`,
#'   as for mortality, or `"undesirable"`, as for a response. It sets the
#'   ranking, and so the order of the treatments, and which side of the range
#'   is a benefit. Defaults to the setting stored in `x`, which is worth
#'   checking.
#' @param order The order of the treatments. Each comparison is written with
#'   the treatment that comes first in this order first, and the limits in
#'   `threshold` apply in that direction. Defaults to the P-score ranking,
#'   best first.
#' @param pooled Which model to use, `"random"` or `"common"`. Defaults to
#'   the random effects model when `x` has one.
#' @param contributions The contribution of each study to each estimate: an
#'   object from `netmeta::netcontrib(x, study = TRUE)`, or `TRUE` to compute
#'   it. By default it is computed when `rob` or `indirectness` is given,
#'   which takes a few seconds for a network of a few dozen studies.
#' @param split The direct and indirect estimates: an object from
#'   [netmeta::netsplit()], or `NULL` to compute them with its default
#'   method, back-calculation. Give one computed with `method = "SIDDE"` to
#'   use that method instead.
#'
#' @section Rules:
#' Every rule below is the one CINeMA implements, as published; where the
#' papers leave a detail open the choice made here is stated.
#'
#' * **Within-study bias** and **indirectness** combine the study judgments
#'   with the percentage contribution of each study to each estimate
#'   (Papakonstantinou et al. 2018), from [netmeta::netcontrib()]. The
#'   *majority* rule takes the level with the largest total contribution,
#'   the more serious level on a tie; the *average* rule scores low 1,
#'   moderate 2 and high 3, averages the scores weighted by contribution and
#'   rounds, halves up; the *highest* rule takes the most serious level among
#'   the studies that contribute more than 0.0001 percent. Low, moderate and
#'   high become no, some and major concerns.
#' * **Reporting bias** is your judgment; CINeMA suggests suspected or
#'   undetected, and suspected is shown as some concerns.
#' * **Imprecision** compares the confidence interval with the range of
#'   little difference. There are no concerns when the interval lies wholly
#'   within the range, or wholly on the side of no effect that the point
#'   estimate is on; some concerns when it crosses no effect but not the
#'   limit on the other side; and major concerns when it passes that limit,
#'   so that it holds important effects in both directions.
#' * **Heterogeneity** judges the prediction interval by the same rule. There
#'   are no concerns when it reaches the same step as the confidence
#'   interval, some concerns when it reaches one step further and major
#'   concerns when it reaches two (Table 4 of Papakonstantinou et al. 2020;
#'   this reproduces every scenario in Figure 3 of Nikolakopoulou et al.
#'   2020). A common effect model has no prediction interval, so
#'   heterogeneity is then not judged.
#' * **Incoherence**, for a comparison with direct and indirect evidence,
#'   uses the test of the difference between them from [netmeta::netsplit()]
#'   (SIDE). With p above 0.10 there are no concerns. Otherwise the areas
#'   below, within and above the range of little difference are compared:
#'   when both confidence intervals reach the same areas there are no
#'   concerns, when they differ in one area some concerns, and when they
#'   differ in two or three major concerns. A comparison with only direct or
#'   only indirect evidence cannot be tested locally, and takes its judgment
#'   from the global design by treatment interaction test of
#'   [netmeta::decomp.design()]: major concerns below 0.05, some from 0.05 to
#'   0.10 and no concerns above; when the network has no closed loop, so the
#'   test cannot be computed, major concerns. Both tests have low power.
#'
#' CINeMA's authors stress that these rules are a starting point: the
#' reasons say what each rule saw, so a judgment can be revised by giving it
#' in `judgments`. CINeMA also leaves any overall rating to the reviewers;
#' this function gives none, though a rating you supply is kept and shown.
#'
#' @section Sources:
#' Nikolakopoulou A, Higgins JPT, Papakonstantinou T, et al. CINeMA: an
#' approach for assessing confidence in the results of a network
#' meta-analysis. PLoS Medicine 2020;17(4):e1003082.
#' \doi{10.1371/journal.pmed.1003082}
#'
#' Papakonstantinou T, Nikolakopoulou A, Higgins JPT, Egger M, Salanti G.
#' CINeMA: software for semiautomated assessment of the confidence in the
#' results of network meta-analysis. Campbell Systematic Reviews
#' 2020;16:e1080. \doi{10.1002/cl2.1080}
#'
#' Papakonstantinou T, Nikolakopoulou A, Rucker G, et al. Estimating the
#' contribution of studies in network meta-analysis: paths, flows and
#' streams. F1000Research 2018;7:610.
#'
#' @return An object of class `cinema`, a list whose main fields are
#'   `judgments`, one row per comparison and domain with the `level` (0, 1
#'   or 2 for no, some or major concerns, `NA` when not judged), the
#'   `judgment` in words, the `reason` and its `source`; `comparisons`, the
#'   network, direct and indirect estimates, prediction intervals and
#'   inconsistency factors on the scale of the effect; `contributions`, the
#'   share of each estimate from each study; `studies`, the study judgments;
#'   `threshold`, `rule`, `global` (the design by treatment test) and `fit`.
#'   It prints as a table of the judgments.
#' @export
#'
#' @examples
#' \donttest{
#' if (requireNamespace("netmeta", quietly = TRUE) &&
#'     requireNamespace("meta", quietly = TRUE)) {
#'   pw <- meta::pairwise(treat = treatment, event = pasi75_r,
#'                        n = pasi75_n, studlab = study,
#'                        data = psoriasis_nma, sm = "OR")
#'   nma <- netmeta::netmeta(pw, common = FALSE)
#'   # Illustrative study judgments, invented for this example: they are
#'   # not published assessments of these trials.
#'   rob <- data.frame(
#'     study = c("CLEAR", "ERASURE", "FEATURE", "FIXTURE", "JUNCTURE"),
#'     judgment = c("high", "low", "some concerns", "low", "some concerns")
#'   )
#'   j <- cinema_judge(nma, rob = rob, threshold = 1.25,
#'                     small_values = "undesirable",
#'                     reporting = data.frame(judgment = "undetected"))
#'   j
#'   head(j$judgments)
#' }
#' }
cinema_judge <- function(x, rob = NULL, indirectness = NULL, reporting = NULL,
                         threshold = NULL, rule = "average", judgments = NULL,
                         small_values = NULL, order = NULL, pooled = NULL,
                         contributions = NULL, split = NULL) {
  cn <- cinema_prepare(x, small_values, order, pooled, split)
  cn$threshold <- cinema_threshold(threshold, cn$ratio)
  rules <- cinema_rules(rule)
  cn$rule <- rules
  cn$studies <- list(rob = cinema_studies(rob, "rob", cn$study_names),
                     indirectness = cinema_studies(indirectness, "indirectness", cn$study_names))
  wants <- !is.null(cn$studies$rob) || !is.null(cn$studies$indirectness) ||
    isTRUE(contributions) || inherits(contributions, "netcontrib")
  if (!is.null(contributions) && !isTRUE(contributions) && !inherits(contributions, "netcontrib")) {
    rlang::abort("`contributions` must be TRUE or an object from netmeta::netcontrib(x, study = TRUE).")
  }
  if (wants) cn <- cinema_add_contributions(cn, contributions)
  cn <- cinema_apply_rules(cn)
  if (!is.null(reporting)) cn <- cinema_override(cn, cinema_reporting(reporting, cn))
  if (!is.null(judgments)) {
    user <- cinema_user_judgments(judgments, cn)
    cn <- cinema_override(cn, user$long)
    cn$overall <- user$overall
  }
  cinema_finish(cn)
}

# The fit, in the order of the treatments, with every pair's network, direct
# and indirect estimates on the scale of the analysis (log for ratios).
cinema_prepare <- function(x, small_values, order, pooled, split) {
  if (!inherits(x, "netmeta")) {
    rlang::abort("`x` must be a network meta-analysis from netmeta::netmeta(), or judgments from cinema_judge().")
  }
  rlang::check_installed("netmeta")
  if (is.null(pooled)) pooled <- if (isTRUE(x$random)) "random" else "common"
  pooled <- match.arg(pooled, c("random", "common"))
  if (is.null(small_values)) small_values <- x$small.values
  if (is.null(small_values)) small_values <- "desirable"
  small_values <- match.arg(small_values, c("desirable", "undesirable"))
  trts <- x$trts
  if (length(trts) < 2) rlang::abort("The network needs at least two treatments.")
  pscore <- netmeta::netrank(x, small.values = small_values)[[paste0("ranking.", pooled)]][trts]
  if (is.null(order)) {
    order <- trts[base::order(-pscore)]
  } else {
    order <- as.character(order)
    if (!setequal(order, trts) || anyDuplicated(order)) {
      rlang::abort(paste0("`order` must name every treatment once: ", paste(trts, collapse = ", ")))
    }
  }
  get <- function(stem) x[[paste0(stem, ".", pooled)]]
  idx <- utils::combn(length(order), 2)
  a <- order[idx[1, ]]
  b <- order[idx[2, ]]
  m <- cbind(a, b)
  # Fields an older netmeta may not have become missing values.
  at <- function(v) if (is.null(v)) rep(NA_real_, length(a)) else v[m]
  pairs <- data.frame(a = a, b = b, key = pair_key(a, b),
                      te = get("TE")[m], se = get("seTE")[m],
                      lo = get("lower")[m], hi = get("upper")[m],
                      plo = if (pooled == "random") at(x$lower.predict) else NA_real_,
                      phi = if (pooled == "random") at(x$upper.predict) else NA_real_,
                      k = x$A.matrix[m], prop = at(x[[paste0("P.", pooled)]]),
                      stringsAsFactors = FALSE)
  pairs$prop[!is.finite(pairs$prop)] <- 0

  if (is.null(split)) {
    split <- netmeta::netsplit(x)
  } else if (!inherits(split, "netsplit")) {
    rlang::abort("`split` must be an object from netmeta::netsplit().")
  }
  sep <- unique(c(split$sep.trts, x$sep.trts, ":"))
  oriented <- function(df) {
    if (is.null(df)) return(NULL)
    ab <- cinema_match_pairs(df$comparison, trts, sep, "netsplit() comparisons")
    i <- match(pairs$key, pair_key(ab$a, ab$b))
    if (anyNA(i)) rlang::abort("`split` must come from netmeta::netsplit() on the same fit as `x`.")
    s <- ifelse(ab$a[i] == pairs$a, 1, -1)
    l <- s * df$lower[i]
    u <- s * df$upper[i]
    list(te = s * df$TE[i], se = df$seTE[i], lo = pmin(l, u), hi = pmax(l, u),
         p = if (!is.null(df$p)) df$p[i] else NA_real_)
  }
  d <- oriented(split[[paste0("direct.", pooled)]])
  ind <- oriented(split[[paste0("indirect.", pooled)]])
  f <- oriented(split[[paste0("compare.", pooled)]])
  if (is.null(d) || is.null(ind) || is.null(f)) {
    rlang::abort(paste0("`split` has no ", pooled, " effects results."))
  }
  pairs[c("d_te", "d_se", "d_lo", "d_hi")] <- d[c("te", "se", "lo", "hi")]
  pairs[c("i_te", "i_se", "i_lo", "i_hi")] <- ind[c("te", "se", "lo", "hi")]
  pairs[c("f_te", "f_se", "f_lo", "f_hi", "f_p")] <- f[c("te", "se", "lo", "hi", "p")]
  has_d <- is.finite(pairs$d_te)
  has_i <- is.finite(pairs$i_te)
  pairs$type <- ifelse(has_d & has_i, "mixed", ifelse(has_d, "direct", "indirect"))
  pw_key <- pair_key(x$treat1, x$treat2)
  direct <- lapply(pairs$key, function(k) unique(as.character(x$studlab[pw_key == k])))

  ratio <- x$sm %in% ratio_measures
  list(fit = x, pooled = pooled, small_values = small_values, order = order,
       trts = trts, pscore = pscore[order], pairs = pairs, direct = direct,
       study_names = unique(as.character(x$studlab)),
       ratio = ratio, sm = x$sm,
       measure = if (x$sm %in% names(measure_names)) measure_names[[x$sm]] else "Effect",
       level = if (is.null(x$level.ma)) x$level else x$level.ma,
       split_method = if (is.null(split$method) || split$method == "Back-calculation") "back-calculation" else split$method,
       global = cinema_global(x, pooled),
       tau2 = if (pooled == "random") x$tau2 else NA_real_,
       n_studies = length(unique(x$studlab)))
}

# Every name is matched against every ordered pair of treatments joined by
# each separator, so a separator inside a treatment name does no harm.
cinema_match_pairs <- function(names, trts, seps, what) {
  grid <- expand.grid(a = trts, b = trts, s = seps, stringsAsFactors = FALSE)
  grid <- grid[grid$a != grid$b, ]
  full <- paste0(grid$a, grid$s, grid$b)
  hit <- match(trimws(as.character(names)), full)
  if (anyNA(hit)) {
    rlang::abort(paste0("Could not match these ", what, " to treatments of the network: ",
                        paste(unique(names[is.na(hit)]), collapse = ", ")))
  }
  data.frame(a = grid$a[hit], b = grid$b[hit], stringsAsFactors = FALSE)
}

# The global design by treatment interaction test, or NULL when the network
# has no loop to test.
cinema_global <- function(x, pooled) {
  dd <- tryCatch(suppressWarnings(netmeta::decomp.design(x)), error = function(e) NULL)
  if (is.null(dd)) return(NULL)
  q <- if (pooled == "random" && !is.null(dd$Q.inc.random)) dd$Q.inc.random else
    dd$Q.decomp["Between designs", , drop = FALSE]
  if (is.null(q) || !nrow(q) || !is.finite(q$df[1]) || q$df[1] < 1 || !is.finite(q$pval[1])) return(NULL)
  list(Q = q$Q[1], df = q$df[1], p = q$pval[1])
}

cinema_add_contributions <- function(cn, contributions) {
  x <- cn$fit
  cb <- if (inherits(contributions, "netcontrib")) contributions else netmeta::netcontrib(x, study = TRUE)
  st <- cb[[paste0("study.", cn$pooled)]]
  if (is.null(st)) {
    rlang::abort(paste0("`contributions` has no study contributions for the ", cn$pooled,
                        " effects model; compute them with netmeta::netcontrib(x, study = TRUE)."))
  }
  ab <- cinema_match_pairs(st$comparison, cn$trts, unique(c(x$sep.trts, ":")),
                           "netcontrib() comparisons")
  if (!setequal(unique(as.character(st$study)), cn$study_names)) {
    rlang::abort("`contributions` must come from the same network as `x`.")
  }
  flow <- nma_contributions(cb, cn$pooled)
  if (!setequal(attr(flow, "trts"), cn$trts)) {
    rlang::abort("`contributions` must come from the same network as `x`.")
  }
  cn$contrib <- data.frame(key = pair_key(ab$a, ab$b), study = as.character(st$study),
                           share = st$contribution, stringsAsFactors = FALSE)
  cn$flow <- flow
  cn$contribution_method <- contribution_method(flow)
  cn
}

# The limits of little difference on the scale of the effect, checked, or
# NULL when none are given.
cinema_threshold <- function(threshold, ratio) {
  if (is.null(threshold)) return(NULL)
  if (!is.numeric(threshold) || !length(threshold) %in% 1:2 || any(!is.finite(threshold))) {
    rlang::abort("`threshold` must be one or two numbers on the scale of the effect.")
  }
  null <- if (ratio) 1 else 0
  if (ratio && any(threshold <= 0)) {
    rlang::abort("`threshold` must be positive for a ratio, such as 1.25 or c(0.8, 1.25).")
  }
  lims <- if (length(threshold) == 1) {
    if (ratio) sort(c(threshold, 1 / threshold)) else sort(c(-abs(threshold), abs(threshold)))
  } else {
    sort(threshold)
  }
  if (lims[1] > null || lims[2] < null) {
    rlang::abort(paste0("The limits in `threshold` must lie on either side of no effect (", null, ")."))
  }
  lims
}

cinema_rules <- function(rule) {
  if (!is.character(rule) || !length(rule) %in% 1:2) {
    rlang::abort("`rule` must be \"average\", \"majority\" or \"highest\", or two of them.")
  }
  rule <- vapply(rule, match.arg, "", choices = c("average", "majority", "highest"), USE.NAMES = FALSE)
  c(bias = rule[1], indirectness = rule[length(rule)])
}

# Judgments as 0, 1 and 2 from the many ways they are written.
cinema_level <- function(v) {
  if (is.factor(v)) v <- as.character(v)
  if (is.numeric(v)) {
    out <- ifelse(v %in% 1:3, v - 1, NA_real_)
    return(list(level = as.integer(out), bad = unique(v[!is.na(v) & is.na(out)])))
  }
  s <- tolower(trimws(as.character(v)))
  s <- gsub("[-_.]+", " ", s)
  s <- gsub(" +", " ", s)
  words <- list(
    c("1", "l", "low", "low risk", "low risk of bias", "low indirectness", "no concerns",
      "no concern", "no", "none", "undetected"),
    c("2", "m", "moderate", "moderate risk", "moderate risk of bias", "moderate indirectness",
      "some concerns", "some concern", "some", "unclear", "unclear risk", "unclear risk of bias",
      "suspected"),
    c("3", "h", "high", "high risk", "high risk of bias", "high indirectness", "major concerns",
      "major concern", "major", "strongly suspected")
  )
  out <- rep(NA_integer_, length(s))
  for (k in 1:3) out[s %in% words[[k]]] <- k - 1L
  blank <- is.na(s) | s %in% c("", "na", "not judged")
  list(level = out, bad = unique(v[is.na(out) & !blank]))
}

# Study level judgments, checked against the studies of the network.
cinema_studies <- function(df, what, studies) {
  if (is.null(df)) return(NULL)
  arg <- paste0("`", what, "`")
  if (!is.data.frame(df) || !all(c("study", "judgment") %in% names(df))) {
    rlang::abort(paste0(arg, " must be a data frame with columns `study` and `judgment`, and optionally `reason`."))
  }
  study <- as.character(df$study)
  twice <- unique(study[duplicated(study)])
  if (length(twice)) rlang::abort(paste0(arg, " names these studies more than once: ", paste(twice, collapse = ", ")))
  lv <- cinema_level(df$judgment)
  if (length(lv$bad) || anyNA(lv$level)) {
    rlang::abort(paste0(arg, " has judgments that are not low, moderate or high",
                        if (length(lv$bad)) paste0(": ", paste(lv$bad, collapse = ", ")) else
                          ", or missing ones", "."))
  }
  missing <- setdiff(studies, study)
  if (length(missing)) {
    rlang::abort(paste0(arg, " has no judgment for these studies of the network: ", paste(missing, collapse = ", ")))
  }
  extra <- setdiff(study, studies)
  if (length(extra)) {
    rlang::warn(paste0(arg, " names studies that are not in the network, which are left out: ",
                       paste(extra, collapse = ", ")))
  }
  keep <- study %in% studies
  reason <- if ("reason" %in% names(df)) as.character(df$reason) else rep(NA_character_, nrow(df))
  reason[is.na(reason)] <- ""
  data.frame(study = study[keep], level = lv$level[keep],
             judgment = cinema_study_words[[what]][lv$level[keep] + 1],
             reason = reason[keep], stringsAsFactors = FALSE)
}

# One of CINeMA's three rules for a comparison, from its studies' levels (0
# to 2) and their contributions.
cinema_rule <- function(levels, shares, rule) {
  keep <- is.finite(shares) & shares > 1e-6
  if (!any(keep)) return(NA_integer_)
  l <- levels[keep]
  w <- shares[keep]
  switch(rule,
    majority = {
      tot <- vapply(0:2, function(k) sum(w[l == k]), numeric(1))
      as.integer(max(which(tot >= max(tot) - 1e-12)) - 1L)
    },
    average = as.integer(floor(sum(w * (l + 1)) / sum(w) + 0.5 + 1e-9) - 1L),
    highest = as.integer(max(l))
  )
}

# The side step of an interval: 0 when it lies within the range of little
# difference or on the point estimate's side of no effect, 1 when it crosses
# no effect but not the limit beyond, 2 when it passes that limit too. On the
# scale of the analysis, with no effect at 0 and L <= 0 <= U.
cinema_step <- function(e, l, u, L, U) {
  out <- ifelse(e >= 0,
                ifelse(l >= 0, 0L, ifelse(l >= L, 1L, 2L)),
                ifelse(u <= 0, 0L, ifelse(u <= U, 1L, 2L)))
  out[which(l >= L & u <= U)] <- 0L
  out
}

# Which of the three areas an interval reaches: 1 below the range, 2 within
# it, 4 above it. A range of no effect alone has no inside to reach.
cinema_zones <- function(l, u, L, U) {
  as.integer((l < L) * 1L + (l <= U & u >= L & U > L) * 2L + (u > U) * 4L)
}

cinema_bits <- function(m) (m %% 2) + ((m %/% 2) %% 2) + ((m %/% 4) %% 2)

# CINeMA's Table 5 (Papakonstantinou et al. 2020): of the three areas, how
# many the direct and the indirect intervals agree on (both reach it or both
# do not), and the level that follows: 3 no concerns, 2 some, 1 or 0 major.
cinema_agreement <- function(zd, zi) {
  common <- 3L - cinema_bits(bitwXor(as.integer(zd), as.integer(zi)))
  list(common = common, level = c(2L, 2L, 1L, 0L)[common + 1])
}

cinema_num <- function(v, ratio) {
  v <- if (ratio) exp(v) else v
  vapply(v, function(z) {
    if (!is.finite(z)) return("NA")
    if (!ratio) return(formatC(z, format = "f", digits = 2))
    if (z >= 100) formatC(z, format = "f", digits = 0) else
      if (z >= 10) formatC(z, format = "f", digits = 1) else
        if (z >= 0.1) formatC(z, format = "f", digits = 2) else
          if (z >= 0.01) formatC(z, format = "f", digits = 3) else
            formatC(z, format = "fg", digits = 2)
  }, character(1))
}

cinema_interval <- function(l, u, ratio) paste(cinema_num(l, ratio), "to", cinema_num(u, ratio))

cinema_estimate <- function(e, l, u, ratio) {
  paste0(cinema_num(e, ratio), " (", cinema_interval(l, u, ratio), ")")
}

cinema_pct <- function(v) paste0(formatC(100 * v, format = "f", digits = 1), "%")

cinema_p <- function(p) {
  if (!is.finite(p)) return("p not available")
  if (p < 0.001) "p < 0.001" else paste0("p = ", formatC(p, format = "f", digits = if (p < 0.01) 3 else 2))
}

cinema_names <- function(s, most = 5) {
  if (length(s) <= most) return(and_list(s))
  paste0(paste(s[seq_len(most)], collapse = ", "), " and ", length(s) - most, " more")
}

# What each area means for the first treatment of a comparison.
cinema_area_words <- function(small_values) {
  benefit_high <- small_values == "undesirable"
  list(low = if (benefit_high) "an important harm" else "an important benefit",
       high = if (benefit_high) "an important benefit" else "an important harm")
}

# An interval's reading against the range, in words: short for a label, long
# for a sentence. Bits as in cinema_zones().
cinema_reading <- function(m, small_values, long = FALSE) {
  w <- cinema_area_words(small_values)
  parts <- list(w$low, "little difference", w$high)
  vapply(m, function(z) {
    hit <- c(z %% 2 == 1, (z %/% 2) %% 2 == 1, z >= 4)
    p <- unlist(parts[hit])
    if (!long) p <- sub("^an important ", "", p)
    if (length(p) == 1) paste(p, "only") else if (length(p) == 2) paste(p, collapse = " or ") else
      paste0(p[1], ", ", p[2], " or ", p[3])
  }, character(1))
}

# Apply every rule. Each domain gives a level (0 to 2, NA when not judged), a
# reason in words and a source.
cinema_apply_rules <- function(cn) {
  pr <- cn$pairs
  n <- nrow(pr)
  out <- list()
  for (d in c("bias", "indirectness")) {
    what <- if (d == "bias") "rob" else "indirectness"
    out[[d]] <- cinema_study_domain(cn, what, cn$rule[[d]])
  }
  out$reporting <- data.frame(level = rep(NA_integer_, n),
                              reason = "Not judged. Reporting bias cannot be computed from the data; give your judgment in `reporting`.",
                              source = "Not judged", stringsAsFactors = FALSE)
  out$imprecision <- cinema_imprecision(cn)
  out$heterogeneity <- cinema_heterogeneity(cn)
  out$incoherence <- cinema_incoherence_rule(cn)
  cn$domains <- out[names(cinema_domain_names)]
  cn
}

cinema_study_domain <- function(cn, what, rule) {
  n <- nrow(cn$pairs)
  sj <- cn$studies[[what]]
  if (is.null(sj)) {
    return(data.frame(level = rep(NA_integer_, n),
                      reason = paste0("Not judged. Give study judgments in `", what,
                                      "` to compute it, or your own judgment in `judgments`."),
                      source = "Not judged", stringsAsFactors = FALSE))
  }
  rows <- lapply(seq_len(n), function(i) {
    c1 <- cn$contrib[cn$contrib$key == cn$pairs$key[i], , drop = FALSE]
    lv <- sj$level[match(c1$study, sj$study)]
    level <- cinema_rule(lv, c1$share, rule)
    list(level = level, reason = cinema_study_reason(lv, c1$share, c1$study, what, rule, level))
  })
  data.frame(level = vapply(rows, `[[`, 0L, "level"),
             reason = vapply(rows, `[[`, "", "reason"),
             source = paste0("Computed: ", rule, " rule over the study judgments, weighted by contribution"),
             stringsAsFactors = FALSE)
}

cinema_study_reason <- function(levels, shares, studies, what, rule, level) {
  keep <- is.finite(shares) & shares > 1e-6
  phrase <- cinema_study_phrase[[what]]
  tot <- vapply(0:2, function(k) sum(shares[keep & levels == k]), numeric(1))
  parts <- vapply(c(2, 1, 0), function(k) {
    hit <- keep & levels == k
    if (!any(hit)) return(NA_character_)
    s <- studies[hit][order(-shares[hit])]
    paste0("studies ", phrase[k + 1], " supply ", cinema_pct(tot[k + 1]), " (", cinema_names(s), ")")
  }, character(1))
  lead <- paste0(capitalize(and_list(parts[!is.na(parts)])), ".")
  tail <- switch(rule,
    average = sprintf(" Average rule: scoring low 1, moderate 2 and high 3, the contribution weighted score is %s, which rounds to %d.",
                      formatC(sum(shares[keep] * (levels[keep] + 1)) / sum(shares[keep]), format = "f", digits = 2),
                      level + 1L),
    majority = paste0(" Majority rule: the largest share, ", cinema_pct(max(tot)), ", comes from studies ",
                      phrase[level + 1], "."),
    highest = paste0(" Highest rule: the most serious judgment among the studies that contribute is ",
                     tolower(cinema_study_words[[what]][level + 1]), ".")
  )
  paste0(lead, tail)
}

# An interval's step in words, naming the treatment its values favor and the
# limit on the other side of no effect.
cinema_step_text <- function(step, within, e, fav, far, ratio, other) {
  ifelse(within, "lies entirely within the range of little difference",
    ifelse(step == 0, paste0("lies entirely on one side of no effect, so no value in it favors ", other),
      ifelse(step == 1,
             paste0("crosses no effect into the range of little difference on the other side, but not beyond ",
                    cinema_num(far, ratio)),
             paste0("passes ", cinema_num(far, ratio),
                    ", the limit on the other side of no effect, so it holds important effects in both directions"))))
}

# The treatment the point estimate favors, and the limit beyond no effect on
# the other side.
cinema_side <- function(cn) {
  pr <- cn$pairs
  lim <- log_or(cn$threshold, cn$ratio)
  first <- (pr$te >= 0) == (cn$small_values == "undesirable")
  list(fav = ifelse(first, pr$a, pr$b), other = ifelse(first, pr$b, pr$a),
       far = ifelse(pr$te >= 0, lim[1], lim[2]), lim = lim)
}

cinema_limits_text <- function(cn) {
  paste(cinema_num(log_or(cn$threshold, cn$ratio), cn$ratio), collapse = " and ")
}

log_or <- function(v, ratio) if (ratio) log(v) else v

cinema_imprecision <- function(cn) {
  pr <- cn$pairs
  n <- nrow(pr)
  if (is.null(cn$threshold)) {
    return(data.frame(level = rep(NA_integer_, n),
                      reason = "Not judged. Give the limits of little difference in `threshold`.",
                      source = "Not judged", stringsAsFactors = FALSE))
  }
  sd <- cinema_side(cn)
  lim <- sd$lim
  step <- cinema_step(pr$te, pr$lo, pr$hi, lim[1], lim[2])
  within <- pr$lo >= lim[1] & pr$hi <= lim[2]
  pct <- paste0(format(100 * cn$level), "%")
  reason <- paste0("The ", pct, " CI, ", cinema_interval(pr$lo, pr$hi, cn$ratio), ", ",
                   cinema_step_text(step, within, pr$te, sd$fav, sd$far, cn$ratio, sd$other),
                   "; the limits of little difference are ", cinema_limits_text(cn), ".")
  data.frame(level = step, reason = reason, source = "Computed: CI against the limits",
             stringsAsFactors = FALSE)
}

cinema_heterogeneity <- function(cn) {
  pr <- cn$pairs
  n <- nrow(pr)
  if (cn$pooled == "common") {
    return(data.frame(level = rep(NA_integer_, n),
                      reason = "Not judged. A common effect model assumes no heterogeneity; use a random effects model to judge it.",
                      source = "Not judged", stringsAsFactors = FALSE))
  }
  if (is.null(cn$threshold)) {
    return(data.frame(level = rep(NA_integer_, n),
                      reason = "Not judged. Give the limits of little difference in `threshold`.",
                      source = "Not judged", stringsAsFactors = FALSE))
  }
  sd <- cinema_side(cn)
  lim <- sd$lim
  ci <- cinema_step(pr$te, pr$lo, pr$hi, lim[1], lim[2])
  pi <- cinema_step(pr$te, pr$plo, pr$phi, lim[1], lim[2])
  ok <- is.finite(pr$plo) & is.finite(pr$phi)
  level <- ifelse(ok, pmax(0L, pi - ci), NA_integer_)
  pct <- paste0(format(100 * cn$level), "%")
  within <- pr$plo >= lim[1] & pr$phi <= lim[2]
  steps <- c(" Both lead to the same conclusion.",
             " The prediction interval reaches one step further than the CI.",
             " The prediction interval reaches two steps further than the CI.")
  tau <- paste0(" The between-study variance, common to the network, is estimated as ",
                formatC(cn$tau2, format = "f", digits = 3), " from ", cn$n_studies,
                " studies; with few studies it, and so the prediction interval, is poorly estimated.")
  reason <- ifelse(ok,
    paste0("The ", pct, " prediction interval, ", cinema_interval(pr$plo, pr$phi, cn$ratio), ", ",
           cinema_step_text(pi, within, pr$te, sd$fav, sd$far, cn$ratio, sd$other), "; the CI ",
           ifelse(pr$lo >= lim[1] & pr$hi <= lim[2], "lies within the range of little difference",
                  c("stays on the same side of no effect", "crosses no effect but not the limit beyond it",
                    "passes the limit beyond no effect")[ci + 1]),
           ".", steps[pmax(0L, pi - ci) + 1], tau),
    "Not judged. The prediction interval could not be computed.")
  data.frame(level = level, reason = reason,
             source = ifelse(ok, "Computed: prediction interval against the limits", "Not judged"),
             stringsAsFactors = FALSE)
}

cinema_incoherence_rule <- function(cn) {
  pr <- cn$pairs
  n <- nrow(pr)
  g <- cn$global
  ratio <- cn$ratio
  what <- if (ratio) "the ratio of the direct to the indirect estimate" else "the direct minus the indirect estimate"
  level <- rep(NA_integer_, n)
  reason <- character(n)
  source <- character(n)
  lim <- if (!is.null(cn$threshold)) log_or(cn$threshold, ratio)
  area_names <- if (!is.null(lim)) {
    c(paste("below", cinema_num(lim[1], ratio)), paste("from", cinema_interval(lim[1], lim[2], ratio)),
      paste("above", cinema_num(lim[2], ratio)))
  }
  areas <- function(m) and_list(area_names[c(m %% 2 == 1, (m %/% 2) %% 2 == 1, m >= 4)])
  global_text <- if (is.null(g)) "" else
    paste0("the global design by treatment test gives Q = ", formatC(g$Q, format = "f", digits = 2),
           " on ", g$df, " df, ", cinema_p(g$p))
  for (i in seq_len(n)) {
    r <- pr[i, ]
    if (r$type == "mixed") {
      lead <- paste0("The inconsistency factor, ", what, ", is ",
                     cinema_estimate(r$f_te, r$f_lo, r$f_hi, ratio), ", ", cinema_p(r$f_p),
                     " (", cn$split_method, "). ")
      source[i] <- "Computed: local test of direct against indirect evidence"
      if (is.finite(r$f_p) && r$f_p > 0.10) {
        level[i] <- 0L
        reason[i] <- paste0(lead, "With p above 0.10 CINeMA gives no concerns whatever the intervals show. ",
                            "The test has low power, so this is weak evidence of agreement, not proof of it.")
      } else if (is.null(lim)) {
        reason[i] <- paste0(lead, "Not judged: with p at 0.10 or less, the rule compares the intervals with the limits of little difference; give them in `threshold`.")
        source[i] <- "Not judged"
      } else {
        zd <- cinema_zones(r$d_lo, r$d_hi, lim[1], lim[2])
        zi <- cinema_zones(r$i_lo, r$i_hi, lim[1], lim[2])
        ag <- cinema_agreement(zd, zi)
        common <- ag$common
        level[i] <- ag$level
        reason[i] <- paste0(lead, "With p at 0.10 or less, CINeMA compares where the two ", format(100 * cn$level),
                            "% CIs lie: the direct one reaches ", areas(zd), ", the indirect one ",
                            areas(zi), ", so they agree on ", common, " of the 3 areas.")
      }
    } else {
      why <- if (r$type == "direct") "no independent indirect evidence exists for this comparison" else
        "no study compares these treatments directly"
      if (is.null(g)) {
        level[i] <- 2L
        reason[i] <- paste0("No local test is possible: ", why, ". The global design by treatment test cannot be computed either, because the network has no closed loop, and CINeMA then gives major concerns.")
      } else {
        level[i] <- if (g$p < 0.05) 2L else if (g$p <= 0.10) 1L else 0L
        reason[i] <- paste0("No local test is possible: ", why, ". Judged from the whole network instead: ",
                            global_text, ", so ",
                            c("above 0.10: no concerns", "from 0.05 to 0.10: some concerns", "below 0.05: major concerns")[level[i] + 1],
                            ". The test has low power.")
      }
      source[i] <- "Computed: global design by treatment test"
    }
  }
  data.frame(level = level, reason = reason, source = source, stringsAsFactors = FALSE)
}

# Replace computed judgments with yours: `user` has key, domain, level and
# reason.
cinema_override <- function(cn, user) {
  for (r in seq_len(nrow(user))) {
    if (is.na(user$level[r])) next
    i <- match(user$key[r], cn$pairs$key)
    d <- user$domain[r]
    cn$domains[[d]]$level[i] <- user$level[r]
    cn$domains[[d]]$reason[i] <- if (nzchar(user$reason[r])) user$reason[r] else "Your judgment; no reason was given."
    cn$domains[[d]]$source[i] <- "Yours"
  }
  cn
}

# Normalize a column name for matching: lower case letters and digits only.
cinema_key <- function(x) gsub("[^a-z0-9]", "", tolower(x))

# The comparison each row of a table of yours refers to, as a pair key.
cinema_rows_pairs <- function(df, cn, arg) {
  nm <- cinema_key(names(df))
  t1 <- match(c("treat1", "treatment1", "t1"), nm)
  t2 <- match(c("treat2", "treatment2", "t2"), nm)
  t1 <- t1[!is.na(t1)][1]
  t2 <- t2[!is.na(t2)][1]
  cmp <- match("comparison", nm)
  if (!is.na(t1) && !is.na(t2)) {
    a <- as.character(df[[t1]])
    b <- as.character(df[[t2]])
    bad <- !a %in% cn$trts | !b %in% cn$trts | a == b
    if (any(bad)) {
      rlang::abort(paste0(arg, " names comparisons that are not in the network: ",
                          paste(unique(paste(a[bad], "vs", b[bad])), collapse = ", ")))
    }
    return(pair_key(a, b))
  }
  if (!is.na(cmp)) {
    seps <- unique(c(":", " vs ", " vs. ", " versus ", " - ", "-", " : ", cn$fit$sep.trts))
    ab <- tryCatch(cinema_match_pairs(df[[cmp]], cn$trts, seps, paste(arg, "comparisons")),
                   error = function(e) rlang::abort(conditionMessage(e), call = NULL))
    return(pair_key(ab$a, ab$b))
  }
  NULL
}

cinema_reporting <- function(reporting, cn) {
  if (!is.data.frame(reporting) || !"judgment" %in% names(reporting)) {
    rlang::abort("`reporting` must be a data frame with a column `judgment`, the comparison it applies to, and optionally `reason`.")
  }
  keys <- cinema_rows_pairs(reporting, cn, "`reporting`")
  if (is.null(keys)) {
    if (nrow(reporting) != 1) {
      rlang::abort("`reporting` needs `treat1` and `treat2`, or `comparison`, unless it has a single row for every comparison.")
    }
    reporting <- reporting[rep(1, nrow(cn$pairs)), , drop = FALSE]
    keys <- cn$pairs$key
  }
  lv <- cinema_level(reporting$judgment)
  if (length(lv$bad)) {
    rlang::abort(paste0("`reporting` has judgments that are not undetected or suspected: ",
                        paste(lv$bad, collapse = ", ")))
  }
  reason <- if ("reason" %in% names(reporting)) as.character(reporting$reason) else rep("", nrow(reporting))
  reason[is.na(reason)] <- ""
  data.frame(key = keys, domain = "reporting", level = lv$level, reason = reason,
             stringsAsFactors = FALSE)
}

# Domain judgments you made, wide (one column per domain) or long (a
# `domain` column), such as the report the CINeMA web application exports.
cinema_user_judgments <- function(df, cn) {
  if (!is.data.frame(df)) rlang::abort("`judgments` must be a data frame.")
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  keys <- cinema_rows_pairs(df, cn, "`judgments`")
  if (is.null(keys)) {
    rlang::abort("`judgments` needs the comparison, as `treat1` and `treat2` or as `comparison`.")
  }
  domain_of <- function(nm) {
    k <- cinema_key(nm)
    ifelse(k %in% c("withinstudybias", "withinstudy", "bias", "riskofbias", "rob", "studylimitations"), "bias",
      ifelse(k %in% c("reportingbias", "reporting", "publicationbias"), "reporting",
        ifelse(k %in% c("indirectness"), "indirectness",
          ifelse(k %in% c("imprecision"), "imprecision",
            ifelse(k %in% c("heterogeneity"), "heterogeneity",
              ifelse(k %in% c("incoherence", "inconsistency"), "incoherence", NA_character_))))))
  }
  nm <- cinema_key(names(df))
  overall <- NULL
  if ("domain" %in% nm) {
    if (!"judgment" %in% nm) rlang::abort("A long `judgments` needs columns `domain` and `judgment`.")
    dom <- domain_of(df[[which(nm == "domain")[1]]])
    if (anyNA(dom)) {
      rlang::abort(paste0("`judgments` has domains that are not CINeMA's six: ",
                          paste(unique(df[[which(nm == "domain")[1]]][is.na(dom)]), collapse = ", ")))
    }
    judg <- df[[which(nm == "judgment")[1]]]
    reason <- if ("reason" %in% nm) as.character(df[[which(nm == "reason")[1]]]) else rep("", nrow(df))
    long <- data.frame(key = keys, domain = dom, judgment = judg, reason = reason, stringsAsFactors = FALSE)
  } else {
    base <- sub("reasons?$", "", nm)
    is_reason <- grepl("reasons?$", nm) & nm != base
    dom <- domain_of(base)
    cols <- which(!is.na(dom) & !is_reason)
    if (!length(cols)) {
      rlang::abort("`judgments` has no column named after a CINeMA domain, such as `Imprecision`, and no `domain` column.")
    }
    long <- do.call(rbind, lapply(cols, function(j) {
      rc <- which(is_reason & dom == dom[j])
      data.frame(key = keys, domain = dom[j], judgment = as.character(df[[j]]),
                 reason = if (length(rc)) as.character(df[[rc[1]]]) else "", stringsAsFactors = FALSE)
    }))
    conf <- which(nm %in% c("confidencerating", "confidence", "overallconfidence"))
    note <- which(nm %in% c("reasonsfordowngrading", "reasonfordowngrading", "downgrading"))
    if (length(conf) || length(note)) {
      overall <- data.frame(key = keys,
                            confidence = if (length(conf)) as.character(df[[conf[1]]]) else NA_character_,
                            note = if (length(note)) as.character(df[[note[1]]]) else NA_character_,
                            stringsAsFactors = FALSE)
    }
  }
  lv <- cinema_level(long$judgment)
  if (length(lv$bad)) {
    rlang::abort(paste0("`judgments` has judgments that are not no, some or major concerns: ",
                        paste(lv$bad, collapse = ", ")))
  }
  long$level <- lv$level
  long$reason[is.na(long$reason)] <- ""
  list(long = long[c("key", "domain", "level", "reason")], overall = overall)
}

# The object users see: judgments in a long table, estimates on the scale of
# the effect, contributions and the settings.
cinema_finish <- function(cn) {
  pr <- cn$pairs
  n <- nrow(pr)
  words <- function(d, level) {
    w <- if (d == "reporting") cinema_reporting_words else cinema_concern_words
    ifelse(is.na(level), "Not judged", w[level + 1])
  }
  long <- do.call(rbind, lapply(seq_len(n), function(i) {
    data.frame(treat1 = pr$a[i], treat2 = pr$b[i], domain = unname(cinema_domain_names),
               level = vapply(cn$domains, function(d) d$level[i], 0L),
               judgment = vapply(names(cinema_domain_names), function(d) words(d, cn$domains[[d]]$level[i]), ""),
               reason = vapply(cn$domains, function(d) d$reason[i], ""),
               source = vapply(cn$domains, function(d) d$source[i], ""),
               stringsAsFactors = FALSE, row.names = NULL)
  }))
  shown <- function(v) if (cn$ratio) exp(v) else v
  comparisons <- data.frame(
    treat1 = pr$a, treat2 = pr$b, evidence = pr$type, studies = pr$k, direct_share = pr$prop,
    estimate = shown(pr$te), lower = shown(pr$lo), upper = shown(pr$hi),
    pred_lower = shown(pr$plo), pred_upper = shown(pr$phi),
    direct = shown(pr$d_te), direct_lower = shown(pr$d_lo), direct_upper = shown(pr$d_hi),
    indirect = shown(pr$i_te), indirect_lower = shown(pr$i_lo), indirect_upper = shown(pr$i_hi),
    ifactor = shown(pr$f_te), ifactor_lower = shown(pr$f_lo), ifactor_upper = shown(pr$f_hi),
    ifactor_p = pr$f_p, stringsAsFactors = FALSE)
  contributions <- if (!is.null(cn$contrib)) {
    i <- match(cn$contrib$key, pr$key)
    out <- data.frame(treat1 = pr$a[i], treat2 = pr$b[i], study = cn$contrib$study,
                      share = cn$contrib$share, stringsAsFactors = FALSE)
    out <- out[order(i, -out$share), ]
    out[out$share > 0, ]
  }
  if (!is.null(contributions)) rownames(contributions) <- NULL
  cn$judgments <- long
  cn$comparisons <- comparisons
  cn$contributions <- contributions
  structure(cn, class = "cinema")
}

#' @export
print.cinema <- function(x, ...) {
  pr <- x$pairs
  cat("CINeMA judgments for ", nrow(pr), " comparisons of ", length(x$order), " treatments (",
      x$pooled, " effects, ", tolower(x$measure), ")\n", sep = "")
  if (!is.null(x$threshold)) {
    cat("Range of little difference: ", paste(format(x$threshold, digits = 3), collapse = " to "),
        ", for the first treatment against the second\n", sep = "")
  }
  wide <- data.frame(Comparison = paste(pr$a, "vs", pr$b), stringsAsFactors = FALSE)
  for (d in names(cinema_domain_names)) {
    wide[[cinema_domain_names[[d]]]] <- x$judgments$judgment[x$judgments$domain == cinema_domain_names[[d]]]
  }
  print(wide, right = FALSE, row.names = FALSE)
  src <- vapply(names(cinema_domain_names), function(d) {
    s <- unique(x$domains[[d]]$source)
    paste0(cinema_domain_names[[d]], ": ", paste(sub("^Computed: ", "computed, ", s), collapse = "; "))
  }, character(1))
  cat("\n", paste(src, collapse = "\n"), "\n", sep = "")
  cat("Domains are shown side by side and never added into a score. The reasons are in $judgments.\n")
  invisible(x)
}

# The judgments a plot works from: `x` as given when it comes from
# cinema_judge(), otherwise judged here with the other arguments.
cinema_input <- function(x, ..., fn) {
  if (inherits(x, "cinema")) {
    if (...length()) {
      rlang::abort(paste0("`x` already holds judgments from cinema_judge(), with their settings; give ",
                          fn, "() no more of them, or give it the netmeta fit instead."))
    }
    return(x)
  }
  cinema_judge(x, ...)
}

# ---- Shared pieces of the plots ------------------------------------------

# A judgment as a colored mark and its words, for panels and readouts.
cinema_chip <- function(level, words) {
  mark <- if (is.na(level)) '<i class="ggx-cn-hollow"></i>' else
    paste0('<i style="background:', cinema_ink$level[level + 1], '"></i>')
  paste0('<span class="ggx-cn-lv">', mark, esc(words), "</span>")
}

# Where a judgment came from, in two or three words.
cinema_source_short <- function(source) {
  s <- source
  s[grepl("rule over the study", source)] <- sub("^Computed: (\\w+) rule.*$", "Computed, \\1 rule",
                                                  source[grepl("rule over the study", source)])
  s[source == "Computed: CI against the limits"] <- "Computed from the CI"
  s[source == "Computed: prediction interval against the limits"] <- "Computed from the prediction interval"
  s[grepl("local test", source)] <- "Computed, local test"
  s[grepl("global design", source)] <- "Computed, global test"
  s
}

# Which judgments were computed by which rule and which are yours, domain by
# domain, for a caption. Domains with the same sources share a sentence.
cinema_sources_text <- function(cn) {
  phrase <- function(s) {
    if (s == "Yours") return("your judgments")
    if (s == "Not judged") return("not judged")
    if (grepl("global design", s)) return("from the global design by treatment test where no local test is possible")
    w <- sub("^Computed: ", "", s)
    w <- sub(" rule over the study judgments, weighted by contribution",
             " rule from the study judgments, each weighted by its contribution", w)
    paste0(if (grepl(" rule from ", w)) "computed by the " else "computed from the ", w)
  }
  said <- vapply(names(cinema_domain_names), function(d) {
    s <- unique(cn$domains[[d]]$source)
    s <- s[order(s %in% c("Yours", "Not judged"), grepl("global design", s))]
    paste(vapply(s, phrase, character(1)), collapse = ", or ")
  }, character(1))
  groups <- split(names(said), factor(said, levels = unique(said)))
  parts <- vapply(names(groups), function(g) {
    who <- unname(cinema_domain_names[groups[[g]]])
    who[-1] <- tolower(who[-1])
    paste0(and_list(who), ": ", g)
  }, character(1))
  paste0(paste(parts, collapse = ". "), ".")
}

# "a" or "an" before a word.
cinema_article <- function(w) paste(if (grepl("^[aeiou]", tolower(w))) "an" else "a", w)

# Lines of text that fit a width, for captions.
cinema_wrap <- function(text, width, pt, family) {
  unlist(lapply(text, wrap_words, width = width, pt = pt, family = family))
}

# A contribution bar in HTML: one block per study, grouped by its judgment
# as CINeMA draws them, low first, and each block as wide as the study's
# share of the estimate.
cinema_bar_html <- function(cn, key, what) {
  sj <- cn$studies[[what]]
  if (is.null(cn$contrib) || is.null(sj)) return("")
  c1 <- cn$contrib[cn$contrib$key == key & cn$contrib$share > 1e-6, , drop = FALSE]
  lv <- sj$level[match(c1$study, sj$study)]
  o <- order(lv, -c1$share)
  c1 <- c1[o, , drop = FALSE]
  lv <- lv[o]
  blocks <- vapply(seq_len(nrow(c1)), function(i) {
    w <- 100 * c1$share[i]
    label <- if (w >= 12) paste0(esc(c1$study[i]), " ", round(w), "%") else if (w >= 6) paste0(round(w), "%") else ""
    paste0('<span class="ggx-cn-seg', if (lv[i] == 2) " ggx-cn-hatch" else "", '" style="width:',
           formatC(w, format = "f", digits = 2), "%;background:", cinema_ink$level[lv[i] + 1],
           '" title="', esc(paste0(c1$study[i], ": ", cinema_pct(c1$share[i]), ", ",
                                  tolower(cinema_study_words[[what]][lv[i] + 1]))), '">', label, "</span>")
  }, character(1))
  paste0('<div class="ggx-cn-bar" role="img" aria-label="',
         esc(paste0("Contributions by ", tolower(cinema_study_title[[what]]), ": ",
                    paste0(c1$study, " ", cinema_pct(c1$share), collapse = ", "))), '">',
         paste(blocks, collapse = ""), "</div>")
}
