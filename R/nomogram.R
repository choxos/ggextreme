#' Draw an interactive nomogram for a regression model
#'
#' Draws the nomogram of a fitted regression model: one axis per predictor,
#' scaled in points, a total points axis and one or more axes that turn the
#' total into a prediction. In the widget every predictor has a handle.
#' Dragging it, clicking a category or using the arrow keys sets a patient's
#' values, and the points, the total and the prediction with its 95%
#' confidence interval follow, computed in the page from the model's
#' coefficients and their covariance.
#'
#' Contributions are computed through the model's design matrix, so
#' transformed and nonlinear terms, such as `log(x)`, `poly()`, `ns()`,
#' `rcs()` or a smooth from 'mgcv', are drawn as they were fitted. An axis
#' whose effect rises and then falls is folded onto more than one line, as
#' rms does. When predictors interact, the axis of a later predictor in the
#' interaction is drawn for the current values of the earlier ones and
#' redraws as they change. Numeric axes run from the 2.5th to the 97.5th
#' percentile of the data unless `ranges` says otherwise.
#'
#' The prediction depends on the model:
#' \describe{
#'   \item{Linear models}{`lm()`, `nlme::gls()`, `quantreg::rq()`,
#'     `rms::ols()` and Gaussian models with an identity link: the predicted
#'     mean, with a prediction interval for `lm()`.}
#'   \item{Generalized linear models}{`glm()`, `MASS::glm.nb()`,
#'     `geepack::geeglm()`, `logistf::logistf()`, `rms::lrm()` and
#'     `mgcv::gam()`: the mean on the response scale, such as a probability
#'     or a rate, through the model's link.}
#'   \item{Mixed models}{`lme4::lmer()`, `lme4::glmer()`, `nlme::lme()` and
#'     `glmmTMB::glmmTMB()`: the prediction for a typical cluster, with the
#'     random effects at zero. On a nonlinear link this is the prediction for
#'     that cluster, not the average over the population. The interval
#'     reflects the fixed effects only. For glmmTMB, zero inflation and
#'     dispersion models are left out.}
#'   \item{Cox models}{`survival::coxph()` and `rms::cph()`: survival at
#'     each of `times`, with the interval `survival::survfit()` gives for
#'     the same patient. A stratified model gets a choice of stratum.}
#'   \item{Parametric survival models}{`survival::survreg()` and
#'     `rms::psm()`: the median survival time and survival at `times`.}
#'   \item{Ordinal models}{`MASS::polr()`, `ordinal::clm()`, `rms::orm()`
#'     and ordinal `rms::lrm()`: the probability of each category or above,
#'     with the probability of every category beside the plot.}
#'   \item{Multinomial models}{`nnet::multinom()`: one nomogram per
#'     category, scaled on its odds against the reference category, with
#'     the probability of every category beside the plot.}
#' }
#'
#' @param fit A fitted regression model. See Details for the classes read.
#' @param data The data the model was fitted to. Defaults to the data named
#'   in the model's call, which is enough when that data is still around.
#' @param values Starting values for the predictors, as a named list. The
#'   others start at the median, or at the most common category.
#' @param labels Axis labels for the predictors, as a named character
#'   vector. The others use the column's `label` attribute or its name.
#' @param ranges Ranges for numeric axes, as a named list of pairs.
#' @param outcome Label of the prediction axis. For a model with several,
#'   a character vector with one label each.
#' @param times For a survival model, the times to predict survival at,
#'   named to label the axes, such as `c("1 year" = 365, "5 years" = 1826)`.
#'   Defaults to the median follow-up time.
#' @param level For a multinomial model, the category whose nomogram the
#'   static copy draws. Defaults to the first after the reference.
#' @param title,caption Title above the nomogram and note below it.
#' @param family Font family. The package ships Lato and registers it on load.
#'
#' @return An object of class `ggnomogram`, which prints as an interactive
#'   widget. Use [graph_widget()], [graph_plot()] or [graph_save()] for the
#'   widget, a static ggplot or a file. [nomogram_predict()] gives the
#'   prediction for any set of values, as the widget computes it.
#' @export
#'
#' @examples
#' bw <- MASS::birthwt
#' bw$race <- factor(bw$race, labels = c("White", "Black", "Other"))
#' bw$smoke <- factor(bw$smoke, labels = c("No", "Yes"))
#' fit <- glm(low ~ age + lwt + race + smoke, family = binomial, data = bw)
#' n <- ggnomogram(fit, outcome = "Risk of low birth weight",
#'                 labels = c(age = "Age (years)", lwt = "Weight (lb)"))
#' n
#' nomogram_predict(n, list(age = 30, lwt = 110, race = "Black", smoke = "Yes"))
ggnomogram <- function(fit, data = NULL, values = NULL, labels = NULL, ranges = NULL,
                       outcome = NULL, times = NULL, level = NULL,
                       title = NULL, caption = NULL, family = "Lato") {
  nm <- nomo_model(fit, data, times)
  spec <- nomo_build(nm, values, labels, ranges, outcome, level)
  lay <- nomo_layout(spec, title, caption, family)
  structure(
    list(plot = nomo_draw(lay), widget_plot = nomo_draw(lay, blank = TRUE),
         width = lay$page$width / 72, height = lay$page$height / 72, title = title,
         on_render = paste0("ggextremeNomogram(el, data); ggextremeDetails(el, ",
                            js_string("About this nomogram"), ", ",
                            js_string(nomo_notes_html(spec)), ");"),
         render_data = nomo_render_data(lay$spec, lay), hover_inv = "",
         spec = lay$spec, layout = lay),
    class = c("ggnomogram", "ggx_graph")
  )
}

#' Predict from a nomogram
#'
#' Gives the linear predictor, its standard error and every prediction the
#' nomogram shows, for any set of predictor values, computed the way the
#' widget computes them. Use it to check a nomogram against the model's own
#' `predict()` method, or to read predictions for a list of patients.
#'
#' @param x A nomogram from [ggnomogram()].
#' @param values A named list of predictor values. Predictors left out take
#'   their starting values.
#'
#' @return A list with the total `points`, the linear predictor `lp` and its
#'   standard error `se`, and `outputs`, a data frame with one row per
#'   prediction and its confidence interval.
#' @export
nomogram_predict <- function(x, values = list()) {
  stopifnot(inherits(x, "ggnomogram"))
  nomo_predict(x$spec, values)
}

# Model adapters --------------------------------------------------------------

# Every adapter returns the same shape: the kind of prediction, the
# coefficients and their covariance, a function giving design rows for new
# data, and which variables each design column depends on.
nomo_model <- function(fit, data, times) {
  nm <- if (inherits(fit, "rms")) nomo_rms(fit)
  else if (inherits(fit, "gam")) nomo_gam(fit)
  else if (inherits(fit, "merMod")) nomo_lme4(fit)
  else if (inherits(fit, "glmmTMB")) nomo_glmmtmb(fit)
  else if (inherits(fit, "lme") || inherits(fit, "gls")) nomo_nlme(fit)
  else if (inherits(fit, "coxph")) nomo_cox(fit)
  else if (inherits(fit, "survreg")) nomo_survreg(fit)
  else if (inherits(fit, "polr")) nomo_polr(fit)
  else if (inherits(fit, "clm")) nomo_clm(fit)
  else if (inherits(fit, "multinom")) nomo_multinom(fit)
  else if (inherits(fit, "logistf")) nomo_logistf(fit)
  else if (inherits(fit, "rq")) nomo_rq(fit)
  else if (inherits(fit, "glm")) nomo_glm(fit)
  else if (inherits(fit, "lm")) nomo_lm(fit)
  else rlang::abort(paste0("ggnomogram() cannot read a model of class `",
                           class(fit)[1], "`."))
  nm$data <- nomo_data(fit, data, nm$vars)
  if (nm$kind %in% c("cox", "aft")) nm <- nomo_times(nm, fit, times)
  nm
}

nomo_check_beta <- function(beta) {
  if (anyNA(beta)) {
    rlang::abort(paste0("The model has coefficients that could not be estimated: ",
                        paste(names(beta)[is.na(beta)], collapse = ", "), "."))
  }
  beta
}

# Design rows from a terms object, with the columns named by the
# coefficients, and the variables each column depends on.
nomo_terms <- function(tt, cols, xlev, contrasts, drop = character(0)) {
  tt <- stats::delete.response(tt)
  if (length(drop)) {
    labs <- attr(tt, "term.labels")
    gone <- which(labs %in% drop)
    if (length(gone)) tt <- stats::drop.terms(tt, gone, keep.response = FALSE)
  }
  if (length(xlev)) xlev <- xlev[intersect(names(xlev), rownames(attr(tt, "factors")))]
  fac <- attr(tt, "factors")
  term_vars <- if (length(fac)) {
    lapply(seq_len(ncol(fac)), function(j) {
      unique(unlist(lapply(rownames(fac)[fac[, j] > 0], function(r) all.vars(str2lang(r)))))
    })
  } else list()
  vars_of <- stats::setNames(vector("list", length(cols)), cols)
  offs <- attr(tt, "offset")
  off_vars <- if (length(offs)) {
    unique(unlist(lapply(offs, function(i) all.vars(attr(tt, "variables")[[i + 1]]))))
  } else character(0)
  design <- function(nd) {
    mf <- stats::model.frame(tt, nd, xlev = xlev, na.action = stats::na.pass)
    X <- stats::model.matrix(tt, mf, contrasts.arg = contrasts)
    miss <- setdiff(cols, colnames(X))
    if (length(miss)) {
      rlang::abort(paste0("The design matrix has no column for ", paste(miss, collapse = ", "), "."))
    }
    off <- stats::model.offset(mf)
    list(X = X[, cols, drop = FALSE], offset = if (is.null(off)) numeric(nrow(nd)) else as.numeric(off),
         assign = attr(X, "assign")[match(cols, colnames(X))])
  }
  list(tt = tt, design = design, term_vars = term_vars, off_vars = off_vars,
       vars = unique(c(unlist(term_vars), off_vars)))
}

# Which variables each design column depends on, from the assign index of a
# design matrix built once.
nomo_colvars <- function(tm, data) {
  a <- tm$design(data[1, , drop = FALSE])$assign
  lapply(a, function(j) if (is.na(j) || j == 0) character(0) else tm$term_vars[[j]])
}

nomo_assemble <- function(kind, tm, beta, V, response, ...) {
  c(list(kind = kind, beta = beta, V = V, design = tm$design, term_vars = tm$term_vars,
         off_vars = tm$off_vars, vars = tm$vars, response = response, colvars = NULL), list(...))
}

nomo_response <- function(fit) {
  f <- tryCatch(stats::formula(fit), error = function(e) NULL)
  if (is.null(f) || length(f) < 3) return("outcome")
  paste(deparse(f[[2]], width.cutoff = 500L), collapse = "")
}

nomo_lm <- function(fit) {
  beta <- nomo_check_beta(stats::coef(fit))
  tm <- nomo_terms(stats::terms(fit), names(beta), fit$xlevels, fit$contrasts)
  nomo_assemble("linear", tm, beta, stats::vcov(fit), nomo_response(fit),
                sigma = stats::sigma(fit), df = stats::df.residual(fit))
}

nomo_glm <- function(fit) {
  beta <- nomo_check_beta(stats::coef(fit))
  tm <- nomo_terms(stats::terms(fit), names(beta), fit$xlevels, fit$contrasts)
  fam <- stats::family(fit)
  if (fam$family == "gaussian" && fam$link == "identity") {
    return(nomo_assemble("linear", tm, beta, stats::vcov(fit), nomo_response(fit),
                         df = stats::df.residual(fit)))
  }
  nomo_assemble("link", tm, beta, stats::vcov(fit), nomo_response(fit),
                link = fam$link, family = fam$family)
}

nomo_logistf <- function(fit) {
  beta <- nomo_check_beta(stats::coef(fit))
  V <- fit$var
  dimnames(V) <- list(names(beta), names(beta))
  data <- fit$model
  xlev <- lapply(Filter(function(v) is.factor(v) || is.character(v), data), function(v) levels(factor(v)))
  tm <- nomo_terms(stats::terms(fit$formula), names(beta), xlev, NULL)
  nomo_assemble("link", tm, beta, V, nomo_response(fit), link = "logit", family = "binomial",
                note = "Firth's penalized likelihood, with covariance from the penalized fit.")
}

nomo_rq <- function(fit) {
  if (length(fit$tau) != 1) rlang::abort("A nomogram needs a quantile regression at a single `tau`.")
  beta <- nomo_check_beta(stats::coef(fit))
  V <- summary(fit, se = "nid", covariance = TRUE)$cov
  dimnames(V) <- list(names(beta), names(beta))
  tm <- nomo_terms(stats::terms(fit), names(beta), fit$xlevels, fit$contrasts)
  nomo_assemble("linear", tm, beta, V, nomo_response(fit), tau = fit$tau,
                note = paste0("Quantile regression at tau = ", fit$tau, ", with standard errors by the ",
                              "Hendricks and Koenker sandwich."))
}

nomo_lme4 <- function(fit) {
  rlang::check_installed("lme4")
  beta <- nomo_check_beta(lme4::fixef(fit))
  V <- as.matrix(stats::vcov(fit))
  frame <- fit@frame
  xlev <- lapply(Filter(function(v) is.factor(v) || is.character(v), frame), function(v) levels(factor(v)))
  tm <- nomo_terms(stats::terms(fit, fixed.only = TRUE), names(beta), xlev,
                   attr(lme4::getME(fit, "X"), "contrasts"))
  fam <- stats::family(fit)
  kind <- if (fam$family == "gaussian" && fam$link == "identity") "linear" else "link"
  nomo_assemble(kind, tm, beta, V, nomo_response(fit), link = fam$link, family = fam$family,
                mixed = TRUE)
}

nomo_glmmtmb <- function(fit) {
  rlang::check_installed("glmmTMB")
  beta <- nomo_check_beta(glmmTMB::fixef(fit)$cond)
  V <- as.matrix(stats::vcov(fit)$cond)
  frame <- fit$frame
  xlev <- lapply(Filter(function(v) is.factor(v) || is.character(v), frame), function(v) levels(factor(v)))
  tm <- nomo_terms(stats::terms(fit), names(beta), xlev, NULL)
  fam <- stats::family(fit)
  kind <- if (fam$family == "gaussian" && fam$link == "identity") "linear" else "link"
  nomo_assemble(kind, tm, beta, V, nomo_response(fit), link = fam$link, family = fam$family,
                mixed = TRUE)
}

nomo_nlme <- function(fit) {
  rlang::check_installed("nlme")
  beta <- nomo_check_beta(if (inherits(fit, "lme")) nlme::fixef(fit) else stats::coef(fit))
  V <- as.matrix(stats::vcov(fit))
  data <- tryCatch(nlme::getData(fit), error = function(e) nomo_call_data(fit))
  xlev <- lapply(Filter(function(v) is.factor(v) || is.character(v), data), function(v) levels(factor(v)))
  tt <- if (inherits(fit, "lme")) fit$terms else stats::terms(stats::formula(fit))
  tm <- nomo_terms(tt, names(beta), xlev, fit$contrasts)
  nomo_assemble("linear", tm, beta, V, nomo_response(fit), mixed = inherits(fit, "lme"))
}

nomo_gam <- function(fit) {
  rlang::check_installed("mgcv")
  beta <- nomo_check_beta(stats::coef(fit))
  V <- stats::vcov(fit)
  tp <- stats::delete.response(fit$pterms)
  fac <- attr(tp, "factors")
  pvars <- if (length(fac)) lapply(seq_len(ncol(fac)), function(j) {
    unique(unlist(lapply(rownames(fac)[fac[, j] > 0], function(r) all.vars(str2lang(r)))))
  }) else list()
  colvars <- vector("list", length(beta))
  np <- length(fit$assign)
  for (j in seq_len(np)) colvars[[j]] <- if (fit$assign[j] == 0) character(0) else pvars[[fit$assign[j]]]
  for (s in fit$smooth) {
    v <- s$term
    if (!identical(s$by, "NA")) v <- c(v, s$by)
    for (j in s$first.para:s$last.para) colvars[[j]] <- v
  }
  design <- function(nd) {
    X <- stats::predict(fit, newdata = nd, type = "lpmatrix")
    off <- attr(X, "model.offset")
    list(X = X[, names(beta), drop = FALSE],
         offset = if (is.null(off) || !length(off)) numeric(nrow(nd)) else as.numeric(off) + numeric(nrow(nd)))
  }
  fam <- stats::family(fit)
  kind <- if (fam$family == "gaussian" && fam$link == "identity") "linear" else "link"
  list(kind = kind, beta = beta, V = V, design = design, colvars = colvars, off_vars = character(0),
       vars = unique(unlist(colvars)), response = nomo_response(fit), link = fam$link,
       family = fam$family, note = "Smooth terms use the Bayesian covariance of the fit (Vp).")
}

nomo_cox <- function(fit) {
  tt <- stats::terms(fit)
  labs <- attr(tt, "term.labels")
  special <- function(name) grepl(paste0("^(survival::)?", name, "\\("), labs)
  if (any(special("tt"))) rlang::abort("A nomogram cannot show a Cox model with `tt()` terms.")
  strata_terms <- labs[special("strata")]
  drop <- c(strata_terms, labs[special("cluster")])
  beta <- nomo_check_beta(stats::coef(fit))
  tm <- nomo_terms(tt, names(beta), fit$xlevels, fit$contrasts, drop = drop)
  strata_vars <- unique(unlist(lapply(strata_terms, function(s) all.vars(str2lang(s)))))
  nomo_assemble("cox", tm, beta, stats::vcov(fit), nomo_response(fit), ctr = fit$means[names(beta)],
                strata_vars = strata_vars, fit = fit)
}

nomo_survreg <- function(fit) {
  if (length(fit$scale) > 1) rlang::abort("A nomogram cannot show a survreg model with a scale per stratum.")
  beta <- nomo_check_beta(stats::coef(fit))
  Vfull <- stats::vcov(fit)
  has_scale <- nrow(Vfull) > length(beta)
  dist <- survival::survreg.distributions[[fit$dist]]
  base <- if (is.null(dist$dist)) fit$dist else dist$dist
  trans <- if (is.null(dist$trans)) "identity" else if (identical(dist$trans, log) || fit$dist %in% c("weibull", "exponential", "rayleigh", "lognormal", "loglogistic")) "log" else "other"
  if (!base %in% c("extreme", "logistic", "gaussian") || trans == "other") {
    rlang::abort(paste0("A nomogram cannot show a survreg model with the `", fit$dist, "` distribution."))
  }
  xlev <- fit$xlevels
  tm <- nomo_terms(stats::terms(fit), names(beta), xlev, fit$contrasts)
  nomo_assemble("aft", tm, beta, Vfull, nomo_response(fit), scale = fit$scale,
                has_scale = has_scale, base = base, trans = trans,
                q50 = survival::survreg.distributions[[base]]$quantile(0.5), fit = fit)
}

nomo_polr <- function(fit) {
  beta <- nomo_check_beta(stats::coef(fit))
  zeta <- fit$zeta
  V <- stats::vcov(fit)
  V <- V[c(names(beta), names(zeta)), c(names(beta), names(zeta))]
  tm <- nomo_terms(stats::terms(fit), names(beta), fit$xlevels, fit$contrasts)
  link <- c(logistic = "logit", probit = "probit", cloglog = "cloglog", loglog = "loglog",
            cauchit = "cauchit")[[fit$method]]
  nomo_assemble("ordinal", tm, beta, V, nomo_response(fit), zeta = unname(zeta), link = link,
                outcome_levels = fit$lev)
}

nomo_clm <- function(fit) {
  if (!is.null(fit$nom.terms) || !is.null(fit$S.terms)) {
    rlang::abort("A nomogram cannot show a clm model with nominal or scale effects.")
  }
  beta <- nomo_check_beta(fit$beta)
  zeta <- fit$alpha
  V <- stats::vcov(fit)
  V <- V[c(names(beta), names(zeta)), c(names(beta), names(zeta))]
  tm <- nomo_terms(fit$terms, names(beta), fit$xlevels, fit$contrasts)
  nomo_assemble("ordinal", tm, beta, V, nomo_response(fit), zeta = unname(zeta), link = fit$link,
                outcome_levels = fit$y.levels)
}

nomo_multinom <- function(fit) {
  B <- stats::coef(fit)
  if (is.null(dim(B))) B <- matrix(B, nrow = 1, dimnames = list(fit$lev[2], names(B)))
  if (anyNA(B)) rlang::abort("The model has coefficients that could not be estimated.")
  tm <- nomo_terms(stats::terms(fit), colnames(B), fit$xlevels, fit$contrasts)
  V <- stats::vcov(fit)
  nomo_assemble("multinomial", tm, B, V, nomo_response(fit), outcome_levels = fit$lev)
}

nomo_rms <- function(fit) {
  rlang::check_installed("rms")
  beta <- nomo_check_beta(stats::coef(fit))
  k <- if (!is.null(fit$non.slopes)) fit$non.slopes else sum(names(beta) %in% c("Intercept", "(Intercept)"))
  slopes <- names(beta)[seq_along(beta) > k]
  colvars <- vector("list", length(slopes))
  names(colvars) <- slopes
  for (nm in names(fit$assign)) {
    v <- all.vars(str2lang(gsub(" \\* ", ":", nm)))
    for (j in fit$assign[[nm]]) colvars[[j - k]] <- v
  }
  design_x <- function(nd) {
    X <- stats::predict(fit, newdata = nd, type = "x")
    X <- matrix(X, nrow = nrow(nd), dimnames = list(NULL, slopes))
    X
  }
  vars <- unique(unlist(colvars))
  resp <- nomo_response(fit)
  V <- as.matrix(stats::vcov(fit, intercepts = "all"))
  if (inherits(fit, "ols")) {
    design <- function(nd) list(X = cbind(Intercept = 1, design_x(nd)), offset = numeric(nrow(nd)))
    return(list(kind = "linear", beta = beta, V = V[names(beta), names(beta)], design = design,
                colvars = c(list(character(0)), unname(colvars)), off_vars = character(0), vars = vars,
                response = resp, sigma = fit$stats[["Sigma"]], df = fit$df.residual))
  }
  if (inherits(fit, "cph")) {
    if (is.null(fit$x) || is.null(fit$y)) {
      rlang::abort("A nomogram of a cph model needs the model fitted with `x = TRUE, y = TRUE`.")
    }
    design <- function(nd) list(X = design_x(nd), offset = numeric(nrow(nd)))
    return(list(kind = "cox", beta = beta, V = V, design = design, colvars = unname(colvars),
                off_vars = character(0), vars = vars, response = resp,
                ctr = stats::setNames(colMeans(fit$x)[seq_along(beta)], names(beta)),
                strata_vars = character(0), fit = fit))
  }
  if (inherits(fit, "psm")) {
    design <- function(nd) list(X = cbind(Intercept = 1, design_x(nd)), offset = numeric(nrow(nd)))
    Vfull <- stats::vcov(fit)
    dist <- survival::survreg.distributions[[fit$dist]]
    base <- if (is.null(dist$dist)) fit$dist else dist$dist
    return(list(kind = "aft", beta = beta, V = Vfull, design = design,
                colvars = c(list(character(0)), unname(colvars)), off_vars = character(0),
                vars = vars, response = resp, scale = fit$scale, has_scale = nrow(Vfull) > length(beta),
                base = base, trans = if (fit$dist %in% c("gaussian", "logistic", "extreme")) "identity" else "log",
                q50 = survival::survreg.distributions[[base]]$quantile(0.5), fit = fit))
  }
  if (inherits(fit, "lrm") || inherits(fit, "orm")) {
    ints <- names(beta)[seq_len(k)]
    if (k == 1) {
      design <- function(nd) list(X = cbind(1, design_x(nd)), offset = numeric(nrow(nd)))
      return(list(kind = "link", beta = beta, V = V[names(beta), names(beta)], design = design,
                  colvars = c(list(character(0)), unname(colvars)), off_vars = character(0),
                  vars = vars, response = resp, link = "logit", family = "binomial"))
    }
    if (inherits(fit, "orm") && !identical(fit$family, "logistic")) {
      rlang::abort("A nomogram of an orm model needs the logistic family.")
    }
    # P(Y >= j) = F(alpha_j + x b), so the thresholds of P(Y <= j - 1) are -alpha_j.
    design <- function(nd) list(X = design_x(nd), offset = numeric(nrow(nd)))
    b <- beta[slopes]
    Vo <- V[c(slopes, ints), c(slopes, ints)]
    Vo[, ints] <- -Vo[, ints]
    Vo[ints, ] <- -Vo[ints, ]
    return(list(kind = "ordinal", beta = b, V = Vo, design = design, colvars = unname(colvars),
                off_vars = character(0), vars = vars, response = resp,
                zeta = -unname(beta[ints]), link = "logit",
                outcome_levels = if (!is.null(fit$yunique)) as.character(fit$yunique) else as.character(seq_len(k + 1))))
  }
  rlang::abort(paste0("ggnomogram() cannot read an rms model of class `", class(fit)[1], "`."))
}

# The data named in a model's call, looked up where its formula was written.
nomo_call_data <- function(fit) {
  call <- tryCatch(stats::getCall(fit), error = function(e) NULL)
  env <- tryCatch(environment(stats::formula(fit)), error = function(e) NULL)
  if (is.null(env)) env <- globalenv()
  if (is.null(call$data)) return(NULL)
  tryCatch(eval(call$data, env), error = function(e) NULL)
}

# The data behind the model: `data`, or the data named in the model's call.
nomo_data <- function(fit, data, vars) {
  if (is.null(data)) {
    data <- nomo_call_data(fit)
    if (is.null(data)) data <- tryCatch(as.data.frame(stats::model.frame(fit)), error = function(e) NULL)
  }
  if (is.null(data)) rlang::abort("Pass the data the model was fitted to as `data`.")
  data <- as.data.frame(data)
  miss <- setdiff(vars, names(data))
  if (length(miss)) {
    rlang::abort(paste0("`data` has no column for ", paste(miss, collapse = ", "),
                        ". Pass the data the model was fitted to as `data`."))
  }
  data[stats::complete.cases(data[vars]), , drop = FALSE]
}

# Survival times for a survival model's prediction axes, and for a Cox model
# the baseline hazard, its variance and the weighted covariate means at each,
# which give survfit()'s interval for any patient.
nomo_times <- function(nm, fit, times) {
  if (is.null(times)) {
    y <- tryCatch(fit$y, error = function(e) NULL)
    if (is.null(y)) y <- stats::model.response(stats::model.frame(fit))
    t <- as.numeric(y[, 1])
    times <- signif(stats::median(t), 2)
  }
  if (!is.numeric(times) || any(!is.finite(times)) || any(times <= 0)) {
    rlang::abort("`times` must be positive numbers.")
  }
  if (is.null(names(times))) names(times) <- paste("time", format(times, trim = TRUE))
  nm$times <- times
  if (nm$kind == "cox") {
    det <- survival::coxph.detail(nm$fit)
    bn <- names(nm$beta)
    means <- matrix(det$means, ncol = length(bn))
    st <- if (is.null(det$strata)) rep("All", length(det$time)) else rep(names(det$strata), det$strata)
    nm$strata <- lapply(unique(st), function(s) {
      i <- which(st == s)
      H0 <- cumsum(det$hazard[i])
      T1 <- cumsum(det$varhaz[i])
      A <- apply(means[i, , drop = FALSE] * det$hazard[i], 2, cumsum)
      A <- matrix(A, ncol = length(bn))
      at <- lapply(times, function(t) {
        k <- max(c(0, which(det$time[i] <= t)))
        if (k == 0) return(list(H0 = 0, T1 = 0, A = rep(0, length(bn))))
        list(H0 = H0[k], T1 = T1[k], A = A[k, ] - nm$ctr * H0[k])
      })
      list(label = sub("^[^=]*=", "", s), at = at)
    })
  }
  nm$fit <- NULL
  nm
}

# Building the nomogram -------------------------------------------------------

nomo_dims <- utils::modifyList(graph_dims, list(
  axis_w = 430,
  label_pt = 10.5,
  tick_pt = 9,
  row_h = 36,
  run_gap = 26,
  stagger = 11,
  grid = 401,
  grid_2d = 61,
  cells = 20000,
  right = 48
))

nomo_prob_ticks <- c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5,
                     0.6, 0.7, 0.8, 0.9, 0.95, 0.98, 0.99, 0.995, 0.998, 0.999)

# One entry per predictor: its kind, the values its axis runs over and the
# value it starts at.
nomo_variable <- function(name, x, label, value, range) {
  if (is.logical(x)) x <- factor(x, levels = c(FALSE, TRUE))
  if (is.character(x)) x <- factor(x)
  if (is.factor(x)) {
    lev <- levels(droplevels(x))
    start <- if (!is.null(value)) as.character(value) else names(which.max(table(droplevels(x))))
    if (!start %in% lev) rlang::abort(paste0("`values$", name, "` must be one of: ", paste(lev, collapse = ", ")))
    return(list(name = name, label = label, type = "cat", values = lev, start = match(start, lev)))
  }
  if (!is.numeric(x)) rlang::abort(paste0("`", name, "` must be numeric, a factor or text."))
  u <- sort(unique(x))
  obs <- range(x)
  if (length(u) <= 10 && all(abs(u - round(u)) < 1e-8)) {
    start <- if (!is.null(value)) value else u[which.min(abs(u - stats::median(x)))]
    if (!start %in% u) rlang::abort(paste0("`values$", name, "` must be one of: ", paste(u, collapse = ", ")))
    return(list(name = name, label = label, type = "disc", values = u, start = match(start, u),
                observed = obs))
  }
  lim <- if (!is.null(range)) sort(range) else stats::quantile(x, c(0.025, 0.975), names = FALSE)
  step <- 10^floor(log10(diff(lim) / 100))
  if (all(abs(x - round(x)) < 1e-8)) step <- max(1, step)
  lim <- c(floor(lim[1] / step) * step, ceiling(lim[2] / step) * step)
  start <- if (!is.null(value)) value else round(stats::median(x) / step) * step
  start <- min(max(start, lim[1]), lim[2])
  digits <- max(0, -floor(log10(step) + 1e-9))
  list(name = name, label = label, type = "cont", lo = lim[1], hi = lim[2], step = step,
       digits = digits, start_value = start, observed = obs)
}

# Group predictors that share a design column, such as the two sides of an
# interaction, into clusters whose axes are read together.
nomo_clusters <- function(colvars, off_vars, vars) {
  parent <- stats::setNames(vars, vars)
  find <- function(v) {
    while (parent[[v]] != v) v <- parent[[v]]
    v
  }
  join <- function(vs) {
    vs <- intersect(vs, vars)
    if (length(vs) < 2) return()
    r <- find(vs[1])
    for (v in vs[-1]) parent[[find(v)]] <<- r
  }
  for (cv in colvars) join(cv)
  join(off_vars)
  roots <- vapply(vars, find, character(1))
  split(vars, factor(roots, levels = unique(roots)))
}

nomo_build <- function(nm, values, labels, ranges, outcome, level) {
  dims <- nomo_dims
  data <- nm$data
  # Design columns and the variables behind each.
  colvars <- nm$colvars
  if (is.null(colvars)) {
    probe <- nm$design(data[1, , drop = FALSE])
    a <- probe$assign
    colvars <- lapply(a, function(j) if (is.na(j) || j == 0) character(0) else nm$term_vars[[j]])
  }
  colvars <- lapply(colvars, function(v) intersect(v, names(data)))
  vars <- intersect(unique(c(unlist(colvars), nm$off_vars)), names(data))
  vars <- setdiff(vars, nm$strata_vars)
  if (!length(vars)) rlang::abort("The model has no predictors to draw.")
  label_of <- function(v) {
    if (!is.null(labels) && !is.na(labels[v])) return(unname(labels[v]))
    column_labels(data, v)
  }
  vs <- lapply(vars, function(v) nomo_variable(v, data[[v]], label_of(v), values[[v]], ranges[[v]]))
  names(vs) <- vars

  clusters <- nomo_clusters(colvars, intersect(nm$off_vars, vars), vars)
  # Categories first, numeric axes last, within a cluster.
  clusters <- lapply(clusters, function(cl) c(cl[vapply(vs[cl], function(v) v$type != "cont", logical(1))],
                                              cl[vapply(vs[cl], function(v) v$type == "cont", logical(1))]))
  ncont <- vapply(clusters, function(cl) sum(vapply(vs[cl], function(v) v$type == "cont", logical(1))), integer(1))
  if (any(ncont > 2)) {
    rlang::abort("A nomogram cannot show an interaction among more than two numeric predictors.")
  }
  # Numeric grids fall on the values a handle can take, so a prediction at
  # any of them is exact; the step widens when the axis is long.
  for (cl in clusters) {
    n2 <- sum(vapply(vs[cl], function(v) v$type == "cont", logical(1)))
    most <- if (n2 == 2) dims$grid_2d else dims$grid
    for (v in cl) {
      if (vs[[v]]$type == "cont") {
        s <- vs[[v]]
        step <- s$step
        for (m in rep(c(2, 2.5, 2), 10)) {
          if ((s$hi - s$lo) / step + 1 <= most) break
          step <- step * m
        }
        lo <- floor(s$lo / step + 1e-9) * step
        hi <- ceiling(s$hi / step - 1e-9) * step
        g <- round(seq(lo, hi, by = step), 10)
        vs[[v]]$step <- step
        vs[[v]]$lo <- lo
        vs[[v]]$hi <- hi
        vs[[v]]$digits <- max(0, -floor(log10(step) + 1e-9))
        vs[[v]]$values <- g
        vs[[v]]$start <- which.min(abs(g - s$start_value))
      }
    }
  }

  # A row of new data holding every predictor at its starting value.
  base_row <- data[1, , drop = FALSE]
  value_of <- function(v, i) {
    s <- vs[[v]]
    x <- data[[v]]
    if (s$type == "cat") {
      lv <- s$values[i]
      if (is.logical(x)) return(as.logical(lv))
      if (is.factor(x)) return(factor(lv, levels = levels(x)))
      return(lv)
    }
    s$values[i]
  }
  for (v in vars) base_row[[v]] <- value_of(v, vs[[v]]$start)

  const_cols <- which(lengths(colvars) == 0)
  const_x <- nm$design(base_row)
  const_off <- if (length(intersect(nm$off_vars, vars))) 0 else const_x$offset[1]
  cols_of <- lapply(clusters, function(cl) which(vapply(colvars, function(cv) length(cv) && all(cv %in% cl), logical(1))))
  has_off <- vapply(clusters, function(cl) length(intersect(nm$off_vars, cl)) > 0, logical(1))

  blocks <- lapply(seq_along(clusters), function(c) {
    cl <- clusters[[c]]
    d <- vapply(vs[cl], function(v) length(v$values), integer(1))
    if (prod(d) > dims$cells) {
      rlang::abort(paste0("The interaction of ", paste(cl, collapse = ", "),
                          " has too many combinations to draw."))
    }
    idx <- do.call(expand.grid, lapply(d, seq_len))
    nd <- base_row[rep(1, nrow(idx)), , drop = FALSE]
    for (j in seq_along(cl)) nd[[cl[j]]] <- value_of(cl[j], idx[[j]])
    des <- nm$design(nd)
    list(vars = cl, dims = unname(d), cols = cols_of[[c]],
         X = des$X[, cols_of[[c]], drop = FALSE],
         off = if (has_off[c]) des$offset else numeric(nrow(nd)))
  })

  equations <- if (nm$kind == "multinomial") rownames(nm$beta) else "main"
  eqs <- lapply(equations, function(e) {
    beta <- if (nm$kind == "multinomial") nm$beta[e, ] else nm$beta
    nomo_equation(nm, vs, blocks, beta, const_cols, const_x$X[1, ], const_off, dims)
  })
  names(eqs) <- equations
  eq_start <- if (nm$kind == "multinomial") {
    if (is.null(level)) 1L else {
      k <- match(level, equations)
      if (is.na(k)) rlang::abort(paste0("`level` must be one of: ", paste(equations, collapse = ", ")))
      k
    }
  } else 1L

  labels_out <- nomo_output_labels(nm, outcome)
  for (e in seq_along(eqs)) eqs[[e]]$outputs <- nomo_outputs(nm, eqs[[e]], labels_out, e)
  list(nm = nm, vars = vs, clusters = clusters, blocks = blocks, eqs = eqs, eq_start = eq_start,
       const_cols = const_cols, const_x = unname(const_x$X[1, ]), const_off = const_off,
       output_labels = labels_out)
}

# Rows, points and the scale for one linear predictor.
nomo_equation <- function(nm, vs, blocks, beta, const_cols, const_x, const_off, dims) {
  beta <- unname(beta)
  const_lp <- sum(const_x[const_cols] * beta[const_cols]) + const_off
  rows <- list()
  ref_lp <- 0
  for (bi in seq_along(blocks)) {
    b <- blocks[[bi]]
    f <- as.numeric(b$X %*% beta[b$cols]) + as.numeric(b$off)
    dims_b <- b$dims
    stride <- cumprod(c(1, dims_b))[seq_along(dims_b)]
    ref <- vapply(b$vars, function(v) vs[[v]]$start, integer(1))
    at <- function(ix) sum((ix - 1) * stride) + 1
    ref_lp <- ref_lp + f[at(ref)]
    for (k in seq_along(b$vars)) {
      earlier <- if (k > 1) dims_b[seq_len(k - 1)] else integer(0)
      nconds <- if (length(earlier)) prod(earlier) else 1
      conds <- vector("list", nconds)
      for (ci in seq_len(nconds)) {
        cix <- if (length(earlier)) as.integer(arrayInd(ci, earlier)) else integer(0)
        prev <- c(cix, ref[k:length(ref)])
        base <- f[at(prev)]
        delta <- vapply(seq_len(dims_b[k]), function(i) {
          ix <- c(cix, i, if (k < length(ref)) ref[(k + 1):length(ref)])
          f[at(ix)] - base
        }, numeric(1))
        conds[[ci]] <- delta
      }
      rows[[length(rows) + 1]] <- list(var = b$vars[k], block = bi, k = k, conds = conds,
                                       lo = min(unlist(conds)), hi = max(unlist(conds)))
    }
  }
  scale <- max(vapply(rows, function(r) r$hi - r$lo, numeric(1)))
  if (!(scale > 0)) rlang::abort("None of the predictors changes the prediction.")
  base0 <- const_lp + ref_lp + sum(vapply(rows, function(r) r$lo, numeric(1)))
  max_pts <- sum(vapply(rows, function(r) (r$hi - r$lo) / scale * 100, numeric(1)))
  list(beta = beta, rows = rows, scale = scale, base0 = base0, max_pts = max_pts,
       const_lp = const_lp)
}

nomo_output_labels <- function(nm, outcome) {
  resp <- nm$response
  out <- switch(
    nm$kind,
    linear = paste("Predicted", resp),
    link = if (nm$family %in% c("binomial", "quasibinomial", "betabinomial")) paste("Probability of", resp)
           else if (nm$link %in% c("log")) paste("Expected", resp) else paste("Mean", resp),
    cox = paste("Survival at", names(nm$times)),
    aft = c("Median survival time", paste("Survival at", names(nm$times))),
    ordinal = paste0("P(", resp, " \u2265 ", nm$outcome_levels[-1], ")"),
    multinomial = paste0("Odds of ", nm$outcome_levels[-1], " vs ", nm$outcome_levels[1])
  )
  if (!is.null(outcome)) {
    if (nm$kind == "multinomial") rlang::abort("`outcome` is not used for a multinomial model.")
    if (length(outcome) != length(out)) {
      rlang::abort(paste0("`outcome` must hold ", if (length(out) == 1) "one label." else paste(length(out), "labels."), ""))
    }
    out <- outcome
  }
  out
}

# The prediction as a function of the linear predictor, for each axis.
nomo_output_funs <- function(nm) {
  inv <- function(link) {
    switch(link,
           loglog = function(z) exp(-exp(-z)),
           cloglog = function(z) 1 - exp(-exp(z)),
           cauchit = stats::pcauchy,
           probit = stats::pnorm,
           logit = stats::plogis,
           stats::make.link(link)$linkinv)
  }
  switch(
    nm$kind,
    linear = list(identity),
    link = list(stats::make.link(nm$link)$linkinv),
    cox = lapply(seq_along(nm$times), function(j) {
      function(lp, stratum = 1) {
        a <- nm$strata[[stratum]]$at[[j]]
        exp(-a$H0 * exp(lp - sum(nm$ctr * nm$beta)))
      }
    }),
    aft = {
      F <- switch(nm$base, extreme = function(z) 1 - exp(-exp(z)), logistic = stats::plogis,
                  gaussian = stats::pnorm)
      tr <- if (nm$trans == "log") log else identity
      untr <- if (nm$trans == "log") exp else identity
      c(list(function(lp) untr(lp + nm$scale * nm$q50)),
        lapply(nm$times, function(t) function(lp) 1 - F((tr(t) - lp) / nm$scale)))
    },
    ordinal = {
      F <- inv(nm$link)
      lapply(seq_along(nm$zeta), function(j) function(lp) 1 - F(nm$zeta[j] - lp))
    },
    multinomial = list(exp)
  )
}

nomo_is_prob <- function(nm, j) {
  switch(nm$kind,
         link = nm$family %in% c("binomial", "quasibinomial", "betabinomial") || nm$link %in% c("logit", "probit", "cloglog", "cauchit"),
         cox = TRUE, aft = j > 1, ordinal = TRUE, FALSE)
}

# Ticks for each prediction axis, placed on the total points scale.
nomo_outputs <- function(nm, eq, labels, e) {
  funs <- nomo_output_funs(nm)
  strata <- if (nm$kind == "cox") seq_along(nm$strata) else 1L
  lp_at <- function(total) eq$base0 + total / 100 * eq$scale
  totals <- seq(0, eq$max_pts, length.out = 401)
  out <- list()
  for (j in seq_along(funs)) {
    per_stratum <- lapply(strata, function(s) {
      g <- if (nm$kind == "cox") funs[[j]](lp_at(totals), s) else funs[[j]](lp_at(totals))
      nomo_axis_ticks(totals, g, nomo_is_prob(nm, j),
                      positive = nm$kind %in% c("aft", "multinomial") || (nm$kind == "link" && !nomo_is_prob(nm, j)))
    })
    out[[j]] <- list(label = if (nm$kind == "multinomial") labels[e] else labels[j], strata = per_stratum)
  }
  out
}

nomo_axis_ticks <- function(totals, g, prob, positive) {
  ok <- is.finite(g)
  totals <- totals[ok]
  g <- g[ok]
  if (length(g) < 2 || diff(range(g)) == 0) return(list(ticks = list()))
  r <- range(g)
  cand <- if (prob) nomo_prob_ticks
  else if (positive && r[1] > 0 && r[2] / r[1] > 8) {
    k <- floor(log10(r[1])):ceiling(log10(r[2]))
    sort(unique(as.vector(outer(c(1, 2, 5), 10^k))))
  } else pretty(r, 7)
  cand <- cand[cand >= r[1] & cand <= r[2]]
  o <- order(g)
  pos <- stats::approx(g[o], totals[o], xout = cand, ties = "ordered")$y
  keep <- is.finite(pos)
  cand <- cand[keep]
  pos <- pos[keep]
  label <- if (prob) {
    vapply(cand, function(v) paste0(format(signif(100 * v, 3), trim = TRUE, drop0trailing = TRUE), "%"), character(1))
  } else vapply(cand, function(v) format(signif(v, 3), trim = TRUE, big.mark = if (abs(v) >= 1e4) "," else ""), character(1))
  # Drop ticks too close to the one kept before.
  o <- order(pos)
  kept <- integer(0)
  for (i in o) if (!length(kept) || abs(pos[i] - pos[kept[length(kept)]]) > 6.5) kept <- c(kept, i)
  list(ticks = lapply(kept, function(i) list(label = label[i], pts = pos[i])))
}

# Predictions in R, the way the widget computes them.
nomo_predict <- function(spec, values, eq = spec$eq_start, stratum = 1) {
  nm <- spec$nm
  vs <- spec$vars
  pos <- lapply(vs, function(v) {
    x <- values[[v$name]]
    if (is.null(x)) return(if (v$type == "cont") v$values[v$start] else v$start)
    if (v$type == "cat") {
      i <- match(as.character(x), v$values)
      if (is.na(i)) rlang::abort(paste0("`", v$name, "` must be one of: ", paste(v$values, collapse = ", ")))
      return(i)
    }
    if (v$type == "disc") {
      i <- match(x, v$values)
      if (is.na(i)) rlang::abort(paste0("`", v$name, "` must be one of: ", paste(v$values, collapse = ", ")))
      return(i)
    }
    x
  })
  e <- spec$eqs[[eq]]
  p <- length(spec$const_x)
  x <- numeric(p)
  x[spec$const_cols] <- spec$const_x[spec$const_cols]
  off <- spec$const_off
  pts <- 0
  for (bi in seq_along(spec$blocks)) {
    b <- spec$blocks[[bi]]
    w <- nomo_weights(b, vs, pos)
    x[b$cols] <- x[b$cols] + colSums(b$X[w$rows, , drop = FALSE] * w$w)
    off <- off + sum(b$off[w$rows] * w$w)
  }
  lp <- sum(x * e$beta) + off
  total <- (lp - e$base0) / e$scale * 100
  out <- nomo_outputs_at(spec, x, lp, eq, stratum)
  list(points = total, lp = lp, se = out$se, outputs = out$table)
}

# Rows of a cluster's block and their weights for a set of values: numeric
# predictors are interpolated between grid points.
nomo_weights <- function(b, vs, pos) {
  stride <- cumprod(c(1, b$dims))[seq_along(b$dims)]
  parts <- lapply(seq_along(b$vars), function(j) {
    v <- vs[[b$vars[j]]]
    if (v$type != "cont") return(list(i = pos[[b$vars[j]]], w = 1))
    g <- v$values
    t <- (min(max(pos[[b$vars[j]]], g[1]), g[length(g)]) - g[1]) / (g[2] - g[1])
    i0 <- min(floor(t) + 1, length(g) - 1)
    f <- t - (i0 - 1)
    list(i = c(i0, i0 + 1), w = c(1 - f, f))
  })
  grid <- expand.grid(lapply(parts, function(p) seq_along(p$i)))
  rows <- apply(grid, 1, function(r) sum((vapply(seq_along(parts), function(j) parts[[j]]$i[r[j]], numeric(1)) - 1) * stride) + 1)
  w <- apply(grid, 1, function(r) prod(vapply(seq_along(parts), function(j) parts[[j]]$w[r[j]], numeric(1))))
  list(rows = rows, w = w)
}

nomo_outputs_at <- function(spec, x, lp, eq, stratum) {
  nm <- spec$nm
  z <- stats::qnorm(0.975)
  quad <- function(g, V) sqrt(max(0, drop(t(g) %*% V %*% g)))
  labels <- spec$output_labels
  row <- function(label, est, lo, hi) data.frame(output = label, estimate = est, lower = lo, upper = hi)
  if (nm$kind %in% c("linear", "link")) {
    se <- quad(x, nm$V)
    q <- if (nm$kind == "linear" && !is.null(nm$df) && is.finite(nm$df) && !isTRUE(nm$mixed)) stats::qt(0.975, nm$df) else z
    f <- if (nm$kind == "linear") identity else stats::make.link(nm$link)$linkinv
    tab <- row(labels[1], f(lp), f(lp - q * se), f(lp + q * se))
    if (nm$kind == "linear" && !is.null(nm$sigma) && !isTRUE(nm$mixed)) {
      s <- sqrt(se^2 + nm$sigma^2)
      tab <- rbind(tab, row("Prediction interval", lp, lp - q * s, lp + q * s))
    }
    return(list(se = se, table = tab))
  }
  if (nm$kind == "cox") {
    V <- nm$V
    xc <- x - nm$ctr
    r <- exp(sum(xc * nm$beta))
    tab <- do.call(rbind, lapply(seq_along(nm$times), function(j) {
      a <- nm$strata[[stratum]]$at[[j]]
      H <- a$H0 * r
      d <- xc * a$H0 - a$A
      seH <- r * sqrt(max(0, a$T1 + drop(t(d) %*% V %*% d)))
      S <- exp(-H)
      row(labels[j], S, S * exp(-z * seH), min(1, S * exp(z * seH)))
    }))
    return(list(se = quad(xc, nm$V), table = tab))
  }
  if (nm$kind == "aft") {
    p <- length(x)
    grad <- function(extra) if (nm$has_scale) c(x, extra) else x
    F <- switch(nm$base, extreme = function(z) 1 - exp(-exp(z)), logistic = stats::plogis,
                gaussian = stats::pnorm)
    tr <- if (nm$trans == "log") log else identity
    untr <- if (nm$trans == "log") exp else identity
    q <- lp + nm$scale * nm$q50
    sq <- quad(grad(nm$scale * nm$q50), nm$V)
    tab <- row(labels[1], untr(q), untr(q - z * sq), untr(q + z * sq))
    for (j in seq_along(nm$times)) {
      u <- (tr(nm$times[[j]]) - lp) / nm$scale
      su <- quad(if (nm$has_scale) c(-x / nm$scale, -u) else -x / nm$scale, nm$V)
      tab <- rbind(tab, row(labels[j + 1], 1 - F(u), 1 - F(u + z * su), 1 - F(u - z * su)))
    }
    return(list(se = quad(x, nm$V[seq_len(p), seq_len(p), drop = FALSE]), table = tab))
  }
  if (nm$kind == "ordinal") {
    p <- length(x)
    inv <- switch(nm$link, loglog = function(z) exp(-exp(-z)), cloglog = function(z) 1 - exp(-exp(z)),
                  cauchit = stats::pcauchy, probit = stats::pnorm, stats::plogis)
    tab <- do.call(rbind, lapply(seq_along(nm$zeta), function(j) {
      g <- c(-x, replace(numeric(length(nm$zeta)), j, 1))
      su <- quad(g, nm$V)
      u <- nm$zeta[j] - lp
      row(labels[j], 1 - inv(u), 1 - inv(u + z * su), 1 - inv(u - z * su))
    }))
    cum <- c(1, tab$estimate, 0)
    probs <- -diff(cum)
    tab <- rbind(tab, row(paste0("P(", nm$response, " = ", nm$outcome_levels, ")"), probs, NA, NA))
    return(list(se = quad(x, nm$V[seq_len(p), seq_len(p), drop = FALSE]), table = tab))
  }
  # Multinomial: every category's probability from all the equations.
  lps <- vapply(spec$eqs, function(e2) sum(x * e2$beta) , numeric(1))
  pr <- exp(c(0, lps)) / sum(exp(c(0, lps)))
  list(se = NA_real_, table = rbind(row(labels[eq], exp(lp), NA, NA),
                                    row(paste0("P(", nm$response, " = ", nm$outcome_levels, ")"), pr, NA, NA)))
}

# Layout ----------------------------------------------------------------------

# Ticks and folded lines for one axis of a predictor: `delta` is its
# contribution at each value, `lo` the lowest over every condition.
nomo_row_ticks <- function(v, delta, lo, scale, axis_w, family, dims) {
  pts <- (delta - lo) / scale * 100
  to_x <- axis_w / 100
  width <- function(s) text_width_card(s, dims$tick_pt, family, 1)
  if (v$type == "cont") {
    g <- v$values
    d <- diff(pts)
    sgn <- sign(ifelse(abs(d) < 1e-7, 0, d))
    if (all(sgn == 0)) sgn[] <- 1
    first <- sgn[sgn != 0][1]
    for (i in seq_along(sgn)) if (sgn[i] == 0) sgn[i] <- if (i == 1) first else sgn[i - 1]
    run_of <- cumsum(c(1, diff(sgn) != 0))
    nr <- max(run_of)
    runs <- lapply(seq_len(nr), function(r) {
      seg <- which(run_of == r)
      p <- pts[c(seg, max(seg) + 1)]
      c(min(p), max(p))
    })
    vals <- pretty(c(v$lo, v$hi), 6)
    vals <- vals[vals >= v$lo - 1e-9 & vals <= v$hi + 1e-9]
    tp <- stats::approx(g, pts, xout = vals, rule = 2)$y
    seg <- pmax(1, pmin(findInterval(vals, g, rightmost.closed = TRUE), length(g) - 1))
    rr <- run_of[seg]
    lab <- format(vals, trim = TRUE, drop0trailing = TRUE, big.mark = if (max(abs(vals)) >= 1e4) "," else "")
    keep <- rep(FALSE, length(vals))
    for (r in unique(rr)) {
      i <- which(rr == r)
      i <- i[order(tp[i])]
      last <- NULL
      for (j in i) {
        if (is.null(last) || abs(tp[j] - tp[last]) * to_x >= (width(lab[j]) + width(lab[last])) / 2 + 3) {
          keep[j] <- TRUE
          last <- j
        }
      }
    }
    ticks <- lapply(which(keep), function(j) list(label = lab[j], pts = tp[j], run = rr[j] - 1L, line = 0L))
    return(list(ticks = ticks, runs = runs, lines = 0L, seg_run = if (nr > 1) run_of - 1L))
  }
  lab <- if (v$type == "cat") v$values else format(v$values, trim = TRUE)
  o <- order(pts)
  ends <- rep(-Inf, 3)
  line <- integer(length(pts))
  for (j in o) {
    left <- pts[j] * to_x - width(lab[j]) / 2
    l <- which(ends <= left - 3)[1]
    if (is.na(l)) l <- which.min(ends)
    line[j] <- l - 1L
    ends[l] <- pts[j] * to_x + width(lab[j]) / 2
  }
  ticks <- lapply(seq_along(pts), function(j) list(label = lab[j], pts = pts[j], run = 0L, line = line[j]))
  list(ticks = ticks, runs = list(range(pts)), lines = max(line))
}

nomo_layout <- function(spec, title, caption, family) {
  dims <- nomo_dims
  vs <- spec$vars
  width <- function(s, pt, bold = FALSE) {
    if (!length(s)) return(0)
    max(text_width_card(as.character(s), pt, family, 1, bold = bold))
  }
  axis_w <- dims$axis_w
  # Ticks for every row, condition and equation.
  eq_rows <- lapply(spec$eqs, function(e) {
    lapply(e$rows, function(r) {
      conds <- lapply(r$conds, function(delta) nomo_row_ticks(vs[[r$var]], delta, r$lo, e$scale, axis_w, family, dims))
      list(var = r$var, block = r$block, k = r$k, lo = r$lo, conds = conds)
    })
  })
  n_rows <- length(eq_rows[[1]])
  row_h <- vapply(seq_len(n_rows), function(i) {
    max(vapply(eq_rows, function(rows) {
      max(vapply(rows[[i]]$conds, function(cd) (length(cd$runs) - 1) * dims$run_gap + cd$lines * dims$stagger, numeric(1)))
    }, numeric(1))) + dims$row_h
  }, numeric(1))
  conditional <- vapply(eq_rows[[1]], function(r) length(r$conds) > 1, logical(1))
  out_labels <- unlist(lapply(spec$eqs, function(e) vapply(e$outputs, `[[`, character(1), "label")))
  label_w <- max(width(vapply(vs, `[[`, character(1), "label"), dims$label_pt),
                 width(c("Points", "Total points", out_labels), dims$label_pt, bold = TRUE)) + 16
  x0 <- label_w
  x1 <- x0 + axis_w
  y <- 20
  points_y <- y
  y <- y + 30
  row_y <- numeric(n_rows)
  for (i in seq_len(n_rows)) {
    row_y[i] <- y + 8
    y <- y + row_h[i]
  }
  total_y <- y + 8
  y <- y + dims$row_h
  # Prediction axes keep only ticks whose labels fit.
  for (k in seq_along(spec$eqs)) {
    e <- spec$eqs[[k]]
    to_x <- axis_w / e$max_pts
    for (j in seq_along(e$outputs)) {
      for (st in seq_along(e$outputs[[j]]$strata)) {
        tk <- e$outputs[[j]]$strata[[st]]$ticks
        if (length(tk) < 2) next
        w <- text_width_card(vapply(tk, `[[`, character(1), "label"), dims$tick_pt, family, 1)
        p <- vapply(tk, `[[`, numeric(1), "pts")
        o <- order(p)
        keep <- integer(0)
        for (i in o) {
          if (!length(keep) || abs(p[i] - p[keep[length(keep)]]) * to_x >= (w[i] + w[keep[length(keep)]]) / 2 + 4) keep <- c(keep, i)
        }
        spec$eqs[[k]]$outputs[[j]]$strata[[st]]$ticks <- tk[sort(keep)]
      }
    }
  }
  n_out <- length(spec$eqs[[1]]$outputs)
  out_y <- total_y + dims$row_h * seq_len(n_out)
  bottom <- max(total_y, out_y) + 18
  page <- graph_canvas(c(0, x1 + dims$right), c(0, bottom), title, caption, character(0),
                       character(0), NULL, dims, family)
  list(dims = dims, page = page, family = family, x0 = x0, x1 = x1, axis_w = axis_w,
       points_y = points_y, row_y = row_y, row_h = row_h, total_y = total_y, out_y = out_y,
       eq_rows = eq_rows, conditional = conditional, spec = spec,
       dx = page$px(0), dy = page$height - page$py(0))
}

# The condition a row's axis is drawn under: the positions of the earlier
# predictors in its cluster.
nomo_cond_index <- function(spec, row, pos) {
  b <- spec$blocks[[row$block]]
  if (row$k == 1) return(1L)
  d <- b$dims[seq_len(row$k - 1)]
  stride <- cumprod(c(1, d))[seq_along(d)]
  ix <- vapply(b$vars[seq_len(row$k - 1)], function(v) pos[[v]], numeric(1))
  as.integer(sum((ix - 1) * stride) + 1)
}

nomo_draw <- function(lay, blank = FALSE) {
  dims <- lay$dims
  px <- lay$page$px
  py <- lay$page$py
  if (blank) {
    return(ggplot() +
             geom_blank(data = data.frame(x = c(0, lay$page$width), y = c(0, lay$page$height)),
                        aes(x = .data$x, y = .data$y)) +
             graph_frame(lay$page, lay$family))
  }
  spec <- lay$spec
  eq <- spec$eq_start
  e <- spec$eqs[[eq]]
  X <- function(pts) lay$x0 + pts / 100 * lay$axis_w
  XT <- function(t) lay$x0 + t / e$max_pts * lay$axis_w
  texts <- function(label, x, y, pt, color, hjust = 0, face = "plain") {
    if (!length(label)) return(empty_texts())
    data.frame(x = px(x), y = py(y), label = as.character(label), size = pt / .pt,
               colour = color, hjust = hjust, fontface = face, stringsAsFactors = FALSE)
  }
  segs <- function(x0, x1, y0, y1, colour) {
    data.frame(x = px(x0), xend = px(x1), y = py(y0), yend = py(y1), colour = colour,
               stringsAsFactors = FALSE)
  }
  axis_rows <- function(label, y, ticks, lo, hi, bold = TRUE) {
    list(
      segs = rbind(segs(lo, hi, y, y, graph_ink$edge),
                   if (length(ticks)) segs(ticks$x, ticks$x, y, y + 4, graph_ink$edge)),
      text = rbind(texts(label, lay$x0 - 12, y, dims$label_pt, graph_ink$title, hjust = 1,
                         face = if (bold) "bold" else "plain"),
                   if (length(ticks)) texts(ticks$label, ticks$x, y + 13 + ticks$line * dims$stagger,
                                            dims$tick_pt, graph_ink$muted, hjust = 0.5))
    )
  }
  pieces <- list()
  pt_ticks <- seq(0, 100, by = 10)
  pieces[[1]] <- axis_rows("Points", lay$points_y,
                           data.frame(x = X(pt_ticks), label = pt_ticks, line = 0), X(0), X(100))
  pieces[[1]]$text$y[-1] <- py(lay$points_y - 9)
  pieces[[1]]$segs$yend[-1] <- py(lay$points_y - 4)

  pos <- lapply(spec$vars, function(v) v$start)
  for (i in seq_along(lay$eq_rows[[eq]])) {
    r <- lay$eq_rows[[eq]][[i]]
    v <- spec$vars[[r$var]]
    cd <- r$conds[[nomo_cond_index(spec, r, pos)]]
    y <- lay$row_y[i]
    tk <- do.call(rbind, lapply(cd$ticks, function(t) data.frame(x = X(t$pts), label = t$label, run = t$run, line = t$line)))
    s <- NULL
    tx <- texts(v$label, lay$x0 - 12, y, dims$label_pt, graph_ink$text, hjust = 1)
    for (ri in seq_along(cd$runs)) {
      yr <- y + (ri - 1) * dims$run_gap
      s <- rbind(s, segs(X(cd$runs[[ri]][1]), X(cd$runs[[ri]][2]), yr, yr, graph_ink$edge))
      if (!is.null(tk)) {
        on <- tk[tk$run == ri - 1, , drop = FALSE]
        if (nrow(on)) {
          s <- rbind(s, segs(on$x, on$x, yr, yr + 4, graph_ink$edge))
          tx <- rbind(tx, texts(on$label, on$x, yr + 13 + on$line * dims$stagger, dims$tick_pt,
                                graph_ink$muted, hjust = 0.5))
        }
      }
    }
    if (lay$conditional[i]) {
      b <- spec$blocks[[r$block]]
      given <- b$vars[seq_len(r$k - 1)]
      cond <- paste(vapply(given, function(g) {
        gv <- spec$vars[[g]]
        paste(gv$label, "=", if (gv$type == "cat") gv$values[gv$start] else format(gv$values[gv$start], trim = TRUE))
      }, character(1)), collapse = ", ")
      tx <- rbind(tx, texts(paste("if", cond), lay$x0 - 12, y + 12, dims$tick_pt, graph_ink$muted, hjust = 1))
    }
    pieces[[length(pieces) + 1]] <- list(segs = s, text = tx)
  }
  tot <- pretty(c(0, e$max_pts), 8)
  tot <- tot[tot <= e$max_pts + 1e-9]
  pieces[[length(pieces) + 1]] <- axis_rows("Total points", lay$total_y,
                                            data.frame(x = XT(tot), label = tot, line = 0), XT(0), XT(e$max_pts))
  for (j in seq_along(e$outputs)) {
    o <- e$outputs[[j]]
    tk <- o$strata[[1]]$ticks
    df <- if (length(tk)) data.frame(x = XT(vapply(tk, `[[`, numeric(1), "pts")),
                                     label = vapply(tk, `[[`, character(1), "label"), line = 0)
    lo <- if (length(tk)) min(df$x) else XT(0)
    hi <- if (length(tk)) max(df$x) else XT(e$max_pts)
    pieces[[length(pieces) + 1]] <- axis_rows(o$label, lay$out_y[j], df, lo, hi)
  }
  segs_all <- do.call(rbind, lapply(pieces, `[[`, "segs"))
  text_all <- do.call(rbind, lapply(pieces, `[[`, "text"))
  ggplot() +
    geom_segment(data = segs_all, aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
                 colour = segs_all$colour, linewidth = 0.8 / .pt) +
    draw_text(text_all, lay$family) +
    graph_frame(lay$page, lay$family)
}

# Everything the widget needs to draw the nomogram and compute predictions.
nomo_render_data <- function(spec, lay) {
  nm <- spec$nm
  vs <- spec$vars
  var_names <- names(vs)
  vars <- lapply(vs, function(v) {
    c(list(name = v$name, label = v$label, type = v$type,
           values = I(if (v$type == "cat") v$values else v$values), start = v$start - 1L),
      if (v$type == "cont") list(lo = v$lo, hi = v$hi, step = v$step, digits = v$digits),
      if (!is.null(v$observed)) list(observed = I(v$observed)))
  })
  blocks <- lapply(spec$blocks, function(b) {
    list(vars = I(match(b$vars, var_names) - 1L), dims = I(b$dims), cols = I(b$cols - 1L),
         X = unname(b$X), off = I(b$off))
  })
  eqs <- lapply(seq_along(spec$eqs), function(k) {
    e <- spec$eqs[[k]]
    rows <- lapply(seq_along(lay$eq_rows[[k]]), function(i) {
      r <- lay$eq_rows[[k]][[i]]
      list(var = match(r$var, var_names) - 1L, block = r$block - 1L, k = r$k - 1L, lo = r$lo,
           y = lay$row_y[i], conditional = lay$conditional[i],
           conds = lapply(r$conds, function(cd) {
             c(list(ticks = lapply(cd$ticks, function(t) list(t$label, round(t$pts, 3), t$run, t$line)),
                    runs = lapply(cd$runs, function(x) I(round(x, 3)))),
               if (!is.null(cd$seg_run)) list(seg_run = I(cd$seg_run)))
           }))
    })
    tot <- pretty(c(0, e$max_pts), 8)
    list(label = names(spec$eqs)[k], beta = I(e$beta), scale = e$scale, base0 = e$base0,
         max_pts = e$max_pts, rows = rows, total = I(tot[tot <= e$max_pts + 1e-9]),
         outputs = lapply(seq_along(e$outputs), function(j) {
           o <- e$outputs[[j]]
           list(label = o$label, y = lay$out_y[j], prob = nomo_is_prob(nm, j),
                strata = lapply(o$strata, function(st) lapply(st$ticks, function(t) list(t$label, round(t$pts, 3)))))
         }))
  })
  kind <- nm$kind
  out <- list(
    kind = kind, dx = lay$dx, dy = lay$dy, x0 = lay$x0, axis_w = lay$axis_w,
    points_y = lay$points_y, total_y = lay$total_y, run_gap = lay$dims$run_gap,
    stagger = lay$dims$stagger, right = lay$x1 + 14,
    vars = unname(vars), blocks = blocks, eqs = eqs, eq_start = spec$eq_start - 1L,
    const = I(spec$const_x * (seq_along(spec$const_x) %in% spec$const_cols)), const_off = spec$const_off,
    labels = I(spec$output_labels), response = nm$response
  )
  if (kind != "multinomial") out$V <- unname(as.matrix(nm$V))
  if (kind == "linear") {
    out$q <- if (!is.null(nm$df) && is.finite(nm$df) && !isTRUE(nm$mixed)) stats::qt(0.975, nm$df) else stats::qnorm(0.975)
    if (!is.null(nm$sigma) && !isTRUE(nm$mixed)) out$sigma <- nm$sigma
  }
  if (kind == "link") out$link <- nm$link
  if (kind == "cox") {
    out$ctr <- I(unname(nm$ctr))
    out$strata <- lapply(nm$strata, function(s) {
      list(label = s$label, at = lapply(unname(s$at), function(a) list(H0 = a$H0, T1 = a$T1, A = I(unname(a$A)))))
    })
  }
  if (kind == "aft") {
    out$aft <- list(scale = nm$scale, has_scale = nm$has_scale, base = nm$base, trans = nm$trans,
                    q50 = nm$q50, times = I(unname(nm$times)))
  }
  if (kind == "ordinal") out$ordinal <- list(zeta = I(nm$zeta), link = nm$link, levels = I(nm$outcome_levels))
  if (kind == "multinomial") out$levels <- I(nm$outcome_levels)
  out
}

nomo_notes_html <- function(spec) {
  nm <- spec$nm
  n <- nrow(nm$data)
  model <- switch(
    nm$kind,
    linear = "a linear model of the mean",
    link = paste0("a generalized linear model with a ", nm$link, " link"),
    cox = "a Cox proportional hazards model",
    aft = paste0("an accelerated failure time model"),
    ordinal = paste0("a cumulative ", nm$link, " model for an ordered outcome"),
    multinomial = "a multinomial logistic model"
  )
  ci <- switch(
    nm$kind,
    linear = "Intervals come from the covariance of the coefficients, with a prediction interval for a new patient when the model has one residual variance.",
    link = "Intervals are computed on the scale of the linear predictor and carried through the link.",
    cox = "Survival and its interval are those survival::survfit() gives for the same patient, with the interval on the log scale.",
    aft = "Intervals use the covariance of the coefficients and of the log scale, on the log time scale.",
    ordinal = "Intervals for the probability of each category or above use the covariance of the coefficients and the thresholds.",
    multinomial = "The probabilities of the categories come from all the equations together, without intervals."
  )
  items <- c(
    Model = paste0("Drawn from ", model, " fitted to ", n, " rows."),
    Points = "Each predictor's axis is its contribution to the linear predictor, scaled so the predictor with the widest effect spans 100 points. The total points give the prediction on the axes below.",
    Intervals = ci,
    if (any(vapply(spec$vars, function(v) v$type == "cont", logical(1))))
      c(Ranges = "Numeric axes run from the 2.5th to the 97.5th percentile of the data; the hover card gives the observed range."),
    if (isTRUE(nm$mixed)) c("Random effects" = "Predictions are for a typical cluster, with the random effects at zero. On a nonlinear link this is not the average over clusters. Intervals reflect the fixed effects only."),
    if (any(vapply(spec$eqs[[1]]$rows, function(r) length(r$conds) > 1, logical(1))))
      c(Interactions = "Predictors that interact are read together: the axis of a later one is drawn for the current values of the earlier ones and redraws as they change."),
    if (!is.null(nm$note)) c(Note = nm$note)
  )
  paste0('<dl class="ggx-notes-list">',
         paste0("<dt>", esc(names(items)), "</dt><dd>", esc(items), "</dd>", collapse = ""),
         "</dl>")
}
