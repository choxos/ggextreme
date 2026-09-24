births <- function() {
  bw <- MASS::birthwt
  bw$race <- factor(bw$race, labels = c("White", "Black", "Other"))
  bw$smoke <- factor(bw$smoke, labels = c("No", "Yes"))
  bw$ht <- factor(bw$ht, labels = c("No", "Yes"))
  bw$ftv3 <- factor(pmin(bw$ftv, 2), labels = c("None", "One", "Two or more"))
  bw
}

lung_data <- function() {
  d <- survival::lung
  d$sex <- factor(d$sex, labels = c("Male", "Female"))
  d[stats::complete.cases(d[c("time", "status", "age", "sex", "ph.ecog")]), ]
}

# Values for nomogram_predict() from rows of data.
values_of <- function(d, vars) {
  lapply(seq_len(nrow(d)), function(i) {
    lapply(stats::setNames(vars, vars), function(v) {
      x <- d[[v]][i]
      if (is.factor(x)) as.character(x) else x
    })
  })
}

expect_matches <- function(n, d, vars, lp, se = NULL, tol = 1e-8) {
  got <- lapply(values_of(d, vars), function(v) nomogram_predict(n, v))
  expect_equal(vapply(got, `[[`, numeric(1), "lp"), as.numeric(lp), tolerance = tol)
  if (!is.null(se)) expect_equal(vapply(got, `[[`, numeric(1), "se"), as.numeric(se), tolerance = tol)
  invisible(got)
}

test_that("linear and generalized linear models match predict()", {
  skip_if_not_installed("MASS")
  bw <- births()
  d <- bw[c(12, 40, 150), ]
  m <- lm(bwt ~ poly(age, 2) + log(lwt) + race * smoke, data = bw)
  p <- predict(m, d, se.fit = TRUE)
  n <- ggnomogram(m)
  expect_s3_class(n, c("ggnomogram", "ggx_graph"))
  expect_matches(n, d, c("age", "lwt", "race", "smoke"), p$fit, p$se.fit)
  pi <- predict(m, d[1, ], interval = "prediction")
  o <- nomogram_predict(n, values_of(d[1, ], c("age", "lwt", "race", "smoke"))[[1]])$outputs
  expect_equal(unname(unlist(o[2, c("lower", "upper")])), unname(pi[1, c("lwr", "upr")]), tolerance = 1e-8)

  m <- glm(low ~ splines::ns(age, 3) + lwt + race + smoke * ht, family = binomial, data = bw)
  p <- predict(m, d, se.fit = TRUE)
  n <- ggnomogram(m)
  got <- expect_matches(n, d, c("age", "lwt", "race", "smoke", "ht"), p$fit, p$se.fit)
  expect_equal(vapply(got, function(g) g$outputs$estimate[1], numeric(1)), unname(plogis(p$fit)))
  # The interaction draws hypertension's axis for each value of smoking.
  rows <- n$spec$eqs[[1]]$rows
  expect_equal(length(rows[[which(vapply(rows, `[[`, character(1), "var") == "ht")]]$conds), 2)

  bw$years <- 1 + bw$age / 10
  m <- glm(ftv ~ race + offset(log(years)), family = poisson, data = bw)
  p <- predict(m, bw[c(12, 40, 150), ], se.fit = TRUE)
  expect_matches(ggnomogram(m), bw[c(12, 40, 150), ], c("race", "years"), p$fit, p$se.fit)

  m <- MASS::glm.nb(ftv ~ age + race, data = bw)
  p <- predict(m, d, se.fit = TRUE)
  expect_matches(ggnomogram(m), d, c("age", "race"), p$fit, p$se.fit)
})

test_that("survival models match survfit() and predict()", {
  skip_if_not_installed("survival")
  lu <- lung_data()
  d <- lu[c(1, 60, 120), ]
  m <- survival::coxph(survival::Surv(time, status) ~ age + sex + ph.ecog, data = lu)
  n <- ggnomogram(m, times = c("1 year" = 365))
  sf <- summary(survival::survfit(m, newdata = d), times = 365)
  got <- lapply(values_of(d, c("age", "sex", "ph.ecog")), function(v) nomogram_predict(n, v)$outputs)
  expect_equal(vapply(got, function(o) o$estimate, numeric(1)), as.numeric(sf$surv), tolerance = 1e-8)
  expect_equal(vapply(got, function(o) o$lower, numeric(1)), as.numeric(sf$lower), tolerance = 1e-6)

  m <- survival::coxph(survival::Surv(time, status) ~ age + ph.ecog + survival::strata(sex), data = lu)
  n <- ggnomogram(m, times = c("1 year" = 365))
  expect_equal(vapply(n$spec$nm$strata, `[[`, character(1), "label"), c("Male", "Female"))
  # The stratum is chosen, not drawn as a predictor.
  expect_equal(names(n$spec$vars), c("age", "ph.ecog"))
  nd <- d[1, ]
  nd$sex <- factor("Female", levels = c("Male", "Female"))
  sf <- summary(survival::survfit(m, newdata = nd), times = 365)
  o <- n$spec |> nomo_predict(values_of(d[1, ], c("age", "ph.ecog"))[[1]], stratum = 2)
  expect_equal(o$outputs$estimate, sf$surv[1], tolerance = 1e-8)

  m <- survival::survreg(survival::Surv(time, status) ~ age + sex, data = lu, dist = "weibull")
  n <- ggnomogram(m, times = c("1 year" = 365))
  q <- predict(m, d, type = "uquantile", p = 0.5, se.fit = TRUE)
  got <- lapply(values_of(d, c("age", "sex")), function(v) nomogram_predict(n, v)$outputs)
  expect_equal(vapply(got, function(o) o$estimate[1], numeric(1)), unname(exp(q$fit)), tolerance = 1e-8)
  expect_equal(vapply(got, function(o) o$lower[1], numeric(1)),
               unname(exp(q$fit - stats::qnorm(0.975) * q$se.fit)), tolerance = 1e-8)
  expect_error(ggnomogram(survival::coxph(survival::Surv(time, status) ~ age + tt(age),
                                          data = lu, tt = function(x, t, ...) x * log(t))),
               "tt\\(\\)")
})

test_that("ordinal and multinomial models give each category's probability", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("nnet")
  bw <- births()
  d <- bw[c(12, 40, 150), ]
  check_probs <- function(n, probs) {
    got <- t(vapply(values_of(d, c("age", "race")), function(v) {
      o <- nomogram_predict(n, v)$outputs
      utils::tail(o$estimate, 3)
    }, numeric(3)))
    expect_equal(unname(got), unname(probs), tolerance = 1e-8)
  }
  m <- MASS::polr(ftv3 ~ age + race, data = bw, Hess = TRUE)
  check_probs(ggnomogram(m), predict(m, d, type = "probs"))
  m <- nnet::multinom(ftv3 ~ age + race, data = bw, trace = FALSE)
  n <- ggnomogram(m, level = "Two or more")
  check_probs(n, predict(m, d, type = "probs"))
  expect_equal(n$spec$eq_start, 2L)
  expect_equal(length(n$render_data$eqs), 2)
  skip_if_not_installed("ordinal")
  m <- ordinal::clm(ftv3 ~ age + race, data = bw)
  check_probs(ggnomogram(m), predict(m, d[c("age", "race")], type = "prob")$fit)
})

test_that("mixed models predict for a typical cluster", {
  skip_if_not_installed("lme4")
  ss <- lme4::sleepstudy
  m <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = ss)
  d <- ss[c(1, 50, 90), ]
  X <- stats::model.matrix(~ Days, d)
  expect_matches(ggnomogram(m), d, "Days", predict(m, d, re.form = NA),
                 sqrt(diag(X %*% as.matrix(stats::vcov(m)) %*% t(X))))
  cb <- lme4::cbpp
  m <- lme4::glmer(cbind(incidence, size - incidence) ~ period + size + (1 | herd),
                   family = binomial, data = cb)
  d <- cb[c(1, 20, 40), ]
  expect_matches(ggnomogram(m), d, c("period", "size"), predict(m, d, re.form = NA))
  expect_match(nomo_notes_html(ggnomogram(m)$spec), "typical cluster", fixed = TRUE)
  skip_if_not_installed("nlme")
  od <- nlme::Orthodont
  m <- nlme::lme(distance ~ age + Sex, random = ~ 1 | Subject, data = od)
  d <- od[c(1, 30, 90), ]
  expect_matches(ggnomogram(m), d, c("age", "Sex"), predict(m, d, level = 0))
})

test_that("rms, mgcv and the other model packages are read", {
  skip_if_not_installed("rms")
  skip_if_not_installed("mgcv")
  skip_if_not_installed("survival")
  bw <- births()
  d <- bw[c(12, 40, 150), ]
  m <- mgcv::gam(low ~ s(age) + lwt + race, family = binomial, data = bw)
  p <- predict(m, d, se.fit = TRUE)
  expect_matches(ggnomogram(m), d, c("age", "lwt", "race"), p$fit, p$se.fit)
  m <- rms::lrm(low ~ rms::rcs(age, 4) + lwt + race, data = bw)
  p <- predict(m, d, se.fit = TRUE)
  expect_matches(ggnomogram(m), d, c("age", "lwt", "race"), p$linear.predictors, p$se.fit)
  lu <- lung_data()
  m <- rms::cph(survival::Surv(time, status) ~ age + sex, data = lu, x = TRUE, y = TRUE, surv = TRUE)
  n <- ggnomogram(m, times = c("1 year" = 365))
  ref <- rms::survest(m, lu[c(1, 60), ], times = 365)
  got <- vapply(values_of(lu[c(1, 60), ], c("age", "sex")),
                function(v) nomogram_predict(n, v)$outputs$estimate, numeric(1))
  expect_equal(got, as.numeric(ref$surv), tolerance = 1e-6)
  expect_error(ggnomogram(rms::cph(survival::Surv(time, status) ~ age, data = lu)), "x = TRUE")
})

test_that("the other linear, robust and penalized models match their fits", {
  skip_if_not_installed("MASS")
  bw <- births()
  d <- bw[c(12, 40, 150), ]
  X <- stats::model.matrix(~ age + smoke, d)
  if (requireNamespace("geepack", quietly = TRUE)) {
    m <- geepack::geeglm(low ~ age + smoke, id = seq_len(nrow(bw)), family = binomial, data = bw)
    expect_matches(ggnomogram(m), d, c("age", "smoke"), X %*% stats::coef(m),
                   sqrt(diag(X %*% stats::vcov(m) %*% t(X))))
  }
  if (requireNamespace("logistf", quietly = TRUE)) {
    m <- logistf::logistf(low ~ age + smoke, data = bw)
    expect_matches(ggnomogram(m), d, c("age", "smoke"), X %*% stats::coef(m),
                   sqrt(diag(X %*% m$var %*% t(X))))
  }
  if (requireNamespace("quantreg", quietly = TRUE)) {
    m <- suppressWarnings(quantreg::rq(bwt ~ age + smoke, data = bw, tau = 0.5))
    V <- suppressWarnings(summary(m, se = "nid", covariance = TRUE)$cov)
    expect_matches(suppressWarnings(ggnomogram(m)), d, c("age", "smoke"), X %*% stats::coef(m),
                   sqrt(diag(X %*% V %*% t(X))))
  }
  if (requireNamespace("nlme", quietly = TRUE)) {
    od <- nlme::Orthodont
    m <- nlme::gls(distance ~ age + Sex, data = od)
    expect_matches(ggnomogram(m), od[c(1, 30, 90), ], c("age", "Sex"), predict(m, od[c(1, 30, 90), ]))
  }
  if (suppressWarnings(requireNamespace("glmmTMB", quietly = TRUE))) {
    sal <- glmmTMB::Salamanders
    m <- suppressWarnings(glmmTMB::glmmTMB(count ~ mined + cover + (1 | site),
                                           family = glmmTMB::nbinom2, data = sal))
    p <- suppressWarnings(predict(m, sal[c(1, 100, 300), ], re.form = NA, se.fit = TRUE))
    expect_matches(suppressWarnings(ggnomogram(m)), sal[c(1, 100, 300), ], c("mined", "cover"),
                   p$fit, p$se.fit)
  }
  skip_if_not_installed("rms")
  skip_if_not_installed("survival")
  m <- rms::ols(bwt ~ age + race, data = bw)
  p <- predict(m, d, se.fit = TRUE)
  expect_matches(ggnomogram(m), d, c("age", "race"), p$linear.predictors, p$se.fit)
  m <- rms::orm(ftv3 ~ age + race, data = bw)
  got <- t(vapply(values_of(d, c("age", "race")), function(v) {
    utils::tail(nomogram_predict(ggnomogram(m), v)$outputs$estimate, 3)
  }, numeric(3)))
  expect_equal(unname(got), unname(predict(m, d, type = "fitted.ind")), tolerance = 1e-8)
  lu <- lung_data()
  m <- rms::psm(survival::Surv(time, status) ~ age + sex, data = lu, dist = "weibull")
  p <- predict(m, lu[c(1, 60, 120), ], se.fit = TRUE)
  expect_matches(ggnomogram(m, times = c("1 year" = 365)), lu[c(1, 60, 120), ], c("age", "sex"),
                 p$linear.predictors, p$se.fit)
})

test_that("bad input is refused with a clear message", {
  skip_if_not_installed("MASS")
  bw <- births()
  m <- glm(low ~ age + race, family = binomial, data = bw)
  expect_error(ggnomogram(m, values = list(race = "Purple")), "must be one of")
  expect_error(ggnomogram(m, outcome = c("a", "b")), "one label")
  expect_error(ggnomogram(stats::prcomp(bw[c("age", "lwt")])), "cannot read a model of class")
  set.seed(1)
  three <- data.frame(y = stats::rnorm(60), a = stats::rnorm(60), b = stats::rnorm(60), c = stats::rnorm(60))
  expect_error(ggnomogram(lm(y ~ a * b * c, data = three)), "more than two numeric")
  expect_error(ggnomogram(m, data = bw["age"]), "no column for race")
})

test_that("the widget and static copies draw", {
  skip_if_not_installed("MASS")
  bw <- births()
  n <- ggnomogram(glm(low ~ age + race + smoke, family = binomial, data = bw),
                  labels = c(age = "Age (years)"), title = "Low birth weight")
  expect_equal(n$on_render, paste0("ggextremeNomogram(el, data); ggextremeDetails(el, ",
                                   js_string("About this nomogram"), ", ",
                                   js_string(nomo_notes_html(n$spec)), ");"))
  w <- graph_widget(n)
  expect_s3_class(w, "girafe")
  expect_identical(w$jsHooks$render[[1]]$data, n$render_data)
  expect_equal(n$render_data$vars[[1]]$label, "Age (years)")
  png <- tempfile(fileext = ".png")
  graph_save(n, png, res = 40, theme = "dark")
  expect_true(file.exists(png))
})
