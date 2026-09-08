#' Simulate standardized mean differences for a meta-regression
#'
#' Generates study-level random effects, balanced group sizes, sample means,
#' and sample standard deviations, then computes Hedges' standardized mean
#' differences and their sampling variances with [metafor::escalc()]. When a
#' scale moderator is supplied, the study-specific heterogeneity variance is
#' `tau2 * exp(zeta * X - zeta^2 / 2)`.
#'
#' @param k Integer. Number of studies.
#' @param mu Numeric vector of length `k`. Location-model mean for each study.
#' @param tau2 Non-negative number. Baseline between-study variance.
#' @param n Positive number. Fixed sample size in each group.
#' @param X Optional numeric vector of length `k` containing the scale
#'   moderator.
#' @param zeta Numeric scalar. Log-linear scale coefficient.
#'
#' @return A `metafor` `escalc` data frame with one row per study. In addition
#'   to `yi` and `vi`, it contains the data-generating quantities, group sample
#'   sizes, sample means, and sample standard deviations.
#' @keywords internal
sim_es_fast <- function(k, mu, tau2, n, X = NULL, zeta = 0) {
  if (!is.null(X) && zeta != 0 && tau2 > 0) {
    tau2 <- tau2 * exp(zeta * X - zeta^2 / 2)
  }
  dat <- data.frame(id = 1:k, mu = mu, tau2 = tau2)

  # random effects (normal distribution)
  dat$deltai <- rnorm(k, 0, sqrt(tau2))

  dat$n1 <- dat$n2 <- n

  true_m1 <- dat$mu + dat$deltai
  true_m2 <- 0

  dat$m1i <- rnorm(k, mean = true_m1, sd = 1 / sqrt(dat$n1))
  dat$m2i <- rnorm(k, mean = true_m2, sd = 1 / sqrt(dat$n2))
  dat$sd1i <- sqrt(stats::rchisq(k, df = dat$n1 - 1) / (dat$n1 - 1))
  dat$sd2i <- sqrt(stats::rchisq(k, df = dat$n2 - 1) / (dat$n2 - 1))

  out <- metafor::escalc(
    "SMD",
    m1i = m1i,
    m2i = m2i,
    sd1i = sd1i,
    sd2i = sd2i,
    n1i = n1,
    n2i = n2,
    data = dat
  )

  out
}

#' Expand and validate a vector of regression coefficients
#'
#' @param beta A coefficient or vector of coefficients.
#' @param n Non-negative integer giving the required number of coefficients.
#' @param name Character scalar used to identify `beta` in error messages.
#'
#' @return A numeric vector of length `n`. A scalar `beta` is recycled; when
#'   `n` is zero, an empty numeric vector is returned.
#' @keywords internal
.expand_beta <- function(beta, n, name) {
  if (n == 0L) {
    return(numeric(0))
  }

  if (length(beta) == 1L) {
    return(rep(beta, n))
  }

  if (length(beta) != n) {
    stop(sprintf("'%s' must have length 1 or length %s.", name, n))
  }

  beta
}

#' Generate the meta-regression location model
#'
#' Draws standardized Gaussian focal and nuisance moderators with a compound-
#' symmetry correlation matrix and computes their linear predictor.
#'
#' @param k Integer. Number of studies.
#' @param nZ Non-negative integer. Number of nuisance moderators.
#' @param nX Non-negative integer. Number of focal moderators.
#' @param b0 Numeric scalar. Intercept.
#' @param bZ Numeric scalar or vector of length `nZ`. Nuisance coefficients.
#' @param bX Numeric scalar or vector of length `nX`. Focal coefficients.
#' @param rXZ Numeric scalar. Common pairwise correlation among all moderators.
#'
#' @return A named list with `M`, the model matrix, and `mu`, the corresponding
#'   vector of study-level means.
#' @keywords internal
get_mu <- function(k, nZ, nX, b0, bZ, bX, rXZ) {
  if (
    length(nZ) != 1L ||
      length(nX) != 1L ||
      any(is.na(c(nZ, nX))) ||
      any(c(nZ, nX) < 0L) ||
      any(c(nZ, nX) != as.integer(c(nZ, nX)))
  ) {
    stop("'nZ' and 'nX' must be non-negative integers.")
  }

  q <- nX + nZ

  bX <- .expand_beta(bX, nX, "bX")
  bZ <- .expand_beta(bZ, nZ, "bZ")
  beta <- c(b0, bX, bZ)

  x_names <- if (nX > 0L) paste0("x", seq_len(nX)) else character(0)
  z_names <- if (nZ > 0L) paste0("z", seq_len(nZ)) else character(0)

  if (q == 0L) {
    M_cov <- matrix(nrow = k, ncol = 0L)
  } else {
    R <- matrix(rXZ, nrow = q, ncol = q)
    diag(R) <- 1

    M_cov <- MASS::mvrnorm(k, rep(0, q), R)
    M_cov <- matrix(M_cov, nrow = k, ncol = q)
    colnames(M_cov) <- c(x_names, z_names)
  }

  M <- cbind(intercept = 1, M_cov)
  mu <- as.numeric(M %*% beta)

  list(M = M, mu = mu)
}

#' Extract Wald inference from a meta-analytic model
#'
#' @param fit A fitted `metafor::rma` model.
#'
#' @return A data frame containing coefficient names, estimates, standard
#'   errors, Wald statistics, and p-values.
#' @keywords internal
wald <- function(fit) {
  data.frame(
    coef = rownames(fit$b),
    b = fit$b,
    se_wald = fit$se,
    stat_wald = fit$zval,
    pval_wald = fit$pval
  )
}

#' Compute CR2 robust inference
#'
#' Applies [metafor::robust()] with singleton study clusters and the
#' `clubSandwich` small-sample correction.
#'
#' @param fit A fitted `metafor::rma` model whose data contain an `id` column.
#'
#' @return A data frame containing coefficient names, robust standard errors,
#'   test statistics, and p-values.
#' @keywords internal
robu <- function(fit) {
  res <- metafor::robust(fit, cluster = id, adjust = TRUE, clubSandwich = TRUE)
  data.frame(
    coef = rownames(res$b),
    se_rob = res$se,
    stat_rob = res$zval,
    pval_rob = res$pval
  )
}

#' Compute sign-flipping inference
#'
#' @param fit A fitted `metafor::rma` model accepted by [flipmeta::flipmeta()].
#'
#' @return A data frame containing coefficient names and sign-flipping
#'   p-values.
#' @keywords internal
flip <- function(fit) {
  res <- flipmeta::flipmeta(fit)$summary_table
  data.frame(
    coef = res$coefficient,
    pval_flip = res$p
  )
}

#' Fit a meta-regression and apply the competing tests
#'
#' Fits an equal- or random-effects meta-regression and combines Wald,
#' Knapp--Hartung, ad hoc Knapp--Hartung, CR2, and sign-flipping results.
#'
#' @param data A data frame containing at least `yi`, `vi`, and `id`.
#' @param mods A model matrix. A column named `intercept`, when present, is
#'   removed because [metafor::rma()] adds the intercept itself.
#' @param method Character scalar passed to the `method` argument of
#'   [metafor::rma()], for example `"EE"` or `"REML"`.
#'
#' @return A data frame with one row per coefficient and columns from all
#'   competing inferential procedures.
#' @keywords internal
fit_meta <- function(data, mods, method = "REML") {
  mods <- as.matrix(mods)
  intercept_col <- which(colnames(mods) == "intercept")

  if (length(intercept_col) > 0L) {
    mods <- mods[, -intercept_col[1L], drop = FALSE]
  }

  if (ncol(mods) == 0L) {
    fit <- metafor::rma(yi = yi, vi = vi, data = data, method = method)
  } else {
    fit <- metafor::rma(
      yi = yi,
      vi = vi,
      mods = mods,
      data = data,
      method = method
    )
  }

  wald_test <- wald(fit)
  knha_test <- knha(fit)
  rob_test <- robu(fit)
  flip_test <- flip(fit)
  Reduce(
    function(x, y) merge(x, y, by = "coef"),
    list(wald_test, knha_test, rob_test, flip_test)
  )
}

#' Failure-tolerant meta-analysis fitting wrapper
#'
#' A wrapper around [fit_meta()] created with [purrr::possibly()]. It returns
#' `NULL` when model fitting or any downstream inferential procedure fails.
#'
#' @inheritParams fit_meta
#' @return The result of [fit_meta()], or `NULL` on error.
#' @keywords internal
sfit_meta <- purrr::possibly(fit_meta)

#' Compute Knapp--Hartung inference
#'
#' Computes both the ordinary Knapp--Hartung adjustment and its ad hoc variant,
#' which truncates the residual scale factor at one.
#'
#' @param fit A fitted `metafor::rma` model.
#'
#' @return A data frame containing coefficient names, standard errors, test
#'   statistics, and p-values for both Knapp--Hartung variants.
#' @keywords internal
knha <- function(fit) {
  w <- as.numeric(1 / (fit$vi + fit$tau2))
  X <- model.matrix(fit)
  B <- c(fit$beta)
  k <- fit$k
  p <- fit$p
  y <- fit$yi

  XtWX <- crossprod(X, X * w)
  S <- solve(XtWX)
  residuals <- as.numeric(y - X %*% B)
  s2 <- sum(w * residuals^2) / (k - p)
  s2_adhoc <- max(s2, 1)
  s2_knha <- s2
  S_knha <- s2_knha * S
  S_ahdoc <- s2_adhoc * S
  se_knha <- sqrt(diag(S_knha))
  se_adhoc <- sqrt(diag(S_ahdoc))
  tval_knha <- B / se_knha
  tval_adhoc <- B / se_adhoc
  pval_knha <- 2 * pt(abs(tval_knha), df = k - p, lower.tail = FALSE)
  pval_adhoc <- 2 * pt(abs(tval_adhoc), df = k - p, lower.tail = FALSE)
  data.frame(
    coef = rownames(fit$beta),
    se_knha,
    se_adhoc,
    stat_knha = tval_knha,
    stat_adhoc = tval_adhoc,
    pval_knha,
    pval_adhoc
  )
}

#' Simulate one meta-regression data set
#'
#' Combines the location-model generator [get_mu()] with the effect-size
#' generator [sim_es_fast()]. The first focal moderator, when present, is used
#' as the scale moderator for study-specific heterogeneity.
#'
#' @inheritParams get_mu
#' @param tau2 Non-negative number. Baseline between-study variance.
#' @param zeta Numeric scalar. Log-linear scale coefficient.
#' @param n Positive number. Fixed sample size in each group.
#'
#' @return A named list with `M`, the simulated model matrix, and `data`, the
#'   `metafor` `escalc` data frame.
#' @keywords internal
sim_meta <- function(
  k,
  nZ,
  nX,
  b0,
  bZ,
  bX,
  rXZ,
  tau2,
  zeta = 0,
  n
) {
  mod <- get_mu(
    k = k,
    nZ = nZ,
    nX = nX,
    b0 = b0,
    bZ = bZ,
    bX = bX,
    rXZ = rXZ
  )

  X_vec <- if (nX > 0L && "x1" %in% colnames(mod$M)) mod$M[, "x1"] else NULL

  es <- sim_es_fast(
    k = k,
    mu = mod$mu,
    tau2 = tau2,
    n = n,
    X = X_vec,
    zeta = zeta
  )

  list(
    M = mod$M,
    data = es
  )
}

#' Run one simulation condition
#'
#' Repeatedly generates a meta-regression data set and analyzes it with
#' [sfit_meta()].
#'
#' @param nsim Positive integer. Number of Monte Carlo replications.
#' @inheritParams sim_meta
#' @param method Character scalar passed to [metafor::rma()].
#'
#' @return A list of length `nsim`; each element is either the data frame
#'   returned by [fit_meta()] or `NULL` if that replication failed.
#' @keywords internal
do_sim <- function(
  nsim = 1,
  k,
  nZ,
  nX,
  b0,
  bZ,
  bX,
  rXZ,
  tau2,
  zeta = 0,
  n,
  method = "REML"
) {
  replicate(
    nsim,
    {
      dat <- sim_meta(
        k = k,
        nZ = nZ,
        nX = nX,
        b0 = b0,
        bZ = bZ,
        bX = bX,
        rXZ = rXZ,
        tau2 = tau2,
        zeta = zeta,
        n = n
      )

      sfit_meta(
        data = dat$data,
        mods = dat$M,
        method = method
      )
    },
    simplify = FALSE
  )
}
