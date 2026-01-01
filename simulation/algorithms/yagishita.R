# ============================================================
# Yagishita STRLS/SLTS (slow and fast variants)
#
# Reference:
# - Yagishita & Ogata (2020) STRLS for sparse robust regression
#   (SLTS <-> STRLS equivalence; lambda scaling)
# ============================================================

# NOTE: This file assumes utils.R is sourced beforehand.

robust_preprocess <- function(y, X, eps = 1e-12) {
  y0 <- (y - median(y)) / (mad(y) + eps)
  Xs <- scale(X)
  list(y = as.numeric(y0), X = as.matrix(Xs))
}

# Slow version: recompute full residuals inside line-search
# Uses lambda_abs (SLTS scale); internally converts to STRLS scale.
yagishita_alg1_slts_slow <- function(
    X, y, h, lambda_abs,
    intercept = TRUE,
    init = c("random", "subsample_lasso", "zero"),
    subsample_size = NULL,
    seed = 1,
    c1 = 2,
    c2 = 1e-4,
    eta_min = 1e-10,
    eta_max = 1e10,
    tmax = 1e6,
    stop_tol = 1e-6,
    preprocess = FALSE,
    keep_hist = FALSE,
    verbose = FALSE
) {
  init <- match.arg(init)
  X <- as.matrix(X)
  y <- as.numeric(y)
  storage.mode(X) <- "double"

  if (preprocess) {
    pp <- robust_preprocess(y, X)
    y <- pp$y
    X <- pp$X
  }

  n <- nrow(X)
  p <- ncol(X)
  if (h < 1 || h > n) stop("h must satisfy 1 <= h <= nrow(X)")
  set.seed(seed)

  # SLTS scale -> STRLS scale (paper): lambda_internal = h * lambda_abs / 4
  lambda <- (h * lambda_abs) / 4

  partial_ok <- !is.null(tryCatch(
    sort.int(c(3, 1, 2), partial = 2, index.return = TRUE, method = "quick")$ix,
    error = function(e) NULL
  ))

  pick_h_smallest_local <- function(v, h) {
    if (partial_ok) {
      sort.int(v, partial = h, index.return = TRUE, method = "quick")$ix[1:h]
    } else {
      order(v)[1:h]
    }
  }

  prox_gamma_Th <- function(u, h, gamma) {
    s <- 2 * gamma + 1
    I <- pick_h_smallest_local(u^2, h)
    alpha <- u
    alpha[I] <- u[I] / s
    Th_alpha <- sum((u[I] / s)^2)
    list(alpha = alpha, I = I, Th = Th_alpha)
  }

  grad_l <- function(beta0, beta, alpha) {
    Xb <- as.numeric(X %*% beta)
    r <- y - (if (intercept) beta0 else 0) - Xb - alpha
    g0 <- if (intercept) (-sum(r)) else 0
    gb <- -as.numeric(crossprod(X, r))
    ga <- -r
    list(r = r, g0 = g0, gb = gb, ga = ga)
  }

  if (init == "random") {
    beta0 <- if (intercept) rnorm(1) else 0
    beta <- rnorm(p)
  } else if (init == "zero") {
    beta0 <- 0
    beta <- rep(0, p)
  } else {
    if (!requireNamespace("glmnet", quietly = TRUE)) {
      stop("Need glmnet for init='subsample_lasso'")
    }
    m <- if (is.null(subsample_size)) h else subsample_size
    m <- max(2, min(n, as.integer(m)))
    idx <- sample.int(n, m)
    lambda_glmnet <- lambda / m
    fit0 <- glmnet::glmnet(
      X[idx, , drop = FALSE], y[idx],
      alpha = 1,
      lambda = lambda_glmnet,
      intercept = intercept,
      standardize = FALSE
    )
    cf <- as.numeric(stats::coef(fit0, s = lambda_glmnet))
    if (intercept) {
      beta0 <- cf[1]
      beta <- cf[-1]
    } else {
      beta0 <- 0
      beta <- cf
    }
  }

  r0 <- as.numeric(y - (if (intercept) beta0 else 0) - X %*% beta)
  pr0 <- prox_gamma_Th(r0, h, gamma = 0.5)
  alpha <- pr0$alpha
  Th_alpha <- pr0$Th

  g <- grad_l(beta0, beta, alpha)
  L_cur <- 0.5 * sum(g$r^2) + 0.5 * Th_alpha + lambda * sum(abs(beta))

  grad0_norm_sq <- (if (intercept) g$g0^2 else 0) + sum(g$gb^2) + sum(g$ga^2)
  stop_rhs <- grad0_norm_sq * stop_tol

  if (keep_hist) L_hist <- c(L_cur) else L_hist <- NULL
  ls_total <- 0L

  beta0_prev <- beta0
  beta_prev <- beta
  alpha_prev <- alpha
  g_prev <- g

  t <- 0
  repeat {
    if (t >= tmax) break

    if (t == 0) {
      eta_b0 <- 1
      eta_b <- 1
      eta_a <- 1
    } else {
      if (intercept) {
        db0 <- beta0 - beta0_prev
        eta_b0 <- if (abs(db0) > 0) abs(g$g0 - g_prev$g0) / abs(db0) else 1
        eta_b0 <- min(eta_max, max(eta_min, eta_b0))
      } else {
        eta_b0 <- 1
      }

      db <- beta - beta_prev
      denom_b <- sum(db^2)
      eta_b <- if (denom_b > 0) sum((g$gb - g_prev$gb) * db) / denom_b else 1
      eta_b <- min(eta_max, max(eta_min, eta_b))

      da <- alpha - alpha_prev
      denom_a <- sum(da^2)
      eta_a <- if (denom_a > 0) sum((g$ga - g_prev$ga) * da) / denom_a else 1
      eta_a <- min(eta_max, max(eta_min, eta_a))
    }

    L_old <- L_cur
    beta0_old <- beta0
    beta_old <- beta
    alpha_old <- alpha
    g_old <- g

    ls_iter <- 0L
    repeat {
      beta0_new <- if (intercept) (beta0 - g$g0 / eta_b0) else 0
      beta_new <- soft_threshold(beta - g$gb / eta_b, lambda / eta_b)

      u <- alpha - g$ga / eta_a
      gamma <- 1 / (2 * eta_a)
      pr <- prox_gamma_Th(u, h, gamma)
      alpha_new <- pr$alpha
      Th_alpha_new <- pr$Th

      # (slow) recompute full residuals each candidate
      r_new <- as.numeric(y - (if (intercept) beta0_new else 0) - X %*% beta_new - alpha_new)
      L_new <- 0.5 * sum(r_new^2) + 0.5 * Th_alpha_new + lambda * sum(abs(beta_new))

      diff_sq <- (if (intercept) eta_b0 * (beta0_new - beta0)^2 else 0) +
        eta_b * sum((beta_new - beta)^2) +
        eta_a * sum((alpha_new - alpha)^2)

      if (L_new <= L_old - (c2 / 2) * diff_sq + 1e-12) break

      eta_b0 <- min(eta_max, c1 * eta_b0)
      eta_b <- min(eta_max, c1 * eta_b)
      eta_a <- min(eta_max, c1 * eta_a)

      ls_iter <- ls_iter + 1L
      if (ls_iter > 2000L) break
    }
    ls_total <- ls_total + ls_iter

    beta0_prev <- beta0
    beta_prev <- beta
    alpha_prev <- alpha
    g_prev <- g

    beta0 <- beta0_new
    beta <- beta_new
    alpha <- alpha_new
    Th_alpha <- Th_alpha_new

    g <- grad_l(beta0, beta, alpha)
    L_cur <- 0.5 * sum(g$r^2) + 0.5 * Th_alpha + lambda * sum(abs(beta))
    if (keep_hist) L_hist <- c(L_hist, L_cur)

    w0 <- if (intercept) (g$g0 - g_old$g0 - eta_b0 * (beta0 - beta0_old)) else 0
    wb <- g$gb - g_old$gb - eta_b * (beta - beta_old)
    wa <- g$ga - g_old$ga - eta_a * (alpha - alpha_old)
    w_norm_sq <- w0^2 + sum(wb^2) + sum(wa^2)

    if (verbose && (t %% 50 == 0 || t < 5)) {
      cat(sprintf(
        "[Yagishita-SLTS-slow] t=%d L=%.6f ls=%d ||w||^2=%.3e\n",
        t + 1, L_cur, ls_iter, w_norm_sq
      ))
    }

    t <- t + 1
    if (w_norm_sq <= stop_rhs) break
  }

  obj_common <- obj_slts(y, X, beta0, beta, h, lambda_abs)

  list(
    beta0 = beta0,
    beta = beta,
    alpha = alpha,
    L = L_cur,
    obj_common = obj_common,
    iter = t,
    line_search_steps = ls_total,
    partial_sort_ok = partial_ok,
    lambda_internal = lambda,
    L_hist = if (keep_hist) L_hist else NULL
  )
}

# Fast version: incremental residual updates in line-search
# Keeps same interface and lambda scaling as the slow version.
yagishita_alg1_slts_fast <- function(
    X, y, h, lambda_abs,
    intercept = TRUE,
    init = c("random", "subsample_lasso", "zero"),
    subsample_size = NULL,
    seed = 1,
    c1 = 2,
    c2 = 1e-4,
    eta_min = 1e-10,
    eta_max = 1e10,
    tmax = 1e6,
    stop_tol = 1e-6,
    preprocess = FALSE,
    keep_hist = FALSE,
    verbose = FALSE
) {
  init <- match.arg(init)
  X <- as.matrix(X)
  y <- as.numeric(y)
  storage.mode(X) <- "double"

  if (preprocess) {
    pp <- robust_preprocess(y, X)
    y <- pp$y
    X <- pp$X
  }

  n <- nrow(X)
  p <- ncol(X)
  if (h < 1 || h > n) stop("h must satisfy 1 <= h <= nrow(X)")
  set.seed(seed)

  lambda <- (h * lambda_abs) / 4

  partial_ok <- !is.null(tryCatch(
    sort.int(c(3, 1, 2), partial = 2, index.return = TRUE, method = "quick")$ix,
    error = function(e) NULL
  ))

  pick_h_smallest_local <- function(v, h) {
    if (partial_ok) {
      sort.int(v, partial = h, index.return = TRUE, method = "quick")$ix[1:h]
    } else {
      order(v)[1:h]
    }
  }

  prox_gamma_Th <- function(u, h, gamma) {
    s <- 2 * gamma + 1
    I <- pick_h_smallest_local(u^2, h)
    alpha <- u
    alpha[I] <- u[I] / s
    Th_alpha <- sum((u[I] / s)^2)
    list(alpha = alpha, I = I, Th = Th_alpha)
  }

  if (init == "random") {
    beta0 <- if (intercept) rnorm(1) else 0
    beta <- rnorm(p)
  } else if (init == "zero") {
    beta0 <- 0
    beta <- rep(0, p)
  } else {
    if (!requireNamespace("glmnet", quietly = TRUE)) {
      stop("Need glmnet for init='subsample_lasso'")
    }
    m <- if (is.null(subsample_size)) h else subsample_size
    m <- max(2, min(n, as.integer(m)))
    idx <- sample.int(n, m)
    lambda_glmnet <- lambda / m
    fit0 <- glmnet::glmnet(
      X[idx, , drop = FALSE], y[idx],
      alpha = 1,
      lambda = lambda_glmnet,
      intercept = intercept,
      standardize = FALSE
    )
    cf <- as.numeric(stats::coef(fit0, s = lambda_glmnet))
    if (intercept) {
      beta0 <- cf[1]
      beta <- cf[-1]
    } else {
      beta0 <- 0
      beta <- cf
    }
  }

  e0 <- as.numeric(y - (if (intercept) beta0 else 0) - X %*% beta)
  pr0 <- prox_gamma_Th(e0, h, gamma = 0.5)
  alpha <- pr0$alpha
  Th_alpha <- pr0$Th

  r <- as.numeric(y - (if (intercept) beta0 else 0) - X %*% beta - alpha)

  g0 <- if (intercept) (-sum(r)) else 0
  gb <- -as.numeric(crossprod(X, r))
  ga <- -r

  L_cur <- 0.5 * sum(r^2) + 0.5 * Th_alpha + lambda * sum(abs(beta))

  grad0_norm_sq <- (if (intercept) g0^2 else 0) + sum(gb^2) + sum(ga^2)
  stop_rhs <- grad0_norm_sq * stop_tol

  if (keep_hist) L_hist <- c(L_cur) else L_hist <- NULL
  ls_total <- 0L

  beta0_prev <- beta0
  beta_prev <- beta
  alpha_prev <- alpha
  g0_prev <- g0
  gb_prev <- gb
  ga_prev <- ga

  t <- 0
  repeat {
    if (t >= tmax) break

    if (t == 0) {
      eta_b0 <- 1
      eta_b <- 1
      eta_a <- 1
    } else {
      if (intercept) {
        db0 <- beta0 - beta0_prev
        eta_b0 <- if (abs(db0) > 0) abs(g0 - g0_prev) / abs(db0) else 1
        eta_b0 <- min(eta_max, max(eta_min, eta_b0))
      } else {
        eta_b0 <- 1
      }

      db <- beta - beta_prev
      denom_b <- sum(db^2)
      eta_b <- if (denom_b > 0) sum((gb - gb_prev) * db) / denom_b else 1
      eta_b <- min(eta_max, max(eta_min, eta_b))

      da <- alpha - alpha_prev
      denom_a <- sum(da^2)
      eta_a <- if (denom_a > 0) sum((ga - ga_prev) * da) / denom_a else 1
      eta_a <- min(eta_max, max(eta_min, eta_a))
    }

    L_old <- L_cur
    beta0_old <- beta0
    beta_old <- beta
    alpha_old <- alpha
    g0_old <- g0
    gb_old <- gb
    ga_old <- ga

    ls_iter <- 0L
    repeat {
      beta0_new <- if (intercept) (beta0 - g0 / eta_b0) else 0
      beta_new <- soft_threshold(beta - gb / eta_b, lambda / eta_b)

      u <- alpha - ga / eta_a
      gamma <- 1 / (2 * eta_a)
      pr <- prox_gamma_Th(u, h, gamma)
      alpha_new <- pr$alpha
      Th_alpha_new <- pr$Th

      d0 <- if (intercept) (beta0_new - beta0) else 0
      db_vec <- beta_new - beta
      da_vec <- alpha_new - alpha

      idx <- which(db_vec != 0)
      if (length(idx) > 0) {
        Xdb <- as.numeric(X[, idx, drop = FALSE] %*% db_vec[idx])
      } else {
        Xdb <- rep(0, n)
      }

      r_new <- r - d0 - Xdb - da_vec
      L_new <- 0.5 * sum(r_new^2) + 0.5 * Th_alpha_new + lambda * sum(abs(beta_new))

      diff_sq <- (if (intercept) eta_b0 * (beta0_new - beta0)^2 else 0) +
        eta_b * sum((beta_new - beta)^2) +
        eta_a * sum((alpha_new - alpha)^2)

      if (L_new <= L_old - (c2 / 2) * diff_sq + 1e-12) break

      eta_b0 <- min(eta_max, c1 * eta_b0)
      eta_b <- min(eta_max, c1 * eta_b)
      eta_a <- min(eta_max, c1 * eta_a)

      ls_iter <- ls_iter + 1L
      if (ls_iter > 2000L) break
    }
    ls_total <- ls_total + ls_iter

    beta0_prev <- beta0
    beta_prev <- beta
    alpha_prev <- alpha
    g0_prev <- g0
    gb_prev <- gb
    ga_prev <- ga

    beta0 <- beta0_new
    beta <- beta_new
    alpha <- alpha_new
    Th_alpha <- Th_alpha_new
    r <- r_new

    g0 <- if (intercept) (-sum(r)) else 0
    gb <- -as.numeric(crossprod(X, r))
    ga <- -r

    L_cur <- 0.5 * sum(r^2) + 0.5 * Th_alpha + lambda * sum(abs(beta))
    if (keep_hist) L_hist <- c(L_hist, L_cur)

    w0 <- if (intercept) (g0 - g0_old - eta_b0 * (beta0 - beta0_old)) else 0
    wb <- gb - gb_old - eta_b * (beta - beta_old)
    wa <- ga - ga_old - eta_a * (alpha - alpha_old)
    w_norm_sq <- w0^2 + sum(wb^2) + sum(wa^2)

    if (verbose && (t %% 50 == 0 || t < 5)) {
      cat(sprintf(
        "[Yagishita-SLTS-fast] t=%d L=%.6f ls=%d ||w||^2=%.3e\n",
        t + 1, L_cur, ls_iter, w_norm_sq
      ))
    }

    t <- t + 1
    if (w_norm_sq <= stop_rhs) break
  }

  obj_common <- obj_slts(y, X, beta0, beta, h, lambda_abs)

  list(
    beta0 = beta0,
    beta = beta,
    alpha = alpha,
    L = L_cur,
    obj_common = obj_common,
    iter = t,
    line_search_steps = ls_total,
    partial_sort_ok = partial_ok,
    lambda_internal = lambda,
    L_hist = if (keep_hist) L_hist else NULL
  )
}
