# ============================================================
# Yang Algorithm 1 (SLTS scale) - slow and fast variants
#
# Reference:
# - Yang, C., et al. (2013) Sparse least trimmed squares for high-dimensional data
#   (Algorithm 1; SLTS objective)
# ============================================================

# NOTE: This file assumes utils.R is sourced beforehand.

# Slow (baseline) implementation: recompute full residuals each step
# This is kept for comparison and benchmarking.
yang_alg1_slts_slow <- function(
    X, y, h, lambda_abs,
    intercept = TRUE,
    init = c("lasso", "zero"),
    seed = 1,
    max_iter = 2000,
    tol = 1e-6,
    min_iter = 30,
    stable_H_K = 5,
    eta_init = 1.0,
    backtrack = 0.5,
    ls_max = 50,
    verbose = FALSE
) {
  init <- match.arg(init)
  set.seed(seed)

  X <- as.matrix(X)
  y <- as.numeric(y)
  n <- nrow(X)
  p <- ncol(X)

  if (h < 1 || h > n) stop("h must satisfy 1 <= h <= n")

  # Prox-grad uses 0.5 * sum r_H^2 + (h*lambda_abs/2)||beta||1 (argmin unchanged)
  lambda_pen <- (h * lambda_abs) / 2

  if (init == "lasso") {
    if (!requireNamespace("glmnet", quietly = TRUE)) {
      stop("Need glmnet for init='lasso'")
    }
    lambda_glmnet <- lambda_pen / max(1, n)
    fit0 <- glmnet::glmnet(
      X, y,
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
  } else {
    beta0 <- if (intercept) mean(y) else 0
    beta <- rep(0, p)
  }

  eta <- eta_init
  H_prev <- integer(0)
  stable_count <- 0
  obj_hist <- numeric(0)

  for (t in seq_len(max_iter)) {
    # (slow) compute full residuals each iteration
    r <- as.numeric(y - (beta0 + X %*% beta))
    H_cur <- sort(pick_h_smallest(r^2, h))

    rH <- r[H_cur]
    XH <- X[H_cur, , drop = FALSE]
    f_old <- 0.5 * sum(rH^2)

    g0 <- if (intercept) (-sum(rH)) else 0
    g <- -as.numeric(crossprod(XH, rH))

    eta_try <- eta
    for (ls in seq_len(ls_max)) {
      beta0_new <- if (intercept) (beta0 - eta_try * g0) else 0
      beta_new <- soft_threshold(beta - eta_try * g, eta_try * lambda_pen)

      # (slow) compute full residuals for line-search candidate
      r_new <- as.numeric(y - (beta0_new + X %*% beta_new))
      f_new <- 0.5 * sum(r_new[H_cur]^2)

      d0 <- if (intercept) (beta0_new - beta0) else 0
      db <- beta_new - beta

      rhs <- f_old +
        (if (intercept) g0 * d0 else 0) +
        sum(g * db) +
        ((if (intercept) d0^2 else 0) + sum(db^2)) / (2 * eta_try)

      if (f_new <= rhs + 1e-12) break
      eta_try <- eta_try * backtrack
    }

    eta <- eta_try
    beta0 <- beta0_new
    beta <- beta_new

    r_up <- as.numeric(y - (beta0 + X %*% beta))
    H_new <- sort(pick_h_smallest(r_up^2, h))
    obj_now <- sum(r_up^2[H_new]) + h * lambda_abs * sum(abs(beta))
    obj_hist <- c(obj_hist, obj_now)

    if (length(H_prev) == h && identical(H_new, H_prev)) {
      stable_count <- stable_count + 1
    } else {
      stable_count <- 0
    }
    H_prev <- H_new

    if (verbose && (t == 1 || t %% 50 == 0)) {
      cat(sprintf(
        "[Yang-SLTS-slow] iter=%d obj=%.6f stableH=%d eta=%.2e\n",
        t, obj_now, stable_count, eta
      ))
    }

    if (t >= max(min_iter, 2)) {
      relchg <- abs(obj_hist[t] - obj_hist[t - 1]) / (abs(obj_hist[t - 1]) + 1e-12)
      if (relchg < tol && stable_count >= stable_H_K) break
    }
  }

  list(
    beta0 = beta0,
    beta = beta,
    inliers = H_prev,
    obj_hist = obj_hist,
    iter = length(obj_hist)
  )
}

# Fast (improved) implementation: incremental residual updates
# Key improvements:
# - Update r <- r - d0 - X db
# - Line-search uses only H subset
# - Pre-allocated objective history

yang_alg1_slts_fast <- function(
    X, y, h, lambda_abs,
    intercept = TRUE,
    init = c("lasso", "zero"),
    seed = 1,
    max_iter = 2000,
    tol = 1e-6,
    min_iter = 30,
    stable_H_K = 5,
    eta_init = 1.0,
    backtrack = 0.5,
    ls_max = 50,
    support_eps = 1e-6,
    use_support_stop = FALSE,
    stable_S_K = 5,
    verbose = FALSE
) {
  init <- match.arg(init)
  set.seed(seed)

  X <- as.matrix(X)
  y <- as.numeric(y)
  n <- nrow(X)
  p <- ncol(X)

  if (h < 1 || h > n) stop("h must satisfy 1 <= h <= n")

  lambda_pen <- (h * lambda_abs) / 2

  if (init == "lasso") {
    if (!requireNamespace("glmnet", quietly = TRUE)) {
      stop("Need glmnet for init='lasso'")
    }
    lambda_glmnet <- lambda_pen / max(1, n)
    fit0 <- glmnet::glmnet(
      X, y,
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
  } else {
    beta0 <- if (intercept) mean(y) else 0
    beta <- rep(0, p)
  }

  r <- as.numeric(y - (beta0 + X %*% beta))
  r2 <- r^2
  H_cur <- sort.int(pick_h_smallest(r2, h))

  eta <- eta_init
  H_prev <- H_cur
  stable_count <- 0

  obj_hist <- numeric(max_iter)

  if (use_support_stop) {
    S_prev <- which(abs(beta) > support_eps)
    stableS_count <- 0
  } else {
    stableS_count <- 0
  }

  for (t in seq_len(max_iter)) {
    rH <- r[H_cur]
    XH <- X[H_cur, , drop = FALSE]
    f_old <- 0.5 * sum(rH^2)

    g0 <- if (intercept) (-sum(rH)) else 0
    g <- -as.numeric(crossprod(XH, rH))

    eta_try <- eta
    beta0_new <- beta0
    beta_new <- beta

    d0_best <- 0
    db_best <- rep(0, p)
    idx_best <- integer(0)

    for (ls in seq_len(ls_max)) {
      beta0_cand <- if (intercept) (beta0 - eta_try * g0) else 0
      beta_cand <- soft_threshold(beta - eta_try * g, eta_try * lambda_pen)

      d0 <- if (intercept) (beta0_cand - beta0) else 0
      db <- beta_cand - beta
      idx <- which(db != 0)

      if (length(idx) > 0) {
        XHdb <- as.numeric(XH[, idx, drop = FALSE] %*% db[idx])
      } else {
        XHdb <- rep(0, h)
      }
      rH_new <- rH - d0 - XHdb
      f_new <- 0.5 * sum(rH_new^2)

      rhs <- f_old +
        (if (intercept) g0 * d0 else 0) +
        sum(g * db) +
        ((if (intercept) d0^2 else 0) + sum(db^2)) / (2 * eta_try)

      if (f_new <= rhs + 1e-12) {
        beta0_new <- beta0_cand
        beta_new <- beta_cand
        d0_best <- d0
        db_best <- db
        idx_best <- idx
        break
      }
      eta_try <- eta_try * backtrack
    }

    eta <- eta_try
    beta0 <- beta0_new
    beta <- beta_new

    r <- r - d0_best
    if (length(idx_best) > 0) {
      r <- r - as.numeric(X[, idx_best, drop = FALSE] %*% db_best[idx_best])
    }

    r2 <- r^2
    H_new <- sort.int(pick_h_smallest(r2, h))
    obj_now <- sum(r2[H_new]) + h * lambda_abs * sum(abs(beta))
    obj_hist[t] <- obj_now

    if (identical(H_new, H_prev)) {
      stable_count <- stable_count + 1
    } else {
      stable_count <- 0
    }
    H_prev <- H_new
    H_cur <- H_new

    if (use_support_stop) {
      S_cur <- which(abs(beta) > support_eps)
      if (length(S_cur) == length(S_prev) && all(S_cur == S_prev)) {
        stableS_count <- stableS_count + 1
      } else {
        stableS_count <- 0
      }
      S_prev <- S_cur
    }

    if (verbose && (t == 1 || t %% 50 == 0)) {
      cat(sprintf(
        "[Yang-SLTS-fast] iter=%d obj=%.6f nnz=%d stableH=%d stableS=%d eta=%.2e\n",
        t, obj_now, nnz(beta), stable_count, stableS_count, eta
      ))
    }

    if (t >= max(min_iter, 2)) {
      relchg <- abs(obj_hist[t] - obj_hist[t - 1]) / (abs(obj_hist[t - 1]) + 1e-12)
      stop_ok <- (relchg < tol && stable_count >= stable_H_K)
      if (use_support_stop) stop_ok <- stop_ok && (stableS_count >= stable_S_K)
      if (stop_ok) break
    }
  }

  iter <- max(which(obj_hist != 0))
  if (!is.finite(iter)) iter <- 1L

  list(
    beta0 = beta0,
    beta = beta,
    inliers = H_cur,
    obj_hist = obj_hist[1:iter],
    iter = iter
  )
}
