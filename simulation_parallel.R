############################################################
## FULL SCRIPT (single file): n-grid multi-method benchmark
## + 4 tuning strategies for LTS regressions:
##   - VAL    : validation split (MSE / F1_support / F1_outlier)
##   - PLUGIN : robust scale (MAD on LTS subset residuals)
##   - STAB   : stability selection (instability minimization)
##   - EBIC   : EBIC on train-only (trimmed SSE-based)
##
## Outputs include:
##   - fit_time / tune_time / total_time per method
##   - lambda_used / scale_used
##   - C_code  : lambda / sqrt(log p / (n*h))
##   - C_paper : lambda / sqrt(log p / h)
##
## NOTE:
## - robustHD::sparseLTS is optional (skipped if robustHD not installed).
## - Uses your existing "avg-scale" base lambda:
##     lambda_base_avg(n,p,h) = c_lambda * sqrt(log(p)/(n*h))
## - Methods internally differ (avg-scale vs sum-scale) as in your original script.
## - Parallelized with doFuture (see run_n_experiment_multi_tune).
############################################################

############################################################
## 0) Packages
############################################################
pkgs <- c("glmnet", "mvtnorm", "ggplot2", "patchwork", "doFuture", "foreach", "future")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(glmnet)
library(mvtnorm)
library(ggplot2)
library(patchwork)
library(doFuture)
library(foreach)
library(future)

if (!requireNamespace("robustHD", quietly = TRUE)) {
  message("robustHD not installed. robustHD::sparseLTS will be skipped unless install.packages('robustHD').")
}

############################################################
## 1) Basic utilities (metrics / standardize / timing)
############################################################

# L1 proximal operator (soft-thresholding)
soft_threshold <- function(z, t) {
  sign(z) * pmax(abs(z) - t, 0)
}

# set-based F1
f1_score_from_sets <- function(set_hat, set_true) {
  set_hat <- unique(as.integer(set_hat))
  set_true <- unique(as.integer(set_true))
  tp <- length(intersect(set_hat, set_true))
  fp <- length(setdiff(set_hat, set_true))
  fn <- length(setdiff(set_true, set_hat))
  prec <- if ((tp + fp) == 0) 0 else tp / (tp + fp)
  rec  <- if ((tp + fn) == 0) 0 else tp / (tp + fn)
  if ((prec + rec) == 0) return(0)
  2 * prec * rec / (prec + rec)
}

support_f1 <- function(beta_hat, beta_true, eps = 1e-8) {
  S_hat <- which(abs(beta_hat) > eps)
  S_true <- which(abs(beta_true) > 0)
  f1_score_from_sets(S_hat, S_true)
}

outlier_f1 <- function(B_hat, B_true) {
  f1_score_from_sets(B_hat, B_true)
}

# bottom-k / top-k indices (tie-safe)
bottomk_idx <- function(x, k) {
  n <- length(x)
  if (k <= 0) return(integer(0))
  if (k >= n) return(seq_len(n))
  x2 <- x
  x2[!is.finite(x2)] <- Inf
  kth <- sort.int(x2, partial = k)[k]
  idx <- which(x2 <= kth)
  if (length(idx) > k) idx <- idx[order(x2[idx], idx)][seq_len(k)]
  if (length(idx) < k) idx <- order(x2)[seq_len(k)]
  idx
}

topk_idx <- function(x, k) {
  n <- length(x)
  if (k <= 0) return(integer(0))
  if (k >= n) return(seq_len(n))
  x2 <- x
  x2[!is.finite(x2)] <- -Inf
  kth <- sort.int(-x2, partial = k)[k]
  idx <- which((-x2) <= kth)
  if (length(idx) > k) idx <- idx[order(-x2[idx], idx)][seq_len(k)]
  if (length(idx) < k) idx <- order(x2, decreasing = TRUE)[seq_len(k)]
  idx
}

# power method for ||X||_2^2 (largest eigenvalue of X^T X)
power_method_norm2_sq <- function(X, n_iter = 20) {
  X <- as.matrix(X)
  p <- ncol(X)
  v <- rnorm(p)
  v <- v / sqrt(sum(v^2) + 1e-12)
  for (i in 1:n_iter) {
    v <- drop(crossprod(X, X %*% v))
    nv <- sqrt(sum(v^2) + 1e-12)
    if (nv == 0) return(0)
    v <- v / nv
  }
  Av <- drop(crossprod(X, X %*% v))
  sum(v * Av)
}

# train standardize and apply to test
standardize_train_test <- function(Xtr, Xte) {
  mu <- colMeans(Xtr)
  sdv <- apply(Xtr, 2, sd)
  sdv[sdv == 0] <- 1
  Xtr_s <- scale(Xtr, center = mu, scale = sdv)
  Xte_s <- scale(Xte, center = mu, scale = sdv)
  list(Xtr = as.matrix(Xtr_s), Xte = as.matrix(Xte_s), center = mu, scale = sdv)
}

# timing wrapper
time_it <- function(expr) {
  tt <- system.time(res <- eval.parent(substitute(expr)))
  list(res = res, elapsed = unname(tt["elapsed"]))
}

############################################################
## 1.5) Extra utilities for tuning/C extraction
############################################################
support_idx <- function(beta, eps = 1e-8) which(abs(beta) > eps)

trimmed_sse <- function(r2, h) {
  idx <- bottomk_idx(r2, h)
  sum(r2[idx])
}

robust_sigma_mad <- function(resid) {
  stats::mad(resid, center = stats::median(resid), constant = 1.4826)
}

calc_C_code  <- function(lambda_used, p, n, h) lambda_used / sqrt(log(p) / (n * h))
calc_C_paper <- function(lambda_used, p, h)     lambda_used / sqrt(log(p) / h)

############################################################
## 2) Base lambda rules (avg-scale) and conversion to sum-scale
############################################################
# avg-scale base lambda: lambda_avg = c * sqrt(log(p)/(n*h))
lambda_base_avg <- function(n, p, h, c_lambda = 1.0) {
  c_lambda * sqrt(log(p) / (n * h))
}

# convert avg-scale lambda to sum-scale lambda
lambda_avg_to_sum <- function(lambda_avg, h) {
  2 * h * lambda_avg
}

############################################################
## 3) Simulation: contaminated LM + clean test
############################################################
simulate_contaminated_lm <- function(
    n = 200, p = 100, s = 10,
    sigma = 1.0, eps = 0.1,
    outlier_shift_y = 8,
    leverage_shift_x = 5,
    n_test = 200,
    seed = 1,
    beta_gen = c("uniform", "fixed"),
    beta_unif_min = 1.0,
    beta_unif_max = 3.0,
    beta_random_sign = TRUE
) {
  beta_gen <- match.arg(beta_gen)
  set.seed(seed)
  
  beta_true <- rep(0, p)
  nz <- seq_len(min(s, p))
  
  if (beta_gen == "uniform") {
    b <- runif(length(nz), min = beta_unif_min, max = beta_unif_max)
    if (beta_random_sign) b <- b * sample(c(-1, 1), length(nz), replace = TRUE)
    beta_true[nz] <- b
  } else {
    beta_true[nz] <- 2
  }
  
  X <- matrix(rnorm(n * p), n, p)
  y <- as.numeric(X %*% beta_true + rnorm(n, sd = sigma))
  
  m_out <- floor(eps * n)
  outlier_idx <- if (m_out > 0) sample.int(n, m_out) else integer(0)
  if (m_out > 0) {
    y[outlier_idx] <- y[outlier_idx] + outlier_shift_y
    X[outlier_idx, ] <- X[outlier_idx, ] + leverage_shift_x * matrix(rnorm(m_out * p), m_out, p) / sqrt(p)
  }
  
  X_test <- matrix(rnorm(n_test * p), n_test, p)
  y_test <- as.numeric(X_test %*% beta_true + rnorm(n_test, sd = sigma))
  
  list(
    X = X, y = y,
    X_test = X_test, y_test = y_test,
    beta_true = beta_true,
    outlier_idx = outlier_idx,
    n = n, p = p, s = s
  )
}

############################################################
## 4) SWLTS-PALM (avg-scale)
############################################################
clip01 <- function(x) pmin(1, pmax(0, x))

project_capped_simplex <- function(v, h, tol = 1e-8, max_iter = 100) {
  low  <- min(v) - 1
  high <- max(v)
  tau <- 0
  for (iter in 1:max_iter) {
    tau <- (low + high) / 2
    w <- clip01(v - tau)
    s <- sum(w)
    if (abs(s - h) <= tol) break
    if (s > h) low <- tau else high <- tau
  }
  clip01(v - tau)
}

swlts_palm <- function(X, y,
                       alpha = 0.75, h = NULL,
                       lambda_avg,
                       max_iter = 300,
                       tol_beta = 1e-4, tol_w = 1e-4,
                       eta_beta = NULL, eta_w = 1.0,
                       inert_beta = 0.0, inert_w = 0.0,
                       final_refit = TRUE, refit_iter = 60,
                       verbose = FALSE) {
  X <- as.matrix(X); y <- as.numeric(y)
  n <- nrow(X); p <- ncol(X)
  if (is.null(h)) h <- max(1, min(n, floor(alpha * n)))
  
  if (is.null(eta_beta)) {
    norm2_sq <- power_method_norm2_sq(X, n_iter = 20)
    if (norm2_sq <= 0) norm2_sq <- 1
    eta_beta <- 0.95 * h / norm2_sq
  }
  
  beta  <- rep(0, p)
  beta0 <- median(y)
  
  r <- y - beta0 - as.numeric(X %*% beta)
  H <- bottomk_idx(r^2, h)
  w <- rep(0, n); w[H] <- 1
  
  beta_prev <- beta
  w_prev <- w
  
  for (t in 1:max_iter) {
    beta_tilde <- beta + inert_beta * (beta - beta_prev)
    w_tilde    <- w    + inert_w    * (w    - w_prev)
    
    r <- y - beta0 - as.numeric(X %*% beta_tilde)
    s <- 0.5 * (r^2)
    
    v <- w_tilde - eta_w * (1 / h) * s
    w_new <- project_capped_simplex(v, h)
    
    g <- -(1 / h) * drop(crossprod(X, w_new * r))
    u <- beta_tilde - eta_beta * g
    beta_new <- soft_threshold(u, eta_beta * lambda_avg)
    
    y_minus_Xb <- y - as.numeric(X %*% beta_new)
    beta0_new <- sum(w_new * y_minus_Xb) / sum(w_new)
    
    rel_beta <- sqrt(sum((beta_new - beta)^2)) / max(1, sqrt(sum(beta^2)))
    rel_w    <- sum(abs(w_new - w)) / h
    
    if (verbose && (t %% 20 == 0)) {
      cat(sprintf("[SWLTS-PALM] iter=%d rel_beta=%.2e rel_w=%.2e\n", t, rel_beta, rel_w))
    }
    
    if (rel_beta < tol_beta && rel_w < tol_w) {
      beta <- beta_new; beta0 <- beta0_new; w <- w_new
      break
    }
    
    beta_prev <- beta; w_prev <- w
    beta <- beta_new; beta0 <- beta0_new; w <- w_new
  }
  
  if (final_refit) {
    idx_in <- order(w, decreasing = TRUE)[1:h]
    w_hard <- rep(0, n); w_hard[idx_in] <- 1
    
    beta_ref <- beta
    beta0_ref <- beta0
    for (j in 1:refit_iter) {
      r_ref <- y - beta0_ref - as.numeric(X %*% beta_ref)
      g_ref <- -(1 / h) * drop(crossprod(X, w_hard * r_ref))
      beta_ref <- soft_threshold(beta_ref - eta_beta * g_ref, eta_beta * lambda_avg)
      y_minus_Xb <- y - as.numeric(X %*% beta_ref)
      beta0_ref <- sum(w_hard * y_minus_Xb) / sum(w_hard)
    }
    beta <- beta_ref; beta0 <- beta0_ref; w <- w_hard
  }
  
  list(beta0 = beta0, beta = beta, w = w, h = h)
}

############################################################
## 5) DCA / DCA_fast / BDCA_fast (avg-scale)
############################################################
trimmed_lts_obj_avg <- function(r2, h, beta, lambda_avg) {
  idx <- bottomk_idx(r2, h)
  (1/(2*h))*sum(r2[idx]) + lambda_avg * sum(abs(beta))
}

dca_sparse_lts_lasso <- function(X, y,
                                 alpha = 0.75, h = NULL,
                                 lambda_avg = 0.05,
                                 max_outer = 60,
                                 inner_iter = 80,
                                 tol = 1e-4,
                                 eta_beta = NULL,
                                 verbose = FALSE) {
  X <- as.matrix(X); storage.mode(X) <- "double"
  y <- as.numeric(y)
  n <- nrow(X); p <- ncol(X)
  
  if (is.null(h)) h <- max(1, min(n, floor(alpha * n)))
  k <- n - h
  
  if (is.null(eta_beta)) {
    norm2_sq <- power_method_norm2_sq(X, n_iter = 20)
    if (norm2_sq <= 0) norm2_sq <- 1
    eta_beta <- 0.95 * h / norm2_sq
  }
  
  beta  <- rep(0, p)
  beta0 <- median(y)
  
  B_prev <- integer(0)
  obj_hist <- numeric(max_outer)
  converged <- FALSE
  
  for (t in 1:max_outer) {
    r <- y - beta0 - as.numeric(X %*% beta)
    q <- r^2
    B <- topk_idx(q, k)
    
    u0 <- 0
    u_beta <- rep(0, p)
    if (k > 0) {
      u0 <- -(1 / h) * sum(r[B])
      u_beta <- -(1 / h) * drop(crossprod(X[B, , drop = FALSE], r[B]))
    }
    
    beta_in  <- beta
    beta0_in <- beta0
    
    for (j in 1:inner_iter) {
      r_in <- y - beta0_in - as.numeric(X %*% beta_in)
      grad_beta <- -(1 / h) * drop(crossprod(X, r_in)) - u_beta
      beta_next <- soft_threshold(beta_in - eta_beta * grad_beta, eta_beta * lambda_avg)
      beta0_next <- (sum(y - as.numeric(X %*% beta_next)) + h * u0) / n
      
      rel <- sqrt(sum((beta_next - beta_in)^2)) / max(1, sqrt(sum(beta_in^2)))
      beta_in <- beta_next
      beta0_in <- beta0_next
      if (rel < tol) break
    }
    
    beta_new <- beta_in
    beta0_new <- beta0_in
    
    r_new <- y - beta0_new - as.numeric(X %*% beta_new)
    obj_hist[t] <- trimmed_lts_obj_avg(r_new^2, h, beta_new, lambda_avg)
    
    same_B <- (length(B_prev) == length(B) && all(sort(B_prev) == sort(B)))
    rel_beta <- sqrt(sum((beta_new - beta)^2)) / max(1, sqrt(sum(beta^2)))
    
    if (verbose) {
      cat(sprintf("DCA    Outer %d | obj=%.6f | rel_beta=%.3e | B_stable=%s\n",
                  t, obj_hist[t], rel_beta, same_B))
    }
    
    beta <- beta_new
    beta0 <- beta0_new
    if (same_B && rel_beta < tol) {
      converged <- TRUE
      obj_hist <- obj_hist[1:t]
      B_prev <- B
      break
    }
    B_prev <- B
  }
  
  list(beta0 = beta0, beta = beta,
       outlier_set = B_prev, h = h,
       converged = converged, objective = obj_hist,
       eta_beta = eta_beta)
}

solve_lasso_subproblem_active_fista <- function(X, y,
                                                beta0, beta,
                                                u0, u_beta,
                                                lambda_avg, h,
                                                A, step,
                                                max_iter = 200,
                                                tol = 1e-6) {
  n <- length(y)
  if (length(A) == 0) return(list(beta0 = beta0, beta = beta))
  
  XA <- X[, A, drop = FALSE]
  b <- beta[A]
  z <- b
  t <- 1
  
  for (iter in 1:max_iter) {
    rz <- y - beta0 - as.numeric(XA %*% z)
    
    delta0 <- (sum(rz) + h * u0) / n
    beta0 <- beta0 + delta0
    rz <- rz - delta0
    
    gradA <- -(1 / h) * drop(crossprod(XA, rz)) - u_beta[A]
    b_new <- soft_threshold(z - step * gradA, step * lambda_avg)
    
    t_new <- (1 + sqrt(1 + 4 * t^2)) / 2
    z <- b_new + ((t - 1) / t_new) * (b_new - b)
    
    rel <- sqrt(sum((b_new - b)^2)) / max(1, sqrt(sum(b^2)))
    b <- b_new
    t <- t_new
    if (rel < tol) break
  }
  
  beta[A] <- b
  r_final <- y - beta0 - as.numeric(X %*% beta)
  beta0 <- beta0 + (sum(r_final) + h * u0) / n
  
  list(beta0 = beta0, beta = beta)
}

kkt_check_lasso <- function(X, y, beta0, beta, u_beta, lambda_avg, h) {
  r <- y - beta0 - as.numeric(X %*% beta)
  grad <- -(1 / h) * drop(crossprod(X, r)) - u_beta
  
  nz <- which(beta != 0)
  z  <- which(beta == 0)
  
  viol_nz <- 0
  if (length(nz) > 0) viol_nz <- max(abs(grad[nz] + lambda_avg * sign(beta[nz])))
  
  viol_z <- 0
  viol_idx <- integer(0)
  if (length(z) > 0) {
    vv <- abs(grad[z]) - lambda_avg
    viol_z <- max(pmax(vv, 0))
    viol_idx <- z[which(vv > 0)]
  }
  
  list(grad = grad, viol_nz = viol_nz, viol_z = viol_z, viol_idx = viol_idx)
}

dca_sparse_lts_lasso_fast <- function(X, y,
                                      alpha = 0.75, h = NULL,
                                      lambda_avg = 0.05,
                                      max_outer = 60,
                                      kkt_tol = 1e-4,
                                      max_kkt_rounds = 10,
                                      active_min = 50,
                                      add_max = 200,
                                      inner_max_iter = 200,
                                      inner_tol = 1e-6,
                                      step = NULL,
                                      verbose = FALSE) {
  X <- as.matrix(X); storage.mode(X) <- "double"
  y <- as.numeric(y)
  n <- nrow(X); p <- ncol(X)
  
  if (is.null(h)) h <- max(1, min(n, floor(alpha * n)))
  k <- n - h
  
  if (is.null(step)) {
    norm2_sq <- power_method_norm2_sq(X, n_iter = 20)
    if (norm2_sq <= 0) norm2_sq <- 1
    step <- 0.95 * h / norm2_sq
  }
  
  beta  <- rep(0, p)
  beta0 <- median(y)
  
  A <- integer(0)
  B_prev <- integer(0)
  obj_hist <- numeric(max_outer)
  converged <- FALSE
  
  for (t in 1:max_outer) {
    r_old <- y - beta0 - as.numeric(X %*% beta)
    B <- topk_idx(r_old^2, k)
    
    u0 <- 0
    u_beta <- rep(0, p)
    if (k > 0) {
      u0 <- -(1 / h) * sum(r_old[B])
      if (k <= 0.3 * n) {
        u_beta <- -(1 / h) * drop(crossprod(X[B, , drop = FALSE], r_old[B]))
      } else {
        rb <- numeric(n); rb[B] <- r_old[B]
        u_beta <- -(1 / h) * drop(crossprod(X, rb))
      }
    }
    
    for (round in 1:max_kkt_rounds) {
      if (length(A) < min(active_min, p)) {
        grad0 <- -(1 / h) * drop(crossprod(X, r_old)) - u_beta
        ord <- order(abs(grad0), decreasing = TRUE)
        A <- sort(unique(c(which(beta != 0), ord[seq_len(min(active_min, p))])))
      }
      
      sol <- solve_lasso_subproblem_active_fista(
        X, y,
        beta0 = beta0, beta = beta,
        u0 = u0, u_beta = u_beta,
        lambda_avg = lambda_avg, h = h,
        A = A, step = step,
        max_iter = inner_max_iter,
        tol = inner_tol
      )
      beta0 <- sol$beta0
      beta  <- sol$beta
      
      kk <- kkt_check_lasso(X, y, beta0, beta, u_beta, lambda_avg, h)
      viol <- max(kk$viol_nz, kk$viol_z)
      
      if (verbose) cat(sprintf("DCA_fast Outer %d / round %d | KKT=%.3e | |A|=%d\n",
                               t, round, viol, length(A)))
      
      if (viol <= kkt_tol) break
      
      idx <- kk$viol_idx
      if (length(idx) == 0) break
      grad <- kk$grad
      idx <- idx[order(abs(grad[idx]), decreasing = TRUE)]
      idx_add <- idx[seq_len(min(add_max, length(idx)))]
      A <- sort(unique(c(A, idx_add, which(beta != 0))))
    }
    
    r_new <- y - beta0 - as.numeric(X %*% beta)
    obj_hist[t] <- trimmed_lts_obj_avg(r_new^2, h, beta, lambda_avg)
    
    same_B <- (length(B_prev) == length(B) && all(sort(B_prev) == sort(B)))
    if (verbose) cat(sprintf("DCA_fast Outer %d | obj=%.6f | B_stable=%s | nnz=%d\n",
                             t, obj_hist[t], same_B, sum(beta != 0)))
    
    if (t >= 2 && same_B) {
      rel_obj <- abs(obj_hist[t] - obj_hist[t-1]) / max(1, abs(obj_hist[t-1]))
      if (rel_obj < 1e-6) {
        converged <- TRUE
        obj_hist <- obj_hist[1:t]
        B_prev <- B
        break
      }
    }
    
    A <- sort(unique(c(A, which(beta != 0))))
    B_prev <- B
  }
  
  list(beta0 = beta0, beta = beta,
       outlier_set = B_prev, h = h,
       converged = converged, objective = obj_hist,
       step = step, active_set = A, kkt_tol = kkt_tol)
}

bdca_sparse_lts_lasso_fast <- function(X, y,
                                       alpha = 0.75, h = NULL,
                                       lambda_avg = 0.05,
                                       max_outer = 60,
                                       kkt_tol = 1e-4,
                                       max_kkt_rounds = 10,
                                       active_min = 50,
                                       add_max = 200,
                                       inner_max_iter = 200,
                                       inner_tol = 1e-6,
                                       step = NULL,
                                       ls_lambda_init = 2.0,
                                       ls_lambda_cap  = 64.0,
                                       ls_beta = 0.5,
                                       ls_rho  = 1e-6,
                                       ls_max_iter = 20,
                                       verbose = FALSE) {
  X <- as.matrix(X); storage.mode(X) <- "double"
  y <- as.numeric(y)
  n <- nrow(X); p <- ncol(X)
  
  if (is.null(h)) h <- max(1, min(n, floor(alpha * n)))
  k <- n - h
  
  if (is.null(step)) {
    norm2_sq <- power_method_norm2_sq(X, n_iter = 20)
    if (norm2_sq <= 0) norm2_sq <- 1
    step <- 0.95 * h / norm2_sq
  }
  
  beta  <- rep(0, p)
  beta0 <- median(y)
  
  A <- integer(0)
  B_prev <- integer(0)
  obj_hist <- numeric(max_outer)
  converged <- FALSE
  
  for (t in 1:max_outer) {
    x0 <- beta0
    x  <- beta
    
    r_old <- y - x0 - as.numeric(X %*% x)
    B <- topk_idx(r_old^2, k)
    
    u0 <- 0
    u_beta <- rep(0, p)
    if (k > 0) {
      u0 <- -(1 / h) * sum(r_old[B])
      if (k <= 0.3 * n) {
        u_beta <- -(1 / h) * drop(crossprod(X[B, , drop = FALSE], r_old[B]))
      } else {
        rb <- numeric(n); rb[B] <- r_old[B]
        u_beta <- -(1 / h) * drop(crossprod(X, rb))
      }
    }
    
    beta0_y <- x0
    beta_y  <- x
    
    for (round in 1:max_kkt_rounds) {
      if (length(A) < min(active_min, p)) {
        grad0 <- -(1 / h) * drop(crossprod(X, r_old)) - u_beta
        ord <- order(abs(grad0), decreasing = TRUE)
        A <- sort(unique(c(which(beta_y != 0), ord[seq_len(min(active_min, p))])))
      }
      
      sol <- solve_lasso_subproblem_active_fista(
        X, y,
        beta0 = beta0_y, beta = beta_y,
        u0 = u0, u_beta = u_beta,
        lambda_avg = lambda_avg, h = h,
        A = A, step = step,
        max_iter = inner_max_iter,
        tol = inner_tol
      )
      beta0_y <- sol$beta0
      beta_y  <- sol$beta
      
      kk <- kkt_check_lasso(X, y, beta0_y, beta_y, u_beta, lambda_avg, h)
      viol <- max(kk$viol_nz, kk$viol_z)
      if (verbose) cat(sprintf("BDCA Outer %d / round %d | KKT=%.3e\n", t, round, viol))
      if (viol <= kkt_tol) break
      
      idx <- kk$viol_idx
      if (length(idx) == 0) break
      grad <- kk$grad
      idx <- idx[order(abs(grad[idx]), decreasing = TRUE)]
      idx_add <- idx[seq_len(min(add_max, length(idx)))]
      A <- sort(unique(c(A, idx_add, which(beta_y != 0))))
    }
    
    d0 <- beta0_y - x0
    d  <- beta_y  - x
    dnorm2 <- d0^2 + sum(d^2)
    if (dnorm2 == 0) { converged <- TRUE; obj_hist <- obj_hist[1:(t-1)]; break }
    
    obj_x <- trimmed_lts_obj_avg(r_old^2, h, x, lambda_avg)
    Xd <- as.numeric(X %*% d)
    
    lam <- min(ls_lambda_init, ls_lambda_cap)
    accept <- FALSE
    
    for (ls in 1:ls_max_iter) {
      r_c <- r_old - lam * (d0 + Xd)
      beta_c0 <- x0 + lam * d0
      beta_c  <- x  + lam * d
      
      obj_c <- trimmed_lts_obj_avg(r_c^2, h, beta_c, lambda_avg)
      
      if (obj_c <= obj_x - ls_rho * (lam^2) * dnorm2) {
        accept <- TRUE
        beta0 <- beta_c0
        beta  <- beta_c
        break
      }
      
      if (lam <= 1 + 1e-12) {
        lam <- 1
        accept <- TRUE
        beta0 <- beta0_y
        beta  <- beta_y
        break
      }
      
      lam <- max(1, lam * ls_beta)
    }
    
    if (!accept) {
      beta0 <- beta0_y
      beta  <- beta_y
      lam <- 1
    }
    
    r_new <- y - beta0 - as.numeric(X %*% beta)
    obj_hist[t] <- trimmed_lts_obj_avg(r_new^2, h, beta, lambda_avg)
    
    A <- sort(unique(c(A, which(beta != 0))))
    
    same_B <- (length(B_prev) == length(B) && all(sort(B_prev) == sort(B)))
    if (verbose) cat(sprintf("BDCA Outer %d | obj=%.6f | lam=%.2f | B_stable=%s\n",
                             t, obj_hist[t], lam, same_B))
    
    if (t >= 2 && same_B) {
      rel_obj <- abs(obj_hist[t] - obj_hist[t-1]) / max(1, abs(obj_hist[t-1]))
      if (rel_obj < 1e-6) {
        converged <- TRUE
        obj_hist <- obj_hist[1:t]
        B_prev <- B
        break
      }
    }
    
    B_prev <- B
  }
  
  list(beta0 = beta0, beta = beta,
       outlier_set = B_prev, h = h,
       converged = converged, objective = obj_hist,
       step = step, active_set = A, kkt_tol = kkt_tol)
}

############################################################
## 6) Yang (sum-scale) + iYang-BB-Restart
############################################################
yang_sumscale <- function(X, y, h, lambda_sum,
                          intercept = TRUE,
                          max_iter = 2000,
                          tol = 1e-6,
                          stable_H_K = 5,
                          eta_init = 1.0,
                          backtrack = 0.5,
                          ls_max = 50,
                          verbose = FALSE) {
  X <- as.matrix(X); y <- as.numeric(y)
  n <- nrow(X); p <- ncol(X)
  
  b <- rep(0, p)
  b0 <- if (intercept) mean(y) else 0
  
  eta <- eta_init
  H_prev <- integer(0)
  stable_cnt <- 0
  obj_prev <- Inf
  
  for (t in 1:max_iter) {
    r <- as.numeric(y - (if (intercept) b0 else 0) - X %*% b)
    H <- sort(bottomk_idx(r^2, h))
    
    if (length(H_prev)==h && identical(H, H_prev)) stable_cnt <- stable_cnt + 1 else stable_cnt <- 0
    H_prev <- H
    
    rH <- r[H]
    f_old <- sum(rH^2)
    
    g0 <- if (intercept) (-2 * sum(rH)) else 0
    g  <- -2 * as.numeric(crossprod(X[H,,drop=FALSE], rH))
    
    eta_try <- eta
    for (ls in 1:ls_max) {
      b0_new <- if (intercept) (b0 - eta_try * g0) else 0
      b_new  <- soft_threshold(b - eta_try * g, eta_try * lambda_sum)
      
      r_new <- as.numeric(y - (if (intercept) b0_new else 0) - X %*% b_new)
      f_new <- sum(r_new[H]^2)
      
      d0 <- if (intercept) (b0_new - b0) else 0
      db <- b_new - b
      rhs <- f_old + (if (intercept) g0*d0 else 0) + sum(g*db) +
        ((if (intercept) d0^2 else 0) + sum(db^2)) / (2*eta_try)
      
      if (f_new <= rhs + 1e-12) break
      eta_try <- eta_try * backtrack
    }
    
    eta <- eta_try
    b0 <- b0_new
    b  <- b_new
    
    obj_now <- f_new + lambda_sum * sum(abs(b))
    relchg <- abs(obj_prev - obj_now) / (abs(obj_prev) + 1e-12)
    obj_prev <- obj_now
    
    if (verbose && (t %% 200 == 0)) cat(sprintf("[Yang] iter=%d relchg=%.2e stable=%d\n", t, relchg, stable_cnt))
    if (t >= 20 && relchg < tol && stable_cnt >= stable_H_K) break
  }
  
  list(beta0 = b0, beta = b)
}

iyang_bb_restart <- function(X, y, h, lambda_sum,
                             intercept = TRUE,
                             max_iter = 2000,
                             tol = 1e-6,
                             stable_H_K = 5,
                             mom = 0.35,
                             eta_scale = 0.99,
                             verbose = FALSE,
                             seed = 1) {
  set.seed(seed)
  X <- as.matrix(X); y <- as.numeric(y)
  n <- nrow(X); p <- ncol(X)
  
  lambda_avg <- lambda_sum / (2*h)
  
  S <- power_method_norm2_sq(X, n_iter=20) + 1e-12
  eta_base <- eta_scale * h / S
  eta_min <- 0.2 * eta_base
  eta_max <- 5.0 * eta_base
  
  b  <- rep(0, p)
  b0 <- if (intercept) mean(y) else 0
  b_prev <- b; b0_prev <- b0
  
  H_prev <- integer(0)
  stable_cnt <- 0
  g_prev <- rep(0, p); have_g_prev <- FALSE
  eta_t <- eta_base
  
  for (t in 1:max_iter) {
    r <- as.numeric(y - (if (intercept) b0 else 0) - X %*% b)
    H <- sort(bottomk_idx(r^2, h))
    H_same <- (length(H_prev)==h && identical(H, H_prev))
    if (H_same) stable_cnt <- stable_cnt + 1 else stable_cnt <- 0
    H_prev <- H
    
    rH <- r[H]
    g0 <- if (intercept) (-mean(rH)) else 0
    g  <- -as.numeric(crossprod(X[H,,drop=FALSE], rH)) / h
    
    mu <- if (H_same) mom else 0
    
    if (have_g_prev && H_same) {
      s <- b - b_prev
      yk <- g - g_prev
      denom <- sum(s * yk)
      if (is.finite(denom) && denom > 1e-14) {
        eta_bb <- sum(s*s) / denom
        if (is.finite(eta_bb)) eta_t <- min(eta_max, max(eta_min, eta_bb))
      } else {
        eta_t <- eta_base
      }
    } else {
      eta_t <- eta_base
    }
    
    b0_new <- if (intercept) (b0 - eta_t*g0 + mu*(b0 - b0_prev)) else 0
    b_new  <- soft_threshold(b - eta_t*g + mu*(b - b_prev), eta_t*lambda_avg)
    
    relchg <- sqrt(sum((b_new - b)^2)) / (sqrt(sum(b^2)) + 1e-12)
    
    b0_prev <- b0; b_prev <- b
    b0 <- b0_new; b <- b_new
    g_prev <- g; have_g_prev <- TRUE
    
    if (verbose && (t %% 200 == 0)) cat(sprintf("[iYang] iter=%d rel=%.2e stable=%d\n", t, relchg, stable_cnt))
    if (t >= 20 && relchg < tol && stable_cnt >= stable_H_K) break
  }
  
  list(beta0 = b0, beta = b)
}

############################################################
## 7) Yagishita-orig / fast (STRLS, sum-scale)
############################################################
strls_obj_sum <- function(X, y, b0, b, I, lambda_sum) {
  r <- as.numeric(y - b0 - X %*% b)
  sum(r[I]^2) + lambda_sum * sum(abs(b))
}

yagishita_fast_strls <- function(X, y, h, lambda_sum,
                                 intercept = TRUE,
                                 step = NULL,
                                 max_iter = 2000,
                                 tol = 1e-6,
                                 stable_I_K = 5,
                                 verbose = FALSE) {
  X <- as.matrix(X); y <- as.numeric(y)
  n <- nrow(X); p <- ncol(X)
  
  if (is.null(step)) {
    L <- 2 * (power_method_norm2_sq(X, n_iter = 20) + 1e-12)
    step <- 0.95 / L
  }
  
  b <- rep(0, p)
  b0 <- if (intercept) mean(y) else 0
  
  I_prev <- integer(0)
  stable_cnt <- 0
  
  for (t in 1:max_iter) {
    r <- as.numeric(y - b0 - X %*% b)
    I <- sort(bottomk_idx(r^2, h))
    
    if (length(I_prev)==h && identical(I, I_prev)) stable_cnt <- stable_cnt + 1 else stable_cnt <- 0
    I_prev <- I
    
    XI <- X[I, , drop=FALSE]
    rI <- y[I] - b0 - as.numeric(XI %*% b)
    
    g <- -2 * as.numeric(crossprod(XI, rI))
    
    b_new <- soft_threshold(b - step * g, step * lambda_sum)
    
    if (intercept) {
      b0_new <- mean(y[I] - as.numeric(XI %*% b_new))
    } else b0_new <- 0
    
    rel <- sqrt(sum((b_new - b)^2)) / (sqrt(sum(b^2)) + 1e-12)
    b <- b_new; b0 <- b0_new
    
    if (verbose && (t %% 200 == 0)) cat(sprintf("[Yag-fast] iter=%d rel=%.2e stable=%d\n", t, rel, stable_cnt))
    if (t >= 20 && rel < tol && stable_cnt >= stable_I_K) break
  }
  
  list(beta0=b0, beta=b)
}

yagishita_orig_strls <- function(X, y, h, lambda_sum,
                                 intercept = TRUE,
                                 max_iter = 2000,
                                 tol = 1e-6,
                                 stable_I_K = 5,
                                 eta_scale = 0.95,
                                 ls_beta = 0.5,
                                 ls_max = 30,
                                 verbose = FALSE) {
  X <- as.matrix(X); y <- as.numeric(y)
  n <- nrow(X); p <- ncol(X)
  
  L0 <- 2 * (power_method_norm2_sq(X, n_iter=20) + 1e-12)
  step_base <- eta_scale / L0
  step_min <- 0.1 * step_base
  step_max <- 10.0 * step_base
  
  b <- rep(0, p)
  b0 <- if (intercept) mean(y) else 0
  
  I_prev <- integer(0)
  stable_cnt <- 0
  
  g_prev <- rep(0, p)
  b_prev <- b
  have_prev <- FALSE
  step_t <- step_base
  
  for (t in 1:max_iter) {
    r <- as.numeric(y - b0 - X %*% b)
    I <- sort(bottomk_idx(r^2, h))
    I_same <- (length(I_prev)==h && identical(I, I_prev))
    if (I_same) stable_cnt <- stable_cnt + 1 else stable_cnt <- 0
    I_prev <- I
    
    XI <- X[I,,drop=FALSE]
    rI <- y[I] - b0 - as.numeric(XI %*% b)
    
    g <- -2 * as.numeric(crossprod(XI, rI))
    
    if (have_prev && I_same) {
      s <- b - b_prev
      yk <- g - g_prev
      denom <- sum(s * yk)
      if (is.finite(denom) && denom > 1e-14) {
        step_bb <- sum(s*s) / denom
        if (is.finite(step_bb)) step_t <- min(step_max, max(step_min, step_bb))
      } else {
        step_t <- step_base
      }
    } else {
      step_t <- step_base
    }
    
    obj_old <- strls_obj_sum(X, y, b0, b, I, lambda_sum)
    
    step_try <- step_t
    accept <- FALSE
    for (ls in 1:ls_max) {
      b_try <- soft_threshold(b - step_try * g, step_try * lambda_sum)
      if (intercept) {
        b0_try <- mean(y[I] - as.numeric(XI %*% b_try))
      } else b0_try <- 0
      
      obj_new <- strls_obj_sum(X, y, b0_try, b_try, I, lambda_sum)
      if (obj_new <= obj_old + 1e-12) {
        accept <- TRUE
        b0 <- b0_try
        b  <- b_try
        break
      }
      step_try <- step_try * ls_beta
      if (step_try < step_min) break
    }
    
    if (!accept) {
      step_try <- step_min
      b <- soft_threshold(b - step_try * g, step_try * lambda_sum)
      if (intercept) b0 <- mean(y[I] - as.numeric(XI %*% b)) else b0 <- 0
    }
    
    rel <- sqrt(sum((b - b_prev)^2)) / (sqrt(sum(b_prev^2)) + 1e-12)
    
    g_prev <- g
    b_prev <- b
    have_prev <- TRUE
    
    if (verbose && (t %% 200 == 0)) cat(sprintf("[Yag-orig] iter=%d rel=%.2e stable=%d\n", t, rel, stable_cnt))
    if (t >= 20 && rel < tol && stable_cnt >= stable_I_K) break
  }
  
  list(beta0=b0, beta=b)
}

############################################################
## 8) Method wrapper: fit_one_method
############################################################
is_sumscale_method <- function(method) {
  method %in% c("Yang_sum", "iYang_BB", "Yagishita_orig", "Yagishita_fast")
}

fit_one_method <- function(method, X, y, alpha, lambda_avg, ctrl = list()) {
  X <- as.matrix(X); y <- as.numeric(y)
  n <- nrow(X)
  h <- max(1, min(n, floor(alpha * n)))
  
  if (method == "robustHD::sparseLTS") {
    if (!requireNamespace("robustHD", quietly = TRUE)) stop("robustHD not installed")
    fit <- robustHD::sparseLTS(X, y, alpha = alpha, lambda = lambda_avg)
    cc <- coef(fit)
    return(list(beta0 = unname(cc[1]), beta = unname(cc[-1])))
  }
  
  if (method == "SWLTS-PALM") {
    fit <- do.call(swlts_palm, c(list(X=X, y=y, alpha=alpha, h=h, lambda_avg=lambda_avg), ctrl))
    return(list(beta0 = fit$beta0, beta = fit$beta))
  }
  
  if (method == "DCA") {
    fit <- do.call(dca_sparse_lts_lasso, c(list(X=X, y=y, alpha=alpha, h=h, lambda_avg=lambda_avg), ctrl))
    return(list(beta0 = fit$beta0, beta = fit$beta))
  }
  if (method == "DCA_fast") {
    fit <- do.call(dca_sparse_lts_lasso_fast, c(list(X=X, y=y, alpha=alpha, h=h, lambda_avg=lambda_avg), ctrl))
    return(list(beta0 = fit$beta0, beta = fit$beta))
  }
  if (method == "BDCA_fast") {
    fit <- do.call(bdca_sparse_lts_lasso_fast, c(list(X=X, y=y, alpha=alpha, h=h, lambda_avg=lambda_avg), ctrl))
    return(list(beta0 = fit$beta0, beta = fit$beta))
  }
  
  if (is_sumscale_method(method)) {
    lambda_sum <- lambda_avg_to_sum(lambda_avg, h)
    
    if (method == "Yang_sum") {
      fit <- do.call(yang_sumscale, c(list(X=X, y=y, h=h, lambda_sum=lambda_sum), ctrl))
      return(list(beta0=fit$beta0, beta=fit$beta))
    }
    if (method == "iYang_BB") {
      fit <- do.call(iyang_bb_restart, c(list(X=X, y=y, h=h, lambda_sum=lambda_sum), ctrl))
      return(list(beta0=fit$beta0, beta=fit$beta))
    }
    if (method == "Yagishita_orig") {
      fit <- do.call(yagishita_orig_strls, c(list(X=X, y=y, h=h, lambda_sum=lambda_sum), ctrl))
      return(list(beta0=fit$beta0, beta=fit$beta))
    }
    if (method == "Yagishita_fast") {
      fit <- do.call(yagishita_fast_strls, c(list(X=X, y=y, h=h, lambda_sum=lambda_sum), ctrl))
      return(list(beta0=fit$beta0, beta=fit$beta))
    }
  }
  
  stop(sprintf("Unknown method: %s", method))
}

############################################################
## 9) Benchmark (fit -> metrics + times + C)
############################################################
benchmark_methods <- function(
    sim,
    methods,
    alpha = 0.75,
    c_lambda = 1.0,
    scale_by_method,
    tune_time_by_method = NULL,
    standardize_for_fair = TRUE,
    ctrl_by_method = list(),
    verbose = FALSE
) {
  X <- sim$X; y <- sim$y
  X_test <- sim$X_test; y_test <- sim$y_test
  beta_true <- sim$beta_true
  out_true <- sim$outlier_idx
  
  n <- nrow(X); p <- ncol(X)
  h <- max(1, min(n, floor(alpha * n)))
  k <- n - h
  
  if (standardize_for_fair) {
    std <- standardize_train_test(X, X_test)
    X <- std$Xtr
    X_test <- std$Xte
  } else {
    X <- as.matrix(X); X_test <- as.matrix(X_test)
  }
  
  rows <- vector("list", length(methods))
  for (ii in seq_along(methods)) {
    m <- methods[ii]
    sc <- as.numeric(scale_by_method[m])
    if (!is.finite(sc)) stop(sprintf("scale_by_method[%s] is not finite", m))
    
    lambda0 <- lambda_base_avg(n=n, p=p, h=h, c_lambda=c_lambda)
    lambda_avg_used <- lambda0 * sc
    
    ctrl <- ctrl_by_method[[m]]
    if (is.null(ctrl)) ctrl <- list()
    
    tt <- time_it({
      fit_one_method(m, X, y, alpha = alpha, lambda_avg = lambda_avg_used, ctrl = ctrl)
    })
    
    b0 <- tt$res$beta0
    b  <- tt$res$beta
    
    fit_time <- tt$elapsed
    tune_time <- 0
    if (!is.null(tune_time_by_method) && !is.null(tune_time_by_method[m]) && is.finite(tune_time_by_method[m])) {
      tune_time <- unname(tune_time_by_method[m])
    }
    total_time <- fit_time + tune_time
    
    yhat <- as.numeric(b0 + X_test %*% b)
    mse <- mean((y_test - yhat)^2)
    
    f1s <- support_f1(b, beta_true)
    
    r_tr <- y - b0 - as.numeric(X %*% b)
    B_hat <- topk_idx(r_tr^2, k)
    f1o <- outlier_f1(B_hat, out_true)
    
    rows[[ii]] <- data.frame(
      method = m,
      fit_time = fit_time,
      tune_time = tune_time,
      total_time = total_time,
      MSE = mse,
      F1_support = f1s,
      F1_outlier = f1o,
      lambda_used = lambda_avg_used,
      scale_used = sc,
      C_code = calc_C_code(lambda_avg_used, p=p, n=n, h=h),
      C_paper = calc_C_paper(lambda_avg_used, p=p, h=h),
      stringsAsFactors = FALSE
    )
    
    if (verbose) {
      cat(sprintf("[%-13s] fit=%.3f tune=%.3f total=%.3f  MSE=%.4f  F1_s=%.3f  F1_o=%.3f  lambda=%.4g  scale=%.3g\n",
                  m, fit_time, tune_time, total_time, mse, f1s, f1o, lambda_avg_used, sc))
    }
  }
  
  do.call(rbind, rows)
}

############################################################
## 10) Split helper for VAL tuning
############################################################
make_train_val_split <- function(sim_full, val_frac = 0.25, seed = 1) {
  set.seed(seed)
  X <- sim_full$X; y <- sim_full$y
  n <- nrow(X)
  n_val <- max(1, floor(val_frac * n))
  idx_val <- sample.int(n, n_val)
  idx_tr  <- setdiff(seq_len(n), idx_val)
  
  out_full <- sim_full$outlier_idx
  out_val <- intersect(out_full, idx_val)
  
  list(
    X_tr = X[idx_tr, , drop = FALSE],
    y_tr = y[idx_tr],
    X_val = X[idx_val, , drop = FALSE],
    y_val = y[idx_val],
    out_val = match(out_val, idx_val),
    beta_true = sim_full$beta_true
  )
}

############################################################
## 10A) Tuning strategy: VAL (validation split)
############################################################
tune_scales_val_per_method <- function(
    sim_full,
    methods,
    scale_grid = 10^seq(-2, 2, by = 0.5),
    val_frac = 0.25,
    tune_seed = 123,
    alpha = 0.75,
    c_lambda = 1.0,
    standardize_for_fair = TRUE,
    ctrl_by_method = list(),
    criterion = "MSE",
    verbose_each = FALSE
) {
  sp <- make_train_val_split(sim_full, val_frac = val_frac, seed = tune_seed)
  
  Xtr <- sp$X_tr; ytr <- sp$y_tr
  Xv  <- sp$X_val; yv  <- sp$y_val
  beta_true <- sp$beta_true
  out_val <- sp$out_val
  
  ntr <- nrow(Xtr); p <- ncol(Xtr)
  htr <- max(1, min(ntr, floor(alpha * ntr)))
  kv <- max(0, nrow(Xv) - max(1, floor(alpha * nrow(Xv))))
  
  if (standardize_for_fair) {
    std <- standardize_train_test(Xtr, Xv)
    Xtr <- std$Xtr
    Xv  <- std$Xte
  } else {
    Xtr <- as.matrix(Xtr); Xv <- as.matrix(Xv)
  }
  
  tune_tbl <- list()
  best_scales <- setNames(rep(NA_real_, length(methods)), methods)
  tune_time_by_method <- setNames(rep(0, length(methods)), methods)
  
  lambda0 <- lambda_base_avg(n=ntr, p=p, h=htr, c_lambda=c_lambda)
  
  for (m in methods) {
    t0 <- proc.time()[["elapsed"]]
    ctrl <- ctrl_by_method[[m]]; if (is.null(ctrl)) ctrl <- list()
    
    score_vec <- rep(Inf, length(scale_grid))
    
    for (gi in seq_along(scale_grid)) {
      sc <- scale_grid[gi]
      lambda_avg_used <- lambda0 * sc
      
      fit_try <- tryCatch({
        fit_one_method(m, Xtr, ytr, alpha=alpha, lambda_avg=lambda_avg_used, ctrl=ctrl)
      }, error = function(e) NULL)
      
      if (is.null(fit_try)) {
        score_vec[gi] <- Inf
        next
      }
      
      b0 <- fit_try$beta0
      b  <- fit_try$beta
      
      yhat <- as.numeric(b0 + Xv %*% b)
      mse <- mean((yv - yhat)^2)
      f1s <- support_f1(b, beta_true)
      
      r_val <- yv - b0 - as.numeric(Xv %*% b)
      Bv_hat <- topk_idx(r_val^2, kv)
      f1o <- outlier_f1(Bv_hat, out_val)
      
      if (criterion == "MSE") score_vec[gi] <- mse
      if (criterion == "F1_support") score_vec[gi] <- -f1s
      if (criterion == "F1_outlier") score_vec[gi] <- -f1o
      
      tune_tbl[[length(tune_tbl) + 1]] <- data.frame(
        method = m, scale = sc, lambda_used = lambda_avg_used,
        C_code = calc_C_code(lambda_avg_used, p=p, n=ntr, h=htr),
        C_paper = calc_C_paper(lambda_avg_used, p=p, h=htr),
        MSE = mse, F1_support = f1s, F1_outlier = f1o,
        stringsAsFactors = FALSE
      )
      
      if (verbose_each) cat(sprintf("[VAL tune %s] scale=%.3g score=%.4f\n", m, sc, score_vec[gi]))
    }
    
    best_scales[m] <- scale_grid[which.min(score_vec)]
    tune_time_by_method[m] <- proc.time()[["elapsed"]] - t0
  }
  
  list(
    best_scales = best_scales,
    tune_time_by_method = tune_time_by_method,
    tune_table = if (length(tune_tbl)>0) do.call(rbind, tune_tbl) else data.frame()
  )
}

############################################################
## 10B) Tuning strategy: PLUGIN (robust scale on LTS subset residuals)
############################################################
tune_scales_plugin_per_method <- function(
    sim_full, methods,
    alpha = 0.75, c_lambda = 1.0,
    standardize_for_fair = TRUE,
    ctrl_by_method = list(),
    c0 = 1.0,
    init_scale = 1.0,
    n_refit = 1
) {
  Xtr <- sim_full$X; ytr <- sim_full$y
  ntr <- nrow(Xtr); p <- ncol(Xtr)
  htr <- max(1, min(ntr, floor(alpha * ntr)))
  
  if (standardize_for_fair) {
    std <- standardize_train_test(Xtr, Xtr)
    Xtr <- std$Xtr
  } else {
    Xtr <- as.matrix(Xtr)
  }
  
  best_scales <- setNames(rep(NA_real_, length(methods)), methods)
  tune_time_by_method <- setNames(rep(0, length(methods)), methods)
  tune_tbl <- list()
  
  lambda0 <- lambda_base_avg(n=ntr, p=p, h=htr, c_lambda=c_lambda)
  
  for (m in methods) {
    t0 <- proc.time()[["elapsed"]]
    ctrl <- ctrl_by_method[[m]]; if (is.null(ctrl)) ctrl <- list()
    
    sc <- init_scale
    sigma_hat_last <- NA_real_
    
    for (rr in 1:(1 + n_refit)) {
      fit <- fit_one_method(m, Xtr, ytr, alpha=alpha, lambda_avg=lambda0*sc, ctrl=ctrl)
      
      r <- ytr - fit$beta0 - as.numeric(Xtr %*% fit$beta)
      idx_good <- bottomk_idx(r^2, htr)
      sigma_hat <- robust_sigma_mad(r[idx_good])
      sigma_hat <- max(sigma_hat, 1e-8)
      
      sigma_hat_last <- sigma_hat
      sc <- c0 * sigma_hat
    }
    
    best_scales[m] <- sc
    tune_time_by_method[m] <- proc.time()[["elapsed"]] - t0
    
    tune_tbl[[length(tune_tbl)+1]] <- data.frame(
      method=m,
      scale=best_scales[m],
      sigma_hat=sigma_hat_last,
      lambda_used=lambda0*best_scales[m],
      C_code=calc_C_code(lambda0*best_scales[m], p=p, n=ntr, h=htr),
      C_paper=calc_C_paper(lambda0*best_scales[m], p=p, h=htr),
      stringsAsFactors=FALSE
    )
  }
  
  list(
    best_scales = best_scales,
    tune_time_by_method = tune_time_by_method,
    tune_table = do.call(rbind, tune_tbl)
  )
}

############################################################
## 10C) Tuning strategy: STAB (instability minimization)
############################################################
tune_scales_stability_per_method <- function(
    sim_full, methods,
    scale_grid = 10^seq(-2, 2, by = 0.5),
    alpha = 0.75, c_lambda = 1.0,
    standardize_for_fair = TRUE,
    ctrl_by_method = list(),
    B = 30,
    subsample_frac = 0.5,
    q_max = NULL,
    eps = 1e-8,
    seed = 1,
    verbose_each = FALSE
) {
  set.seed(seed)
  X <- sim_full$X; y <- sim_full$y
  n <- nrow(X); p <- ncol(X)
  h <- max(1, min(n, floor(alpha * n)))
  
  if (standardize_for_fair) {
    std <- standardize_train_test(X, X)
    X <- std$Xtr
  } else {
    X <- as.matrix(X)
  }
  
  best_scales <- setNames(rep(NA_real_, length(methods)), methods)
  tune_time_by_method <- setNames(rep(0, length(methods)), methods)
  tune_tbl <- list()
  
  lambda0 <- lambda_base_avg(n=n, p=p, h=h, c_lambda=c_lambda)
  
  for (m in methods) {
    t0 <- proc.time()[["elapsed"]]
    ctrl <- ctrl_by_method[[m]]; if (is.null(ctrl)) ctrl <- list()
    
    instab_vec <- rep(Inf, length(scale_grid))
    
    for (gi in seq_along(scale_grid)) {
      sc <- scale_grid[gi]
      lambda_avg_used <- lambda0 * sc
      
      sel_count <- integer(p)
      q_list <- integer(B)
      
      for (b in 1:B) {
        idx <- sample.int(n, size = max(2, floor(subsample_frac * n)), replace = FALSE)
        fit <- tryCatch(
          fit_one_method(m, X[idx,,drop=FALSE], y[idx], alpha=alpha, lambda_avg=lambda_avg_used, ctrl=ctrl),
          error = function(e) NULL
        )
        if (is.null(fit)) next
        
        S <- support_idx(fit$beta, eps=eps)
        if (length(S) > 0) sel_count[S] <- sel_count[S] + 1L
        q_list[b] <- length(S)
      }
      
      pi_hat <- sel_count / B
      instab <- mean(2 * pi_hat * (1 - pi_hat))
      qbar <- mean(q_list)
      
      if (!is.null(q_max) && is.finite(q_max) && qbar > q_max) instab <- Inf
      
      instab_vec[gi] <- instab
      
      tune_tbl[[length(tune_tbl)+1]] <- data.frame(
        method=m,
        scale=sc,
        lambda_used=lambda_avg_used,
        C_code=calc_C_code(lambda_avg_used, p=p, n=n, h=h),
        C_paper=calc_C_paper(lambda_avg_used, p=p, h=h),
        instab=instab,
        qbar=qbar,
        stringsAsFactors=FALSE
      )
      
      if (verbose_each) cat(sprintf("[STAB tune %s] scale=%.3g instab=%.4f qbar=%.1f\n", m, sc, instab, qbar))
    }
    
    best_scales[m] <- scale_grid[which.min(instab_vec)]
    tune_time_by_method[m] <- proc.time()[["elapsed"]] - t0
  }
  
  list(
    best_scales = best_scales,
    tune_time_by_method = tune_time_by_method,
    tune_table = if (length(tune_tbl)>0) do.call(rbind, tune_tbl) else data.frame()
  )
}

############################################################
## 10D) Tuning strategy: EBIC (train-only, trimmed SSE)
############################################################
ebic_lts <- function(X, y, b0, b, h, gamma = 0.5, eps = 1e-12) {
  X <- as.matrix(X); y <- as.numeric(y)
  n <- nrow(X); p <- ncol(X)
  
  r <- y - b0 - as.numeric(X %*% b)
  sse <- trimmed_sse(r^2, h)
  sse <- max(sse, eps)
  
  s <- length(support_idx(b))
  h * log(sse / h) + s * log(h) + 2 * gamma * s * log(p)
}

tune_scales_ebic_per_method <- function(
    sim_full, methods,
    scale_grid = 10^seq(-2, 2, by = 0.5),
    alpha = 0.75, c_lambda = 1.0,
    standardize_for_fair = TRUE,
    ctrl_by_method = list(),
    gamma = 0.5,
    verbose_each = FALSE
) {
  Xtr <- sim_full$X; ytr <- sim_full$y
  ntr <- nrow(Xtr); p <- ncol(Xtr)
  htr <- max(1, min(ntr, floor(alpha * ntr)))
  
  if (standardize_for_fair) {
    std <- standardize_train_test(Xtr, Xtr)
    Xtr <- std$Xtr
  } else {
    Xtr <- as.matrix(Xtr)
  }
  
  best_scales <- setNames(rep(NA_real_, length(methods)), methods)
  tune_time_by_method <- setNames(rep(0, length(methods)), methods)
  tune_tbl <- list()
  
  lambda0 <- lambda_base_avg(n=ntr, p=p, h=htr, c_lambda=c_lambda)
  
  for (m in methods) {
    t0 <- proc.time()[["elapsed"]]
    ctrl <- ctrl_by_method[[m]]; if (is.null(ctrl)) ctrl <- list()
    
    scores <- rep(Inf, length(scale_grid))
    
    for (gi in seq_along(scale_grid)) {
      sc <- scale_grid[gi]
      lambda_avg_used <- lambda0 * sc
      
      fit_try <- tryCatch(
        fit_one_method(m, Xtr, ytr, alpha=alpha, lambda_avg=lambda_avg_used, ctrl=ctrl),
        error = function(e) NULL
      )
      if (is.null(fit_try)) {
        scores[gi] <- Inf
        next
      }
      
      scores[gi] <- ebic_lts(Xtr, ytr, fit_try$beta0, fit_try$beta, h=htr, gamma=gamma)
      
      tune_tbl[[length(tune_tbl)+1]] <- data.frame(
        method=m,
        scale=sc,
        lambda_used=lambda_avg_used,
        C_code=calc_C_code(lambda_avg_used, p=p, n=ntr, h=htr),
        C_paper=calc_C_paper(lambda_avg_used, p=p, h=htr),
        EBIC=scores[gi],
        gamma=gamma,
        stringsAsFactors=FALSE
      )
      
      if (verbose_each) cat(sprintf("[EBIC tune %s] scale=%.3g EBIC=%.4f\n", m, sc, scores[gi]))
    }
    
    best_scales[m] <- scale_grid[which.min(scores)]
    tune_time_by_method[m] <- proc.time()[["elapsed"]] - t0
  }
  
  list(
    best_scales = best_scales,
    tune_time_by_method = tune_time_by_method,
    tune_table = if (length(tune_tbl)>0) do.call(rbind, tune_tbl) else data.frame()
  )
}

############################################################
## 10E) Dispatcher for tuning strategies
############################################################
tune_scales_dispatch <- function(
    tune_strategy = c("VAL","PLUGIN","STAB","EBIC"),
    sim_full,
    methods,
    alpha = 0.75,
    c_lambda = 1.0,
    standardize_for_fair = TRUE,
    ctrl_by_method = list(),
    # shared grid
    scale_grid = 10^seq(-2, 2, by = 0.5),
    # VAL
    val_frac = 0.25,
    tune_seed = 123,
    criterion = "MSE",
    # PLUGIN
    c0 = 1.0, init_scale = 1.0, n_refit = 1,
    # STAB
    B = 30, subsample_frac = 0.5, q_max = NULL, stab_seed = 1,
    # EBIC
    gamma = 0.5
) {
  tune_strategy <- match.arg(tune_strategy)
  
  if (tune_strategy == "VAL") {
    return(tune_scales_val_per_method(
      sim_full=sim_full, methods=methods,
      scale_grid=scale_grid, val_frac=val_frac, tune_seed=tune_seed,
      alpha=alpha, c_lambda=c_lambda, standardize_for_fair=standardize_for_fair,
      ctrl_by_method=ctrl_by_method, criterion=criterion
    ))
  }
  if (tune_strategy == "PLUGIN") {
    return(tune_scales_plugin_per_method(
      sim_full=sim_full, methods=methods,
      alpha=alpha, c_lambda=c_lambda, standardize_for_fair=standardize_for_fair,
      ctrl_by_method=ctrl_by_method, c0=c0, init_scale=init_scale, n_refit=n_refit
    ))
  }
  if (tune_strategy == "STAB") {
    return(tune_scales_stability_per_method(
      sim_full=sim_full, methods=methods,
      scale_grid=scale_grid,
      alpha=alpha, c_lambda=c_lambda, standardize_for_fair=standardize_for_fair,
      ctrl_by_method=ctrl_by_method,
      B=B, subsample_frac=subsample_frac, q_max=q_max, seed=stab_seed
    ))
  }
  if (tune_strategy == "EBIC") {
    return(tune_scales_ebic_per_method(
      sim_full=sim_full, methods=methods,
      scale_grid=scale_grid,
      alpha=alpha, c_lambda=c_lambda, standardize_for_fair=standardize_for_fair,
      ctrl_by_method=ctrl_by_method, gamma=gamma
    ))
  }
  stop("Unknown tune_strategy")
}

############################################################
## 11) n-grid experiment (multi tuning strategies)
############################################################
run_n_experiment_multi_tune <- function(
    n_grid = c(200, 500, 1000, 2000),
    R_rep = 3,
    methods_to_run,
    tune_strategies = c("VAL","PLUGIN","STAB","EBIC"),
    # data gen params
    p = 100, s = 10, sigma = 1.0, eps = 0.1,
    outlier_shift_y = 8, leverage_shift_x = 5, n_test = 200,
    beta_gen = "uniform",
    beta_unif_min = 1.0,
    beta_unif_max = 3.0,
    beta_random_sign = TRUE,
    # benchmark params
    alpha = 0.75, c_lambda = 1.0, standardize_for_fair = TRUE,
    # tuning params
    scale_grid = 10^seq(-2, 2, by = 0.5),
    val_frac = 0.25,
    criterion = "MSE",
    ctrl_by_method = list(),
    # PLUGIN
    plugin_c0 = 1.0, plugin_init_scale = 1.0, plugin_n_refit = 1,
    # STAB
    stab_B = 30, stab_frac = 0.5, stab_q_max = NULL,
    # EBIC
    ebic_gamma = 0.5,
    seed_base = 1000,
    verbose_each_iter = TRUE,
    parallel = TRUE,
    future_plan = "multisession",
    workers = NULL
) {
  task_grid <- expand.grid(
    n = n_grid,
    rep = seq_len(R_rep),
    stringsAsFactors = FALSE
  )
  
  if (parallel) {
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    if (is.null(workers)) {
      future::plan(future_plan)
    } else {
      future::plan(future_plan, workers = workers)
    }
    doFuture::registerDoFuture()
  }
  
  run_one_task <- function(ni, rr) {
    seed_i <- seed_base + 10 * ni + rr
    
    if (verbose_each_iter) {
      cat("\n====================================================\n")
      cat(sprintf("[n-experiment] n=%d | rep=%d/%d | seed=%d\n", ni, rr, R_rep, seed_i))
      cat("====================================================\n")
    }
    
    sim <- simulate_contaminated_lm(
      n = ni, p = p, s = s,
      sigma = sigma, eps = eps,
      outlier_shift_y = outlier_shift_y,
      leverage_shift_x = leverage_shift_x,
      n_test = n_test,
      seed = seed_i,
      beta_gen = beta_gen,
      beta_unif_min = beta_unif_min,
      beta_unif_max = beta_unif_max,
      beta_random_sign = beta_random_sign
    )
    
    rows_list <- list()
    tune_rows <- list()
    row_idx <- 1
    
    for (ts in tune_strategies) {
      tuned <- tune_scales_dispatch(
        tune_strategy = ts,
        sim_full = sim,
        methods = methods_to_run,
        alpha = alpha,
        c_lambda = c_lambda,
        standardize_for_fair = standardize_for_fair,
        ctrl_by_method = ctrl_by_method,
        scale_grid = scale_grid,
        val_frac = val_frac,
        tune_seed = seed_i + 999,
        criterion = criterion,
        c0 = plugin_c0,
        init_scale = plugin_init_scale,
        n_refit = plugin_n_refit,
        B = stab_B,
        subsample_frac = stab_frac,
        q_max = stab_q_max,
        stab_seed = seed_i + 777,
        gamma = ebic_gamma
      )
      
      best_scales <- tuned$best_scales
      tune_time_by_method <- tuned$tune_time_by_method
      tt <- if (!is.null(tuned$tune_table)) tuned$tune_table else data.frame()
      
      if (nrow(tt) > 0) {
        tt$n <- ni; tt$rep <- rr; tt$seed <- seed_i; tt$tune_strategy <- ts
        tune_rows[[length(tune_rows) + 1]] <- tt
      }
      
      tbl <- benchmark_methods(
        sim,
        methods = methods_to_run,
        alpha = alpha,
        c_lambda = c_lambda,
        scale_by_method = best_scales,
        tune_time_by_method = tune_time_by_method,
        standardize_for_fair = standardize_for_fair,
        ctrl_by_method = ctrl_by_method,
        verbose = FALSE
      )
      
      tbl$n <- ni
      tbl$rep <- rr
      tbl$seed <- seed_i
      tbl$tune_strategy <- ts
      
      if (verbose_each_iter) {
        print(tbl[, c("tune_strategy","method","fit_time","tune_time","total_time",
                      "MSE","F1_support","F1_outlier","lambda_used","scale_used","C_code")])
      }
      
      rows_list[[row_idx]] <- tbl
      row_idx <- row_idx + 1
    }
    
    list(
      res_all = do.call(rbind, rows_list),
      tune_rows = if (length(tune_rows) > 0) do.call(rbind, tune_rows) else NULL
    )
  }
  
  if (parallel) {
    results <- foreach(i = seq_len(nrow(task_grid)), .packages = pkgs) %dofuture% {
      run_one_task(task_grid$n[i], task_grid$rep[i])
    }
  } else {
    results <- lapply(seq_len(nrow(task_grid)), function(i) {
      run_one_task(task_grid$n[i], task_grid$rep[i])
    })
  }
  
  res_all <- do.call(rbind, lapply(results, `[[`, "res_all"))
  tune_tables <- lapply(results, `[[`, "tune_rows")
  tune_tables <- tune_tables[!vapply(tune_tables, is.null, logical(1))]
  
  res_mean <- aggregate(
    cbind(fit_time, tune_time, total_time, MSE, F1_support, F1_outlier) ~ n + method + tune_strategy,
    data = res_all,
    FUN = mean
  )
  
  out <- list(res_all = res_all, res_mean = res_mean)
  if (length(tune_tables) > 0) out$tune_table_all <- do.call(rbind, tune_tables)
  out
}

############################################################
## 12) Plot (n vs metrics) with tune_strategy facet
############################################################
plot_metrics_vs_n <- function(res_mean) {
  res_mean$n <- as.numeric(res_mean$n)
  
  p_elapsed <- ggplot(res_mean, aes(x = n, y = total_time, color = method)) +
    geom_line() + geom_point() +
    labs(title = "Total time vs n", x = "n", y = "Total time (sec)", color = "Method") +
    theme_minimal() + scale_y_log10() +
    facet_wrap(~ tune_strategy)
  
  p_mse <- ggplot(res_mean, aes(x = n, y = MSE, color = method)) +
    geom_line() + geom_point() +
    labs(title = "Test MSE vs n", x = "n", y = "MSE", color = "Method") +
    theme_minimal() +
    facet_wrap(~ tune_strategy)
  
  p_f1s <- ggplot(res_mean, aes(x = n, y = F1_support, color = method)) +
    geom_line() + geom_point() +
    coord_cartesian(ylim = c(0, 1)) +
    labs(title = "F1_support vs n", x = "n", y = "F1_support", color = "Method") +
    theme_minimal() +
    facet_wrap(~ tune_strategy)
  
  p_f1o <- ggplot(res_mean, aes(x = n, y = F1_outlier, color = method)) +
    geom_line() + geom_point() +
    coord_cartesian(ylim = c(0, 1)) +
    labs(title = "F1_outlier vs n", x = "n", y = "F1_outlier", color = "Method") +
    theme_minimal() +
    facet_wrap(~ tune_strategy)
  
  (p_elapsed + p_mse) / (p_f1s + p_f1o)
}

############################################################
## 13) Example run
############################################################
methods_to_run <- c(
  "robustHD::sparseLTS",
  "SWLTS-PALM",
  "DCA", "DCA_fast", "BDCA_fast",
  "Yang_sum",
  "iYang_BB",
  "Yagishita_orig",
  "Yagishita_fast"
)

ctrl_by_method <- list(
  "SWLTS-PALM" = list(max_iter = 300, final_refit = TRUE, verbose = FALSE),
  "DCA" = list(max_outer = 40, inner_iter = 60, tol = 1e-4, verbose = FALSE),
  "DCA_fast" = list(max_outer = 40, kkt_tol = 1e-4, inner_max_iter = 150, verbose = FALSE),
  "BDCA_fast" = list(max_outer = 40, kkt_tol = 1e-4, inner_max_iter = 150, verbose = FALSE,
                     ls_lambda_init = 2, ls_lambda_cap = 32, ls_rho = 1e-6),
  "Yang_sum" = list(max_iter = 2000, tol = 1e-6, stable_H_K = 5, verbose = FALSE),
  "iYang_BB" = list(max_iter = 2000, tol = 1e-6, stable_H_K = 5, mom = 0.35, verbose = FALSE),
  "Yagishita_orig" = list(max_iter = 2000, tol = 1e-6, stable_I_K = 5, verbose = FALSE),
  "Yagishita_fast" = list(max_iter = 2000, tol = 1e-6, stable_I_K = 5, verbose = FALSE)
)

# Run
out <- run_n_experiment_multi_tune(
  n_grid = c(500, 1000, 2000),
  R_rep = 3,
  methods_to_run = methods_to_run,
  tune_strategies = c("VAL","PLUGIN","EBIC","STAB"),
  p = 10, s = 5,
  eps = 0.1,
  alpha = 0.8,
  c_lambda = 1.0,
  scale_grid = 10^seq(-2, 2, by = 0.5),
  criterion = "MSE",
  standardize_for_fair = TRUE,
  ctrl_by_method = ctrl_by_method,
  beta_gen = "uniform",
  beta_unif_min = -1.0,
  beta_unif_max = 1.0,
  beta_random_sign = TRUE,
  # tuning hyper
  plugin_c0 = 1.0, plugin_init_scale = 1.0, plugin_n_refit = 1,
  stab_B = 30, stab_frac = 0.5, stab_q_max = 2*10,
  ebic_gamma = 0.5,
  verbose_each_iter = TRUE,
  parallel = TRUE,
  future_plan = "multisession",
  workers = NULL
)

print(out$res_mean)
plot_metrics_vs_n(out$res_mean)
