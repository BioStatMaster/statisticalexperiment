# ============================================================
# Utilities for SLTS/STRLS simulations
#
# References:
# - Alfons et al. (2021), robustHD R package: https://CRAN.R-project.org/package=robustHD
# - Yang et al. (2013), Sparse least trimmed squares for high-dimensional data
# - Yagishita & Ogata (2020), STRLS/SLTS equivalence for sparse robust regression
# ============================================================

# soft-thresholding: proximal operator for L1 penalty
soft_threshold <- function(z, tau) sign(z) * pmax(abs(z) - tau, 0)

# pick indices of the h smallest values (partial sort when possible)
pick_h_smallest <- function(v, h) {
  n <- length(v)
  if (h < 1 || h > n) stop("h must satisfy 1 <= h <= length(v)")
  idx <- tryCatch({
    sort.int(v, partial = h, index.return = TRUE, method = "quick")$ix[1:h]
  }, error = function(e) NULL)
  if (is.null(idx)) idx <- order(v)[1:h]
  idx
}

# count non-zeros
nnz <- function(beta, eps = 1e-8) sum(abs(beta) > eps)

# standardize X column-wise
standardize_X <- function(X) {
  Xs <- scale(X)
  list(
    X = as.matrix(Xs),
    center = attr(Xs, "scaled:center"),
    scale = attr(Xs, "scaled:scale")
  )
}

# Common SLTS objective for fair comparison
# Q(beta0,beta) = sum_{h smallest} r_i^2 + h * lambda_abs * ||beta||_1
obj_slts <- function(y, X, beta0, beta, h, lambda_abs) {
  r <- as.numeric(y - (beta0 + X %*% beta))
  r2 <- r^2
  H <- pick_h_smallest(r2, h)
  sum(r2[H]) + h * lambda_abs * sum(abs(beta))
}

# Inlier set from coefficients
get_H_from_coef <- function(y, X, beta0, beta, h) {
  r <- as.numeric(y - (beta0 + X %*% beta))
  pick_h_smallest(r^2, h)
}

# Support recovery metrics
support_metrics <- function(beta_hat, beta_true, eps = 1e-6) {
  beta_hat <- as.numeric(beta_hat)
  beta_true <- as.numeric(beta_true)

  S_hat <- abs(beta_hat) > eps
  S_true <- abs(beta_true) > 0

  TP <- sum(S_hat & S_true)
  FP <- sum(S_hat & !S_true)
  FN <- sum(!S_hat & S_true)
  TN <- sum(!S_hat & !S_true)

  precision <- if ((TP + FP) == 0) 1 else TP / (TP + FP)
  recall <- if ((TP + FN) == 0) 1 else TP / (TP + FN)
  f1 <- if ((precision + recall) == 0) 0 else 2 * precision * recall / (precision + recall)

  exact_support <- as.integer(all(S_hat == S_true))

  data.frame(
    TP = TP, FP = FP, FN = FN, TN = TN,
    precision = precision,
    recall = recall,
    F1 = f1,
    exact_support = exact_support,
    s_hat = sum(S_hat),
    s_true = sum(S_true)
  )
}

# Inlier/outlier metrics when outlier indices are known
inlier_metrics <- function(H_hat, outlier_idx, n) {
  H_hat <- as.integer(H_hat)
  outlier_idx <- as.integer(outlier_idx)

  true_inliers <- setdiff(seq_len(n), outlier_idx)
  out_in_H <- sum(H_hat %in% outlier_idx)
  in_in_H <- length(H_hat) - out_in_H

  data.frame(
    outlier_in_H = out_in_H,
    outlier_rate_in_H = out_in_H / length(H_hat),
    inlier_purity = in_in_H / length(H_hat),
    inlier_coverage = sum(true_inliers %in% H_hat) / length(true_inliers)
  )
}

# Mean-squared error for coefficients
mse_beta <- function(beta_hat, beta_true) {
  mean((as.numeric(beta_hat) - as.numeric(beta_true))^2)
}
