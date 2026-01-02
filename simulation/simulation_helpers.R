# Simulation helper definitions
#
# 이 파일은 시뮬레이션 관련 함수 정의를 모아두는 곳입니다.
# - simulate_contaminated_lm(): 데이터 생성
# - tune_scales_per_method(): 스케일(람다 계수) 튜닝
# - benchmark_methods(): 메트릭 계산

allowed_methods <- c(
  "robustHD::sparseLTS",
  "SWLTS-PALM",
  "DCA-TopK-LTS",
  "Yang",
  "iYang",
  "Yag-orig",
  "Yag-fast",
  "DCA",
  "DCA_fast",
  "BDCA_fast"
)

validate_methods <- function(methods) {
  unknown <- setdiff(methods, allowed_methods)
  if (length(unknown) > 0) {
    stop(sprintf(
      "Unknown method(s): %s\nAllowed: %s",
      paste(unknown, collapse = ", "),
      paste(allowed_methods, collapse = ", ")
    ))
  }
}

resolve_method_fitter <- function(method, method_fitters) {
  if (!is.null(method_fitters) && !is.null(method_fitters[[method]])) {
    return(method_fitters[[method]])
  }
  NULL
}

f1_score <- function(truth_idx, pred_idx, n_total) {
  truth <- rep(FALSE, n_total)
  pred <- rep(FALSE, n_total)
  truth[truth_idx] <- TRUE
  pred[pred_idx] <- TRUE

  tp <- sum(truth & pred)
  fp <- sum(!truth & pred)
  fn <- sum(truth & !pred)
  if (tp == 0 && fp == 0 && fn == 0) return(1)
  if (tp == 0) return(0)
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  if (precision + recall == 0) return(0)
  2 * precision * recall / (precision + recall)
}

standardize_matrix <- function(X, center = TRUE, scale = TRUE) {
  mu <- if (center) colMeans(X) else rep(0, ncol(X))
  sd <- if (scale) apply(X, 2, sd) else rep(1, ncol(X))
  sd[sd == 0] <- 1
  Xs <- sweep(X, 2, mu, "-")
  Xs <- sweep(Xs, 2, sd, "/")
  list(X = Xs, mu = mu, sd = sd)
}

simulate_contaminated_lm <- function(n, p, s,
                                     sigma = 1.0, eps = 0.1,
                                     outlier_shift_y = 8,
                                     leverage_shift_x = 5,
                                     n_test = 200,
                                     seed = 1) {
  set.seed(seed)

  beta_true <- c(rep(2, s), rep(0, p - s))
  X <- matrix(rnorm(n * p), n, p)
  y <- as.numeric(X %*% beta_true + rnorm(n, sd = sigma))

  n_out <- max(0, floor(eps * n))
  outlier_idx <- if (n_out > 0) sample(seq_len(n), n_out) else integer(0)

  if (length(outlier_idx) > 0) {
    y[outlier_idx] <- y[outlier_idx] + outlier_shift_y
    X[outlier_idx, 1:s] <- X[outlier_idx, 1:s] + leverage_shift_x
  }

  X_test <- matrix(rnorm(n_test * p), n_test, p)
  y_test <- as.numeric(X_test %*% beta_true + rnorm(n_test, sd = sigma))

  list(
    X = X,
    y = y,
    X_test = X_test,
    y_test = y_test,
    beta_true = beta_true,
    support_idx = which(beta_true != 0),
    outlier_idx = outlier_idx,
    n = n,
    p = p,
    s = s
  )
}

fit_method <- function(method, X, y, alpha, lambda, verbose = FALSE, method_fitters = NULL) {
  fitter <- resolve_method_fitter(method, method_fitters)
  if (!is.null(fitter)) {
    return(fitter(X, y, alpha = alpha, lambda = lambda, verbose = verbose))
  }
  if (method == "DCA") {
    return(dca_sparse_lts_lasso(X, y, alpha = alpha, lambda = lambda, verbose = verbose))
  }
  if (method == "DCA_fast") {
    return(dca_sparse_lts_lasso_fast(X, y, alpha = alpha, lambda = lambda, verbose = verbose))
  }
  if (method == "BDCA_fast") {
    return(bdca_sparse_lts_lasso_fast(X, y, alpha = alpha, lambda = lambda, verbose = verbose))
  }
  stop(sprintf(
    "Unknown method: %s. Provide a matching entry in method_fitters or use built-in DCA variants.",
    method
  ))
}

extract_predictions <- function(fit, X) {
  drop(fit$beta0 + X %*% fit$beta)
}

benchmark_methods <- function(sim_full,
                              methods,
                              alpha = 0.75,
                              c_lambda = 1.0,
                              scale_by_method = NULL,
                              standardize_for_fair = TRUE,
                              seed = 1,
                              verbose = FALSE,
                              method_fitters = NULL) {
  set.seed(seed)
  validate_methods(methods)

  X <- sim_full$X
  y <- sim_full$y
  X_test <- sim_full$X_test
  y_test <- sim_full$y_test
  support_idx <- sim_full$support_idx
  outlier_idx <- sim_full$outlier_idx

  if (standardize_for_fair) {
    std <- standardize_matrix(X)
    X <- std$X
    X_test <- sweep(X_test, 2, std$mu, "-")
    X_test <- sweep(X_test, 2, std$sd, "/")
  }

  res <- list()
  for (m in methods) {
    scale <- if (!is.null(scale_by_method)) scale_by_method[[m]] else 1
    lambda <- c_lambda * scale

    start_time <- Sys.time()
    fit <- fit_method(
      m,
      X,
      y,
      alpha = alpha,
      lambda = lambda,
      verbose = verbose,
      method_fitters = method_fitters
    )
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

    pred_test <- extract_predictions(fit, X_test)
    mse <- mean((y_test - pred_test)^2)

    pred_support <- which(fit$beta != 0)
    f1_support <- f1_score(support_idx, pred_support, length(fit$beta))

    pred_outliers <- fit$outlier_set
    f1_outlier <- f1_score(outlier_idx, pred_outliers, nrow(X))

    res[[m]] <- data.frame(
      method = m,
      elapsed = elapsed,
      MSE = mse,
      F1_support = f1_support,
      F1_outlier = f1_outlier,
      lambda_used = lambda,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, res)
}

tune_scales_per_method <- function(sim_full,
                                   methods,
                                   scale_grid,
                                   val_frac = 0.25,
                                   tune_seed = 1,
                                   alpha = 0.75,
                                   c_lambda = 1.0,
                                   standardize_for_fair = TRUE,
                                   seed = 1,
                                   criterion = "MSE",
                                   verbose_each = FALSE,
                                   method_fitters = NULL) {
  set.seed(seed)
  validate_methods(methods)
  X <- sim_full$X
  y <- sim_full$y
  n <- nrow(X)

  set.seed(tune_seed)
  val_size <- max(1, floor(val_frac * n))
  val_idx <- sample(seq_len(n), val_size)
  train_idx <- setdiff(seq_len(n), val_idx)

  X_train <- X[train_idx, , drop = FALSE]
  y_train <- y[train_idx]
  X_val <- X[val_idx, , drop = FALSE]
  y_val <- y[val_idx]

  support_idx <- sim_full$support_idx
  outlier_idx <- intersect(sim_full$outlier_idx, train_idx)
  outlier_idx_train <- match(outlier_idx, train_idx)

  if (standardize_for_fair) {
    std <- standardize_matrix(X_train)
    X_train <- std$X
    X_val <- sweep(X_val, 2, std$mu, "-")
    X_val <- sweep(X_val, 2, std$sd, "/")
  }

  tune_rows <- list()
  best_scales <- setNames(rep(NA_real_, length(methods)), methods)

  for (m in methods) {
    best_value <- if (criterion == "MSE") Inf else -Inf

    for (sc in scale_grid) {
      lambda <- c_lambda * sc
      fit <- fit_method(
        m,
        X_train,
        y_train,
        alpha = alpha,
        lambda = lambda,
        verbose = verbose_each,
        method_fitters = method_fitters
      )
      pred_val <- extract_predictions(fit, X_val)
      mse <- mean((y_val - pred_val)^2)

      pred_support <- which(fit$beta != 0)
      f1_support <- f1_score(support_idx, pred_support, length(fit$beta))

      pred_outliers <- fit$outlier_set
      f1_outlier <- f1_score(outlier_idx_train, pred_outliers, nrow(X_train))

      row <- data.frame(
        method = m,
        scale = sc,
        MSE = mse,
        F1_support = f1_support,
        F1_outlier = f1_outlier,
        stringsAsFactors = FALSE
      )
      tune_rows[[length(tune_rows) + 1]] <- row

      metric_value <- row[[criterion]]
      if (criterion == "MSE") {
        if (metric_value < best_value) {
          best_value <- metric_value
          best_scales[m] <- sc
        }
      } else {
        if (metric_value > best_value) {
          best_value <- metric_value
          best_scales[m] <- sc
        }
      }
    }
  }

  list(
    best_scales = best_scales,
    tune_table = do.call(rbind, tune_rows)
  )
}
