# ============================================================
# Simulation runner for SLTS/STRLS algorithms
#
# - Separates algorithm implementations into files under algorithms/
# - Supports beta generation via Uniform(a, b)
# - Allows enabling/disabling Yagishita (slow/fast) runs
# - Plots Yang (slow/fast), Yagishita (slow/fast), robustHD
#
# References:
# - robustHD: https://CRAN.R-project.org/package=robustHD
# - Yang et al. (2013), Sparse least trimmed squares
# - Yagishita & Ogata (2020), STRLS/SLTS equivalence
# ============================================================

source("simulation/utils.R")
source("simulation/algorithms/yang.R")
source("simulation/algorithms/yagishita.R")

# ---- package checks
# Helper: ensure required package is installed.
require_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Package '%s' is required but not installed.", pkg))
  }
}

# ---- data generation
# beta_dist:
#   - "fixed": beta_value is used for non-zero entries
#   - "uniform": beta_i ~ Unif(beta_range[1], beta_range[2])
# Generate contaminated regression data with leverage + response outliers.
simulate_contaminated <- function(
    n = 120,
    p = 60,
    k = 8,
    rho = 0.5,
    epsilon = 0.1,
    shift = 5,
    sigma = 1,
    beta_dist = c("fixed", "uniform"),
    beta_value = 1,
    beta_range = c(0.5, 1.5),
    seed = 1
) {
  beta_dist <- match.arg(beta_dist)
  set.seed(seed)

  require_pkg("mvtnorm")
  Sigma <- rho ^ abs(outer(1:p, 1:p, "-"))
  X <- mvtnorm::rmvnorm(n, sigma = Sigma)

  if (k < 1 || k > p) stop("k must satisfy 1 <= k <= p")

  if (beta_dist == "fixed") {
    beta_true <- c(rep(beta_value, k), rep(0, p - k))
  } else {
    if (length(beta_range) != 2 || beta_range[1] > beta_range[2]) {
      stop("beta_range must be length-2 with beta_range[1] <= beta_range[2]")
    }
    beta_true <- c(
      stats::runif(k, min = beta_range[1], max = beta_range[2]),
      rep(0, p - k)
    )
  }

  e <- rnorm(n, sd = sigma)
  m <- ceiling(epsilon * n)
  out_idx <- sample(1:n, m)

  # y outliers + X leverage outliers
  e[out_idx] <- e[out_idx] + shift
  y <- as.numeric(X %*% beta_true + e)
  X[out_idx, ] <- X[out_idx, ] + shift

  list(X = X, y = y, beta_true = beta_true, outlier_idx = out_idx)
}

# ---- robustHD wrapper (sparseLTS)
# Fit robustHD::sparseLTS with lambda_abs on SLTS scale.
fit_sparseLTS_abs <- function(
    X, y, alpha_frac, lambda_abs,
    normalize = FALSE,
    intercept = TRUE,
    nsamp = c(200, 5),
    ncstep = 2,
    initial = "sparse",
    seed = 1,
    ncores = 1
) {
  require_pkg("robustHD")
  robustHD::sparseLTS(
    x = X,
    y = y,
    alpha = alpha_frac,
    lambda = lambda_abs,
    mode = "lambda",
    normalize = normalize,
    intercept = intercept,
    nsamp = nsamp,
    ncstep = ncstep,
    initial = initial,
    ncores = ncores,
    seed = seed,
    model = FALSE
  )
}

# Extract coefficients from sparseLTS fit.
extract_sparseLTS_coef <- function(fit, intercept = TRUE) {
  raw <- NULL
  if (!is.null(fit$raw.coefficients)) raw <- fit$raw.coefficients
  if (is.null(raw) && !is.null(fit$coefficients)) raw <- fit$coefficients
  if (is.null(raw)) raw <- as.numeric(stats::coef(fit))

  raw <- as.numeric(raw)
  if (intercept) {
    list(beta0 = raw[1], beta = raw[-1])
  } else {
    list(beta0 = 0, beta = raw)
  }
}

# ---- single-lambda comparison (all methods)
# Run Yang/Yagishita/robustHD on a single lambda_abs and collect metrics.
compare_one_lambda_all <- function(
    X, y,
    beta_true = NULL,
    outlier_idx = NULL,
    lambda_abs,
    alpha_frac = 0.8,
    standardize = TRUE,
    support_eps = 1e-6,
    seed = 1,
    include_yang_slow = TRUE,
    include_yagishita = TRUE,
    yang_multistart = list(
      enable = FALSE,
      n_start = 5,
      top_k = 2,
      short_ctrl = list(max_iter = 80, min_iter = 10, tol = 1e-3),
      refine_ctrl = list(max_iter = 800, min_iter = 30, tol = 1e-6)
    ),
    yang_init = "lasso",
    yang_ctrl = list(
      max_iter = 2000,
      tol = 1e-6,
      min_iter = 30,
      stable_H_K = 5,
      eta_init = 1.0,
      backtrack = 0.5,
      ls_max = 50,
      use_support_stop = FALSE,
      stable_S_K = 5
    ),
    yagishita_ctrl = list(
      init = "random",
      subsample_size = NULL,
      c1 = 2,
      c2 = 1e-4,
      eta_min = 1e-10,
      eta_max = 1e10,
      tmax = 1e6,
      stop_tol = 1e-6,
      preprocess = FALSE,
      keep_hist = FALSE
    ),
    robust_nsamp = c(200, 5),
    robust_ncstep = 2,
    verbose = FALSE
) {
  X <- as.matrix(X)
  y <- as.numeric(y)
  n <- nrow(X)
  h <- floor(alpha_frac * n)

  beta_true_eval <- beta_true

  if (standardize) {
    st <- standardize_X(X)
    X <- st$X
    if (!is.null(beta_true)) {
      beta_true_eval <- as.numeric(beta_true) * as.numeric(st$scale)
    }
  }

  results <- list()

  # ---- Yang (multistart + Top-K refinement)
  # Multi-start is designed to be cheap:
  #  1) run short iterations for N starts
  #  2) select Top-K by objective
  #  3) refine only Top-K with longer iterations
  # This pattern improves robustness without heavy cost.
  run_yang_once <- function(seed_use, ctrl, fast = TRUE) {
    fn <- if (fast) yang_alg1_slts_fast else yang_alg1_slts_slow
    base_args <- list(
      X = X,
      y = y,
      h = h,
      lambda_abs = lambda_abs,
      intercept = TRUE,
      init = yang_init,
      seed = seed_use,
      verbose = verbose
    )
    if (fast) base_args$support_eps <- support_eps
    do.call(fn, c(base_args, ctrl))
  }

  run_yang_multistart <- function(fast = TRUE) {
    if (!isTRUE(yang_multistart$enable)) return(NULL)

    n_start <- max(1L, as.integer(yang_multistart$n_start))
    top_k <- max(1L, min(n_start, as.integer(yang_multistart$top_k)))
    short_ctrl <- modifyList(yang_ctrl, yang_multistart$short_ctrl)
    refine_ctrl <- modifyList(yang_ctrl, yang_multistart$refine_ctrl)

    # Step 1: cheap screening
    short_fits <- vector("list", n_start)
    short_obj <- numeric(n_start)
    for (i in seq_len(n_start)) {
      fit <- run_yang_once(seed_use = seed + i, ctrl = short_ctrl, fast = fast)
      short_fits[[i]] <- fit
      short_obj[i] <- tail(fit$obj_hist, 1)
    }

    # Step 2: Top-K re-ranking
    top_idx <- order(short_obj)[seq_len(top_k)]

    # Step 3: refine Top-K
    refine_fits <- vector("list", top_k)
    refine_obj <- numeric(top_k)
    for (j in seq_len(top_k)) {
      fit <- run_yang_once(seed_use = seed + top_idx[j], ctrl = refine_ctrl, fast = fast)
      refine_fits[[j]] <- fit
      refine_obj[j] <- tail(fit$obj_hist, 1)
    }

    # Final: best by objective
    best_idx <- which.min(refine_obj)
    list(
      fit = refine_fits[[best_idx]],
      best_obj = refine_obj[best_idx],
      short_obj = short_obj,
      refine_obj = refine_obj,
      top_idx = top_idx
    )
  }

  # Yang (slow)
  if (include_yang_slow) {
    t_yang_slow <- system.time({
      if (isTRUE(yang_multistart$enable)) {
        ms <- run_yang_multistart(fast = FALSE)
        fit_yang_slow <- ms$fit
      } else {
        fit_yang_slow <- run_yang_once(seed_use = seed, ctrl = yang_ctrl, fast = FALSE)
      }
    })
    results$yang_slow <- list(fit = fit_yang_slow, time = t_yang_slow)
  }

  # Yang (fast)
  t_yang_fast <- system.time({
    if (isTRUE(yang_multistart$enable)) {
      ms <- run_yang_multistart(fast = TRUE)
      fit_yang_fast <- ms$fit
    } else {
      fit_yang_fast <- run_yang_once(seed_use = seed, ctrl = yang_ctrl, fast = TRUE)
    }
  })
  results$yang_fast <- list(fit = fit_yang_fast, time = t_yang_fast)

  # Yagishita (slow/fast) - optional
  if (include_yagishita) {
    require_pkg("glmnet")

    t_yagi_slow <- system.time({
      fit_yagi_slow <- do.call(
        yagishita_alg1_slts_slow,
        c(list(
          X = X,
          y = y,
          h = h,
          lambda_abs = lambda_abs,
          intercept = TRUE,
          seed = seed,
          verbose = verbose
        ), yagishita_ctrl)
      )
    })
    results$yagi_slow <- list(fit = fit_yagi_slow, time = t_yagi_slow)

    t_yagi_fast <- system.time({
      fit_yagi_fast <- do.call(
        yagishita_alg1_slts_fast,
        c(list(
          X = X,
          y = y,
          h = h,
          lambda_abs = lambda_abs,
          intercept = TRUE,
          seed = seed,
          verbose = verbose
        ), yagishita_ctrl)
      )
    })
    results$yagi_fast <- list(fit = fit_yagi_fast, time = t_yagi_fast)
  }

  # robustHD::sparseLTS
  t_lts <- system.time({
    fit_lts <- fit_sparseLTS_abs(
      X,
      y,
      alpha_frac = alpha_frac,
      lambda_abs = lambda_abs,
      normalize = FALSE,
      intercept = TRUE,
      nsamp = robust_nsamp,
      ncstep = robust_ncstep,
      seed = seed,
      ncores = 1
    )
  })
  cf_lts <- extract_sparseLTS_coef(fit_lts, intercept = TRUE)

  # ---- helper to build table rows
  make_row <- function(method, beta0, beta, elapsed, iter) {
    obj <- obj_slts(y, X, beta0, beta, h, lambda_abs)
    Hhat <- get_H_from_coef(y, X, beta0, beta, h)

    if (!is.null(beta_true_eval)) {
      sup <- support_metrics(beta, beta_true_eval, eps = support_eps)
    } else {
      sup <- data.frame(
        TP = NA,
        FP = NA,
        FN = NA,
        TN = NA,
        precision = NA,
        recall = NA,
        F1 = NA,
        exact_support = NA,
        s_hat = nnz(beta),
        s_true = NA
      )
    }

    if (!is.null(outlier_idx)) {
      inl <- inlier_metrics(Hhat, outlier_idx, n = n)
    } else {
      inl <- data.frame(
        outlier_in_H = NA,
        outlier_rate_in_H = NA,
        inlier_purity = NA,
        inlier_coverage = NA
      )
    }

    data.frame(
      method = method,
      lambda_abs = lambda_abs,
      elapsed_sec = as.numeric(elapsed["elapsed"]),
      iter = iter,
      nnz = nnz(beta),
      obj = obj,
      sup,
      inl
    )
  }

  rows <- list()

  if (include_yang_slow) {
    rows$yang_slow <- make_row(
      "Yang (Alg1, slow)",
      results$yang_slow$fit$beta0,
      results$yang_slow$fit$beta,
      results$yang_slow$time,
      results$yang_slow$fit$iter
    )
  }

  rows$yang_fast <- make_row(
    "Yang (Alg1, fast)",
    results$yang_fast$fit$beta0,
    results$yang_fast$fit$beta,
    results$yang_fast$time,
    results$yang_fast$fit$iter
  )

  if (include_yagishita) {
    rows$yagi_slow <- make_row(
      "Yagishita (STRLS, slow)",
      results$yagi_slow$fit$beta0,
      results$yagi_slow$fit$beta,
      results$yagi_slow$time,
      results$yagi_slow$fit$iter
    )
    rows$yagi_fast <- make_row(
      "Yagishita (STRLS, fast)",
      results$yagi_fast$fit$beta0,
      results$yagi_fast$fit$beta,
      results$yagi_fast$time,
      results$yagi_fast$fit$iter
    )
  }

  rows$lts <- make_row(
    "robustHD::sparseLTS (mode=lambda)",
    cf_lts$beta0,
    cf_lts$beta,
    t_lts,
    NA_integer_
  )

  tab <- do.call(rbind, rows)

  list(
    table = tab,
    fits = list(
      yang_slow = if (include_yang_slow) results$yang_slow$fit else NULL,
      yang_fast = results$yang_fast$fit,
      yagi_slow = if (include_yagishita) results$yagi_slow$fit else NULL,
      yagi_fast = if (include_yagishita) results$yagi_fast$fit else NULL,
      sparseLTS = fit_lts
    ),
    coefs = list(
      yang_slow = if (include_yang_slow) list(beta0 = results$yang_slow$fit$beta0, beta = results$yang_slow$fit$beta) else NULL,
      yang_fast = list(beta0 = results$yang_fast$fit$beta0, beta = results$yang_fast$fit$beta),
      yagi_slow = if (include_yagishita) list(beta0 = results$yagi_slow$fit$beta0, beta = results$yagi_slow$fit$beta) else NULL,
      yagi_fast = if (include_yagishita) list(beta0 = results$yagi_fast$fit$beta0, beta = results$yagi_fast$fit$beta) else NULL,
      sparseLTS = list(beta0 = cf_lts$beta0, beta = cf_lts$beta)
    ),
    h = h
  )
}

# ---- simulation over n-grid
# Run repeated simulations over n_grid and summarize metrics per method.
run_metrics_vs_n_one_lambda <- function(
    n_grid,
    p = 100,
    R = 20,
    k_frac = 0.2,
    alpha_frac = 0.8,
    epsilon = 0.1,
    rho = 0.5,
    shift = 5,
    sigma = 1,
    beta_dist = c("fixed", "uniform"),
    beta_value = 1,
    beta_range = c(0.5, 1.5),
    lambda_abs = 0.05,
    standardize = TRUE,
    support_eps = 1e-6,
    seed0 = 123,
    include_yang_slow = TRUE,
    include_yagishita = TRUE,
    yang_multistart = list(
      enable = FALSE,
      n_start = 5,
      top_k = 2,
      short_ctrl = list(max_iter = 80, min_iter = 10, tol = 1e-3),
      refine_ctrl = list(max_iter = 800, min_iter = 30, tol = 1e-6)
    ),
    yang_init = "lasso",
    yang_ctrl = list(
      max_iter = 2000,
      tol = 1e-6,
      min_iter = 30,
      stable_H_K = 5,
      eta_init = 1.0,
      backtrack = 0.5,
      ls_max = 50,
      use_support_stop = FALSE,
      stable_S_K = 5
    ),
    yagishita_ctrl = list(
      init = "random",
      subsample_size = NULL,
      c1 = 2,
      c2 = 1e-4,
      eta_min = 1e-10,
      eta_max = 1e10,
      tmax = 1e6,
      stop_tol = 1e-6,
      preprocess = FALSE,
      keep_hist = FALSE
    ),
    robust_nsamp = c(200, 5),
    robust_ncstep = 2,
    verbose = FALSE
) {
  beta_dist <- match.arg(beta_dist)
  set.seed(seed0)

  out <- vector("list", length(n_grid) * R)
  idx <- 1L

  for (n in n_grid) {
    k <- max(1L, min(p, as.integer(floor(k_frac * p))))

    for (r in seq_len(R)) {
      seed_r <- seed0 + 100000L + 1000L * n + 10L * p + r

      dat <- simulate_contaminated(
        n = n,
        p = p,
        k = k,
        rho = rho,
        epsilon = epsilon,
        shift = shift,
        sigma = sigma,
        beta_dist = beta_dist,
        beta_value = beta_value,
        beta_range = beta_range,
        seed = seed_r
      )

      X <- dat$X
      y <- dat$y
      beta_true <- dat$beta_true

      if (standardize) {
        st <- standardize_X(X)
        X_use <- st$X
        beta_true_use <- as.numeric(beta_true) * as.numeric(st$scale)
      } else {
        X_use <- as.matrix(X)
        beta_true_use <- beta_true
      }

      res <- compare_one_lambda_all(
        X = X_use,
        y = y,
        beta_true = beta_true_use,
        outlier_idx = dat$outlier_idx,
        lambda_abs = lambda_abs,
        alpha_frac = alpha_frac,
        standardize = FALSE,
        support_eps = support_eps,
      seed = seed_r,
      include_yang_slow = include_yang_slow,
      include_yagishita = include_yagishita,
      yang_multistart = yang_multistart,
      yang_init = yang_init,
      yang_ctrl = yang_ctrl,
        yagishita_ctrl = yagishita_ctrl,
        robust_nsamp = robust_nsamp,
        robust_ncstep = robust_ncstep,
        verbose = verbose
      )

      tab <- res$table

      for (m in seq_len(nrow(tab))) {
        method <- tab$method[m]
        beta_hat <- switch(
          method,
          "Yang (Alg1, slow)" = res$coefs$yang_slow$beta,
          "Yang (Alg1, fast)" = res$coefs$yang_fast$beta,
          "Yagishita (STRLS, slow)" = res$coefs$yagi_slow$beta,
          "Yagishita (STRLS, fast)" = res$coefs$yagi_fast$beta,
          "robustHD::sparseLTS (mode=lambda)" = res$coefs$sparseLTS$beta,
          NULL
        )

        beta_mse <- if (!is.null(beta_hat)) {
          mse_beta(beta_hat, beta_true_use)
        } else {
          NA_real_
        }

        out[[idx]] <- data.frame(
          n = n,
          p = p,
          rep = r,
          method = method,
          lambda_abs = lambda_abs,
          F1 = tab$F1[m],
          exact_support = tab$exact_support[m],
          beta_mse = beta_mse,
          elapsed_sec = tab$elapsed_sec[m]
        )
        idx <- idx + 1L
      }
    }

    cat(sprintf("done n=%d (p=%d)\n", n, p))
  }

  do.call(rbind, out)
}

# Summarize a metric by n and method with mean/sd/se.
summarize_by_n_metric <- function(
    res_long,
    metric = c("F1", "exact_support", "beta_mse", "elapsed_sec")
) {
  metric <- match.arg(metric)

  mean_df <- aggregate(res_long[[metric]] ~ n + method, data = res_long,
    FUN = function(x) mean(x, na.rm = TRUE)
  )
  sd_df <- aggregate(res_long[[metric]] ~ n + method, data = res_long,
    FUN = function(x) sd(x, na.rm = TRUE)
  )
  m_df <- aggregate(res_long[[metric]] ~ n + method, data = res_long,
    FUN = function(x) sum(!is.na(x))
  )

  colnames(mean_df)[3] <- "mean"
  colnames(sd_df)[3] <- "sd"
  colnames(m_df)[3] <- "m"

  tmp <- merge(mean_df, sd_df, by = c("n", "method"))
  tmp <- merge(tmp, m_df, by = c("n", "method"))
  tmp$se <- tmp$sd / sqrt(tmp$m)
  tmp$metric <- metric
  tmp
}

# Plot metric vs n with optional error bars.
plot_metric_vs_n <- function(
    summary_df,
    error = c("se", "sd", "none"),
    logy = FALSE,
    main = NULL,
    ylab = NULL
) {
  error <- match.arg(error)

  metric_name <- unique(summary_df$metric)
  if (length(metric_name) != 1) metric_name <- "metric"

  if (is.null(main)) main <- paste0(metric_name, " vs n")
  if (is.null(ylab)) ylab <- metric_name

  methods <- unique(summary_df$method)
  x_vals <- sort(unique(summary_df$n))

  y_mean <- summary_df$mean
  y_err <- if (error == "sd") summary_df$sd else if (error == "se") summary_df$se else rep(0, nrow(summary_df))

  y_min <- min(y_mean - y_err, na.rm = TRUE)
  y_max <- max(y_mean + y_err, na.rm = TRUE)

  if (!logy) {
    plot(range(x_vals), c(y_min, y_max),
      type = "n", xlab = "n", ylab = ylab, main = main
    )
  } else {
    plot(
      range(x_vals),
      c(max(y_min, 1e-12), y_max),
      type = "n",
      xlab = "n",
      ylab = paste0(ylab, " (log scale)"),
      main = main,
      log = "y"
    )
  }

  pch_vec <- seq_along(methods)
  lty_vec <- seq_along(methods)

  for (j in seq_along(methods)) {
    mth <- methods[j]
    dfm <- summary_df[summary_df$method == mth, ]
    dfm <- dfm[order(dfm$n), ]

    lines(dfm$n, dfm$mean, lty = lty_vec[j])
    points(dfm$n, dfm$mean, pch = pch_vec[j])

    if (error != "none") {
      err <- if (error == "sd") dfm$sd else dfm$se
      arrows(
        dfm$n, dfm$mean - err,
        dfm$n, dfm$mean + err,
        angle = 90, code = 3, length = 0.05
      )
    }
  }

  legend("topright", legend = methods, lty = lty_vec, pch = pch_vec, bty = "n")
}

# ---- demo (example usage)
# NOTE: Increase n/R to match your experiment size.
if (identical(environment(), globalenv())) {
  n_grid <- c(200, 500)
  p <- 100
  R <- 3
  lambda_abs <- 0.5

  res_long <- run_metrics_vs_n_one_lambda(
    n_grid = n_grid,
    p = p,
    R = R,
    k_frac = 0.2,
    alpha_frac = 0.8,
    epsilon = 0.1,
    rho = 0.5,
    shift = 5,
    sigma = 1,
    beta_dist = "uniform",
    beta_range = c(0.5, 1.5),
    lambda_abs = lambda_abs,
    standardize = TRUE,
    support_eps = 1e-6,
    seed0 = 123,
    include_yang_slow = TRUE,
    include_yagishita = FALSE,
    yang_multistart = list(
      enable = TRUE,
      n_start = 5,
      top_k = 2,
      short_ctrl = list(max_iter = 80, min_iter = 10, tol = 1e-3),
      refine_ctrl = list(max_iter = 800, min_iter = 30, tol = 1e-6)
    ),
    yang_init = "lasso",
    verbose = FALSE
  )

  f1_sum <- summarize_by_n_metric(res_long, metric = "F1")
  plot_metric_vs_n(
    f1_sum,
    error = "se",
    logy = FALSE,
    main = sprintf("Support recovery (F1) vs n (p=%d, R=%d, lambda=%.3f)", p, R, lambda_abs),
    ylab = "F1"
  )
}
