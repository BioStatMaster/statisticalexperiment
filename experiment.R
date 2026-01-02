# n-grid experiment runner and plotting utilities
#
# This script sources algorithm and simulation helpers stored in separate files.
# - algorithms/README.R: 알고리즘 관련 정의
# - simulation/simulation_helpers.R: 시뮬레이션 관련 정의

source("algorithms/README.R")
source("simulation/simulation_helpers.R")

`%||%` <- function(x, y) {
  # NULL coalescing helper
  if (!is.null(x)) x else y
}

# Derive best scales robustly from tuning output
resolve_best_scales <- function(tuned, methods, scale_grid, criterion) {
  # 1) if tuned already provides best_scales and it looks complete, use it
  if (!is.null(tuned$best_scales)) {
    best_scales <- tuned$best_scales
    if (!is.null(names(best_scales))) {
      missing_methods <- setdiff(methods, names(best_scales))
    } else {
      missing_methods <- methods
    }

    if (length(missing_methods) == 0 && length(best_scales) > 0) {
      return(best_scales)
    }
  }

  # 2) otherwise, derive from tuning table
  if (is.null(tuned$tune_table) || nrow(tuned$tune_table) == 0) {
    # fallback: use the first scale in the grid for all methods
    fallback_scale <- scale_grid[[1]]
    return(setNames(rep(fallback_scale, length(methods)), methods))
  }

  tt <- tuned$tune_table
  if (!all(c("method", "scale", criterion) %in% colnames(tt))) {
    stop("tune_table must contain columns: method, scale, and criterion metric")
  }

  best_scales <- setNames(rep(NA_real_, length(methods)), methods)
  for (m in methods) {
    tt_m <- tt[tt$method == m, , drop = FALSE]
    if (nrow(tt_m) == 0) {
      best_scales[m] <- scale_grid[[1]]
      next
    }

    # max for F1, min for MSE
    if (criterion == "MSE") {
      best_row <- tt_m[which.min(tt_m[[criterion]]), , drop = FALSE]
    } else {
      best_row <- tt_m[which.max(tt_m[[criterion]]), , drop = FALSE]
    }

    # Ensure a single numeric scale (avoid length-zero assignment)
    best_scale <- best_row$scale
    if (length(best_scale) == 0 || is.na(best_scale)) {
      best_scales[m] <- scale_grid[[1]]
    } else {
      best_scales[m] <- as.numeric(best_scale[[1]])
    }
  }

  best_scales
}

############################################################
## (A) n-grid 실험: (옵션) n별 튜닝 + 최종평가 + 결과 저장
############################################################

run_n_experiment <- function(
  n_grid = c(200, 500, 1000, 2000),
  R_rep = 3,
  methods_to_run,
  # data gen params
  p = 100, s = 10, sigma = 1.0, eps = 0.1,
  outlier_shift_y = 8, leverage_shift_x = 5, n_test = 200,
  # benchmark params
  alpha = 0.75, c_lambda = 1.0, standardize_for_fair = TRUE,
  # tuning params
  do_tune = TRUE,
  scale_grid = 10^seq(-2, 2, by = 0.5),
  val_frac = 0.25,
  criterion = "MSE",          # "MSE" or "F1_support" or "F1_outlier"
  # if do_tune = FALSE, use this fixed scales
  fixed_scales = NULL,
  # optional custom method fitters
  method_fitters = NULL,
  # seeds / print
  seed_base = 1000,
  verbose_each_iter = TRUE
) {
  all_rows <- list()
  tune_rows <- list()
  idx <- 1

  for (ni in n_grid) {
    for (rr in 1:R_rep) {
      seed_i <- seed_base + 10 * ni + rr

      if (verbose_each_iter) {
        cat("\n====================================================\n")
        cat(sprintf("[n-experiment] n=%d | rep=%d/%d | seed=%d\n", ni, rr, R_rep, seed_i))
        cat("====================================================\n")
      }

      # 1) simulate for this n
      sim <- simulate_contaminated_lm(
        n = ni, p = p, s = s,
        sigma = sigma, eps = eps,
        outlier_shift_y = outlier_shift_y,
        leverage_shift_x = leverage_shift_x,
        n_test = n_test,
        seed = seed_i
      )

      # 2) choose scales (tune or fixed)
      if (do_tune) {
        tuned <- tune_scales_per_method(
          sim_full = sim,
          methods = methods_to_run,
          scale_grid = scale_grid,
          val_frac = val_frac,
          tune_seed = seed_i + 999,  # 튜닝 split seed
          alpha = alpha,
          c_lambda = c_lambda,
          standardize_for_fair = standardize_for_fair,
          seed = 1,
          criterion = criterion,
          verbose_each = FALSE,
          method_fitters = method_fitters
        )

        best_scales <- resolve_best_scales(
          tuned = tuned,
          methods = methods_to_run,
          scale_grid = scale_grid,
          criterion = criterion
        )

        tt <- tuned$tune_table %||% data.frame()
        if (nrow(tt) > 0) {
          tt$n <- ni
          tt$rep <- rr
          tt$seed <- seed_i
          tune_rows[[idx]] <- tt
        }
      } else {
        if (is.null(fixed_scales)) stop("do_tune=FALSE이면 fixed_scales를 반드시 넣어야 합니다.")
        best_scales <- fixed_scales
      }

      # 3) final evaluation on test set (original sim$X_test)
      tbl <- benchmark_methods(
        sim,
        methods = methods_to_run,
        alpha = alpha,
        c_lambda = c_lambda,
        scale_by_method = best_scales,
        standardize_for_fair = standardize_for_fair,
        seed = 1,
        verbose = FALSE,
        method_fitters = method_fitters
      )

      tbl$n <- ni
      tbl$rep <- rr
      tbl$seed <- seed_i
      tbl$scale_used <- as.numeric(best_scales[tbl$method])  # method별로 적용된 scale 기록

      if (verbose_each_iter) {
        print(tbl[, c("method", "elapsed", "MSE", "F1_support", "F1_outlier", "lambda_used", "scale_used", "n", "rep")])
      }

      all_rows[[idx]] <- tbl
      idx <- idx + 1
    }
  }

  res_all <- do.call(rbind, all_rows)

  # n, method별 평균
  res_mean <- aggregate(
    cbind(elapsed, MSE, F1_support, F1_outlier) ~ n + method,
    data = res_all,
    FUN = mean
  )

  out <- list(res_all = res_all, res_mean = res_mean)
  if (do_tune && length(tune_rows) > 0) {
    out$tune_table_all <- do.call(rbind, tune_rows)
  }
  out
}

############################################################
## (B) ggplot2: n에 따른 지표 plot (method별 선)
############################################################

plot_metrics_vs_n <- function(res_mean) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
  library(ggplot2)
  if (!requireNamespace("patchwork", quietly = TRUE)) install.packages("patchwork")
  library(patchwork)

  res_mean$n <- as.numeric(res_mean$n)

  p_elapsed <- ggplot(res_mean, aes(x = n, y = elapsed, color = method)) +
    geom_line() + geom_point() +
    labs(title = "Elapsed vs n", x = "n", y = "Elapsed (sec)", color = "Method") +
    theme_minimal()

  p_f1 <- ggplot(res_mean, aes(x = n, y = F1_support, color = method)) +
    geom_line() + geom_point() +
    coord_cartesian(ylim = c(0, 1)) +
    labs(title = "F1_support vs n", x = "n", y = "F1_support", color = "Method") +
    theme_minimal()

  p_mse <- ggplot(res_mean, aes(x = n, y = MSE, color = method)) +
    geom_line() + geom_point() +
    labs(title = "Test MSE vs n", x = "n", y = "MSE", color = "Method") +
    theme_minimal()

  p_elapsed + p_f1 + p_mse
}

############################################################
## (C) 실행 스크립트 안내
############################################################
# 실제 전체 시뮬레이션을 돌리는 코드는 run_all_simulation.R에 있습니다.
# 해당 파일을 실행하면 모든 알고리즘(DCA, DCA_fast, BDCA_fast)에 대해
# n-grid 실험과 플롯을 수행합니다.
