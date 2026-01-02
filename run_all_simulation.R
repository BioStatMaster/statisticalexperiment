# Run full n-grid simulation across all algorithms and plot results.

source("experiment.R")

methods_to_run <- c(
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

# -----------------------------
# User config
# -----------------------------
# n 설정
n_grid <- c(200, 500, 1000, 2000)

# 튜닝 여부
do_tune <- TRUE

# 수동 lambda 설정(예시)
# - NULL이면 튜닝 또는 기본 scale 사용
# - 숫자 하나면 모든 method에 동일 적용
# - 이름 있는 벡터면 method별로 적용
# manual_lambda <- c(DCA = 0.05, DCA_fast = 0.05, BDCA_fast = 0.05)
manual_lambda <- NULL

# 튜닝 옵션
scale_grid <- 10^seq(-2, 2, by = 0.5)
criterion <- "MSE"

# 공통 lambda 계수
c_lambda <- 1.0

fixed_scales <- NULL
method_fitters <- list()
# 외부 알고리즘은 패키지가 설치되어 있으면 자동으로 등록합니다.
if (requireNamespace("robustHD", quietly = TRUE)) {
  method_fitters[["robustHD::sparseLTS"]] <- function(X, y, alpha, lambda, verbose = FALSE) {
    fit <- robustHD::sparseLTS(x = X, y = y, alpha = alpha, lambda = lambda)
    if (is.null(fit$beta) || is.null(fit$intercept)) {
      stop("robustHD::sparseLTS 결과에서 beta/intercept를 찾지 못했습니다.")
    }
    outlier_set <- if (!is.null(fit$raw$best)) fit$raw$best else integer(0)
    list(beta0 = fit$intercept, beta = fit$beta, outlier_set = outlier_set)
  }
}

# 추가 외부 알고리즘은 아래처럼 등록해서 사용하세요.
# method_fitters[["SWLTS-PALM"]] <- function(X, y, alpha, lambda, verbose = FALSE) {
#   # 반환값은 list(beta0=..., beta=..., outlier_set=...) 형태여야 합니다.
# }

resolve_methods_to_run <- function(methods, fitters) {
  available <- names(default_method_fitters)
  if (!is.null(fitters)) {
    available <- unique(c(available, names(fitters)))
  }
  allowed <- intersect(methods, allowed_methods)
  kept <- intersect(allowed, available)
  dropped <- setdiff(methods, kept)
  if (length(dropped) > 0) {
    message(sprintf("method_fitters 미등록으로 제외: %s", paste(dropped, collapse = ", ")))
  }
  kept
}

methods_to_run <- resolve_methods_to_run(methods_to_run, method_fitters)
if (!is.null(manual_lambda)) {
  if (length(manual_lambda) == 1) {
    fixed_scales <- setNames(rep(manual_lambda / c_lambda, length(methods_to_run)), methods_to_run)
  } else {
    if (is.null(names(manual_lambda))) {
      stop("manual_lambda가 여러 개면 method 이름이 필요합니다.")
    }
    missing_lambda <- setdiff(methods_to_run, names(manual_lambda))
    if (length(missing_lambda) > 0) {
      stop(sprintf("manual_lambda에 없는 메서드: %s", paste(missing_lambda, collapse = ", ")))
    }
    fixed_scales <- manual_lambda[methods_to_run] / c_lambda
  }
  do_tune <- FALSE
}

call_with_supported_args <- function(fn, args) {
  keep <- names(formals(fn))
  args <- args[names(args) %in% keep]
  do.call(fn, args)
}

result <- call_with_supported_args(
  run_n_experiment,
  list(
    n_grid = n_grid,
    R_rep = 3,
    methods_to_run = methods_to_run,
    do_tune = do_tune,
    scale_grid = scale_grid,
    criterion = criterion,
    fixed_scales = fixed_scales,
    c_lambda = c_lambda,
    method_fitters = method_fitters,
    verbose_each_iter = TRUE
  )
)

# -----------------------------
# Plot example
# -----------------------------
plot_obj <- plot_metrics_vs_n(result$res_mean)
print(plot_obj)
