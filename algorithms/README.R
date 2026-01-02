# Algorithm helper definitions
#
# 이 파일은 알고리즘 관련 함수 정의를 모아두는 곳입니다.
# 아래에 DCA 계열 3가지 알고리즘을 포함합니다.

############################################################
## [공통 Helper 함수들]
## - 세 함수(DCA / DCA_fast / BDCA_fast) 모두에서 사용
############################################################

# L1 proximal operator (라쏘의 핵심 연산: soft-thresholding)
soft_threshold <- function(z, t) {
  sign(z) * pmax(abs(z) - t, 0)
}

# ||X||_2^2 = 최대 고유값(=largest eigenvalue of X^T X) 근사 (스텝사이즈 추정용)
power_method_norm2_sq <- function(X, n_iter = 20) {
  X <- as.matrix(X)
  p <- ncol(X)
  v <- rnorm(p)
  v <- v / sqrt(sum(v^2))
  for (i in 1:n_iter) {
    v <- drop(crossprod(X, X %*% v))
    nv <- sqrt(sum(v^2))
    if (nv == 0) return(0)
    v <- v / nv
  }
  Av <- drop(crossprod(X, X %*% v))
  sum(v * Av)  # Rayleigh quotient
}

# x에서 "작은 값 k개"의 인덱스를 반환 (partial 정렬의 호환성 문제 회피)
bottomk_idx <- function(x, k) {
  n <- length(x)
  if (k <= 0) return(integer(0))
  if (k >= n) return(seq_len(n))

  x2 <- x
  x2[!is.finite(x2)] <- Inf  # NA/NaN/Inf 방어

  # k번째로 작은 값(임계값)만 partial로 빠르게 구함
  kth <- sort.int(x2, partial = k)[k]
  idx <- which(x2 <= kth)

  # tie(동점) 때문에 idx가 k보다 많을 수 있어 정확히 k개로 맞춤
  if (length(idx) > k) {
    idx <- idx[order(x2[idx], idx)][seq_len(k)]
  } else if (length(idx) < k) {
    idx <- order(x2)[seq_len(k)]
  }
  idx
}

# x에서 "큰 값 k개"의 인덱스를 반환
topk_idx <- function(x, k) {
  n <- length(x)
  if (k <= 0) return(integer(0))
  if (k >= n) return(seq_len(n))

  x2 <- x
  x2[!is.finite(x2)] <- -Inf

  # 큰 값 k개 = (-x2)의 작은 값 k개
  kth <- sort.int(-x2, partial = k)[k]
  idx <- which((-x2) <= kth)  # 즉 x2 >= -kth

  if (length(idx) > k) {
    idx <- idx[order(-x2[idx], idx)][seq_len(k)]
  } else if (length(idx) < k) {
    idx <- order(x2, decreasing = TRUE)[seq_len(k)]
  }
  idx
}

# (진짜) sparse LTS 목적함수 값 계산: (1/(2h))*sum_{smallest h} r^2 + lambda*||beta||_1
trimmed_lts_obj <- function(r2, h, beta, lambda) {
  idx <- bottomk_idx(r2, h)
  (1 / (2 * h)) * sum(r2[idx]) + lambda * sum(abs(beta))
}

############################################################
## [1] DCA 기본 버전 (단순/느린 버전)
## - inner에서 p 전체 변수에 대해 ISTA를 돌림 -> 큰 p에서 느릴 수 있음
############################################################

dca_sparse_lts_lasso <- function(X, y,
                                 alpha = 0.75, h = NULL,
                                 lambda = 0.05,
                                 max_outer = 60,
                                 inner_iter = 80,
                                 tol = 1e-4,
                                 eta_beta = NULL,
                                 verbose = FALSE) {
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  y <- as.numeric(y)
  n <- nrow(X)
  p <- ncol(X)

  if (is.null(h)) h <- max(1, min(n, floor(alpha * n)))
  k <- n - h  # outlier로 취급되는 개수

  # 스텝사이즈(안전한 값): eta ≈ h / ||X||_2^2
  if (is.null(eta_beta)) {
    norm2_sq <- power_method_norm2_sq(X, n_iter = 20)
    if (norm2_sq <= 0) norm2_sq <- 1
    eta_beta <- 0.95 * h / norm2_sq
  }

  beta <- rep(0, p)
  beta0 <- median(y)

  B_prev <- integer(0)
  obj_hist <- numeric(max_outer)
  converged <- FALSE

  for (t in 1:max_outer) {
    # (A) outlier set B 선택: residual^2 큰 k개
    r <- y - beta0 - as.numeric(X %*% beta)
    q <- r^2
    B <- topk_idx(q, k)

    # (B) subgradient u 계산 (h(·) = (1/(2h))*sum_{i in B} r_i^2 의 gradient)
    u0 <- 0
    u_beta <- rep(0, p)
    if (k > 0) {
      u0 <- -(1 / h) * sum(r[B])
      # B가 작으면 subset crossprod가 대개 빠름
      u_beta <- -(1 / h) * drop(crossprod(X[B, , drop = FALSE], r[B]))
    }

    # (C) convex subproblem을 ISTA로 근사 풀이 (full p)
    beta_in <- beta
    beta0_in <- beta0

    for (j in 1:inner_iter) {
      Xb <- as.numeric(X %*% beta_in)
      r_in <- y - beta0_in - Xb

      # grad_beta = -(1/h) X^T r - u_beta
      grad_beta <- -(1 / h) * drop(crossprod(X, r_in)) - u_beta

      beta_next <- soft_threshold(beta_in - eta_beta * grad_beta, eta_beta * lambda)

      # beta0의 닫힌해:
      # d/d beta0: -(1/h) sum(r) - u0 = 0  -> beta0 = mean(y - X beta) + (h/n)*u0
      beta0_next <- (sum(y - as.numeric(X %*% beta_next)) + h * u0) / n

      rel <- sqrt(sum((beta_next - beta_in)^2)) / max(1, sqrt(sum(beta_in^2)))
      beta_in <- beta_next
      beta0_in <- beta0_next
      if (rel < tol) break
    }

    beta_new <- beta_in
    beta0_new <- beta0_in

    # (D) 모니터링(진짜 trimmed 목적함수)
    r_new <- y - beta0_new - as.numeric(X %*% beta_new)
    obj_hist[t] <- trimmed_lts_obj(r_new^2, h, beta_new, lambda)

    # 종료 조건(간단): B가 안정 + beta 변화 작음
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

############################################################
## [2] DCA 개선 버전 (DCA_fast)
## - 핵심: convex subproblem(라쏘)을
##   (i) Active set(working set)로 작은 변수집합 A에서만 풀고
##   (ii) KKT 조건으로 위반 변수를 추가하여 "정확도 유지"
##   (iii) inner는 FISTA(가속) 사용
############################################################

# Active-set에서 라쏘 subproblem을 FISTA로 풂
solve_lasso_subproblem_active_fista <- function(X, y,
                                                beta0, beta,
                                                u0, u_beta,
                                                lambda, h,
                                                A, step,
                                                max_iter = 200,
                                                tol = 1e-6) {
  n <- length(y)
  if (length(A) == 0) return(list(beta0 = beta0, beta = beta))

  XA <- X[, A, drop = FALSE]

  b <- beta[A]   # active 변수만
  z <- b         # FISTA 보조변수
  t <- 1

  for (iter in 1:max_iter) {
    # r = y - beta0 - X_A z
    rz <- y - beta0 - as.numeric(XA %*% z)

    # beta0 최적 갱신(추가 계산 적게): sum(r) = -h*u0
    delta0 <- (sum(rz) + h * u0) / n
    beta0 <- beta0 + delta0
    rz <- rz - delta0

    # grad wrt beta_A: -(1/h) X_A^T r - u_beta[A]
    gradA <- -(1 / h) * drop(crossprod(XA, rz)) - u_beta[A]

    b_new <- soft_threshold(z - step * gradA, step * lambda)

    # FISTA momentum
    t_new <- (1 + sqrt(1 + 4 * t^2)) / 2
    z <- b_new + ((t - 1) / t_new) * (b_new - b)

    rel <- sqrt(sum((b_new - b)^2)) / max(1, sqrt(sum(b^2)))
    b <- b_new
    t <- t_new
    if (rel < tol) break
  }

  beta[A] <- b

  # beta 고정 후 beta0를 한 번 더 정확히 맞춤
  r_final <- y - beta0 - as.numeric(X %*% beta)
  beta0 <- beta0 + (sum(r_final) + h * u0) / n

  list(beta0 = beta0, beta = beta)
}

# KKT 체크: 라쏘 최적조건 위반 변수 찾기(정확도 유지용)
kkt_check_lasso <- function(X, y, beta0, beta, u_beta, lambda, h) {
  r <- y - beta0 - as.numeric(X %*% beta)
  grad <- -(1 / h) * drop(crossprod(X, r)) - u_beta

  nz <- which(beta != 0)
  z <- which(beta == 0)

  # beta_j != 0 -> grad_j + lambda*sign(beta_j) = 0 이어야 함
  viol_nz <- 0
  if (length(nz) > 0) {
    viol_nz <- max(abs(grad[nz] + lambda * sign(beta[nz])))
  }

  # beta_j == 0 -> |grad_j| <= lambda 이어야 함
  viol_z <- 0
  viol_idx <- integer(0)
  if (length(z) > 0) {
    vv <- abs(grad[z]) - lambda
    viol_z <- max(pmax(vv, 0))
    viol_idx <- z[which(vv > 0)]
  }

  list(grad = grad, viol_nz = viol_nz, viol_z = viol_z, viol_idx = viol_idx)
}

dca_sparse_lts_lasso_fast <- function(X, y,
                                      alpha = 0.75, h = NULL,
                                      lambda = 0.05,
                                      max_outer = 60,
                                      # KKT(정확도 유지) 관련
                                      kkt_tol = 1e-4,
                                      max_kkt_rounds = 10,
                                      # active set 크기 관련
                                      active_min = 50,
                                      add_max = 200,
                                      # inner(FISTA) 관련
                                      inner_max_iter = 200,
                                      inner_tol = 1e-6,
                                      step = NULL,
                                      verbose = FALSE) {
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  y <- as.numeric(y)
  n <- nrow(X)
  p <- ncol(X)

  if (is.null(h)) h <- max(1, min(n, floor(alpha * n)))
  k <- n - h

  # step-size
  if (is.null(step)) {
    norm2_sq <- power_method_norm2_sq(X, n_iter = 20)
    if (norm2_sq <= 0) norm2_sq <- 1
    step <- 0.95 * h / norm2_sq
  }

  beta <- rep(0, p)
  beta0 <- median(y)

  A <- integer(0)     # active set (outer 간 warm-start)
  B_prev <- integer(0)
  obj_hist <- numeric(max_outer)
  converged <- FALSE

  for (t in 1:max_outer) {
    # (A) outlier set B 선택
    r_old <- y - beta0 - as.numeric(X %*% beta)
    q <- r_old^2
    B <- topk_idx(q, k)

    # (B) subgradient u 계산
    u0 <- 0
    u_beta <- rep(0, p)
    if (k > 0) {
      u0 <- -(1 / h) * sum(r_old[B])
      if (k <= 0.3 * n) {
        u_beta <- -(1 / h) * drop(crossprod(X[B, , drop = FALSE], r_old[B]))
      } else {
        rb <- numeric(n)
        rb[B] <- r_old[B]
        u_beta <- -(1 / h) * drop(crossprod(X, rb))
      }
    }

    # (C) convex subproblem을 Active-set + KKT로 정확히 풂
    for (round in 1:max_kkt_rounds) {
      # active set이 너무 작으면 grad 큰 변수들로 초기화
      if (length(A) < min(active_min, p)) {
        grad0 <- -(1 / h) * drop(crossprod(X, r_old)) - u_beta
        ord <- order(abs(grad0), decreasing = TRUE)
        A <- sort(unique(c(which(beta != 0), ord[seq_len(min(active_min, p))])))
      }

      sol <- solve_lasso_subproblem_active_fista(
        X, y,
        beta0 = beta0, beta = beta,
        u0 = u0, u_beta = u_beta,
        lambda = lambda, h = h,
        A = A, step = step,
        max_iter = inner_max_iter,
        tol = inner_tol
      )
      beta0 <- sol$beta0
      beta <- sol$beta

      kk <- kkt_check_lasso(X, y, beta0, beta, u_beta, lambda, h)
      viol <- max(kk$viol_nz, kk$viol_z)

      if (verbose) {
        cat(sprintf("DCA_fast Outer %d / round %d | KKT viol=%.3e | |A|=%d\n",
                    t, round, viol, length(A)))
      }

      if (viol <= kkt_tol) break

      # 위반 변수를 active에 추가(크게 위반하는 것부터)
      idx <- kk$viol_idx
      if (length(idx) == 0) break
      grad <- kk$grad
      idx <- idx[order(abs(grad[idx]), decreasing = TRUE)]
      idx_add <- idx[seq_len(min(add_max, length(idx)))]
      A <- sort(unique(c(A, idx_add, which(beta != 0))))
    }

    # (D) objective 기록
    r_new <- y - beta0 - as.numeric(X %*% beta)
    obj_hist[t] <- trimmed_lts_obj(r_new^2, h, beta, lambda)

    same_B <- (length(B_prev) == length(B) && all(sort(B_prev) == sort(B)))
    if (verbose) {
      cat(sprintf("DCA_fast Outer %d | obj=%.6f | B_stable=%s | nnz=%d | |A|=%d\n",
                  t, obj_hist[t], same_B, sum(beta != 0), length(A)))
    }

    # 종료 조건: B 안정 + obj 변화 거의 없음
    if (t >= 2 && same_B) {
      rel_obj <- abs(obj_hist[t] - obj_hist[t - 1]) / max(1, abs(obj_hist[t - 1]))
      if (rel_obj < 1e-6) {
        converged <- TRUE
        obj_hist <- obj_hist[1:t]
        B_prev <- B
        break
      }
    }

    # active set warm-start 갱신
    A <- sort(unique(c(A, which(beta != 0))))
    B_prev <- B
  }

  list(beta0 = beta0, beta = beta,
       outlier_set = B_prev, h = h,
       converged = converged, objective = obj_hist,
       step = step, active_set = A, kkt_tol = kkt_tol)
}

############################################################
## [3] BDCA 버전 (BDCA_fast)
## - DCA_fast로 DCA-step y_k를 구한 뒤,
## - d = y_k - x_k 방향으로 lambda >= 1 line-search(Armijo-type)를 수행
## - 안 되면 lambda=1로 자동 fallback (즉 DCA_fast와 동일)
############################################################

bdca_sparse_lts_lasso_fast <- function(X, y,
                                       alpha = 0.75, h = NULL,
                                       lambda = 0.05,
                                       max_outer = 60,
                                       # subproblem 정확도(=정확성 유지)
                                       kkt_tol = 1e-4,
                                       max_kkt_rounds = 10,
                                       active_min = 50,
                                       add_max = 200,
                                       inner_max_iter = 200,
                                       inner_tol = 1e-6,
                                       step = NULL,
                                       # BDCA line search 파라미터
                                       ls_lambda_init = 2.0,
                                       ls_lambda_cap = 64.0,
                                       ls_beta = 0.5,      # backtracking shrink (0<ls_beta<1)
                                       ls_rho = 1e-6,      # Armijo 강도(너무 크게 잡으면 lambda=1만 나올 수 있음)
                                       ls_max_iter = 20,
                                       verbose = FALSE) {
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  y <- as.numeric(y)
  n <- nrow(X)
  p <- ncol(X)

  if (is.null(h)) h <- max(1, min(n, floor(alpha * n)))
  k <- n - h

  if (is.null(step)) {
    norm2_sq <- power_method_norm2_sq(X, n_iter = 20)
    if (norm2_sq <= 0) norm2_sq <- 1
    step <- 0.95 * h / norm2_sq
  }

  beta <- rep(0, p)
  beta0 <- median(y)

  A <- integer(0)
  B_prev <- integer(0)
  obj_hist <- numeric(max_outer)
  converged <- FALSE

  for (t in 1:max_outer) {
    # 현재점 x_k 저장
    x0 <- beta0
    x <- beta

    # residual at x_k
    r_old <- y - x0 - as.numeric(X %*% x)
    q <- r_old^2

    # (A) outlier set B
    B <- topk_idx(q, k)

    # (B) subgradient u
    u0 <- 0
    u_beta <- rep(0, p)
    if (k > 0) {
      u0 <- -(1 / h) * sum(r_old[B])
      if (k <= 0.3 * n) {
        u_beta <- -(1 / h) * drop(crossprod(X[B, , drop = FALSE], r_old[B]))
      } else {
        rb <- numeric(n)
        rb[B] <- r_old[B]
        u_beta <- -(1 / h) * drop(crossprod(X, rb))
      }
    }

    # ---------- (C) DCA step y_k 를 DCA_fast 방식으로 계산 ----------
    beta0_y <- x0
    beta_y <- x

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
        lambda = lambda, h = h,
        A = A, step = step,
        max_iter = inner_max_iter,
        tol = inner_tol
      )
      beta0_y <- sol$beta0
      beta_y <- sol$beta

      kk <- kkt_check_lasso(X, y, beta0_y, beta_y, u_beta, lambda, h)
      viol <- max(kk$viol_nz, kk$viol_z)

      if (verbose) {
        cat(sprintf("BDCA   Outer %d / round %d | KKT viol=%.3e | |A|=%d\n",
                    t, round, viol, length(A)))
      }
      if (viol <= kkt_tol) break

      idx <- kk$viol_idx
      if (length(idx) == 0) break
      grad <- kk$grad
      idx <- idx[order(abs(grad[idx]), decreasing = TRUE)]
      idx_add <- idx[seq_len(min(add_max, length(idx)))]
      A <- sort(unique(c(A, idx_add, which(beta_y != 0))))
    }

    # 방향 d = y_k - x_k
    d0 <- beta0_y - x0
    d <- beta_y - x
    dnorm2 <- d0^2 + sum(d^2)
    if (dnorm2 == 0) {
      converged <- TRUE
      obj_hist <- obj_hist[1:(t - 1)]
      break
    }

    # 현재 목적함수
    obj_x <- trimmed_lts_obj(r_old^2, h, x, lambda)

    # line search를 빠르게 하기 위해 Xd를 1회만 계산
    Xd <- as.numeric(X %*% d)

    lam <- min(ls_lambda_init, ls_lambda_cap)
    accept <- FALSE

    for (ls in 1:ls_max_iter) {
      # residual r(lam) = r_old - lam*(d0 + Xd)
      r_c <- r_old - lam * (d0 + Xd)
      beta_c0 <- x0 + lam * d0
      beta_c <- x + lam * d

      obj_c <- trimmed_lts_obj(r_c^2, h, beta_c, lambda)

      # Armijo-type 조건(간단 형태)
      if (obj_c <= obj_x - ls_rho * (lam^2) * dnorm2) {
        accept <- TRUE
        beta0 <- beta_c0
        beta <- beta_c
        break
      }

      # 최악이면 lam=1(DCA step)로 fallback
      if (lam <= 1 + 1e-12) {
        lam <- 1
        accept <- TRUE
        beta0 <- beta0_y
        beta <- beta_y
        break
      }

      lam <- max(1, lam * ls_beta)
    }

    if (!accept) {
      # 그래도 안전하게 DCA step으로
      beta0 <- beta0_y
      beta <- beta_y
      lam <- 1
    }

    # objective 기록
    r_new <- y - beta0 - as.numeric(X %*% beta)
    obj_hist[t] <- trimmed_lts_obj(r_new^2, h, beta, lambda)

    # active set warm-start 업데이트
    A <- sort(unique(c(A, which(beta != 0))))

    same_B <- (length(B_prev) == length(B) && all(sort(B_prev) == sort(B)))
    if (verbose) {
      cat(sprintf("BDCA   Outer %d | obj=%.6f | lambda_ls=%.2f | B_stable=%s | nnz=%d\n",
                  t, obj_hist[t], lam, same_B, sum(beta != 0)))
    }

    if (t >= 2 && same_B) {
      rel_obj <- abs(obj_hist[t] - obj_hist[t - 1]) / max(1, abs(obj_hist[t - 1]))
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
       step = step, active_set = A,
       kkt_tol = kkt_tol,
       ls = list(lambda_init = ls_lambda_init, rho = ls_rho, beta = ls_beta))
}
