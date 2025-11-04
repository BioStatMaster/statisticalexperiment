set.seed(1) # 문제 풀기 전 실행

## 1
### 1.1
# -3부터 3까지 0.01 간격으로 x값 생성
x <- seq(-3, 3, by = 0.01)

# 표준정규분포의 확률밀도함수 직접 정의
f_x <- (1 / sqrt(2 * pi)) * exp(-(x^2) / 2)

# 그래프 그리기
plot(x, f_x, type = "l", main = "Standard Normal PDF (No dnorm())",
     xlab = "x", ylab = "Density")


### 1.2
# 1~6의 자연수 각각이 동일확률 1/6로 나옴
# 6의 약수 = 1, 2, 3, 6 → 확률 = 4/6 = 2/3
p <- 2/3
#주사위 200번 던질 때, 약수가 150~170번 나올 확률
p1 <- pbinom(170, size = 200, prob = p) - pbinom(149, size = 200, prob = p)
  
## 2
#### 2.1
# ames.csv 파일 불러오기
ames <- read.csv("ames.csv")

#### 2.2
# Gr.Liv.Area 변수만 추출
area <- ames$Gr.Liv.Area

#### 2.3
# 모집단에서 크기 50의 랜덤 표본 추출
sample_area <- sample(area, size = 50)
xbar <- mean(sample_area)
# 표본평균과 표본표준편차 계산
s <- sd(sample_area)
n <- 50
# 90% 신뢰구간 (z = 1.645 사용)
z <- qnorm(0.95)
lower <- xbar - z * s / sqrt(n)
upper <- xbar + z * s / sqrt(n)

#### 2.4
#조건문 사용
# 모평균 = 모집단 평균
mu <- mean(area)

# 신뢰구간에 포함 여부 (조건문 사용)
if (lower <= mu & mu <= upper) {
  is_contained <- TRUE
} else {
  is_contained <- FALSE
}

## 3
#### 3.1
#표준편차 = sqrt(Σ(x - 평균)^2 / N)
mean_area <- mean(area)
result1 <- sqrt(sum((area - mean_area)^2) / length(area))


#### 3.2
# 표본 표준편차 = sqrt(Σ(x - 평균)^2 / (n - 1))
result2 <- sqrt(sum((area - mean_area)^2) / (length(area) - 1))

#### 3.3
ratio <- result1 / result2

## 4

### 4.1
# 귀무가설 : μ = 247
# 대립가설 : μ ≠ 247

### 4.2
# 표본평균 = 276, 모집단표준편차 = 52, n = 11
Z <- (276 - 247) / (52 / sqrt(11))

### 4.3
# 유의수준 5% → 양측검정이므로 상위 2.5% 분위수 사용
z_crit <- qnorm(0.975)

if (abs(Z) > z_crit) {
  conclusion <- "귀무가설 기각: 평균은 247명이 아니다."
} else {
  conclusion <- "귀무가설 채택: 평균은 247명이다."
}

# 결론 출력
conclusion