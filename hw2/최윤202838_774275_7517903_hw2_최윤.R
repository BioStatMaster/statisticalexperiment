set.seed(1) # 문제 풀기 전 실행

## 1
### 1.1
x_vals <- seq(from = -3, to = 3, by = 0.01)
fx_vals <- (1 / sqrt(2 * pi)) * exp(-x_vals^2 / 2)
plot(x_vals, fx_vals, 
     type = "l",
     main = "Standard Normal PDF (f(x))", 
     xlab = "x", 
     ylab = "Density",
     col = "blue",
     lwd = 2)

### 1.2
p1 <- pbinom(170, size = 200, prob= 4/6) - pbinom(149, size=200, prob=4/6)

## 2
#### 2.1
ames <- read.csv("C:/Users/chlyu/iCloudDrive/PARA/1. Project/통계학 실험/과제/HW2/ames.csv", header=TRUE)

#### 2.2
area <- ames$Gr.Liv.Area

 
#### 2.3
sample_area <- sample(area, 50)
sample_mean <- mean(sample_area)
sample_sd <- sd(sample_area)
n <- 50
z_critical <- qnorm(0.95)
margin_error <- z_critical * (sample_sd / sqrt(n))
lower <- sample_mean - margin_error
upper <- sample_mean + margin_error

#### 2.4
#조건문 사용
pop_mean <- mean(area)
if (pop_mean > lower & pop_mean < upper) {
  is_contained <- TRUE
} else {
  is_contained <- FALSE
}

## 3
#### 3.1
result1 <- sqrt(sum((area - mean(area))^2) / length(area))

#### 3.2
n <- length(area)
result2 <- sqrt(sum((area - mean(area))^2) / (n - 1))

#### 3.3
ratio <- result1 / result2

## 4

### 4.1
# 귀무가설 : 서울대학교 통계학과 대학원생들의 인스타그램 팔로잉 수 평균은 247명이다. (H0: mu = 247)
# 대립가설 : 서울대학교 통계학과 대학원생들의 인스타그램 팔로잉 수 평균은 247명이 아니다. (H1: mu != 247)

### 4.2
xbar <- 276 
mu <- 247 
sigma <- 52
n <- 11
Z <- (xbar - mu) / (sigma / sqrt(n))

### 4.3
# 유의수준 5% 양측검정의 임계값은 qnorm(0.975) (약 1.96)입니다. 
# 4.2에서 계산한 검정통계량 Z (약 1.849)는 임계값의 절대값(1.96)보다 작습니다. 
# 따라서 유의수준 5% 하에서 귀무가설을 기각하지 못합니다. 
# 결론: 평균 팔로잉 수가 247명이 아니라고 할 수 없습니다.