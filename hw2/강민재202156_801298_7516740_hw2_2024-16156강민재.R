set.seed(1) # 문제 풀기 전 실행

## 1
### 1.1
x <- seq(-3, 3, length.out = 200)
y <- (1 / sqrt(2 * pi)) * exp(-(x^2) / 2)
plot(x, y, type = "l")

### 1.2
p1 <- pbinom(170, 200, 2/3) - pbinom(149, 200, 2/3)

## 2
#### 2.1
ames <- read.csv("C:/Users/강민재/Downloads/ames (4).csv")

#### 2.2
area <- ames$Gr.Liv.Area

#### 2.3
lower <- mean(sample(area, 50)) - qnorm(1 - 0.1/2) * (sd(sample(area, 50)) / sqrt(50))
upper <- mean(sample(area, 50)) + qnorm(1 - 0.1/2) * (sd(sample(area, 50)) / sqrt(50))

#### 2.4
#조건문 사용
is_contained <- (mean(area) >= lower) & (mean(area) <= upper)

## 3
#### 3.1
result1 <- sqrt(sum((area - mean(area))^2) / length(area))

#### 3.2
result2 <- sqrt(sum((area - mean(area))^2) / (length(area) - 1))

#### 3.3
ratio <- result1 / result2

## 4

### 4.1
# 귀무가설 : 실제 평균 팔로잉 수가 247명이다.
# 대립가설 : 실제 평균 팔로잉 수가 247명이 아니다.

### 4.2
Z <- (276 - 247) / (52 / sqrt(11))

### 4.3
abs(Z) > qnorm(1 - 0.05/2)
# 귀무가설을 기각하지 않는다. 즉, 실제 평균 팔로잉 수가 247명이라고 보기 어렵다고는 할 수 없다.