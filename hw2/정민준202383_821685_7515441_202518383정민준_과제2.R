set.seed(1)

## 1
### 1.1
x <- seq(-3, 3, length.out = 200)
y <- (1 / sqrt(2 * pi)) * exp(-x^2 / 2)
plot(x, y, type = "l", col = "blue", main = "Standard Normal PDF", ylab = "f(x)", xlab = "x")

### 1.2
p1 <- pbinom(170, 200, 2/3) - pbinom(149, 200, 2/3)

## 2
#### 2.1
ames <- read.csv("ames.csv")

#### 2.2
area <- ames$Gr.Liv.Area

#### 2.3
sample_data <- sample(area, 50)
xbar <- mean(sample_data)
s <- sd(sample_data)
n <- length(sample_data)
error <- qnorm(0.95) * s / sqrt(n)
lower <- xbar - error
upper <- xbar + error

#### 2.4
mu <- mean(area)
is_contained <- ifelse(mu >= lower & mu <= upper, TRUE, FALSE)

## 3
#### 3.1
mu <- mean(area)
result1 <- sqrt(sum((area - mu)^2) / length(area))

#### 3.2
xbar <- mean(area)
result2 <- sqrt(sum((area - xbar)^2) / (length(area) - 1))

#### 3.3
ratio <- result1 / result2

## 4
### 4.1
# 귀무가설 : μ = 247
# 대립가설 : μ ≠ 247

### 4.2
xbar <- 276
mu0 <- 247
sigma <- 52
n <- 11
Z <- (xbar - mu0) / (sigma / sqrt(n))

### 4.3
critical <- qnorm(0.975)
p_value <- 2 * (1 - pnorm(abs(Z)))
# 결론: |Z| > 1.96이므로 귀무가설을 기각한다. 평균 팔로잉 수는 247명이 아니다.
