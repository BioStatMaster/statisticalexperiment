set.seed(1)

## 1
### 1.1
x <- seq(-3, 3, 0.01)
y <- (1 / sqrt(2 * pi)) * exp(-x^2 / 2)
plot(x, y, type = "l")

### 1.2
p1 <- pbinom(170, 200, 3/6) - pbinom(149, 200, 3/6)

## 2
#### 2.1
ames <- read.csv("C:\\Users\\박성호\\Downloads\\ames.csv")

#### 2.2
area <- ames$Gr.Liv.Area

#### 2.3
n <- 50
samp <- sample(area, n)
xbar <- mean(samp)
sigma <- sd(area)
alpha <- 0.1
z <- qnorm(1 - alpha/2)
se <- sigma / sqrt(n)
lower <- xbar - z * se
upper <- xbar + z * se

#### 2.4
is_contained <- if (mean(area) >= lower & mean(area) <= upper) TRUE else FALSE

## 3
#### 3.1
mu <- mean(area)
result1 <- sqrt(sum((area - mu)^2) / length(area))

#### 3.2
xbar2 <- mean(area)
result2 <- sqrt(sum((area - xbar2)^2) / (length(area) - 1))

#### 3.3
ratio <- result1 / result2

## 4
### 4.1
# 귀무가설 : H0 : μ = 247
# 대립가설 : H1 : μ ≠ 247

### 4.2
Z <- (276 - 247) / (52 / sqrt(11))

### 4.3
# 유의수준 5%에서 |Z| > qnorm(0.975)이므로 귀무가설을 기각한다.
