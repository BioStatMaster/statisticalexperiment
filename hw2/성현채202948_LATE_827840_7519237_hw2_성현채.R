set.seed(1) # 문제 풀기 전 실행

## 1
### 1.1
x <- seq(-3, 3, length.out = 200)
f <- (1/sqrt(2*pi)) * exp(-x^2/2)
plot(x,f, type='l', main="표준정규분포 확률밀도함수", xlab='x', ylab='f(x)')

### 1.2
p1 <- pbinom(170, size=200, prob=2/3) - pbinom(149, size=200, prob=2/3)


## 2
#### 2.1
ames <- read.csv("ames.csv")

#### 2.2
area <- ames$Gr.Liv.Area

#### 2.3
set.seed(1)
sample_area <- sample(area, size=50)
xmean <- mean(sample_area)
mu <- mean(area)
s <- sd(sample_area)
n <- 50
alpha <- 0.1


lower <- xmean - qnorm(alpha/2, mu, s)*(s/sqrt(n))
upper <- xmean + qnorm(alpha/2, mu, s)*(s/sqrt(n))

#### 2.4
#조건문 사용
if (mu >= lower && mu <= upper) {
  is_contained <- TRUE
} else {is_contained <- FALSE}


## 3
#### 3.1
n_pop <- length(area)
mean_pop <- mean(area)
result1 <- sqrt(sum((area - mean_pop)^2) / n_pop)

#### 3.2
result2 <- sqrt(sum((area - mean_pop)^2) / (n_pop - 1))

#### 3.3
ratio <- result1 / result2

## 4

### 4.1
# 귀무가설 : mu = 247
# 대립가설 : mu != 247

### 4.2
xbar <- 276
mu0 <- 247
sigma <- 52
n1 <- 11
Z <- (xbar - mu0) / (sigma/sqrt(n))

### 4.3
alpha1 <- 0.05
if (abs(Z) > qnorm(1 - alpha/2)) {print("귀무가설 기각")} else {print("기각 안 함")}
# 귀무가설 기각




