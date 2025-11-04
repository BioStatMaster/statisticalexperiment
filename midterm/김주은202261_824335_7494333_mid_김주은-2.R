set.seed(1) # 문제 풀기 전 실행시켜주세요.

## 1
#### 1.1
x <- seq(from=-4, to=4, by=0.01)

#### 1.2
fx1 <- (1/sqrt(2*pi))*exp(-(x^2)/2)

#### 1.3
plot(x, fx1, xlim=c(-4,4), type='l')

## 2
#### 2.1
n <- 20000
u <- runif(n, min = 0, max = 1)

#### 2.2
fx2 <- qnorm(u)

#### 2.3
hist(fx2, breaks = 30, freq = F)

#### 2.4
# 표본의 크기 n을 늘린다. ex. 20000 --> 100000

## 3
#### 3.1
bodydims <- read.csv("C:/Users/HOME/Downloads/bodydims.csv", header=T)

#### 3.2
bodydims.m <- bodydims[bodydims$sex == 1, ]
bodydims.f <- bodydims[bodydims$sex == 0, ]

#### 3.3
m1 <- mean(bodydims.m$hgt)
n1 <- length(bodydims.m$hgt)
s1 <- var(bodydims.m$hgt) * ((n1-1)/n1)

m2 <- mean(bodydims.f$hgt)
n2 <- length(bodydims.f$hgt)
s2 <- var(bodydims.f$hgt) * ((n2-1)/n2)

## 4
#### 4.1
# 귀무가설: 모평균(mu) == 11
# 대립가설: 모평균(mu) != 11 (양측검정)

#### 4.2
mu0 <- 11 
xbar <- 10 
sigma <- sqrt(4.5)
n <- 36 
alpha <- 0.05

Z <- (xbar - mu0) / (sigma / sqrt(n))
Z

#### 4.3
# 조건문 사용
Z_alpha <- qnorm(1 - alpha/2)

if (abs(Z) >= Z_alpha) {
  is_reject <- TRUE
} else {
  is_reject <- FALSE
}
is_reject

#### 4.4
# 검정통계량의 절댓값이 양측 검정의 임계값 ±1.96보다 크므로,
# 유의수준 5%에서 귀무가설을 기각할 수 있다.