set.seed(1) # 문제 풀기 전 실행시켜주세요.

## 1
#### 1.1
x <- seq(-4, 4, 0.01)

#### 1.2
fx1 <- exp((-x^2)/2)/sqrt(2*pi)

#### 1.3
plot(x, fx1, type="l")

## 2
#### 2.1
u <- runif(20000, min=0, max=1)

#### 2.2
fx2 <- qnorm(u)

#### 2.3
hist(fx2, freq=F, breaks=seq(min(fx2), max(fx2), length.out=31))

#### 2.4
# 표본의 수를 늘린다

## 3
#### 3.1
bodydims <- read.csv("C:/Users/zimin/Downloads/bodydims.csv", header=T)

#### 3.2
bodydims.m <- bodydims[bodydims$sex==1,]
bodydims.f <- bodydims[bodydims$sex==0,]

#### 3.3
n1 = length(bodydims.m)
m1 <- mean(bodydims.m$hgt)
s1 <- var(bodydims.m$hgt)*(n1-1)/n1

n2 = length(bodydims.f)
m2 <- mean(bodydims.f$hgt)
s2 <- var(bodydims.f$hgt)*(n2-1)/n2

## 4
#### 4.1
# 귀무가설: u=11
# 대립가설: u!=11

#### 4.2
Z <- (10-11)/(sqrt(4.5/36))

#### 4.3
# 조건문 사용
is_reject <- abs(Z)>qnorm(1-0.05/2)

#### 4.4
# 검정통계량이 기각역에 포함되므로 귀무가설을 기각한다다