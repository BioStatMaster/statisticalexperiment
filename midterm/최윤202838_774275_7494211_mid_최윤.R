set.seed(1) # 문제 풀기 전 실행시켜주세요.

## 1
#### 1.1
x <- seq(from=-4, to=4, by=0.01)

#### 1.2
fx1 <- dnorm(x, 0, 1)

#### 1.3
plot(x,fx1)

## 2
#### 2.1
u <- TODO

#### 2.2
fx2 <- pnorm(u, 0, 1)

#### 2.3
hist(fx2, breaks=30)

#### 2.4
# 표본의 크기를 키운다

## 3
#### 3.1
bodydims <- read.csv("C:/Users/chlyu/iCloudDrive/PARA/1. Project/통계학 실험/중간고사/bodydims.csv", header=T)

#### 3.2
bodydims.m <- bodydims$sex == 1 
bodydims.f <- bodydims$sex == 0

#### 3.3
m1 <- mean(bodydims.m$hgt)
s1 <- var(bodydims.m$hgt)

m2 <- mean(bodydims.f$hgt)
s2 <- var(bodydims.f$hgt)

## 4
#### 4.1
# 귀무가설: TODO
# 대립가설: TODO

#### 4.2
Z <- TODO

#### 4.3
# 조건문 사용
is_reject <- TODO

#### 4.4
# TODO