set.seed(1) # 문제 풀기 전 실행시켜주세요.

## 1
#### 1.1
x <- seq(from = -4, to = 4, by = 0.01)

#### 1.2
fx1 <- 1/sqrt(2 * pi) * exp(-x^2/2)

#### 1.3
plot(x, fx1)

## 2
#### 2.1
u <- runif(20000, min = 0, max = 1)

#### 2.2
fx2 <- qnorm(u)

#### 2.3
hist(fx2, by = 30)

#### 2.4
lines(x,fx1, col='red')
# 랜덤 표본의 개수를 더 증가시킨다. 

## 3
#### 3.1
bodydims <- as.data.frame(bodydims.csv)

#### 3.2
bodydims.m <- bodydims$sex==1
bodydims.f <- bodydims$sex==0

#### 3.3
m1 <- mean(bodydims.m$hgt)
s1 <- sd(bodydims.m$hgt)

m2 <- mean(bodydims.f$hgt)
s2 <- sd(bodydims.f$hgt)

## 4
#### 4.1
# 귀무가설: 전체 통계학 실험 수강생들의 커피 소비량의 실제 평균은 11잔이다.
# 대립가설: 전체 통계학 실험 수강생들의 커피 소비량의 실제 평균은 11잔이 아니다.

#### 4.2
Z <- TODO

#### 4.3
# 조건문 사용
is_reject <- TODO

#### 4.4
# TODO