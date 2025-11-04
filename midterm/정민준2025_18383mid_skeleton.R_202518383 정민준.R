set.seed(1) # 문제 풀기 전 실행시켜주세요.

## 1
#### 1.1
x <- c(-4:4, by=0.01)

#### 1.2
fx1 <- 1/sqrt(2 * pi)* exp^(-x^2/2)

#### 1.3
graph <- plot(x, fx1)
## 2
#### 2.1
u <- random(c(0,1), 20000)

#### 2.2
fx2 <- quantile(u)

#### 2.3
hist(fx2, xlim = c(0:30), percentage = T)

#### 2.4
# 랜덤으로 뽑은 표본의 수를 늘린다다

## 3
#### 3.1
bodydims <- read.table("bodydims.ccsv", header = T)

#### 3.2
bodydims.m <- bodydims$age if bodysims$age == 1
bodydims.f <- bodydims$age if bodysims$age == 0

#### 3.3
m1 <- mean(bodydims.m)
s1 <- var(bodydims.m)

m2 <- mean(bodydims.f)
s2 <- var(bodydims.f)


## 4
#### 4.1
# 귀무가설: X<11
# 대립가설: X=11

#### 4.2
Z <- 11-10/sqrt(4.5) * 36

#### 4.3
# 조건문 사용
is_reject <- TODO

#### 4.4
# TODO