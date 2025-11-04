set.seed(1) # 문제 풀기 전 실행시켜주세요.

## 1
#### 1.1
x <- seq(from = -4, to = 4, by = 0.01)

#### 1.2
fx1 <- 1/sqrt(2*pi) * exp((-x^2)/2)

#### 1.3
plot(x, fx1, type ='l')

## 2 
#### 2.1
u <- runif(20000, 0, 1)

#### 2.2
fx2 <- qnorm(u)

#### 2.3
hist(fx2, breaks = seq(min(fx2), max(fx2), length.out = 31), probability = T)

#### 2.4
lines(x,fx1, col='red')
# 히스토그램을 그릴 때 구간의 수를 늘린다.

## 3
#### 3.1
bodydims <- read.csv('/Users/hannahkim/Downloads/25-2 통계학실험/bodydims.csv', header = T)

#### 3.2
bodydims.m <- bodydims[bodydims$sex == 1,]
bodydims.f <- bodydims[bodydims$sex == 0,]

#### 3.3
m1 <- mean(bodydims.m$hgt)
n1 <- length(bodydims.m$hgt)
s1 <- var(bodydims.m$hgt) * ((n1 - 1) / n1)

m2 <- mean(bodydims.f$hgt)
n2 <- length(bodydims.f$hgt)
s2 <- var(bodydims.f$hgt) * ((n2 - 1) / n2)

## 4
#### 4.1
# 귀무가설: mu = 11 (뮤는 11이다.)
# 대립가설: mu /= 11 (뮤는 11이 아니다.)

#### 4.2
Z <- (10 - 11) / sqrt(4.5 / 36)

#### 4.3
# 조건문 사용
is_reject <- if (abs(Z) >= qnorm(1 - 0.05 / 2)) {
  print('TRUE')
} else {
  print('FALSE')
}

#### 4.4
# 귀무가설이 기각되었으므로 전체 통계학 실험 수강생들의 일주일 커피 소비량의 실제 평균은 11잔이 아니라고 할 수 있다.