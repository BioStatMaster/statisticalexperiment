set.seed(1) # 문제 풀기 전 실행시켜주세요.

## 1
#### 1.1
x <- c(seq(from=-4, to=4, by=0.01))

#### 1.2
fx1 <- (1/sqrt(2*pi))*exp(-(x^2)/2)

#### 1.3
plot(x, fx1, xlim=c(-4, 4), type="l")

## 2
#### 2.1
u <- runif(20000, min=0, max=1)

#### 2.2
fx2 <- qnorm(u)

#### 2.3
hist(fx2, breaks = seq(min(fx2), max(fx2), length.out=31), freq=F)

#### 2.4
# 표본의 개수를 늘린다.

## 3
#### 3.1
bodydims <- read.csv("C:\\Users\\april\\Desktop\\심희원\\대학생활\\2025-2\\통계학실험\\bodydims.csv")

#### 3.2
bodydims.m <- bodydims[bodydims$sex==1,]
bodydims.f <- bodydims[bodydims$sex==0,]

#### 3.3
m1 <- mean(bodydims.m$hgt)
s1 <- var(bodydims.m$hgt)*((n-1)/n)

m2 <- mean(bodydims.f$hgt)
s2 <- var(bodydims.f$hgt)*((n-1)/n)

## 4
#### 4.1
# 귀무가설: mu = 11 / 전체 통계학 실험 수강생들의 일주일 커피 소비량은 11잔이다.
# 대립가설: mu != 11 / 전체 통계학 실험 수강생들의 일주일 커피 소비량은 11잔이 아니다.

#### 4.2
mu0 <- 11
mu <- 10
var <- 4.5
n <- 36

Z <- (mu - mu0)/sqrt(var)/sqrt(n)

#### 4.3
# 조건문 사용
alpha <- 0.05

if ((Z<qnorm(alpha/2)) | (Z > -qnorm(alpha/2))){
  is_reject <- TRUE
} else {
  is_reject <- FALSE
}

#### 4.4
# 검정을 진행한 결과, is_reject에 FALSE 값이 저장되었으므로 귀무가설이 기각되고, 대립가설이 채택되었다. 즉, 전체 통계학 실험 수강생들의 일주일 커피 소비량의 실제 평균은 11잔이 아니라고 할 수 있다.