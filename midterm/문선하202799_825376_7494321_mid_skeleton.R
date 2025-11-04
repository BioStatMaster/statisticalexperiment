set.seed(1) # 문제 풀기 전 실행시켜주세요.

## 1
#### 1.1
x <- seq(-4,4,0.01)

#### 1.2
fx1 <- exp(-x^2/2)/(sqrt(2)*pi)

#### 1.3
plot(x,fx1,type="l")

## 2
#### 2.1
u <- runif(20000,0,1)

#### 2.2
fx2 <- qnorm(u)

#### 2.3
hist(fx2, probability=T, breaks=29)

#### 2.4
# 2.1에서의 표본 수를 더 늘린다. 

## 3
#### 3.1
bodydims <- read.csv("C:/Users/munsu/Downloads/bodydims.csv", header=T)

#### 3.2
bodydims.m <- bodydims[bodydims$sex==1,]
bodydims.f <- bodydims[bodydims$sex==0,]

#### 3.3
m1 <- mean(bodydims.m$hgt)
s1 <- sqrt(var(bodydims.m$hgt)*(length(bodydims.m)-1)/length(bodydims.m))

m2 <- mean(bodydims.f$hgt)
s2 <- sqrt(var(bodydims.f$hgt)*(length(bodydims.f)-1)/length(bodydims.f))

## 4
#### 4.1
# 귀무가설: mu=11(전체 통계학 실험 수강생들의 일주일 커피 소비량의 실제 평균은 11잔이다. )
# 대립가설: mu≠11(전체 통계학 실험 수강생들의 일주일 커피 소비량의 실제 평균은 11잔이 아니다. )

#### 4.2
Z <- (10-11)/(sqrt(4.5)/6)

#### 4.3
# 조건문 사용
is_reject <- if (abs(Z)>abs(qnorm(0.05/2))){TRUE}else{FALSE}

#### 4.4
# is_reject가 TRUE이므로 귀무가설을 기각할 수 있고, 실제 평균은 11잔이 통계적으로 유의하게 아니라고 말할 수 있다. 