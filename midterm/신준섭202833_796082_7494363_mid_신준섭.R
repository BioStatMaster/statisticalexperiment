set.seed(1) # 문제 풀기 전 실행시켜주세요.

## 1
#### 1.1
x <- seq(from=-4, to=4, by=0.1)

#### 1.2
fx1 <- 1/sqrt(2*pi)*exp(-x^2/2)

#### 1.3
plot(x, fx1, main="표준정규분포의 확률밀도함수", type='l')

## 2
#### 2.1
u <- runif(20000, min=0, max=1)

#### 2.2
fx2 <- qnorm(u)

#### 2.3
hist(fx2, probability=T, breaks=31)

#### 2.4
lines(x,fx1,col='red')
#랜덤 표본의 개수를 늘리면, 즉 runif함수의 20000을 더 큰 수로 늘리면 히스토그램이 정규문포에 근사된다.

## 3
#### 3.1
bodydims <- read.csv("C:/Users/sjs27/Downloads/bodydims.csv")

#### 3.2
bodydims.m <- bodydims[bodydims$sex==1]
bodydims.f <- bodydims[bodydims$sex==0]

#### 3.3
n1=length(bodydims.m$hgt)
m1 <- mean(bodydims.m$hgt)
s1 <- var(bodydims.m$hgt)*(n1-1)/n1

n2=length(bodydims.f$hgt)
m2 <- mean(bodydims.f$hgt)
s2 <- var(bodydims.f$hgt)*(n2-1)/n2
s
## 4
#### 4.1
# 귀무가설: 전체 통계학 실험 수강생들의 일주일 커피 소비량의 평균은 11잔이다.
# 대립가설: 전체 통계학 실험 수강생들의 일주일 커피 소비량의 평균은 11잔이 아니다.

#### 4.2
Z <- (10-11)/sqrt(4.5/36)

#### 4.3
# 조건문 사용
is_reject <- qnorm(0.025)>=abs(Z) | qnorm(0.975)<=abs(Z)
is_reject

#### 4.4
# 검정통계량 Z의 값이 z0.025보다 낮으므로 is_reject의 값이 TRUE가 나오고, 이는 유의수준 5% 하에서 귀무가설을 기각할 수 있음을 뜻한다. 