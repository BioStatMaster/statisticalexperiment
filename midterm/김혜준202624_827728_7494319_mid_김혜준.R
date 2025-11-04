set.seed(1) # 문제 풀기 전 실행시켜주세요.

## 1
#### 1.1
x <- seq(from=-4, to=4, by=0.01)

#### 1.2
fx1 <- 1/sqrt(2*pi)*exp(-(x^2)/2)

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
# 표본의 수를 20000보다 키우면 빨간 선에 더 가까워진다.

## 3
#### 3.1
bodydims <- read.csv("C:/Users/eg070/Downloads/bodydims.csv", header=T)

#### 3.2
bodydims.m <- bodydims[bodydims$sex==1,]
bodydims.f <- bodydims[bodydims$sex==0,]

#### 3.3
m1 <- mean(bodydims.m$hgt)
s1 <- var(bodydims.m$hgt)*(length(bodydims.m)-1)/length(bodydims.m)

m2 <- mean(bodydims.f$hgt)
s2 <- var(bodydims.f$hgt)*(length(bodydims.f)-1)/length(bodydims.f)

## 4
#### 4.1
# 귀무가설: H0: 통계학 실험 수강생들의 일주일 커피 소비량의 평균은 11잔이다.
# 대립가설: H1: 통계학 실험 수강생들의 일주일 커피 소비량의 평균은 11잔이 아니다.

#### 4.2
xbar<-10
mu<-11
sigma<-4.5
n<-36
Z <- (xbar-mu)/(sigma/sqrt(n))

#### 4.3
# 조건문 사용
is_reject <- if (abs(qnorm(0.025)) < Z) {TRUE} else {FALSE}

#### 4.4
print(is_reject)
# is_reject가 FALSE이므로 귀무가설을 기각할 수 없다. 통계학 실험 수강생들의 일주일 커피 소비량의 평균은 11잔이다.
