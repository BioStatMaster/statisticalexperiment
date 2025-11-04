set.seed(1) # 문제 풀기 전 실행시켜주세요.

## 1
#### 1.1
x <- seq(from=-4,to=4,by=0.01)

#### 1.2
fx1 <- (1/sqrt(2*pi))*exp(-(x^2)/2)

#### 1.3
plot(x,fx1,xlim=c(-4,4))

## 2
#### 2.1
u <- runif(20000,min=0,max=1)

#### 2.2
fx2 <- qnorm(u,mean=0,sd=1)

#### 2.3
hist(fx2,breaks=31,freq=F)

#### 2.4
#구간의 수를 30보다 더 크게 설정한다.

## 3
#### 3.1
bodydims <- read.csv("C:/Users/강민재/Downloads/bodydims.csv", header=T)

#### 3.2
bodydims.m <- bodydims[bodydims$sex==1,]
bodydims.f <- bodydims[bodydims$sex==0,]

#### 3.3
m1 <- mean(bodydims.m$hgt)
s1 <- var(bodydims.m$hgt)*(length(bodydims.m$hgt)-1/length(bodydims.m$hgt))

m2 <- mean(bodydims.f$hgt)
s2 <- var(bodydims.f$hgt)*(length(bodydims.f$hgt)-1/length(bodydims.f$hgt))

## 4
#### 4.1
# 귀무가설: 전체 통계학 실험 수강생들의 일주일 커피 소비량의 실제 평균은 11잔이다.
# 대립가설: 전체 통계학 실험 수강생들의 일주일 커피 소비량의 실제 평균은 11잔이 아니다.

#### 4.2
Z <- (11-10)/(sqrt(4.5)/sqrt(36))

#### 4.3
# 조건문 사용
is_reject <- abs(Z)>qnorm(1-0.05/2)

#### 4.4
# 귀무가설을 기각할수 있으므로, 대립가설을 채택한다. 즉, 전체 통계학 실험 수강생들의 일주일 커피 소비량의 실제 평균은 11잔이 아니다.