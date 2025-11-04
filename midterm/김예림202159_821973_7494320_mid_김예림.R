set.seed(1) # 문제 풀기 전 실행시켜주세요.

## 1
#### 1.1
x <- seq(from=-4, to=4, by=0.01)


#### 1.2
fx1 <- (1/sqrt(2*pi))*exp(-x*x/2)

#### 1.3
plot(x,fx1,type='h',xlim=c(-4,4))

## 2
#### 2.1
u <- runif(20000,0,1)

#### 2.2
fx2 <- dnorm(u,0,1)

#### 2.3
hist(fx2,breaks= 30,probability=T)

#### 2.4
# 랜덤 표본의 개수가 증가하면 정규분포에 더 가까워져 빨간 선에 더 가까워질 수 있다.

## 3
#### 3.1
bodydims <- read.table("C:/Users/PCuser/Downloads/bodydims.csv",header=T)

#### 3.2 
bodydims.m <- bodydims[bodydims$sex==1,]
bodydims.f <- bodydims[bodydims$sex==0,]

#### 3.3
m1 <- mean(bodydims.m$hgt)
s1 <- var(bodydims.m$hgt)*(len(bodydims.m)+1)/(len(bodydims.m))

m2 <- mean(bodydims.f$hgt)
s2 <- var(bodydims.f$hgt)*(len(bodydims.f)+1)/(len(bodydims.f))

## 4
#### 4.1
# 귀무가설: 전체 통계학 실험 수강생들의 일주일 커피 소비량은 11잔이다.
# 대립가설: 전체 통계학 실험 수강생들의 일주일 커피 소비량은 11잔이 아니다.

#### 4.2
Z <- (10-11)/sqrt(4.5/36)

#### 4.3
# 조건문 사용
is_reject <- if (pnorm(z)<0.05) {TRUE} else {FALSE}

#### 4.4
# 전체 통계학 실험 수강생들의 일주일 커피 소비량의 실제 평균은 11잔이 아니다.