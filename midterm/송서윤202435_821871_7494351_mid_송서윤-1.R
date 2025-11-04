set.seed(1) # 문제 풀기 전 실행시켜주세요.

## 1
#### 1.1
x <- seq(from=-4,to=4,by=0.01)

#### 1.2
fx1 <- (1/sqrt(2*pi))*exp(-x^2/2)

#### 1.3
plot(x,fx1,xlim=c(-4,4))

## 2
#### 2.1
u <- runif(20000,min=0,max=1)

#### 2.2
fx2 <- qnorm(u,mean=0,sd=1)

#### 2.3
hist(fx2,breaks=30,freq=FALSE)

#### 2.4
# 표본의 크기를 20000보다 크게 하면 큰 수의 법칙에 의해 히스토그램이 빨간선에 더 가깝게 수렴하게 된다.

## 3
#### 3.1
bodydims <- read.csv("C:/Users/Administrator/Downloads/bodydims.csv")

#### 3.2
bodydims.m <- bodydims[bodydims$sex==1,]
bodydims.f <- bodydims[bodydims$sex==0,]

#### 3.3
m1 <- mean(bodydims.m$hgt)
s1 <- var(bodydims.m$hgt)*(length(bodydims.m)-1/length(bodydims.m))

m2 <- mean(bodydims.f$hgt)
s2 <- var(bodydims.f$hgt)*(length(bodydims.f)-1/length(bodydims.f))

## 4
#### 4.1
# 귀무가설: 전체 통계학 실험 수강생들의 일주일 커피 소비량의 실제 평균은 11잔이다. u=11
# 대립가설: 전체 통계학 실험 수강생들의 일주일 커피 소비량의 실제 평균은 11잔이 아니다. u≠11

#### 4.2
x_bar<-10
mu0<-11
sigma<-sqrt(4.5)
n<-36
Z <- (x_bar-mu0)/(sigma/sqrt(n))

#### 4.3
# 조건문 사용
alpha<-0.05
if (abs(Z)>=qnorm(1-alpha/2)){
  is_reject <- TRUE} else {
  is_reject <- FALSE
}

#### 4.4
# 검정통계량 Z의 절댓값이 유의수준 하에서 양측검정의 기각역보다 크므로 귀무가설이 기각된다. 따라서 통계학 실험 수강생들의 일주일 커피 소비량의 실제 평균이 11잔이라고 말할 수 없다. 