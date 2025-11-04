set.seed(1) # 문제 풀기 전 실행시켜주세요.

## 1
#### 1.1
x <- c(seq(from=-4,to=4,by=0.01))

#### 1.2
fx1 <- (1/sqrt(2*pi))*exp(-(x^2)/2)

#### 1.3
plot(x,fx1,xlim=c(-4,4),type="l")

#### 2
#### 2.1
u <- runif(20000,min=0,max=1)

#### 2.2
fx2 <- qnorm(u,mean=0,sd=1)

#### 2.3
hist(fx2,breaks=30,freq=F)

#### 2.4
# n(20000)을 늘릴 수 있을만큼 늘린다

## 3
#### 3.1
bodydims <- read.csv("bodydims.csv",header=T)
bodydims
#### 3.2
bodydims.m <- 
bodydims.f <- 

#### 3.3
m1 <- mean(bodydims.m$hgt)
s1 <- var(bodydims.m$hgt)

m2 <- mean(bodydims.f$hgt)
s2 <- var(bodydims.f$hgt)

## 4
#### 4.1
# 귀무가설: H_0 = 11
# 대립가설: H_1 ~= 11

#### 4.2
Z <- qnorm(0.025,0,1)

#### 4.3
# 조건문 사용
is_reject <- if(((10-11)/(sqrt(4.5)/sqrt(36)))<Z) {"TRUE"} else {"FALSE"}
is_reject
#### 4.4
# is_reject에는 TRUE가 저장되었다. 이는 검정통계량이 Z값보다 작아 기각역에 속하기
# 때문이다. 따라서 해당 가설검정은 통계적으로 유의하며
# 귀무가설을 기각하고 대립가설을 채택해야한다
# 따라서 전체 통계학 실험 수강생들의 
# 일주일 커피 소비량의 실제 평균은 11잔이 아니라고 할 수 있다.