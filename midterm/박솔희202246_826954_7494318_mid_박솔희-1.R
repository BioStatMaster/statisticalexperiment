set.seed(1) # 문제 풀기 전 실행시켜주세요.

## 1
#### 1.1
x <- seq(from=-4,to=4,by=0.01)

#### 1.2
fx1 <- (1/sqrt(2*pi))*exp(-x^2/2)

#### 1.3
plot(x,fx1,type='l')

## 2
#### 2.1
u <- runif(20000, min=0,max=1)

#### 2.2
fx2 <- qnorm(u)

#### 2.3
hist(fx2,breaks=30,probability=T)

#### 2.4
# 표본의 크기를 늘리면 된다. 즉 u에 runif(3000,min=0,max=1)을 할당하는 등의 방법을 사용할 수 있다.

## 3
#### 3.1
bodydims <- read.csv("C://Users//Admin//Downloads//bodydims.csv", header=T) 

#### 3.2
bodydims.m <- bodydims[bodydims$sex==1,]
bodydims.f <- bodydims[bodydims$sex==0,]

#### 3.3
n1 <- nrow(bodydims.m)
m1 <- mean(bodydims.m$hgt)
s1 <- ((n1-1)/n1)*var(bodydims.m$hgt)

n2 <- nrow(bodydims.f)
m2 <- mean(bodydims.f$hgt)
s2 <- ((n2-1)/n2)*var(bodydims.f$hgt)

## 4
#### 4.1
# 귀무가설: 전체 통계학 실험 수강생들의 중간고사 기간 커피 소비량의 평균은 11잔이다.
# 대립가설: 전체 통계학 실험 수강생들의 중간고사 기간 커피 소비량의 평균은 11잔이 아니다.

#### 4.2
Z <- (10-11)/sqrt(4.5/36)

#### 4.3
# 조건문 사용
is_reject <- if (abs(Z)>=qnorm(0.975)) {"TRUE"} else {"FALSE"}

#### 4.4
# 검정통계량의 절댓값이 0.975분위수보다 크므로 통계적으로 유의하고, 따라서 귀무가설은 기각될 것이다. 