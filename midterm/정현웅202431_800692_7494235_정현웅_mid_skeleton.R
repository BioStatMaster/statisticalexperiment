set.seed(1) # 문제 풀기 전 실행시켜주세요.

## 1
#### 1.1
x <- seq(from=-4, to=4, by=0.01)

#### 1.2
fx1 <- (1/sqrt(2*pi))*exp(-x^2/2) 

#### 1.3
plot(x,fx1,xlim=c(-4,4),type='l')

## 2
#### 2.1
u <- runif(20000,min=0,max=1)

#### 2.2
TODO

#### 2.3
hist(fx2,breaks=seq(max(fx2),min(fx2),length.out=31),protability=T)

#### 2.4
# TODO

## 3
#### 3.1
bodydims <- read.csv("C:/Users/유빈/Downloads/bodydims (2).csv",header=T)

#### 3.2
bodydims.m <- bodydims[bodydims$sex==1,]
bodydims.f <- bodydims[bodydims$sex==0,]

#### 3.3
m1 <- mean(bodydims.m$hgt)
s1 <- var(bodydims.m$hgt)*(length(bodydims.m$hgt)-1)/length(bodydims.m$hgt)

m2 <- mean(bodydims.f$hgt)
s2 <- var(bodydims.f$hgt)*(length(bodydims.f$hgt)-1)/length(bodydims.f$hgt)

## 4
#### 4.1
# 귀무가설: 평균 커피 소비량은 11잔이다
# 대립가설: 평균 커피 소비량은 10잔이다

#### 4.2
z <- (10-11)/(sqrt(4.5)/6)

#### 4.3
# 조건문 사용
is_reject <- if ((pnorm(z,0,1))<0.05) {print("True")} else { print("False")}

#### 4.4
#검정결과가 유의하므로 귀무가설은 기각된다.
#평균 커피 소비량이 11잔보다 적음 