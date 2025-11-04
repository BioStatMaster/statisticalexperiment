set.seed(1) # 문제 풀기 전 실행시켜주세요.

## 1
#### 1.1
x <- seq(from=-4, to=4, by=0.01)

#### 1.2
fx1 <- 1/sqrt(2*pi)*exp(-x^2/2)

#### 1.3
plot(x, fx1, main="표준정규분포의 확률밀도함수 그래프", type='l')

## 2
#### 2.1
u <- runif(20000, min=0, max=1)

#### 2.2
fx2 <- qnorm(u)

#### 2.3
hist(fx2, breaks=30, freq=F)

lines(x, fx1, col='red')

#### 2.4
# 2.3에서 구간의 수를 30보다 늘린다.

## 3
#### 3.1
bodydims <- read.csv("/Users/kimseojin/Downloads/bodydims.csv", header=T)



#### 3.2
bodydims.m <- bodydims[bodydims$sex==1]
bodydims.f <- bodydims[bodydims$sex==0]

#### 3.3
m1 <- mean(bodydims.m$hgt)
s1 <- var(bodydims.m$hgt)*(n-1)/n

m2 <- mean(bodydims.f$hgt)
s2 <- var(bodydims.f$hgt)*(n-1)/n

## 4
#### 4.1
# 귀무가설: H0 == 11
# 대립가설: H1 != 11

#### 4.2
Z <- (10-11)/(sqrt(4.5/36))

#### 4.3
if(pnorm(Z)*2<=0.05){is_reject <- TRUE}else{is_reject<-FALSE}

#### 4.4
# 귀무가설을 기각해야 한다.
