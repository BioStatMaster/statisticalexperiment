set.seed(1) # 문제 풀기 전 실행시켜주세요.

## 1
#### 1.1
x <- c(seq(-4,4,by=0.01))

#### 1.2
fx1 <- (1/sqrt(2*pi))*exp(-x^2/2)

#### 1.3
plot(x,fx1,type="l",xlim=c(-4,4),main="표준정규분포의 확률밀도함수")

## 2
#### 2.1
u <- runif(20000,0,1)

#### 2.2
fx2 <- qnorm(u,0,1)

#### 2.3
hist(fx2, breaks=30, ylab="각 구간의 확률밀도")

#### 2.4
# 구간의 수를 늘린다.

## 3
#### 3.1
bodydims <- read.csv("bodydims.csv")

#### 3.2
bodydims.m <- bodydims[(bodydims$sex==1),]
bodydims.f <- bodydims[(bodydims$sex==0),]

#### 3.3
m1 <- mean(bodydims.m$hgt)
s1 <- ((length(bodydims.m$hgt)-1)/length(bodydims.m$hgt))*var(bodydims.m$hgt)

m2 <- mean(bodydims.f$hgt)
s2 <- ((length(bodydims.f$hgt)-1)/length(bodydims.f$hgt))*var(bodydims.f$hgt)

## 4
#### 4.1
# 귀무가설: mu = 11 (전체 통계학 실험 수강생들의 일주일 커피 소비량의 실제 평균은 11이다.)
# 대립가설: mu != 11 (전체 통계학 실험 수강생들의 일주일 커피 소비량의 실제 평균은 11이 아니다.)

#### 4.2
Z <- (10-11)/sqrt(4.5/36)

#### 4.3
# 조건문 사용
is_reject <- if(Z<=qnorm(0.025)){
  "TRUE"} else{
    "FALSE"}

#### 4.4
# 귀무가설을 기각한다. (전체 통계학 실험 수강생들의 일주일 커피 소비량의 실제 평균은 11잔이 아니다.)