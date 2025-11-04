set.seed(1) # 문제 풀기 전 실행시켜주세요.

## 1
#### 1.1
x <- seq(-4, 4, by=0.1)     #시퀀스 함수를 이용해 벡터 생성성

#### 1.2
fx1 <- (1/sqrt(2*pi)*exp(-x^2/2))   #식에 맞게 숫자를 대입하되, exp 이용용

#### 1.3
plot(x, fx1, type="l", main="확률밀도함수그래프")  #type를 l로 해서 선 모양 생성성

## 2
#### 2.1
u <- runif(20000, max=1, min=0)   #랜덤표본 20000개, 그 범위를 runif로 설정정

#### 2.2
fx2 <- qnorm(u)   #각 표본에 대해 분위수값 구함(qnorm)

#### 2.3
hist(fx2, breaks=30, freq=FALSE, main="히스토그램")  #break로 구간 수, freq=f로
#y축을 밀도로 함함

#### 2.4
# 표본의 크기를 더 크게한다

## 3
#### 3.1
bodydims <- read.csv("bodydims.csv")   #파일을 불러오고 열려고 했으나, 잘 열리지 않네요...

#### 3.2
bodydims.m <- subset(bodydims, sex==1)   #남성, 여성의 자료를 각각 배정정
bodydims.f <- subset(bodydims, sex==0)

#### 3.3
m1 <- mean(bodydims.m$hgt)   #평균균
s1 <- var(bodydims.m$hgt*((nrow(bodydims.m)-1)/nrow(bodydims.m)))  #n-1/n을 해야함함

m2 <- mean(bodydims.f$hgt)
s2 <- var(bodydims.f$hgt*((nrow(bodydims.f)-1)/nrow(bodydims.f)))

## 4
#### 4.1
# 귀무가설: u=11
# 대립가설: u≠11       양측가설임임

#### 4.2
Z <- (10-11)/sqrt(4.5/36)   #(표본평균-모평균)/루트(분산/표본 수)

#### 4.3
# 조건문 사용
is_reject <- if (abs(Z)>=qnorm(1-0.05/2)) {print ("TRUE")} else {print ("FALSE")}
#양측가설이므로 절댓값 사용, 알파/2로 해줌줌
#### 4.4
# Z값이 기각역에 속하므로, 귀무가설을 유의수준 5%에서 기각할 수 있다. 
# 따라서, 소비량의 평균이 11잔이 아니라고 할 수 있다.