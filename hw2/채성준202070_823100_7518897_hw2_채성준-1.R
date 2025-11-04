set.seed(1) # 문제 풀기 전 실행

## 1
### 1.1
x<-seq(from=-3,to=3,by=0.1) # x 값 지정
fx<-(1/sqrt(2*pi))*exp(-x^2/2) # 주어진 식을 활용해 fx값 지정
plot(x,fx,xlim=c(-3,3),type="l") # plot 함수 사용해 그래프 그리기
# type="l"로 지정하여 선 형태가 나오도록 유도

### 1.2
p1 <- pbinom(170,200,2/3) - pbinom(149,200,2/3)
# 누적확률을 구하는 pbinom 함수를 사용, 170까지의 누적확률에서 149까지의 
# 누적확률을 빼 150~170 사이의 확률을 구함.

## 2
#### 2.1
ames <- read.csv("C:/Users/성준/Downloads/ames.csv", header=T)
# read.csv 사용해 파일 불러온뒤 ames에 지정

#### 2.2
area <- ames$Gr.Liv.Area
# $를 사용해 ames 안의 Gr.Liv.Area를 변수 area에 지정

#### 2.3
xbar <- mean(sample(area,50))
sigma <- sd(sample(area,50))
n <- 50
# 편의를 위해 미리 표본평균, 표준편차, n값을 구하고 지정

lower <- xbar - qnorm(0.95)*(sigma/sqrt(n))
upper <- xbar + qnorm(0.95)*(sigma/sqrt(n))
# 신뢰구간 공식에 따라 상한과 하한을 계산 후 각 변수에 지정
# 90% 신뢰구간이므로 qnorm(0.95) 사용

#### 2.4
#조건문 사용
if( (lower<=mean(area)) & (mean(area)<=upper) ) {is_contained<-TRUE} else 
  {is_contained<-FALSE} # 모평균이 lower과 upper 사이에 존재한다면 변수를 TRUE
# 그렇지 않다면 변수를 FALSE로 지정
is_contained # 확인용 코드

## 3
#### 3.1
mu <- mean(area) # 편의상 모평균을 변수 mu로 지정
num <- length(area)

result1 <- sqrt(sum((area-mu)^2)/num)
# 모집단의 표준편차 구하는 과정에 따라 계산

#### 3.2
Xbar <- mean(area)
num <- length(area)
result2 <- sqrt(sum((area-Xbar)^2)/num)
# area가 표본으로 주어졌으므로 표본평균으로 계산하여 마찬가지로 표준편차 구함

#### 3.3
ratio <- result1/result2 # 비율 계산

## 4

### 4.1
# 귀무가설 : mu=247
# 대립가설 : mu≠247

### 4.2
Z <- (276 - 247) / (52 / sqrt(11))
# 검정통계량 구하는 공식에 해당되는 수 대입해서 계산

### 4.3
qnorm(0.975)
# Z < qnorm(0.975) 이므로 기각 불가능
# 평균이 247명이라 할 수 없다.