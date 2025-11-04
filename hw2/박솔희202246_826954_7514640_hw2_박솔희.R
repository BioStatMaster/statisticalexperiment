set.seed(1) # 문제 풀기 전 실행

## 1
### 1.1
x <- seq(-3,3,by=0.1)
fx <- (1/sqrt(2*pi))*exp(-x^2/2)
plot(x,fx,type='l')

### 1.2
p1 <- pbinom(170, 200, 2/3) - pbinom(150, 200, 2/3)

## 2
#### 2.1
ames <- read.csv("C://Users//Admin//Downloads//ames.csv", header=T)

#### 2.2
area <- ames$Gr.Liv.Area

#### 2.3
s<-sample(area, size=50)
lower <- mean(s) - qnorm(0.05)*sd(s)/sqrt(50)
upper <- mean(s) + qnorm(0.05)*sd(s)/sqrt(50)

#### 2.4
#조건문 사용
is_contained <- if ( (mean(area)>=lower) & (mean(area)<=upper) ) {"True"} else {"False"}
is_contained

## 3
#### 3.1
result1 <- sqrt(sum((area-mean(area))^2)/length(area))

#### 3.2
result2 <- sqrt(sum((area-mean(area))^2)/(length(area)-1))

#### 3.3
ratio <- result1/result2
ratio

## 4

### 4.1
# 귀무가설 : 서울대학교 통계학과 대학원생들의 인스타그램 팔로잉 수 평균이 247명이다.
# 대립가설 : 서울대학교 통계학과 대학원생들의 인스타그램 팔로잉 수 평균이 247명이 아니다.

### 4.2
Z <- (276-247) / (52/sqrt(11))

### 4.3
abs(Z) >= qnorm(0.975)
#기각역에 속하는지 판정한 위 코드의 값이 False이므로 귀무가설을 기각할 수 없다.