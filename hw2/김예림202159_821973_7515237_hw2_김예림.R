set.seed(1) # 문제 풀기 전 실행

## 1
### 1.1
x<-c(-3:3)
fx<- (1/sqrt(2*pi))*exp(-x*x/2)
plot(x,fx,type='h',xlim=c(-3,3))

### 1.2
p1 <- pbinom(170,200,4/6)-pbinom(149,200,4/6)

## 2
#### 2.1
ames <- read.csv("C:/Users/PCuser/Downloads/ames.csv")


#### 2.2
area <- ames$Gr.Liv.Area

#### 2.3
lower <- mean(sample(area,50))+qnorm(0.05)*(sd(area)/sqrt(50))
upper <- mean(sample(area,50))-qnorm(0.05)*(sd(area)/sqrt(50))

#### 2.4
#조건문 사용
is_contained <- if ((lower<mean(area))&(mean(area)<upper)) {"TRUE"} else {"FALSE"}

## 3
#### 3.1
result1 <- sqrt(sum(((area-mean(area))^2))/length(area))

#### 3.2
result2 <-  sqrt(sum(((area-mean(area))^2))/(length(area)-1))

#### 3.3
ratio <- result1/result2

## 4

### 4.1
# 귀무가설 : 평균 팔로잉 수가 247명이다.
# 대립가설 : 평균 팔로잉 수가 247명이 아니다.

### 4.2
Z <- (276-247)/(52/sqrt(11))

### 4.3
if(abs(Z)>qnorm(0.975)) {"귀무가설 기각"} else {"귀무가설을 기각할 수 없다."}
#귀무가설을 기각할 수 없다.
