set.seed(1) # 문제 풀기 전 실행

## 1
### 1.1
x <- seq(-3, 3, 0.1)
fx <- 1/sqrt(2*pi)*exp(-x^2/2)
plot(x, fx, main="표준 정규분포의 확률밀도함수의 그래프", type="l")

### 1.2
p1 <- pbinom(170,size=200,prob=2/3)-pbinom(149,size=200,prob=2/3)

## 2
#### 2.1
ames <- read.csv("/Users/kimseojin/Downloads/ames.csv", header=T)

#### 2.2
area <- ames$Gr.Liv.Area

#### 2.3
s <- sample(area, size=50)
n <- length(area)
sigma <- sqrt(var(area)*(n-1)/n)
lower <- mean(s)+qnorm(0.05, 0, 1)*sigma/sqrt(50)
upper <- mean(s)-qnorm(0.05, 0, 1)*sigma/sqrt(50)

#### 2.4
if((lower<=mean(area))&(upper>=mean(area))){is_contained<-TRUE} else{is_contained<-FALSE}

## 3
#### 3.1
result1 <- sqrt(sum((area-mean(area))^2)/n)

#### 3.2
result2 <- sqrt(sum((area-mean(area))^2)/(n-1))

#### 3.3
ratio <- result1/result2

## 4

### 4.1
# 귀무가설 : H0: 평균==247
# 대립가설 : H1: 평균!=247

### 4.2
Z <- (276-247)/(52/sqrt(11))

### 4.3
# 귀무가설을 기각할 수 없다.
if (abs(qnorm(0.025))<abs(Z)) {reject<-TRUE} else {reject<-FALSE}

    
