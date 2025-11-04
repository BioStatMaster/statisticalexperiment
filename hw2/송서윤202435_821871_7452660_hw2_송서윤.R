set.seed(1) # 문제 풀기 전 실행

## 1
### 1.1
x <- seq(-3,3,by=0.01) #범위 설정
f_x <- (1/sqrt(2*pi))*exp(-x^2/2) #확률밀도함수 식 작성
plot(x,f_x,type="l",main="Standard Normal PDF", xlab="x", ylab="f(x)")

### 1.2
p1 <- sum(dbinom(150:170,size=200, prob=2/3))

## 2
#### 2.1
ames <- read.csv("C:/Users/Administrator/Downloads/ames.csv",header=TRUE)

#### 2.2
area <- ames$Gr.Liv.Area

#### 2.3
pop.mean <- mean(area)
pop.sd <- sd(area)
n.sample <- 50
sample.data <- sample(area,n.sample)
sample.mean <- mean(sample.data)
sample.sd <- sd(sample.data)
alpha <- 0.1
lower <- sample.mean-qnorm(1-alpha/2)*(sample.sd/sqrt(n.sample))
upper <- sample.mean+qnorm(1-alpha/2)*(sample.sd/sqrt(n.sample))

#### 2.4
#조건문 사용
if(pop.mean >= lower & pop.mean <= upper) {
  is_contained <- TRUE
} else {
  is_contained <- FALSE
}

## 3
#### 3.1
result1 <- sqrt(sum((area-mean(area))^2)/length(area))

#### 3.2
result2 <- sqrt(sum((area-mean(area))^2)/length(area)-1)

#### 3.3
ratio <- result1/result2

## 4

### 4.1
# 귀무가설 : 서울대 통계학과 대학원생의 평균 팔로잉 수 u=247
# 대립가설 : 서울대 통계학과 대학원생의 평균 팔로잉 수 u≠247

### 4.2
mu0 <- 247
xbar <- 276
sigma <- 52
n <- 11
alpha <- 0.05
Z <- (xbar-mu0)/(sigma/sqrt(n))

### 4.3
if (abs(Z)>qnorm(1-alpha/2)) {
  print("False")
} else {
  print("True")
}
#귀무가설을 기각할 수 없게 되었으므로, 서울대 통계학과 대학원생의 평균 팔로잉 수가 247명이라고 말할 수 있다.