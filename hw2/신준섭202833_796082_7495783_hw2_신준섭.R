set.seed(1) # 문제 풀기 전 실행

## 1
### 1.1
x <- seq(from=-3, to=3, by=0.1)
fx <- 1/sqrt(2*pi)*exp(-x^2/2)
plot(x, fx, type="l", main="표준정규분포 확률밀도함수")

### 1.2
p1 <- pbinom(170, size=200, prob=2/3)-pbinom(150, size=200, prob=2/3)

## 2
#### 2.1
ames <- read.csv("C:/Users/sjs27/Downloads/ames.csv", header=T)

#### 2.2
area <- ames$Gr.Liv.Area

#### 2.3
r <- sample(area, 50, replace=F)
lower <- qnorm(0.05, mean(r), sd(r))
upper <- qnorm(0.95, mean(r), sd(r))

#### 2.4
#조건문 사용
is_contained <- 1 == if(lower<=mean(area) & mean(area)<=upper) {print(1)} else {print(0)}

## 3
#### 3.1
v <- 0
for (i in area){
  v <- v + abs(i-mean(area))
}
result1 <- sqrt(v)

#### 3.2
n <- length(area)
result2 <- sqrt(v/(n-1)) 

#### 3.3
ratio <- result1/result2

## 4

### 4.1
# 귀무가설 : 실제 평균 팔로잉 수가 247명이다.
# 대립가설 : 실제 평균 팔로잉 수가 247명이 아니다.

### 4.2
Z <- (276-247)/(52/sqrt(11))

### 4.3
qnorm(0.975) <= Z
#실행값이 FALSE이므로 귀무가설을 기각할 수 없다. 따라서 유의수준 5% 하에서 팔로잉 수의 평균은 247명이라고 할 수 있다.