set.seed(1) # 문제 풀기 전 실행

## 1
### 1.1
x <- seq(from=-3, to=3, by=0.01)
fx1 <- exp(-x**2 / 2) / sqrt(2*pi)
plot(x, fx1, xlim = c(-3,3), type='l')

### 1.2
p1 <- pbinom(170, size = 200, prob = 4/6) - pbinom(149, size = 200, prob = 4/6)
# 6의 약수는 1,2,3,6이니까 4/6



## 2
#### 2.1
ames <- read.csv("C:/Users/ss/Downloads/ames.csv")

#### 2.2
area <- ames$Gr.Liv.Area

#### 2.3
lower <- { n <- 50; xbar <- mean(sample(area, n)); sigma <- sqrt(sum((area - mean(area))^2) / length(area)); xbar - qnorm(0.95) * (sigma / sqrt(n)) }
upper <- lower + 2 * qnorm(0.95) * ( sqrt(sum((area - mean(area))^2) / length(area)) / sqrt(50) )

#### 2.4
#조건문 사용
is_contained <- if (lower <= mean(area) & mean(area) <= upper) TRUE else FALSE


## 3
#### 3.1

result1 <- sqrt( sum( (area -  mean(area))^2 ) / length(area) )


#### 3.2
result2 <- sqrt( sum( (area - mean(area))^2 ) / (length(area) - 1) )
result2

#### 3.3
ratio <- result1 / result2

## 4

### 4.1
# 귀무가설 : 서울대학교 통계학과 대학원생들의 평균 인스타그램 팔로잉 수는 276명이다.
# 대립가설 : 서울대학교 통계학과 대학원생들의 평균 인스타그램 팔로잉 수는 276명이 아니다.

### 4.2
Z <- (276 - 247) / (52 / sqrt(11))


### 4.3
is_reject <- if (abs(Z) > qnorm(1-0.05/2)) TRUE else FALSE
# False. 서울대학교 통계학과 대학원생들의 평균 인스타그램 팔로잉 수는 276명이 아니다.

