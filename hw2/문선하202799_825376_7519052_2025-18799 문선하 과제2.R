set.seed(1) # 문제 풀기 전 실행

## 1
### 1.1
x <- seq(-3, 3, by = 0.1)
y <- (1 / sqrt(2 * pi)) * exp(-x^2 / 2)
plot(x, y, type = "l")

### 1.2
p1 <- sum(dbinom(150:170, size = 200, prob = 2/3))

## 2
#### 2.1
ames <- read.csv("C:/Users/munsu/Downloads/ames.csv", header=T)

#### 2.2
area <- ames$Gr.Liv.Area

#### 2.3
lower <- mean(sample(area, 50)) - qnorm(0.95) * sd(sample(area, 50)) / sqrt(50)
upper <- mean(sample(area, 50)) + qnorm(0.95) * sd(sample(area, 50)) / sqrt(50)

#### 2.4
#조건문 사용
is_contained <- if ((mean(area)<= upper)&(mean(area)>= lower)) {TRUE} else
{FALSE}

## 3
#### 3.1
result1 <- sqrt(sum((area - mean(area))^2) / length(area))

#### 3.2
result2 <- sqrt(sum((area - mean(area))^2) / (length(area) - 1))

#### 3.3
ratio <- result1 / result2

## 4

### 4.1
# 귀무가설 : u=247
# 대립가설 : u≠247

### 4.2
Z <- (276 - 247) / (52 / sqrt(11))

### 4.3
if (abs(Z) > qnorm(0.975)) {print ("기각")} else {print ("기각 불가")}
# 기각할 수 없다. 따라서 평균을 247명이라고 할 수 없다. 