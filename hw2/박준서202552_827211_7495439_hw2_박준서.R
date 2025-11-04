set.seed(1) # 문제 풀기 전 실행

## 1
### 1.1
x <- seq(-3, 3, by = 0.1)
y <- (1 / sqrt(2 * pi)) * exp(-x^2 / 2)
plot(x, y, type = "l")
#벡터 생성, 식 정리, 그래프 적용
### 1.2
p1 <- sum(dbinom(150:170, size = 200, prob = 2/3))
#150부터 170까지로 설정하여 계산, 합하기 위해 sum 이용
## 2
#### 2.1
ames <- read.csv("C:/Users/jerom/Downloads/ames.csv")
#컴퓨터 경로에서 불러옴
#### 2.2
area <-  ames$Gr.Liv.Area
#$붙혀줌
#### 2.3
sample_area <- sample(area, 50)
mean_area <- mean(sample_area)
sd_area <- sd(sample_area)
n <- length(sample_area)

error <- qnorm(0.95) * sd_area / sqrt(n)
lower <- mean_area - error
upper <- mean_area + error
#표본평균, 표준편차, 표본 개수 함수로 써서 가독성, 그 후에 오차범위 지정하고 대입
#### 2.4
#조건문 사용
is_contained <- if ((mean(area)<= upper)&(mean(area)>= lower)) {print ("TRUE")}else
{print ("FALSE")}
#upper, lower을 경계로 하여서 조건문 사용, &로 연결(둘다 참)

## 3
#### 3.1
result1 <- sqrt(sum((area - mean(area))^2) / length(area))
#분산 구하는 공식적용

#### 3.2
result2 <- sqrt(sum((area - mean(area))^2) / (length(area) - 1))
#표본분산 구할때는 n-1로 나눔
#### 3.3
ratio <- result1 / result2

## 4

### 4.1
# 귀무가설 : u=247
# 대립가설 : u≠247
#양측가설임임
### 4.2
Z <- (276 - 247) / (52 / sqrt(11))
#관측값 구하는 공식에 숫자 대입함함
### 4.3
if (abs(Z) > qnorm(0.975)) {print ("기각")} else {print ("기각 불가")}
#기각할 수 없다. 따라서, 평균이 247명이 아니라고 할 수 없다.
#양측가설이라 0.05를 2로 나눠줌