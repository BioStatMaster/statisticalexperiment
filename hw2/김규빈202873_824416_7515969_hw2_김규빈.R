set.seed(1) # 문제 풀기 전 실행

## 1
### 1.1
xrange <- seq(from = -3, to = 3, by = 0.01) # x축 범위
yrange <- (1 / sqrt(2 * pi)) * exp(-(xrange^2 / 2)) # y축 범위
plot(xrange, yrange, type = "l", xlab = "x", ylab = "y") # 그래프 그리기


### 1.2
n <- 200 # 시행 횟수
p <- 2/3 # 6의 약수가 나올 확률
p1 <- pbinom(170, size = n, prob = p) - pbinom(149, size = n, prob = p) 
#  6의 약수가 150번 이상 170번 이하로 나올 확률
p1


## 2
#### 2.1
ames <- read.csv("C:/Users/binis/Downloads/ames.csv") # ames 파일 불러오기

#### 2.2
area <- ames$Gr.Liv.Area # ames에서 Gr.Liv.Area 파일을 추출하여 area에 저장장

#### 2.3
area.sample <- sample(area, 50) #area에서 크기 50인 표본 추출
mu <- mean(area.sample) #표본평균
alpha <- 0.1 # 유의수준
n <- 50 # 표본 크기
sd <- sqrt(var(area) * (n-1)/n) # 모표준편차
lower <- mu - qnorm(1 - alpha/2) * sd / sqrt(n) # 신뢰구간의 하한선 
upper <- mu + qnorm(1 - alpha/2) * sd / sqrt(n) # 신뢰구간의 상한선 

#### 2.4
#조건문 사용
is_contained <- if((lower < mu) & (mu < upper)) {print("TRUE")} else {print ("False")}
# 모평균이 신뢰구간 안에 포함되는지 확인

## 3
#### 3.1
result1 <- sqrt(sum((area - mean(area))^2) / length(area)) # area가 모집단일 때 표준편차 

#### 3.2
result2 <- sqrt(sum((area - mean(area))^2) / (n - 1)) # area가 표본일 때 표준편차

#### 3.3
ratio <- result1 / result2 # result1 의 result2 에 대한 비에 대응하는 비율
ratio


## 4

### 4.1
# 귀무가설 : 서울대학교 통계학과 대학원생들의 실제 평균 인스타그램 팔로워 수는 247명이다.
# 대립가설 : 서울대학교 통계학과 대학원생들의 실제 평균 인스타그램 팔로워 수는 247명이 아니다.

### 4.2
sigma <- 52    # 모집단의 표준편차
n <- 11        # 표본 크기
mu <- 276   # 표본 평균
m <- 247    # 귀무가설상 평균
alpha <- 0.05  # 유의수준
Z <- (mu - m) / (sigma / sqrt(n))

### 4.3
limit <- qnorm(1-alpha / 2)
limit

# 기준선(limit)인 1.959964는 검정통계량 값 1.849656보다 크므로, 귀무가설을 기각하지 못한다.
# 즉, 서울대학교 통계학과 대학원생들의 실제 평균 인스타그램 팔로워 수가 247명이 아니라고 단정할 수 없다. 