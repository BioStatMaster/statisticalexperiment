set.seed(1) # 문제 풀기 전 실행

## 1
### 1.1
x <- seq(from=-3, to=3, by =0.1)
fx <- {1/sqrt(2*pi)}*exp(-x^2/2)
plot(x,fx,type="l")

### 1.2
# 해당 시행은 횟수가 200, 성공확률이 2/3인 독립시행이다.
# 또한 P(150 <= X <= 170)은 P(X <= 170) - P(X <= 149)와 같으므로 아래와 같이 계산할 수 있다.
p1 <- pbinom(170, size = 200, prob = 2/3) - pbinom(149, size = 200, prob = 2/3)


## 2
#### 2.1
ames <- read.csv("C:/Users/ceramics/OneDrive/Desktop/ames.csv")

#### 2.2
area <- ames$Gr.Liv.Area

#### 2.3
n <- length(area)
SD <- sqrt(var(area)*{(n-1)/n}) # R은 분산 함수가 표본분산을 기준으로 설정되어 있으므로, n-1/n을 곱해줘 모분산의 형태로 변형해준다.

area.sample <- sample(area,50)
z <- qnorm(0.95)
lower <- mean(area.sample) - z*(SD/sqrt(50))
upper <- mean(area.sample) + z*(SD/sqrt(50))

#### 2.4
#조건문 사용
if (mean(area)>=lower & mean(area)<=upper) {
  is_contained <- TRUE} else {
    is_contained <- FALSE
    }


## 3
#### 3.1
N <- length(area)
sum_deviation1 <- sum((area - mean(area))^2)
area_variance <- sum_deviation1 / N # 모분산은 편차의 제곱의 합을 n으로 나눈다.
result1 <- sqrt(area_variance)

#### 3.2
sample_variance <- sum_deviation1 / (N-1) # 표본분산은 편차의 제곱의 합을 n-1로 나눈다.
result2 <- sqrt(sample_variance)

#### 3.3
ratio <- result1 / result2 # 두 값의 비율은 n-1/n이다.


## 4

### 4.1
# 귀무가설: 서울대학교 통계학과 대학원생들의 인스타그램 팔로잉 수 평균은 247명이다.
# 대립가설: 서울대학교 통계학과 대학원생들의 인스타그램 팔로잉 수 평균은 247명이 아니다.

### 4.2
Z <- (276-247) / (52/sqrt(11))

### 4.3
print(Z)
# 기각역은 Z > 1.96이다. Z값을 출력해보면 그 값은 약 1.85로 기각역에 속하지 않는다. 따라서 귀무가설을 기각할 수 없다.
# 결론적으로, 유의수준 5% 하에서 실제 평균 팔로잉 수가 247명이 아니라고 할 수 없다.