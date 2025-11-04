set.seed(1) # 문제 풀기 전 실행

## 1
### 1.1
x <- seq(-3, 3, length.out = 100)
fx <- 1 / sqrt(2 * pi) * exp(-x^2 / 2)
plot(x, fx, type = "l", main = "Standard Normal PDF", ylab = "f(x)")

### 1.2
# 150명 이상, 170명 이하의 확률은 170명 이하 확률에서 149명 이하 확률은 뺀 것과 같음.
p1 <- pbinom(170, size= 200, prob= 2/3) - pbinom(149, size=200, prob=2/3)

## 2
#### 2.1
ames <- read.csv("ames.csv")

#### 2.2
area <- ames$Gr.Live.Area

#### 2.3
# 크기 50의 표본인 k의 평균을 가운데에 두고, +- 계산.
k <- sample(area,size=50)
lower <- mean(k) - qnorm(0.10/2)*(sd(k)/sqrt(50))
upper <- mean(k) + qnorm(0.10/2)*(sd(k)/sqrt(50))

#### 2.4
#조건문을 사용해 mean(area)가 구간 사이에 있으면 TRUE, 아니면 FALSE를 출력.
is_contained <- if (lower <= mean(area) & mean(area) <= upper){
  print("TRUE")} else {print("FALSE")}
## 3
#### 3.1
result1 <- sqrt(sum((area - mean(area))^2) / length(area))

#### 3.2
result2 <- sqrt(sum((area - mean(area))^2) / (length(area)-1))

#### 3.3
ratio <- sqrt((length(area)-1) / length(area))

## 4

### 4.1
# 귀무가설 : 실제 평균 팔로잉 수는 247명이다. (H0: mu = 247)
# 대립가설 : 실제 평균 팔로잉 수는 247명이 아니다. (H1: mu != 247)

### 4.2
x_bar <- 276
mu0 <- 247
sigma <- 52
n <- 11
Z <- (x_bar - mu0) / (sigma / sqrt(n)) # 계산 시에는 약 1.8505 정도의 값이 나옴.

### 4.3
alpha <- 0.05
z_critical <- qnorm(1 - alpha/2) # 임계값 z_critical 값을 계산
                                 # 양측검정이므로 alpha를 2로 나눔.
                                 # z_critical은 약 1.95996으로 계산됨.
                                 # Z 값: 약 1.8505 (문제 4.2에서 계산됨)

# 결론: 검정통계량 Z의 절대값(약 1.8085)이 임계값(약 1.96)보다 작으므로,
# 귀무가설 H0: mu = 247을 기각하지 못한다.
# 즉, 유의수준 5% 하에서, 서울대학교 통계학과 대학원생들의
# 실제 평균 팔로잉 수가 247명이 아니라고 할 만한 통계적 증거가 부족하다.