set.seed(1) # 문제 풀기 전 실행
par(family="AppleGothic") # 한글 깨짐 이슈로 추가함

## 1
### 1.1
x <- seq(-3, 3, length.out = 1000)
fx <- (1/sqrt(2*pi))*exp((-x^2)/2)
plot(x, fx, type = 'l', main = '표준정규분포의 확률밀도함수')

### 1.2
# 6의 약수가 나올 확률 = 4/6
# 6의 약수가 나오는 횟수를 확률변수 X라고 하면 X는 이항분포 B(200, 4/6)을 따른다.
n <- 200
p <- 4/6

p1 <- pbinom(170, n, p) - pbinom(149, n, p)

## 2
#### 2.1
ames <- read.csv("/Users/hannahkim/Downloads/25-2 통계학실험/ames.csv")

#### 2.2
area <- ames$Gr.Liv.Area

#### 2.3
xbar <- mean(sample(area, 50))

n <- length(area)
sigma <- sqrt(var(area) * (n-1)/n)

alpha <- 0.1

lower <- xbar - qnorm(1-alpha/2) * sigma / sqrt(50)
upper <- xbar + qnorm(1-alpha/2) * sigma / sqrt(50)

#### 2.4
#조건문 사용
is_contained <- if (lower < mean(area) & mean(area) < upper) {
  TRUE
} else {
  FALSE
}

## 3
#### 3.1
result1 <- sqrt((mean(area^2) - mean(area)^2))

#### 3.2
n <- length(area)
result2 <- sqrt((mean(area^2) - mean(area)^2) * n/(n-1))

#### 3.3
ratio <- result1 / result2

## 4

### 4.1
# 귀무가설 : mu = 247
# 대립가설 : mu /= 247

### 4.2
Z <- (276-247) / (52/sqrt(11))

### 4.3
if (abs(Z) >= qnorm(1-0.025)) {
  print('기각할 수 있다')
} else {
  print('기각할 수 없다')
}
# 귀무가설을 기각할 수 없으므로 유의수준 5%에서 평균 팔로잉 수가 247명과 다르다고 볼 수 없다.