set.seed(1) 

## 1
### 1.1
x <- seq(-3, 3, length.out = 1000)
fx <- (1 / sqrt(2 * pi)) * exp(-(x^2) / 2)
plot(x, fx, type= 'l', main = '표준 정규분포 확률밀도함수')

### 1.2
p_170 <- pbinom(170, size=200, prob=2/3)
p_149 <- pbinom(149, size=200, prob=2/3)

p1 <- p_170 - p_149
p1

## 2
#### 2.1
ames <- read.csv("C:/Users/HOME/Downloads/ames.csv", header=T)

#### 2.2
area <- ames$Gr.Liv.Area

#### 2.3
set.seed(1)

sample_50 <- sample(area, 50)
x_bar <- mean(sample_50)

n <- length(area)
var_p <- var(area) * ((n-1)/n)

alpha <- 0.1

lower <- x_bar - qnorm(1 - alpha/2) * sqrt(var_p/50)
upper <- x_bar + qnorm(1 - alpha/2) * sqrt(var_p/50)

#### 2.4
#조건문 사용
mu <- mean(area)

if (mu > lower & mu < upper) {
  is_contained <- TRUE
} else {
  is_contained <- FALSE
}

## 3
#### 3.1
result1 <- sqrt( sum((area - mu)^2) / n )

#### 3.2
xbar <- mean(area) #area를 표본으로 가정했을 때 표본평균
result2 <- sqrt( sum( (area - xbar)^2 ) / (n - 1) ) 

#### 3.3
ratio <- result1 / result2

## 4

### 4.1
# 귀무가설 : μ = 247
# 대립가설 : μ ≠ 247

### 4.2
Z <- (276 - 247) / (52 / sqrt(11))
Z

### 4.3
alpha2 <- 0.05
z_alpha <- qnorm(1 - (alpha2/2))

if (abs(Z) >= z_alpha) {
  result <- "귀무가설 기각"
} else {
  result <- "귀무가설 채택"
}
result

# 검정통계량 z = 1.85로, 이는 유의수준 5%에서의 기각역에 속하지 않으므로
# 귀무가설(H0: μ = 247)을 기각할 수 없다.
# 따라서, 실제 평균 팔로잉 수가 247명과 통계적으로 유의하게 다르다고 보기 어렵다.