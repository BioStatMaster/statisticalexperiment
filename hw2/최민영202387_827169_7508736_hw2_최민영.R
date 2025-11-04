set.seed(1) # 문제 풀기 전 실행

## 1
### 1.1
x <- seq(-3, 3, by = 0.01)
fx <- (1/sqrt(2*pi)) * exp(-x^2/2)
plot(x, fx)

### 1.2
p1 <- pbinom(170, size=200, prob=2/3)-pbinom(149, size=200, prob=2/3)

## 2
#### 2.1
ames <- read.csv("C:/Users/mchoi/Desktop/ames.csv")

#### 2.2
area <- ames$Gr.Liv.Area

#### 2.3
sample_size <- 50
area.sample <- sample(area, sample_size)
sample_mean <- mean(area.sample)

alpha <- 0.1
n <- length(area)
mu <- mean(area)
sigma <- sqrt(var(area) * (n-1)/n)

z <- -qnorm(alpha/2)

lower <- sample_mean - z*(sigma/sqrt(sample_size))
upper <- sample_mean + z*(sigma/sqrt(sample_size))

#### 2.4
#조건문 사용
is_contained <- if ( (lower <= mu)&(mu <= upper) ) {TRUE} else {FALSE}

## 3
#### 3.1
N <- length(area)
mean_area <- mean(area)
sum_devsq <- sum((area-mean_area)^2)

result1 <- sqrt(sum_devsq/N)

#### 3.2
n <- length(area)
mean_area <- mean(area)
sum_devsq <- sum((area-mean_area)^2)

result2 <- sqrt(sum_devsq/(n-1))

#### 3.3
ratio <- result1/result2

## 4

### 4.1
# 귀무가설 : mu = 247
# 대립가설 : mu != 247

### 4.2
alpha <- 0.05
mu <- 247
x_bar <- 276
n <- 11
sigma <- 52

Z <- (x_bar - mu)/( sigma/sqrt(n) )

### 4.3
z_crit <- qnorm(1-alpha/2)
if (abs(Z)>=z_crit) {print("귀무가설을 기각한다")} else {print("귀무가설을 기각할 수 없다")}

# 가설검정 결과에 따라 귀무가설을 기각할 수 없다. 즉, 평균이 247명과 다르다고 볼 만큼 반증의 강도가 세지 않다.