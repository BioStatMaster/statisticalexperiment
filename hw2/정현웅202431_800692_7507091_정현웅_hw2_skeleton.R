set.seed(1) 

## 1
### 1.1
x <- seq(-3, 3, by = 0.001)
fx <- (1 / sqrt(2 * pi)) * exp(-(x^2) / 2)
plot(x, fx, type = "l")

### 1.2
p1 <-  pbinom(170, size = 200, prob = 2/3) - pbinom(149, size = 200, prob = 2/3)

## 2
#### 2.1
ames <- read.csv("C:/Users/유빈/Downloads/ames (1).csv")

#### 2.2
area <- ames$Gr.Liv.Area

#### 2.3
area_sample <- sample(area,50)
mean_sample <- mean(area_sample)
lower <- mean_sample-qt(0.95, 49)*sd(area_sample)/sqrt(50)
upper <- mean_sample+qt(0.95, 49)*sd(area_sample)/sqrt(50)

#### 2.4
#조건문 사용
pop_mean <- mean(area)
is_contained <- if (pop_mean >= lower && pop_mean <= upper) print('TRUE') else print('FALSE')

## 3
#### 3.1
ma <- mean(area)
result1 <- sqrt(mean((area - ma)^2))

#### 3.2
n <- length(area)
ma <- mean(area)
result2 <- sqrt(sum((area - ma)^2) / (n - 1))

#### 3.3
ratio <- result1 / result2

## 4

### 4.1
# 귀무가설 : u = 247
# 대립가설 : u!= 247

### 4.2
z <- (276 - 247) / (52 / sqrt(11))

### 4.3
# qnorm(0.975) = 1.96, 1.85 < 1.96이므로 귀무가설을 기각하지 못함 
