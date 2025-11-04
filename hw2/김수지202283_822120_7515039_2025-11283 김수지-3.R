set.seed(1) # 문제 풀기 전 실행

## 1
### 1.1
x <- seq(-3, 3, length.out = 1000)
fx <- (1 / sqrt(2 * pi)) * exp(-(x^2) / 2)
plot(x, fx, type= 'l', main = '표준 정규분포 확률밀도함수')


### 1.2
p1 <- sum(dbinom(150:170, size =  200, prob =  4 / 6))

## 2
#### 2.1
ames <- read.csv("c:/data/ames.csv",header=T)

#### 2.2
area <- ames$Gr.Liv.Area

#### 2.3
x<-mean(sample(area,size=50))
n <- length(area)
v <- var(area) * ((n-1)/n)
lower <- x - qnorm(1 - 0.1/2) * sqrt(v/50)
upper <- x + qnorm(1 - 0.1/2) * sqrt(v/50)


#### 2.4
#조건문 사용
if ((mean(area)<upper)&(mean(area)>lower)){is_contained <- TRUE}else {is_continued<-FALSE}

## 3
#### 3.1
result1 <- sqrt( sum((area - mean(area))^2) / n )

#### 3.2
result2 <- sqrt( sum( (area - mean(area))^2 ) / (n - 1) )

#### 3.3
ratio <- result1 / result2

## 4

### 4.1
# 귀무가설 : 실제 평균 팔로잉 수가 247명이다
# 대립가설 : 실제 평균 팔로잉 수가 247명이 아니다

### 4.2
Z <- (276 - 247) / (52 / sqrt(11))

### 4.3
alpha2 <- 0.05
z_alpha <- qnorm(1 - (alpha2/2))

if (abs(Z) >= z_alpha) {
  result <- "귀무가설 기각"
} else {
  result <- "귀무가설 채택"
}
result
# 검정통계량이 기각역에 속하지 않기에 귀무가설을 기각할 수 없다. 따라서 실제 평균 팔로잉 수가 247명과 다르다고 볼 수 없다.

