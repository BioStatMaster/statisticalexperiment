set.seed(1) # 문제 풀기 전 실행

## 1
### 1.1
x <- seq(from=-3, to=3, length.out=1000)
y <- (1/sqrt(2*pi)) * exp(-(x^2)/2)
plot(x, y, type="l")

### 1.2
p1 <- pbinom(170, size=200, prob=2/3) - pbinom(149, size=200, prob=2/3)
#이항분포 이용. 1~6까지 자연수 중 6의 약수 1,2,3,6 중 하나가 나올 확률은 4/6. 170번 이하로 나올 확률에서 149번 이하로 나올 확률을 뺐다.

## 2
#### 2.1
ames <- read.csv("C:/Users/eg070/pyprg/lab/ames.csv", header=T)

#### 2.2
area <- ames$Gr.Liv.Area

#### 2.3
area.sample <- sample(area, 50, replace=FALSE)
N <- length(area)
lower <- mean(area.sample)-qnorm(0.95)*sqrt(var(area)*(N-1)/N)/sqrt(50)
upper <- mean(area.sample)+qnorm(0.95)*sqrt(var(area)*(N-1)/N)/sqrt(50)

#### 2.4
#조건문 사용
is_contained <- (lower<=mean(area)) & (mean(area)<=upper)

## 3
#### 3.1
variance <- sum((area-mean(area))^2)/length(area)
result1 <- sqrt(variance)

#### 3.2
samplevariance <- sum((area-mean(area))^2) /(length(area) - 1)
result2 <- sqrt(samplevariance)

#### 3.3
ratio <- result1/result2

## 4

### 4.1
# 귀무가설 : H0: (서울대 통계학과 대학원생의 평균 인스타 팔로잉 수) = 247
# 대립가설 : H1: (서울대 통계학과 대학원생의 평균 인스타 팔로잉 수) != 247

### 4.2
Z <- (276-247)/(52/sqrt(11))

### 4.3
abs(qnorm(0.025)) < abs(Z)
# FALSE, 귀무가설이 기각되지 않는다.