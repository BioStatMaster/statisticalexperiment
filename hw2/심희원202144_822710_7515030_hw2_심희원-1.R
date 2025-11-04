set.seed(1) # 문제 풀기 전 실행

## 1
### 1.1
x<-c(seq(-3, 3, by=0.01))
fx<-(1/sqrt(2*pi))exp(-(x^2)/2)
plot(x, fx, type="l")

### 1.2
p1 <- pbinom(170, size=200, prob=(2/3)) - pbinom(149, size=200, prob=(2/3))

## 2
#### 2.1
ames <- read.csv("C:\\Users\\april\\Desktop\\심희원\\대학생활\\2025-2\\통계학실험\\ames.csv")

#### 2.2
area <- ames$Gr.Liv.Area

#### 2.3
lower <- mean(sample(area, 50))+qnorm(0.1/2)*sqrt(var(sample(area, 50)))/sqrt(50)
upper <- mean(sample(area, 50))-qnorm(0.1/2)*sqrt(var(sample(area, 50)))/sqrt(50)

#### 2.4
#조건문 사용
if ((mean(area)<lower) & (mean(area)<upper)){
  is_contained <- TRUE} else {
    is_contained <- FALSE
  }

## 3
#### 3.1
for (i in area){
  sum <- ((mean(area) - area[i])^2)
}
result1 <- sqrt(sum/length(area))

#### 3.2
for (i in area){
  sum <- ((mean(area) - area[i])^2)
}
result2 <- sqrt(sum/(length(area) - 1))

#### 3.3
ratio <- result1/result2

## 4

### 4.1
# 귀무가설 : 통계학과 대학원생들의 인스타그램 팔로잉 수 평균은 247명이다. 
# 대립가설 : 통계학과 대학원생들의 인스타그램 팔로잉 수 평균은 247명이 아니다.

### 4.2
mu0 = 247
mu = 276
n = 11
sigma = 52

Z <- (mu - mu0)/(sigma/sqrt(n))

### 4.3
if ((Z<qnorm(0.05)*sigma/sqrt(n))|(Z>qnorm(0.05)*sigma/sqrt(n))) {
  print("TRUE")
} else {
  print("FALSE")
}

# 가설검정 결과 검정통계량이 기각역 안에 포함되므로 귀무가설을 기각할 수 있다. 즉, 통계학과 대학원생들의 인스타그램 팔로잉 수 평균은 247명이 아니라고 할 수 있다.