set.seed(1) # 문제 풀기 전 실행

## 1
### 1.1
#plot과 xlim을 통해 요구한 함수를 그리자.
x <- seq(-3, 3, by = 0.01)
plot(x, exp((-x^2)/2)/sqrt(2*pi), xlim=c(-3,3))

### 1.2
#6의 약수가 나올 확률은 2/3이고, 각 시행은 독립이므로 이항분포의 확률을 구하는 상황임을 알 수 있다. 170회 이하 나올 확률에서 149 이하 나올 확률을 뺴주어서 요구사항을 충족시키자.
p1<-pbinom(170,200,2/3)-pbinom(149,200,2/3)

## 2
#### 2.1
#read.csv를 이용해주자.
ames <- read.csv("ames.csv")

#### 2.2
#ames$Gr.Liv.Area를 불러오자.
area <- ames$Gr.Liv.Area

#### 2.3
#표본평균은 mean(sample(area,50)), 모표준편차는 sqrt(var(area)*(length(area)-1)/length(area))이고 표본크기가 50임을 고려하여 신뢰구간을 써주자.
s=sample(area,50)
lower <-mean(s)+qnorm(0.05)*sqrt(var(area)*(length(area)-1)/length(area))/sqrt(50)
upper <- mean(s)-qnorm(0.05)*sqrt(var(area)*(length(area)-1)/length(area))/sqrt(50)

#### 2.4
#조건문을 사용한다.
is_contained <- if (lower<mean(area)&(mean(area)<upper)) {TRUE} else {FALSE}

## 3
#### 3.1
#모분산공식활용
result1 <- sqrt(sum((area-mean(area))^2)/length(area))

#### 3.2
#표본분산공식활용
result2 <- sqrt(sum((area-mean(area))^2)/(length(area)-1))

#### 3.3
ratio <- result1/result2

## 4

### 4.1
# 귀무가설 : mu=247
# 대립가설 : mu!=247

### 4.2
Z <- (276-247)/(52/sqrt(11))

### 4.3
#양측 검정을 실시하자.
if (abs(Z) >= qnorm(0.975)) {print("귀무가설을 기각한다.")} else {print("귀무가설을 기각할 수 없다.")}
#관측값이 기각역에 속하지 않아 귀무가설을 기각할 수 없다. 즉, 관측값이 통계 적으로 유의미하지않다.
