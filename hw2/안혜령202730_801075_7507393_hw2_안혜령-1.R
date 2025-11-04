set.seed(1) # 문제 풀기 전 실행

## 1
### 1.1
x<-seq(-3,3, length.out=10000)
fx<-1/sqrt(2*pi)*exp(-x^2/2)
plot(x,fx,type="l", main="표준 정규분포의 확률밀도함수", xlab="x", ylab="f(x)")

### 1.2
p1 <- pbinom(170,200,2/3)-pbinom(150,200,2/3)

## 2
#### 2.1
ames <- read.csv("ames.csv")

#### 2.2
area <- ames$Gr.Liv.Area

#### 2.3
n<-50
var<-((n-1)/n)*var(area)
lower <- sample(area,n)+dnorm(0.05)*sqrt(var)/sqrt(n)
upper <- sample(area,n)-dnorm(0.05)*sqrt(var)/sqrt(n)

#### 2.4
#조건문 사용
is_contained <- if((lower<mean(area))&(mean(area)<upper)){"TRUE"} else{"FALSE"}

## 3
#### 3.1
k<-c()
for(i in 1:length(area)){
  k[i]<- (area[i]-mean(area))^2}
result1 <- sqrt(sum(k)/length(area))

#### 3.2
y<-c()
for(i in 1:length(area)){
  y[i]<- (area[i]-mean(area))^2}
result2 <- sqrt(sum(y)/(length(area)-1))

#### 3.3
ratio <- result1/result2

## 4

### 4.1
# 귀무가설 : mu = 276
# 대립가설 : mu != 276

### 4.2
Z <- (247-276)/(52/sqrt(11))

### 4.3
qnorm(0.025)
# 검정통계량이 더 크기 때문에 귀무가설을 기각할 수 없다.