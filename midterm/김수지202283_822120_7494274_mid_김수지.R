set.seed(1)# 문제 풀기 전 실행시켜주세요.

## 1
#### 1.1
x <- seq(from=-4,to=4,by=0.01)

#### 1.2
fx1 <- exp(-x^2/2)/sqrt(2*pi)

#### 1.3
plot(x,fx1,xlim=c(-4,4))

## 2
#### 2.1
u <- runif(20000,0,1)

#### 2.2
fx2 <- dnorm(u)

#### 2.3
hist(fx2,probability=T,length.out=31)

#### 2.4
lines(x,fx1, col='red') 
# 히스토그램을 빨간선에 가깝게 만들기 위해서는 표본의 개수를 늘리는 방식으로 코드를 수정하면 된다. 

## 3
#### 3.1
bodydims <- read.csv("c:/data/bodydims.csv")

#### 3.2
bodydims.m <- bodydims[bodydims$sex==1,]
bodydims.f <- bodydims[bodydims$sex==0,]

#### 3.3
m1 <- mean(bodydims.m$hgt)
s1 <- var(bodydims.m$hgt)*(length(bodydims.m)-1/length(bodydims.m))

m2 <- mean(bodydims.f$hgt)
s2 <- var(bodydims.f$hgt)*(length(bodydims.f)-1/length(bodydims.f))
## 4
#### 4.1
# 귀무가설: 전체 통계학 실험 수강생들의 일주일 커피 소비량의 실제 평균은 11잔이다
# 대립가설: 전체 통계학 실험 수강생들의 일주일 커피 소비량의 실제 평균은 11잔이 아니다

#### 4.2
Z <- (10-11)/(sqrt(4.5)/sqrt(36))

#### 4.3
# 조건문 사용
if (abs(Z)>(-qnorm(0.05))){
  is_reject <- TRUE} else{is_reject<-FALSE}
#### 4.4
# print(is_reject)를 실행 시켰을 때 True가 나왔으므로 귀무가설을 기각하고 대립가설을 채택할 수 있다. 따라서 전체 통계학 실험 수강생들의 일주일 커피 소비량의 실제 평균은11잔이 아니라고 할 수 있다. 

