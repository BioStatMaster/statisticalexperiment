set.seed(1) # 문제 풀기 전 실행시켜주세요.

## 1
#### 1.1
x <- c(-4:4)
print(x)

#### 1.2
fx1 <- 1/(2*pi)^1/2 * exp(-x^2 /2)
print(fx1)
#### 1.3


## 2
#### 2.1
u <-  sample(U, 20000)

#### 2.2
fx2 <- fivenum(u)

#### 2.3
print(hist(fx2, 30))

#### 2.4
# 구간의 수를 더 크게한다.

## 3
#### 3.1
bodydims <- read.csv("C:\\Users\\박성호\\Downloads\\bodydims.csv")

#### 3.2
bodydims.m <- 
  bodydims.f <- TODO
print(bodydims.m)

#### 3.3
m1 <- TODO
s1 <- TODO

m2 <- TODO
s2 <- TODO

## 4
#### 4.1
# 귀무가설: 전체 통계학 실험 수강생들의 일주일 커피 소비량의 실제 평균은 11잔이 아니다.
# 대립가설: 전체 통계학 실험 수강생들의 일주일 커피 소비량의 실제 평균은 11잔이다.

#### 4.2
Z <- TODO

#### 4.3
# 조건문 사용
is_reject <- TODO

#### 4.4
# TODO