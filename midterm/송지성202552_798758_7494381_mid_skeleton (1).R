set.seed(1) # 문제 풀기 전 실행시켜주세요.

## 1
#### 1.1
x <- seq(from=-4, to=4, by=0.01)

#### 1.2
fx1 <- {1/sqrt(2*pi)}*{exp(1)^(-x^2/2)}

#### 1.3
plot(x,fx1,type="l")

## 2
#### 2.1
u <- c()
for (i in 1:20000) {
  u[i] <- sample(seq(from=0, to=1))
}

#### 2.2
fx2 <- qnorm(u,mean=0, sd=1)

#### 2.3
hist(fx2, breaks=seq(length.out=31), freq=F)

#### 2.4
# 구간의 수를 30개보다 더욱 늘린다.

## 3
#### 3.1
bodydims <- read.table("C:/Users/ceramics/OneDrive/Desktop/bodydims.csv")

#### 3.2
bodydims.m <- c(bodydims[bodydims$sex==1])
bodydims.f <- c(bodydims[bodydims$sex==0])

#### 3.3
length.m = length(bodydims.m$hgt)
m1 <- mean(bodydims.m$hgt)
s1 <- var(bodydims.m$hgt)*(length.m-1)/length.m

length.f = length(bodydims.f$hgt)
m2 <- mean(bodydims.w$hgt)
s2 <- var(bodydims.f$hgt)*(length.f-1)/length.f

## 4
#### 4.1
# 귀무가설: 전체 통계학 실험 수강생들의 일주일 커피 소비량의 평균은 11잔이다.
# 대립가설: 전체 통계학 실험 수강생들의 일주일 커피 소비량의 평균은 11잔이 아니다.

#### 4.2
Z <- (10-11)/sqrt(4.5)

#### 4.3
# 조건문 사용
q <- qnorm(0.05,0,1)
if (Z<=q) {
  is_reject <- Z<=q} else {
    is_reject <- Z>=q
  }

#### 4.4
# is_reject의 값은 True로 나온다. 즉, 검정통계량이 기각역 내에 위치한다는 뜻이므로 귀무가설은 기각된다.