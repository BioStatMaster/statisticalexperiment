set.seed(1) # 문제 풀기 전 실행시켜주세요.

## 1
#### 1.1
x <- seq(from=-4, to=4, by=0.01)

#### 1.2
fx1 <- c()
for (i in x) {
  fx1 <- (1/sqrt(2*pi))*exp(1)^((-x^2)/2)
}

#### 1.3
plot(x, fx1)

## 2
#### 2.1
u <- runif(20000, min=0, max=1)

#### 2.2
fx2 <- c()
for (i in u){
  fx2 <- qnorm(u)
}

#### 2.3
hist(fx2, breaks=seq(min(fx2), max(fx2), length.out = 31), freq=F)

#### 2.4
#균등분포에서 뽑는 표본의 수를 늘리면 히스토그램이 빨간선에 더 가까워질 것이다.

## 3
#### 3.1
bodydims <- read.csv("C:/Users/mchoi/Desktop/bodydims.csv")

#### 3.2
bodydims.m <- bodydims[bodydims$sex==1,] ##
bodydims.f <- bodydims[bodydims$sex==0,] ##

#### 3.3
n1 <- length(bodydims.m)
m1 <- mean(bodydims.m$hgt)
s1 <- (n1-1)/n1 * var(bodydims.m$hgt)

n2 <- length(bodydims.f)
m2 <- mean(bodydims.f$hgt)
s2 <- (n2-1)/n2 * var(bodydims.f$hgt)

## 4
#### 4.1
# 귀무가설: H = 11
# 대립가설: H != 11

#### 4.2
x_bar = 10
n = 36
sigma = 4.5
mu = 11

Z <- (x_bar - mu) / ( sigma/sqrt(n) )

#### 4.3
# 조건문 사용
is_reject <- (Z <= qnorm(0.025)) | (Z >= -qnorm(0.025))

#### 4.4
# is_reject가 FALSE이므로 귀무가설을 기각할 수 없다.