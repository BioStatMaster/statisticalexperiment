set.seed(1) # 문제 풀기 전 실행시켜주세요.

## 1
#### 1.1
x <- seq(-4, 4, by=0.1)

#### 1.2
fx1 <- 1/sqrt(2 * pi) * (exp(1) ^ -((x^2)/2))

#### 1.3
hist(fx1, xlim=c(-4, 4))
help(hist)

help(seq)

## 2
#### 2.1
u <- sample(c(0,1), size=20000, replace=TRUE)

#### 2.2
fx2 <- pnorm(u, mean=0, sd=1)


#### 2.3
hist(fx2, dnorm(u), xlim=range(30))
     
#### 2.4
# 구간의 수를 늘린다.

## 3
#### 3.1
bodydims <- read.table(bodydims.csv, header=T)

#### 3.2
bodydims.m <- c(
  for (i in bodydims$sex) {
    if (i==1) {bodydims.m <- i}
  }
)
bodydims.f <- c(
  for (i in bodydims$sex) {
    if (i==0) {bodydims.f <- i}
  }
)

#### 3.3
m1 <- mean(bodydims.m)
s1 <- var(bodydims.m) * n

m2 <- mean(bodydims.f)
s2 <- var(bodydims.f) * n

## 4
#### 4.1
# 귀무가설: 모평균이 11이다.
# 대립가설: 모평균이 11이 아니다.

#### 4.2
x.mean <- 10
n <- 36
sd1 <- sqrt(4.5)
Z <- pnorm(x.mean, mean=11, sd=sd1)

#### 4.3
# 조건문 사용
is_reject <- if (Z >= 0.025 && Z <= 0.975) {"TRUE"} else {"FALSE"} 
print(is_reject)
     
#### 4.4
# TRUE => 귀무가설은 기각되었다.