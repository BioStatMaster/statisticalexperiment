set.seed(1) # 문제 풀기 전 실행시켜주세요.

## 1
#### 1.1
x <- seq(from = -4, to = 4, by = 0.01)

#### 1.2

fx1 <- c()

for (j in 1: length(x) ){
  fx1[j] = exp((-1/2)*(x[j]*x[j])) * 1/ sqrt(2*pi)
}


#### 1.3
plot(x, fx1, xlim = c(-4,4) )

## 2
#### 2.1
u <- runif(20000, 0, 1)

#### 2.2
fx2 <- qnorm(u, 0, 1)

#### 2.3
hist(fx2, probability = T, breaks = seq(min(fx2), max (fx2), length.out = 31 )  )

#### 2.4
# 2.1의 표본 개수를 더 증가시킨다

## 3
#### 3.1
file.path <- file.choose()
print(file.path)

bodydims <- read.csv("C:\\Users\\User01\\Downloads\\bodydims.csv")

#### 3.2
bodydims.m <- bodydims[(bodydims$sex == 1), ]
bodydims.f <- bodydims[(bodydims$sex == 0), ]

#### 3.3
m1 <- mean(bodydims.m$hgt)
n<- length(bodydims.m$hgt)
s1 <- (n-1)/n * var(bodydims.m$hgt)

m2 <- mean(bodydims.f$hgt)
p <- length(bodydims.f$hgt)
s2 <- (p-1)/p * var(bodydims.f$hgt)

## 4
#### 4.1
# 귀무가설: H0: 모평균 == 11
# 대립가설: H1: 모평균 != 11

#### 4.2
Z <- (10-11)/sqrt ((4.5)/36)

#### 4.3
# 조건문 사용


if (pnorm( Z , 0 , 1) < 0.025) {
  a <- ("FALSE")
} else{
  a <- ("TRUE")
}
is_reject <- a

#### 4.4
print(is_reject)
# 귀무가설은 기각된다. 즉 전체 수강생들의 커피 소비량 평균은 11이 아닌 것으로 보인다. 