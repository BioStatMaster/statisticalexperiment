set.seed(1) # 문제 풀기 전 실행시켜주세요.

## 1
#### 1.1
x <- c(seq(from=-4, to=4, by=0.01))

#### 1.2
fx1 <- (sqrt(2*pi))^-1 * exp(1)^(-(x^2)/2)

#### 1.3
plot(x, fx1, type='l')

## 2
#### 2.1
u <- runif(20000, min=0, max=1)

#### 2.2
fx2 <- qnorm(u, mean=0, sd=1)

#### 2.3
hist(fx2, breaks=30, freq=F)

#### 2.4
# 2.1에서 랜덤 표본을 더욱 많이 뽑으면 된다.

## 3
#### 3.1
bodydims <- read.csv("C:\\Users\\jiyon\\Downloads\\bodydims.csv", header=T)

#### 3.2
bodydims.m <- bodydims[bodydims$sex==1, ]
bodydims.f <- bodydims[bodydims$sex==0, ]

#### 3.3
n_m <- length(bodydims.m$hgt)
m1 <- mean(bodydims.m$hgt)
s1 <- ((n_m-1)/n_m)*var(bodydims.m$hgt)

n_f <- length(bodydims.f$hgt)
m2 <- mean(bodydims.f$hgt)
s2 <- ((n_f-1)/n_f)*var(bodydims.f$hgt)

## 4
#### 4.1
# 귀무가설: 전체 통계학 실험 수강생들의 일주일 커피 소비량의 실제 평균은 11잔이다.
# 대립가설: 전체 통계학 실험 수강생들의 일주일 커피 소비량의 실제 평균은 11잔이 아니다.

#### 4.2
se = 4.5/sqrt(36)
Z <- (10-11)/se

#### 4.3
# 조건문 사용
is_reject <- if (Z <= qnorm(0.025, mean=0, sd=1) | Z >= qnorm(0.025, mean=0, sd=1, lower.tail=F)){TRUE} else {FALSE}


#### 4.4
print(is_reject)
# is_reject가 FALSE이므로 귀무가설을 기각할 수 없다. 따라서 전체 통계학 실험 수강생들의 일주일 커피 소비량의 실제 평균은 11잔이라고 봐야 한다. 