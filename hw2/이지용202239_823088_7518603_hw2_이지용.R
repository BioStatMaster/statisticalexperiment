set.seed(1) # 문제 풀기 전 실행

## 1
### 1.1
x <- seq(from=-3, to=3, by=0.01)
fx <- sqrt(2 * pi)^-1 * exp(-(x^2) / 2)
plot(x, fx, type='l')

### 1.2
n_dice <- 200
p_dice <- 2/3

#pbinom은 P(X <= x)일 확률을 구해주기 때문에 150번 이상을 구하려면 149로 계산해야 한다. 
p1 <- pbinom(170, size=n_dice, prob=p_dice) - pnorm(149, size=n_dice, prob=p_dice)


## 2
#### 2.1
ames <- read.csv("C:\\Users\\jiyon\\Downloads\\ames.csv", header=T)

#### 2.2
area <- ames$Gr.Liv.Area

#### 2.3
N_area <- length(area)
sd <- sqrt(var(area) * (N_area-1)/N_area )
se <- sd / sqrt(50)

mean_area <- mean(area)
rand_area <- sample(area, 50)
xmean_area <- mean(rand_area)

z <- qnorm(0.05, mean=0, sd=1, lower.tail=F)

lower <- xmean_area - z * se
upper <- xmean_area + z * se

#### 2.4
#조건문 사용
is_contained <- if (mean_area >= lower & mean_area <= upper) {TRUE} else {FALSE}

## 3
#### 3.1
result1 <- sqrt(sum((area - mean_area)^2) / N_area)

#### 3.2
result2 <- sqrt(sum(area - mean_area)^2) / N_area-1

#### 3.3
ratio <- result1 / result2

## 4

### 4.1
# 귀무가설 : 서울대학교 통계학과 대학원생들의 인스타그램 평균 팔로잉 수가 247명이다.
# 대립가설 : 서울대학교 통계학과 대학원생들의 인스타그램 평균 팔로잉 수가 247명이 아니다.

### 4.2
mean_insta <- 247
sd_insta <- 52
xmean_insta <- 276
n_insta <- 11
se_insta <- sd_insta / sqrt(n_insta)

Z <- (xmean_insta - mean_insta) / se_insta

### 4.3
if (Z < qnorm(0.025, mean=0, sd=1) | Z > qnorm(0.025, mean=0, sd=1, lower.tail=F)) {TRUE} else {FALSE}
# FALSE가 나오므로 가설검정 결과 검정통계량이 기각역에 포함되지 않으므로 귀무가설을 기각할 수 없다. 따라서 서울대학교 통계학과 대학원생들의 인스타그램 팔로잉 수 평균은 247명이라고 봐야 한다.