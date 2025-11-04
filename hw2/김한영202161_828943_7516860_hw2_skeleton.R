set.seed(1) # 문제 풀기 전 실행

## 1
### 1.1
x <- seq(from=-3,to=3,by=0.1)
fx <- ((1/sqrt(2*pi))*exp(-(x^2/2)))
plot(x,fx,type="l")
### 1.2
p1 <- pbinom(170, 200, 4/6) - pbinom(149, 200, 4/6)
p1
## 2
#### 2.1
ames <- read.csv("ames.csv",header=T)

#### 2.2
area <- ames$Gr.Liv.Area

#### 2.3
sample_50 <- sample(area,50)
sample_mean <- mean(sample_50)
sample_var <- var(sample_50)
n <- 50
N <- length(area)
se <- sqrt(sample_var / n)
z <- qnorm(0.05)
lower <- sample_mean + z * se
upper <- sample_mean - z * se

#### 2.4
#조건문 사용
is_contained <- if (lower <= mean(area) && mean(area) <= upper) {TRUE} else {FALSE}

## 3
#### 3.1
temp <- c()
pop_mean <- mean(area)
for(i in 1:length(area)){
  temp[i] <- (area[i]^2)
}
result1 <- sqrt(mean(temp) - pop_mean^2)
#### 3.2
temp <- c()
sample_50 <- sample(area,50)
sample_mean <- mean(sample_50)
for(i in 1:length(sample_50)){
  temp[i] <- (sample_50[i])^2
}
result2 <- sqrt((sum(temp)-50*sample_mean^2)/49)

#### 3.3
ratio <- result1 / result2
ratio
## 4

### 4.1
# 귀무가설 : mu_0 = 247
# 대립가설 : mu_0 ~= 247

### 4.2
Z <- (276-247)/(52/sqrt(11))
Z
if (abs(Z) > qnorm(0.975)) {print("H_1")} else {print("H_0")}
### 4.3
# Z값은 약 1.85로 1.96보다 작다. 따라서 기각역에 속하지 않는다.
# 서울대 통계학과 대학원생들의 실제 평균 팔로잉 수가 247명과
# 통계적으로 유의한 차이가 있다고 볼 수 없다.
# 귀무가설 채택
