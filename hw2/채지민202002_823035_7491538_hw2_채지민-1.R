set.seed(1) # 문제 풀기 전 실행

## 1
### 1.1
x<--3:3
fx<-1/sqrt(2*pi)*exp(-x^2/2)
plot(x,fx,type="l")

### 1.2
p1 <- pbinom(170, size=200, prob=2/3)-pbinom(149, size=200, prob=2/3)


## 2
#### 2.1
ames <- read.csv("C:/Users/zimin/OneDrive/문서/ames.csv", header=T)


#### 2.2
area <- ames$Gr.Liv.Area

#### 2.3
x<-sample(area, 50)
xbar<-mean(x)
n<-length(x)
alpha<-0.1

N<-length(area)
mu_pop<-mean(area)
sig_pop<-sqrt(var(area)*(N-1)/N)

z <-qnorm(1-alpha/2)

lower <- xbar-z*sig_pop/sqrt(n)
upper <- xbar-z*sig_pop/sqrt(n)

#### 2.4
#조건문 사용
is_contained <- (lower<mu_pop)&(mu_pop<upper)

## 3
#### 3.1
sum_sq<-0
for (i in 1:N){sum_sq<sum_sq+(area[i]-mu_pop)^2}
result1 <- sqrt(sum_sq/N)

#### 3.2
sum_sq2<-0
for (i in 1:N){sum_sq2<-sum_sq2+(area[i]-mu_pop)^2}
result2 <- sqrt(sum_sq2/(N-1))

#### 3.3
ratio <- result1/result2

## 4

### 4.1
# 귀무가설 : mu=247
# 대립가설 : mu!=247

### 4.2
Z <- (276-247)/(52/sqrt(11))

### 4.3
print(abs(Z)>qnorm(1-0.05/2))
# 검정통계량이 기각역에 포함되므로 귀무가설을 기각한다