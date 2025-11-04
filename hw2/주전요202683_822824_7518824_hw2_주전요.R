set.seed(1) # 문제 풀기 전 실행

## 1
### 1.1
x <- seq(from = -4, to = 4, by = 0.01)

fx1 <- c()

for (j in 1: length(x) ){
  fx1[j] = exp((-1/2)*(x[j]*x[j])) * 1/ sqrt(2*pi)
}

plot(x , fx1, xlim = c(-3, 3), type = 'l')

### 1.2
#이항분포이다. 성공 확률(6의 약수가 나올 확률)은 2/3 이고, 200번 시행한다.
p1 <- pbinom (170, size = 200, prob =  2/3) - pbinom (150, size = 200, prob =  2/3)

## 2
#### 2.1

file.path <- file.choose()
print(file.path)


ames <- read.csv("C:\\Users\\User01\\Downloads\\ames.csv")


#### 2.2
area <- ames$Gr.Liv.Area

#### 2.3
sample1 <- sample(area, 50)
mean1 <- mean(sample1)


lower <- mean1 - qnorm(0.95, 0, 1) *sd(area) / sqrt(50)

upper <- mean1 + qnorm(0.95, 0, 1) *sd(area) / sqrt(50)

#### 2.4
#조건문 사용
is_contained <- a
mu <- mean(area)
if ((lower< mu) & (mu < upper))  {
  a <- ("TRUE")
}  else {
  a <- ("False")
}

print(is_contained)

##3
#### 3.1


pjh1 <- sum ((area - mu )^2)
pvar1 <- pjh1 / length(area)
result1 <- sqrt(pvar1) 

print(result1)

#### 3.2
pjh2 <- sum ((area - mu )^2)
pvar2 <- pjh2 / (length (area)-1)
result2 <- sqrt(pvar2)


#### 3.3
ratio <- result1/ result2

## 4

### 4.1
# 귀무가설 : mu == 247
# 대립가설 : mu != 247

### 4.2
Z <- (276 - 247)/ ( 52/sqrt(11) ) 
print(Z)
# Z = 1.849656

### 4.3
a <- qnorm (0.975, 0, 1)
print(a)
# a = 1.959964

# Z < a 이므로 유의수준 5% 하에서 귀무가설은 기각되지 않는다.
# 즉 모평균이 247이 아니라고 할만큼 뚜렷한 증거가 없는 것으로 파악된다.