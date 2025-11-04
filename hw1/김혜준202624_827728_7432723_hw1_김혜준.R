## 0
# 안녕하세요. 문제 3.1에서 크기 50인 두 벡터의 각 요소를 더해 크기 50인 새로운 벡터를 만들라는 것인지, 아니면 각각의 요소를 변화시키지 않고 크기가 100인 새 벡터를 만들라는 것인지 불분명해 문의 드립니다.

## 1
### 1.1
six <- seq(6, 500, by=6) #1부터 500까지 자연수 중 6의 배수
total <- sum(six) #합

### 1.2
m <- mean(six) #평균
s <- sd(six) #표준편차

## 2
### 2.1
X <- matrix(seq(1, 36), nrow=6, byrow=T)

### 2.2
Y <- X%%4

## problem 3
data("iris")
setosa <- as.data.frame(iris[1:50, 1:4])

### 3.1
Sepal <- setosa[,1] + setosa[,2] #setosa의 1열인 Sepal.Length와 2열인 Sepal.Width를 더함

### 3.2
summary(Sepal)

### 3.3
plot(setosa$Petal.Length, setosa$Petal.Width,
     main="Correlation of Petal Length and Petal Width",
     xlab="Length", ylab="Width", xlim=c(0.5,2), ylim=c(0,1))

### 3.4
result <- cor(setosa$Petal.Length, setosa$Petal.Width)

