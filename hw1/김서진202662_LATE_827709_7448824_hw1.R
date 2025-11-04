## 0
# 조교님 수고하십니다

## 1
### 1.1
six <- c(seq(from=6, to=500, by=6))
total <- sum(six)

### 1.2
m <- mean(six)
s <- sd(six)

## 2
### 2.1
X <- matrix(c(1:36), nrow=6, byrow=T)

### 2.2
Y <- matrix(c(X%%4), nrow=6)

## problem 3
data("iris")
setosa <- as.data.frame(iris[1:50, 1:4])

### 3.1
Sepal <- c(setosa$Sepal.Length+setosa$Sepal.Width)

### 3.2
summary(Sepal)

### 3.3
plot(setosa$Petal.Length,setosa$Petal.Width,main="Correlation of Petal Length and Petal Width",xlab="Length",ylab="Width",xlim=c(0.5,2),ylim=c(0,1))

### 3.4
result <- cor(setosa$Petal.Length,setosa$Petal.Width)
