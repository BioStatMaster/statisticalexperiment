## 0
# 고생이 많으십니다.

## 1
### 1.1
six <- seq(from=6, to=500, by=6)
total <- sum(six)

### 1.2
m <- mean(six)
s <- sd(six)

## 2
### 2.1
X <- matrix(1:36,nrow=6, byrow=T)

### 2.2
Y <- X/4

## problem 3
data("iris")
setosa <- as.data.frame(iris[1:50, 1:4])
head(setosa)

### 3.1
Sepal <- setosa[,1]+setosa[,2]

### 3.2
summary(Sepal)

### 3.3
plot(setosa$Petal.Length,setosa$Petal.Width,
     xlim=c(0.5:2),ylim=c(0:1),
     main="Correlation of Petal Length and Petal Width",
     xlab="Length", ylab="Width")

### 3.4
result <- cor(setosa$Petal.Length,setosa$Petal.Width)