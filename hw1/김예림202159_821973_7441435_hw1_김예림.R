## 0
# 조교님 안녕하세요

## 1
### 1.1
six <- seq(from=0,to=500,by=6)[-1]

total <- sum(six)


### 1.2
m <- mean(six)
s <- sd(six)

## 2
### 2.1
X <- matrix(c(1:36),nrow=6,byrow=T)

### 2.2
Y <- X%%4


## problem 3
data("iris")
setosa <- as.data.frame(iris[1:50, 1:4])

### 3.1
Sepal <- setosa$Sepal.Length + setosa$Sepal.Width

### 3.2
summary(Sepal)

### 3.3

plot(setosa$Petal.Length, setosa$Petal.Width,xlim=c(0.5,2),ylim=c(0,1),main = 'Correlation of Petal Length and Petal Width', xlab="Length",ylab="Width")


### 3.4
result <- cor(setosa$Petal.Length,setosa$Petal.Width)