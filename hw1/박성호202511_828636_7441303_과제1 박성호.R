# 0
### 조교님 항상 수고하십니다. 좋은 수업 감사합니다!

# Prob 1
### 1.1
six <- seq(6,500,6)
total <- sum(six)

### 1.2
m <- mean(six)
s <- sd(six)

# Prob 2
### 2.1
X <- matrix(seq(1,36), nrow=6, byrow=T)

### 2.2
Y <- X%%4

##Prob 3
data("iris")
setosa <- as.data.frame(iris[1:50, 1:4])

### 3.1
Sepal <- setosa[,1]+setosa[,2]

### 3.2
print(summary(Sepal))

### 3.3
plot(setosa[,3], setosa[,4], main="Correlation of Petal Length and Petal Width", xlab="Length", ylab="Width", xlim=c(0.5,2), ylim=c(0,1))

### 3.4
result <- cor(setosa[,3], setosa[,4])