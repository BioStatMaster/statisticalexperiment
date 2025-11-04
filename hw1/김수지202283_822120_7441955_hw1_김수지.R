## 0
# TODO 

## 1
### 1.1
six <- seq(from=6,to=500,by=6)
total <- sum(six)

### 1.2
m <- mean(six)
s <- sd(six)
  
## 2
### 2.1
X <- matrix(1:36,nrow=6,ncol=6,byrow=True)

### 2.2
Y <- x%%4

## problem 3
data("iris")
setosa <- as.data.frame(iris[1:50, 1:4])

### 3.1
Sepal <- setosa[,1]+setosa[,2]

### 3.2
summary(Sepal)

### 3.3
plot(setosa[,3],setosal[,4],main='Correlation of Petal Length and Petal Width',xlim=c(0.5,2),ylim=c(0,1),xlab='Length',ylab='Width')

### 3.4
result <- cor(setosa[,3],setosa[,4])

