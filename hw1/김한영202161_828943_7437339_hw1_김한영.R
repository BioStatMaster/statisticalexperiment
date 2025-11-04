## 0
# 조교님 수업 잘 듣고있습니다!!!

## 1
### 1.1
six <- seq(6, 500, by = 6)
total <- sum(six)

### 1.2
m <- mean(six)
s <- sd(six) 

## 2
### 2.1
X <- matrix(1:36, nrow = 6, ncol = 6, byrow = TRUE)

### 2.2
Y <- X %% 4

## problem 3
data("iris")
setosa <- as.data.frame(iris[1:50, 1:4])

### 3.1
Sepal <- setosa$Sepal.Length + setosa$Sepal.Width
Sepal
### 3.2
summary(Sepal)

### 3.3
plot(setosa$Petal.Length, setosa$Petal.Width,
     main = "Correlation of Petal Length and Petal Width",
     xlab = "Length",
     ylab = "Width",
     xlim = c(0.5, 2),
     ylim = c(0, 1))

### 3.4
result <- cor(setosa$Petal.Length, setosa$Petal.Width)