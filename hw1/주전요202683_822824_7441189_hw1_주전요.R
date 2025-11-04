## 0
# 수업 이해 잘 되게 설명해주셔서 매우 만족하며 듣고 있습니다. 감사합니다.

## 1
### 1.1
six <- seq(from = 6, to = 500, by= 6)

total <- sum (six)

### 1.2
m <- mean(six)
s <- sd(x)

## 2
### 2.1
X <- matrix(c(1:36), nrow = 6, byrow = T)

### 2.2
Y <- X %% 4

## problem 3
data("iris")
setosa <- as.data.frame(iris[1:50, 1:4])

### 3.1
Sepal <- setosa$Sepal.Length + setosa$Sepal.Width

### 3.2
summary(Sepal)

### 3.3
plot(Petal.Width ~ Petal.Length,    
     data = setosa,              
     main = "Correlation of Petal Length and Petal Width", 
     xlab = "Length",         
     ylab = "Width",          
     xlim = c(0.5, 2),        
     ylim = c(0, 1)           
)

### 3.4
result <- cor(setosa$Petal.Length, setosa$Petal.Width)