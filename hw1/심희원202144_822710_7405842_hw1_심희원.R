## 0
# 항상 수업 감사드립니다

## 1
### 1.1
six <- seq(from=6, to=500, by=6)
total <- sum(six)

### 1.2
m <- mean(six)
s <- sd(six)

## 2
### 2.1
### 1부터 36까지의 정수 벡터를 생성한 후 matrix 함수를 통해
### 행이 6개이며 행 방향으로 숫자를 채우는 행렬을 생성했다.
X <- matrix(c(1:36), nrow=6, byrow=T)

### 2.2
### 2.1과 같은 방식으로 행렬을 만든 후
### for문을 중첩 사용해 각 성분을 4로 나누어 다시 같은 자리에 저장했다.
Y <- matrix(c(1:36), nrow=6, byrow=T)
for (i in 1:6){
  for (j in 1:6){
    Y[i, j] <- Y[i, j]%%4
  }
}

## problem 3
data("iris")
setosa <- as.data.frame(iris[1:50, 1:4])

### 3.1
### setosa의 1, 2열을 불러와 더한다.
Sepal <- setosa[,1] + setosa[,2]

### 3.2
summary(Sepal)

### 3.3
plot(setosa[,c(3,4)], main="Correlation of Petal Length and Petal Width", xlab="Length", ylab="Width", xlim=c(0.5,2), ylim=c(0,1))

### 3.4
result <- cor(setosa[,3], setosa[,4])