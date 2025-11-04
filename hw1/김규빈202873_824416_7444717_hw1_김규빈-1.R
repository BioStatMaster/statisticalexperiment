## 0
# 늘 좋은 강의 해 주셔서 감사합니다! :) 

## 1
### 1.1
six <- seq(6, 500, by = 6) # 6에서 시작하여 500까지 6씩 커지게 함
total <- sum(six)

### 1.2
m <- mean(six)
s <- sd(six)

## 2
### 2.1
X <- matrix(1:36, nrow = 6, ncol = 6, byrow = TRUE) # 숫자가 행 순서로 배열되어 있으므로 byrow는 TRUE로 설정

### 2.2
Y <- X %% 4 # 행렬 X의 각 값을 4로 나눈 나머지의 값들을 Y에 저장

## problem 3
data("iris")
setosa <- as.data.frame(iris[1:50, 1:4])
head(setosa)

### 3.1
Sepal <- setosa$Sepal.Length + setosa$Sepal.Width # 자료 setosa에서 $를 활용해 꽃받침의 길이와 너비 값을 추출

### 3.2
summary(Sepal) #summary()를 이용해 Sepal의 수치적 요약 제시

### 3.3
plot(setosa$Petal.Length, setosa$Petal.Width, main =  "Correlation of Petal Length and Petal Width", xlab = Length, ylab = Width, xlim = c(0.5, 2), ylim = c(0, 1) )

### 3.4
result <- cor(setosa$Petal.Length, setosa$Petal.Width) # cor()을 이용해 꽃잎 길이와 꽃잎 너비의 상관계수 계산