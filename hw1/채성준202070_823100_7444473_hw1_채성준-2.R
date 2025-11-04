## 0
# 항상 신속하면서, 또 핵심적으로 수업 해주셔서 감사합니다!

## 1
### 1.1
# seq(from, to, by) 함수를 사용해 간격이 6인 시퀀스를 생성
# sum 함수로 합 구하기
six <- seq(from = 6, to = 500, by = 6)
total <- sum(six)

### 1.2
# mean 함수 사용해 평균 도출
# sd 함수 사용해 표준편차 도출
m <- mean(six)
s <- sd(six)

## 2
### 2.1
# matrix 함수 사용해 행렬 생성, 이때 범위는 벡터로, 열의 개수는 6개로 지정
X <- matrix(c(1:36), nrow = 6, byrow = T)

### 2.2
# X를 4로 나눈 나머지를 행렬 각 성분마다 적용
Y <- X %% 4

## problem 3
data("iris")
setosa <- as.data.frame(iris[1:50, 1:4])
head(setosa) # 앞 6행 출력

### 3.1
# $열이름으로 데이터 불러온 후 합으로 Sepal 지정
Sepal <- setosa$Sepal.Length + setosa$Sepal.Width

### 3.2
# summary 함수 사용해 수치적으로 요약
summary(Sepal)

### 3.3
# plot 함수 사용해 산점도 생성
# 조건에 맞게 제목, 축 이름, 축 범위 지정
plot(
  x = setosa$Petal.Length,
  y = setosa$Petal.Width,
  main = "Correlation of Petal Length and Petal Width",
  xlab = "Length",
  ylab = "Width",
  xlim = c(0.5, 2),
  ylim = c(0, 1)
)

### 3.4
# cor 함수 사용해 상관계수를 result에 지정
result <- cor(setosa$Petal.Length, setosa$Petal.Width)