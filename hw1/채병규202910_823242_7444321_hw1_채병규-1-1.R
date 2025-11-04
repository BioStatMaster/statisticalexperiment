## 0
# 특별히 개선되었으면 좋을 것 같은 부분이나 아쉬운 점은 아직 못
# 느꼈습니다. 굳이 꼽자면, 향후 기말고사 일정은 사전에 미리 공지가
# 잘 되었으면 합니다.앞으로도 지금처럼 열심히 가르쳐 주시면
# 하는 바람이 있고, 또 저도 통실 수업에서 많이 배워가고 싶습니다.

## 1
### 1.1
# 1부터 500까지, 6의 배수를 변수 six에 저장하고 그 합을 변수 total에 저장하기
six <- seq(from = 6, to = 498, by = 6)
total <- sum(six)

### 1.2
# six 벡터의 평균을 변수 m에, 표준편차를 변수 s에 저장하기
m <- mean(six)
s <- sd(six)

## 2
### 2.1
# 1~36까지 정수가 포함된 6 X 6 행렬을 변수 X에 저장하기
X <- matrix(c(1:36), nrow = 6, byrow = T)

### 2.2
# X를 4로 나눈 나머지를 변수 Y에 저장하기
Y <- X %% 4

## problem 3
# 데이터 "iris"를 불러와 setosa에 저장하기
data("iris")
setosa <- as.data.frame(iris[1:50, 1:4])

### 3.1
# Sepal.Length(1열)와 Sepal.Width를 더한 벡터를 변수 Sepal에 저장하기

Sepal <- setosa[,1] + setosa[,2] 

### 3.2
# Sepal 벡터의 수치적 요약을 summary 함수로 구하기
summary(Sepal)

### 3.3
# Petal.Lenghth(3열)와 Petal.Width(4열)의 산점도를 plot 함수로 그리기
plot(setosa[,3], setosa[,4],
     main = "Correlation of Petal Length and Petal Width",
     xlab = "Length",
     ylab = "Width",
     xlim = c(0.5, 2),
     ylim = c(0, 1))

### 3.4
# Petal.Lenghth(3열)와 Petal.Width(4열)의 상관계수를 result에 저장
result <- cor(setosa[,3], setosa[,4])