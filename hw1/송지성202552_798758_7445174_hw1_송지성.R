## 0
# 조교님 안녕하세요. 수업 내용이 통계학 진도와 정확하게 일치하여 이해하기 편합니다.(강의력도 너무 좋으십니다.)

## 1
### 1.1
# seq() 함수를 이용하면 시작값, 끝값, 간격을 지정하여 등차수열을 생성할 수 있다.
# 6의 배수이므로 시작값은 6, 끝값은 500, 간격은 6으로 설정해야 한다.
# 생성된 six 벡터의 모든 값을 더하기 위해 sum() 함수를 사용한다.
six <- seq(from = 6, to = 500, by = 6)
total <- sum(six)

### 1.2
# 평균은 mean() 함수를, 표준편차는 sd() 함수를 사용한다.
m <- mean(six)
s <- sd(six)

## 2
### 2.1
# 6x6 행렬을 만들기 위해 matrix() 함수를 사용한다.
# 1부터 36까지의 숫자를 채워 넣고, byrow=TRUE 옵션을 사용하여 행 기준으로 입력한다.
X <- matrix(1:36, nrow = 6, ncol = 6, byrow = TRUE)

### 2.2
# 행렬 X의 각 원소를 4로 나눈 나머지를 구하기 위해 %%연산자를 사용한다.
Y <- X %% 4

## problem 3
data("iris")
setosa <- as.data.frame(iris[1:50, 1:4])

### 3.1
# setosa 데이터에서 Sepal.Length(1열)와 Sepal.Width(2열)를 더한 결과를 벡터 형태로 저장한다.
Sepal <- setosa[,1] + setosa[,2]

### 3.2
# Sepal 벡터의 수치적 요약값을 구해야 한다.
# summary() 함수를 사용하면 최솟값, 1사분위수, 중앙값, 평균, 3사분위수, 최댓값을 알 수 있다.
summary(Sepal)

### 3.3
# setosa 데이터의 Petal.Length(3열)을 x축, Petal.Width(4열)을 y축으로 하여 산점도를 그려야 한다.
# 산점도를 그리기 위해 plot() 함수를 사용하며 그래프의 제목(main), x축 이름(xlab), y축 이름(ylab), 축 범위(xlim, ylim)를 지정한다.
plot(
  setosa$Petal.Length,
  setosa$Petal.Width,
  main = "Correlation of Petal Length and Petal Width",
  xlab = "Length",
  ylab = "Width",
  xlim = c(0.5, 2),
  ylim = c(0, 1)
)

### 3.4
# 두 변수의 관계를 수치로 표현하기 위해 cor() 함수를 사용한다.
result <- cor(setosa$Petal.Length, setosa$Petal.Width)