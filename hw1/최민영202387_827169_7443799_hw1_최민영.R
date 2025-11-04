## 0
# 안녕하세요 조교님 항상 자세히 설명해주셔서 이해가 잘 됩니다. 감사합니다!

## 1
### 1.1
six <- seq(from=6, to=500, by=6) #6부터 500까지 6의 간격으로 벡터 생성
total <- sum(six) #sum 함수를 이용하여 합 저장

### 1.2
m <- mean(six) #평균
s <- sd(six)   #표준편차

## 2
### 2.1
X <- matrix(1:36, nrow=6, byrow=T) #1부터 36까지의 숫자를 가지고 행 6개, 열 6개의 행렬 생성

### 2.2
Y <- X %% 4 #X의 각 성분을 4로 나눈 나머지를 성분으로 갖게 함

## problem 3
data("iris")
setosa <- as.data.frame(iris[1:50, 1:4])

### 3.1
Sepal <- setosa$Sepal.Length + setosa$Sepal.Width #1열과 2열을 더한 벡터를 변수에 저장한다.

### 3.2
summary(Sepal) #summary 함수를 이용해 Sepal에 저장한 벡터의 수치적 요약을 구한다.

### 3.3
plot(setosa$Petal.Length, setosa$Petal.Width, main='Correlation of Petal Length and Petal Width', xlab='Length', ylab='Width', xlim=c(0.5, 2), ylim=c(0,1))
#산점도를 그리기 위해 plot 함수를 사용하고, 하위레벨 선택사항들의 값을 설정해준다.

### 3.4
result <- cor(setosa$Petal.Length, setosa$Petal.Width) #cor 함수를 이용하여 상관계수를 계산한다.