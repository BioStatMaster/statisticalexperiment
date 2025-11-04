## 0
# 조교님 항상 수업시간마다 열심히 수업해주시고 학생들 많이 배려해주셔서 감사합니다! 

## 1
### 1.1
six <- seq(from=6, to=500, by=6) # 6부터 500까지 간격이 6인 벡터를 생성해 six에 저장
total <- sum(six) # sum()으로 합 계산해 total에 저장

### 1.2
m <- mean(six)
s <- sd(six)
# mean()으로 평균, sd()로 표준편차 계산해 변수에 저장

## 2
### 2.1
X <- matrix(1:36, nrow=6, byrow=T)
# 1부터 36까지의 자연수로 이루어진 벡터로 6열 행렬 생성 

### 2.2
Y <- X%%4
# 행렬 X의 성분을 4로 나눈 나머지를 성분으로 갖는 행렬 Y

## problem 3
data("iris")
setosa <- as.data.frame(iris[1:50, 1:4])

### 3.1
Sepal <- setosa[,1]+setosa[,2]
# 행렬 setosa의 1열벡터와 2열벡터의 합을 Sepal에 저장

### 3.2
summary(Sepal)
# summary()로 최솟값, 제1사분위수, 중앙값, 평균, 제3사분위수, 최댓값 계산

### 3.3
Length <- setosa[,3]
Width <- setosa[,4]
plot(Length, Width, main='Correlation of Petal Length and Petal Width', xlim=c(0.5,2), ylim=c(0,1), xlab='Length', ylab='Width')
# 행렬 setosa의 3열과 4열을 각각 변수 Length와 Width에 저장해 plot()으로 산점도 그리기

### 3.4
result <- cor(Length, Width)
# cor()사용해 상관계수 계산