## 0
# 조교님 안녕하세요 좋은 수업 자료 덕분에 통계학 수업까지 잘 이해하고 있습니다!

## 1
### 1.1
six <- seq(6, 500, by = 6)
# 6부터 500까지 6씩 증가 

total <- sum(six)
# 수열 six의 총합


### 1.2
m <- mean(six)
# six 벡터의 평균

s <- sqrt(var(six) * (length(six) - 1) / length(six))
# six 벡터의 모표준편차

## 2
### 2.1
X <- matrix(1:36, ncol = 6, byrow = TRUE)
# 1부터 36까지 6 by 6 행렬 생성


### 2.2
Y <- X %% 4
# 행렬 X의 각 성분을 4로 나눈 나머지를 성분으로 갖는 행렬 생성


## problem 3
data("iris")
setosa <- as.data.frame(iris[1:50, 1:4])

### 3.1
Sepal <- setosa$Sepal.Length + setosa$Sepal.Width
# sepal의 length와 width를 더함


### 3.2
summary(Sepal)
# 3.1에서 구한 벡터의 수치적 요약


### 3.3
plot(
  x = setosa$Petal.Length,
  y = setosa$Petal.Width,
  main = "Correlation of Petal Length and Petal Width", # 제목 설정
  xlab = "Length", # x축 이름
  ylab = "Width", # y축 이름
  xlim = c(0.5, 2), # x축 범위 설정
  ylim = c(0, 1), # y축 범위 설정
)
# setosa에 저장된 데이터에서, Petal.Length(3열)-x축와 Petal.Width(4열)-y축의 산점도


### 3.4
result <- cor(setosa$Petal.Length, setosa$Petal.Width) 

# Petal.Length(3열)와 Petal.Width(4열)의 상관계수