## 0
# 안녕하세요. 최대한 열심히 해보겠습니다

## 1
### 1.1
six <- seq(from=6, to=500, by=6)   #시퀀스를 이용해 벡터 생성(간격 6)
total <- sum(six)    #다  더하는 함수 이용용

### 1.2
m <- mean(six)   #평균, 표준편차 함수 이용
s <- sd(six)

## 2
### 2.1
X <- matrix (c(1:36), nrow=6, byrow=T)    #행렬을 생성하는데, 들어갈 숫자들을 
                   #설정하기 위해 벡터를 만들어줌(1:36), 세로줄 6개개
### 2.2
Y <- X%%4          #X를 4로 나눈 나머지(행렬 각 성분마다 적용됨)

## problem 3
data("iris")
setosa <- as.data.frame(iris[1:50, 1:4])
head(setosa)         #head 추가해줌줌
### 3.1
Sepal <- setosa$Sepal.Length + setosa$Sepal.Width #데이터$열이름+데이터$열이름

### 3.2
summary(Sepal) #정보 요약

### 3.3
plot(
  x=setosa$Petal.Length,
  y=setosa$Petal.Width,
  main= "Correlation of Petal Length and Petal Width",
  xlab= "Length",
  ylab= "Width",
  xlim= c(0.5, 2),
  ylim= c(0,1)
)           #제목,  축, 범위 설정, 산점도 그리기


### 3.4
result <- cor(setosa$Petal.Length, setosa$Petal.Width)#3열, 4열의 상관계수 저장장