## 0
# 조교님 안녕하세요. 좋은 강의 감사합니다.

## 1
### 1.1
six <- seq(6, 500, by = 6)
total <- sum(six)
#seq(from, to, by): 일정한 간격의 수열을 만드는 함수
# sum(): 벡터의 모든 원소를 더함
# six 벡터의 합을 구해서 total에 저장

### 1.2
m <- mean(six)
s <- sd(six)
# mean(): 평균을 계산하는 함수
# six의 평균값(즉, 6~500 사이 6의 배수의 평균)을 계산
# sd(): 표준편차(Standard Deviation)를 계산하는 함수
# 데이터의 흩어진 정도를 나타냄

## 2
### 2.1
X <- matrix(1:36, nrow= 6, byrow = TRUE)
# matrix(): 행렬 생성 함수
# 1부터 36까지의 숫자를 6행으로 만들고 (nrow=6), byrow=TRUE로 설정해 행 단위로 채움

### 2.2
Y <- X %% 4
# %%: 나머지 연산자 (modulus)
# X의 각 원소를 4로 나눈 나머지를 계산하여 같은 크기의 행렬로 반환


## problem 3
data("iris")
setosa <- as.data.frame(iris[1:50, 1:4])
# data(): R 내장 데이터셋 불러오기
# iris 데이터 중 1~50번째 행(즉, 'setosa' 종)만 선택
# 첫 4개의 열(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)만 사용
# data.frame으로 변환 (정식 데이터프레임으로 작업하기 위함)


### 3.1
Sepal<- setosa$Sepal.Length + setosa$Sepal.Width
# '$' 연산자: 데이터프레임에서 특정 열(column)을 선택
# 두 변수를 더해서 새로운 벡터 생성 (꽃받침 길이 + 폭)
# Sepal이라는 새로운 변수에 저장

### 3.2
summary(Sepal)
# summary(): 벡터나 데이터프레임의 요약 통계량을 보여줌
# Sepal의 최소값, 1사분위수, 중앙값, 평균, 3사분위수, 최대값을 출력


### 3.3
plot(setosa$Petal.Length, setosa$Petal.Width,
     main = 'Correlation of Petal Length and Petal Width',
     xlab = "Length", ylab = "Width",
     xlim = c(0.5,2), ylim = c(0,1))
# plot(x, y): 산점도(scatter plot) 그리기
# x축: Petal.Length, y축: Petal.Width
# main, xlab, ylab: 그래프 제목과 축 이름 지정
# xlim, ylim: 그래프 범위 설정 (너비 제한)
# -> 꽃잎 길이와 폭의 상관관계 시각화


### 3.4
result <- cor(setosa$Petal.Length, setosa$Petal.Width)
# cor(x, y): 상관계수(correlation coefficient) 계산
# 두 변수 간의 선형 관계 강도를 -1~1 사이 값으로 반환
# 1에 가까울수록 강한 양의 상관관계, -1에 가까울수록 강한 음의 상관관계
# Petal.Length와 Petal.Width가 비슷하게 증가하는지를 수치로 평가