## 0
# 조교님 감사합니다! 개선했으면 사항은 없습니다 :) 

## 1
### 1.1
six <- seq(from = 6, to = 498, by = 6) # 1~500에서 가장 작은 6의 배수는 6, 가장 큰 6의 배수는 498. 6~498까지의 수를 간격을 6으로 하여 나열하면 6에서 498까지의 6의 배수의 수열이 됨.
total <- sum(six) # sum 함수를 사용하여 시퀀스 six의 모든 개체의 합을 구함.

### 1.2
m <- mean(six) # six의 평균을 구하는 함수
s <- sqrt(var(six)) # var는 분산을 구하는 함수. 분산의 제곱근은 표준편차.

## 2
### 2.1
X <- matrix(c(1:36), nrow = 6, byrow = T) # 성분은 1부터 36까지의 자연수, 행은 6개, 가로부터 채움움

### 2.2
Y <- matrix(X*0.25, nrow = 6, byrow = T) # 2.1과 동일, 그러나 성분이 x의 성분의 0.25배 

## 3
data("iris")
setosa <- as.data.frame(iris[1:50, 1:4])

### 3.1
Sepal <- setosa$Sepal.Length + setosa$Sepal.Width # setosa에서 열 "Sepal.Length"와 "Sepal.Width"를 추출한 뒤 더함

### 3.2
summary(Sepal) # Sepal의 수치적 요약: summary 함수를 통해

### 3.3
plot(setosa$Petal.Length, setosa$Petal.Width, # x축은 setosa에서 추출한 Petal.Length, y축은 setosa에서 추출한 Petal.Width 
     main = "Correlation of Petal Length and Petal Width", # 제목
     xlab = "Length", ylab = "Width", # 각 축의 이름
     xlim = c(0.5, 2), ylim = c(0.1)) # 각 축의 범위위

### 3.4
result <- cor(setosa$Petal.Length, setosa$Petal.Width) # 상관계수는 cor 함수로 계산 