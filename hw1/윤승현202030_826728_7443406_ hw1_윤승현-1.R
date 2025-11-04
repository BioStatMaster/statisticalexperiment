
# 문제 0
# 조교님, 안녕하세요. 과제 채점하시느라 고생 많으십니다.


# 문제 1 - Vector

# 문제 1.1
six <- seq(6, 500, 6)
total <- sum(six)


# 문제 1.2
m <- mean(six)
s <- sd(six)


# 문제 2 - Matrix

# 문제 2.1
X <- matrix(1:36, nrow = 6, ncol = 6, byrow = TRUE)


# 문제 2.2
# skeleton 파일에 따라 변수명을 Y로 수정
Y <- X %% 4


# 문제 3 - DataFrame
data("iris")
setosa <- as.data.frame(iris[1:50, 1:4])


# 문제 3.1
Sepal <- setosa$Sepal.Length + setosa$Sepal.Width


# 문제 3.2
summary(Sepal)


# 문제 3.3
plot(
  x = setosa$Petal.Length,
  y = setosa$Petal.Width,
  main = "Correlation of Petal Length and Petal Width",
  xlab = "Length",
  ylab = "Width",
  xlim = c(0.5, 2),
  ylim = c(0, 1)
)


# 문제 3.4
result <- cor(setosa$Petal.Length, setosa$Petal.Width)