set.seed(1) # 문제 풀기 전 실행시켜주세요.

## 1
#### 1.1
x <- c(seq(from = -4, to = 4, by = 0.01))
# -4부터 4까지 0.1 간격으로 커지는 벡터

#### 1.2
fx1 <- 1 / (2 * pi) ^ (1/2) * exp(x) (-x ^ 2 / 2)


#### 1.3
print(fx1(x))

## 2
#### 2.1
runif(n, min = 0, max = 1) # 균등분포 U ~ [0, 1]


#### 2.2
fx2 <- TODO

#### 2.3
hist(u, fx2, )


#### 2.4
# 생성하는 랜덤 표본의 수를 늘리면 히스토그램을 현재보다 빨간선에 더 가깝게 만들 수 있다.

## 3
#### 3.1
bodydims <- read.csv(bodydims.csv) # read.csv를 통해 bodydims.csv 파일 불러오기 

#### 3.2
bodydims.m <- factor(bodydims$sex=1) 
bodydims.f <- factor(bodydims$sex=0) # 변수 sex 값에 따라 분류



#### 3.3
m1 <- mean(bodydims.m$hgt)
s1 <- var(bodydims.m$hgt)

m2 <- mean(bodydims.f$hgt)
s2 <- var(bodydims.f$hgt) # 각 성별의 자료에서 $를 통해 신장만 추출 

## 4
#### 4.1
# 귀무가설: 전체 통계학 실험 수강생들의 일주일 커피 소비량의 실제 평균은 11잔이 아니다
# 대립가설: 전체 통계학 실험 수강생들의 일주일 커피 소비량의 실제 평균은 11잔이다

#### 4.2
Z <- (11 - 10) / {(4.5) ^ (1 / 2) / (36) ^ (1 / 2)} * 1.96 # 1.96 -> 유의수준 5%

#### 4.3
# 조건문 사용
is_reject <- if (Z > 5){print("TRUE")} else {print("FALSE")}

#### 4.4
# 검정 통계량 값(5.543717)이 5를 초과하므로, 귀무가설은 기각된다. 
# 즉 전체 통계학 실험 수강생들의 일주일 커피 소비량 실제 평균은 11잔이다. 