set.seed(1) # 문제 풀기 전 실행시켜주세요.

## 1
#### 1.1
x <- seq(from=-4, to=4, by=0.01) #seq 함수를 이용하여 변수 x에 저장

#### 1.2
fx1 <- (1/sqrt(2 * pi))*exp(-x^2/2) #정규분포함수 

#### 1.3
plot(x, fx1,                                   #산점도를 plot 함수로 만들기
     type = 'l',                               # 그래프를 선으로 그리기 위하여
     main = "표준정규분포 확률밀도함수",       # 그래프의 제목
     xlab = "x", 
     ylab = "f(x)") 
## 2
#### 2.1
u <- runif(20000, min = 0, max = 1)         # 0~1 범위 내에서, 2만 번 시행

#### 2.2
fx2 <- qnorm(u, mean=0, sd=1) 

#### 2.3
hist(fx2,                        #히스토그램 그리기
     breaks = 30, 
     freq = FALSE, 
     main = "표준정규분포 분위수를 히스토그램으로", 
     xlab = "F-1(U)",
     ylab = "Probability Density")

#### 2.4
# 답: 표본의 개수를 현재의 20000개보다 더 증가시킨다면
# 히스토그램을 더 빨간선에 가깝게 만들 수 있을 것이다. 

## 3
#### 3.1
bodydims <- read.csv("bodydims.csv")

#### 3.2
bodydims.m <- bodydims[bodydims$sex==1] #bodydims.sex가 1이면 남성으로
bodydims.f <- bodydims[bodydims$sex==0] #bodydims.sex가 0이면 여성으로

#### 3.3
m1 <- mean(bodydims.m$hgt)            #남자 평균
n_1 <- bodydims.m$hgt
n <- len(n_1)
s1 <- var(bodydims.m$hgt) * ((n-1)/n)  #남자 분산

m2 <- mean(bodydims.f$hgt)             #여자 평균
n_2 <- bodydims.f$hgt
k <- len(n_2)
s2 <- var(bodydims.f$hgt) * ((k-1)/k)   #여자 분산

## 4
#### 4.1
# 귀무가설: 전체 통계학 실험 수강생들의 일주일 커피 소비량의 실제 평균은 11자이다.
# 대립가설: 전체 통계학 실험 수강생들의 일주일 커피 소비량의 실제 평균은 11잔이 아니다.

#### 4.2
Z <- (10-11)/(sqrt(4.5)/sqrt(36)) 
# {표본평균 - (귀무가설에 따른) 모평균}을 표준편차/ sqrt(표본 수)로 나눈다. 

#### 4.3
# 조건문 사용.

critical_value <- 1.96 # 유의수준 5%에 해당하는 양측 검정의 임계값

if (abs(Z) >= critical_value) {
  is_reject <- print("TRUE")
  }else {
    is_reject <- print("FALSE")
    }        # 조건문 작성: |Z| >= 1.96 이면 TRUE (기각)

#### 4.4 
# 위 4.3의 식에 따라, 검정통계량을 Z를 계산하면 1.96의 절댓값보다 큰, 
# 약 -2.8284가 나와, 기각역(Z ≤ -1.96 또는 Z ≥ 1.96)에 속하게 된다. 따라서 TRUE가 출력된다.
# 결론적으로, 유의수준 5% 하에서, 검정통계량 Z(-2.8284)가 기각역에
# 속하여 기각여부가 TRUE가 나오므로, 귀무가설을 기각한다.
# 다시 말해, 전체 통계학 실험 수강생들의 일주일 커피 소비량의
# 실제 평균은 11잔이 아니라고 할 수 있다.