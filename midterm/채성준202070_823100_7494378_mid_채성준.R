set.seed(1) # 문제 풀기 전 실행시켜주세요.

## 1
#### 1.1
x <- seq(from=-4,to=4,by=0.01)
# seq함수 사용해 벡터 생성 : 시작과 끝을 from, to로 규정하고 by로 간격 정함

#### 1.2
fx1 <- (1/sqrt(2*pi)*exp(-x^2/2))
# 함숫값에 해당하는 주어진 식을 sqrt(제곱근), exp(지수) 함수 등을 사용해
# fx1에 대응되도록 표현

#### 1.3
plot(x,fx1,xlim=c(-4,4),type='l')
# plot 함수 사용해 변수 두개를 x, fx1로 잡은 뒤 type를 "l"로 지정하여 직선 유도
# xlim 코드를 통해 x값의 범위 지정

## 2
#### 2.1
u <- runif(20000, 0, 1)
# 균등분포에서 무작위 표본을 사용하는 runif 함수 사용
# 균등분포 U(a,b) 일때 runif(n, a, b)

#### 2.2
fx2 <- qnorm(u, 0, 1)
# 정규분포의 분위수를 출력하는 함수는 qnorm
# 표준정규분포의 평균은 0이고 표준편차는 1이므로 이를 지정

#### 2.3
hist(fx2, freq=F, breaks=seq(min(fx2),max(fx2),length.out=31))
# hist 함수 사용해 히스토그램 생성
# freq=F 코드 작성함으로서 세로축이 density로 표현되도록 함
# breaks=~ 코드 작성함으로서 가로축에 표현되는 수의 범위 지정, length.out=31
# 로 지정함으로써 구간의 개수가 30이 되도록 함

#### 2.4
# 구간의 길이(여기서는 length.out에 해당하는 수)를 늘리면 된다.

## 3
#### 3.1
bodydims <- read.csv("C:/Users/성준/Downloads/bodydims.csv",header=T)
# read.csv 함수 사용해 불러온 후 bodydims 변수에 지정
# ("파일 저장 경로", header=T) 형식

#### 3.2
bodydims.m <- bodydims[bodydims$sex==1,]
bodydims.f <- bodydims[bodydims$sex==0,]
# 벡터 bodydims에서 sex값이 1인 변수만 추출해서 m에 지정하도록 함
# 이때 행에 존재하므로 대괄호[] 안에 뒤에 콤마를 붙여 행에 해당하는 변수를
# 추출하도록 함

#### 3.3
n<-length(bodydims.m$hgt)
m1 <- mean(bodydims.m$hgt)
s1 <- var(bodydims.m$hgt)*(n-1)/n
# bodydims 내에서 hgt 변수 찾는 것이므로 $로 연결
# 각각 mean 함수와 var 함수를 사용해 평균과 분산 구함
# 사전에 n(hgt변수의 개수)를 추가로 지정하여 분산에 (n-1)/n을 곱함으로써
# 정확도를 높임

n<-length(bodydims.f$hgt)
m2 <- mean(bodydims.f$hgt)
s2 <- var(bodydims.f$hgt)*(n-1)/n
# 위의 경우과 같은 논리에서 bodydims.f의 데이터로만 바꿔줌

## 4
#### 4.1
# 귀무가설: 전체 통계학 실험 수강생들의 일주일 커피 소비량 실제 평균은 11잔이다.
# 대립가설: 전체 통계학 실험 수강생들의 일주일 커피 소비량 실제 평균은 11잔이 아니다.

#### 4.2
Z <- -0.07856742
# (표본평균) - mu / (sigma/sqrt(n))으로 구함
# mu=11, n=36
# sigma=sqrt(4.5)
(-1)/sqrt(4.5)/6
  
#### 4.3
# 조건문 사용
if (dnorm(qnorm(0.05))==Z){is_reject<-FALE} else {is_reject<-TRUE}
is_reject
# dnorm qnorm 사용해 검정통계량과 비교 후 같으면 true, 다르면 false 내림

#### 4.4
# 귀무가설을 기각하고 대립가설을 채택한다.
# 즉, 통계학실험 수강생들의 일주일 커피 소비량 실제 평균이 11잔이 아니라고 결론
# 내릴 수 있다.






