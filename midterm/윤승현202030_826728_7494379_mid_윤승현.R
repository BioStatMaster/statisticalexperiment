set.seed(1)

## 1
#### 1.1
x <- seq(-4,4,0.01)

#### 1.2
#plot을 이용해서 함수를 저장해주자.
fx1 <- plot(x,exp((-x^2)/2)/sqrt(2*pi), type="l")

#### 1.3
#1.2의 식과 같다.
fx1 <- plot(x,exp((-x^2)/2)/sqrt(2*pi), type="l")

## 2
#### 2.1
#균등분포의 랜덤추출은 runif를 사용한다.
u <- runif(20000,0,1)

#### 2.2
#정규분포의 분위수는 qnorm을 사용한다.
fx2 <- qnorm(u,0,1)

#### 2.3
#hist를 이용하되, 구간의 수를 30으로 하므로 length.out을 31로 설정하여야한다.
hist(fx2,breaks =seq(min(fx2),max(fx2), length.out=31), freq=FALSE)

#### 2.4
#정규분포의 표준편차를 조정해주자. 균등분포 (0,1)의 표준편차는 1/12이고 표본 수가 20000개 이므로 표준편차를 1/(12*sqrt(20000))로조정해주면 가까워진다.

## 3
#### 3.1
#read.csv를 사용하여 파일을 열어주자.
bodydims <- read.csv("bodydims.csv")

#### 3.2
#행 추출을 사용하여 성별별로 자료를 나눠주자.
bodydims.m <-bodydims[bodydims$sex==1,]
bodydims.f <-bodydims[bodydims$sex==0,]

#### 3.3
#모집단으로 가정했으므로 분산을 구할때, 길이-1/길이를 꼭 곱해줘야 모분산이 된다.
m1 <- mean(bodydims.m$hgt)
s1 <- var(bodydims.m$hgt)*(length(bodydims.m)-1)/(bodydims.m)

m2 <- mean(bodydims.f$hgt)
s2 <-var(bodydims.f$hgt)*(length(bodydims.f)-1)/(bodydims.f)

## 4
#### 4.1
# 귀무가설: mu=11
# 대립가설: mu!=11

#### 4.2
#모 표준편차를 알고 표본의 수가 충분히 크므로 중심극한 정리에 의해 표본평균이 정규분포를 따른다는 사실을 이용해검정통계량을 구해주자.
Z <- (10-11)/(sqrt(4.5)/sqrt(36))

#### 4.3
# 조건문 사용-
#Z가 음수임이 자명하므로 양측 검정 중 왼쪽만 보도록 하자. 이때 true임을 알 수 있다.

is_reject <- if (pnorm(Z)<=0.025) {TRUE} else {FALSE}

#### 4.4
#관측값이 기각역에 속하므로 귀무가설을 기각하고 통계적으로 유의미하다.