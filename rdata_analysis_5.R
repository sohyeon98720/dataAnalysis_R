df <- data.frame(sex=c('M','F',NA,'M','F'),
                 score=c(5,4,3,4,NA))
df
# 결측치 확인 함수 -> dis.na(변수)
is.na(df)

# 결측치 빈도 출력
table(is.na(df))
table(is.na(df$sex))
table(is.na(df$score))

# 결측치가 있는 데이터는 평균 및 합계 산출이 안됨
mean(df$score) # -> na
sum(df$score) # -> na

# 결측치있는 행 제거하기
library(dplyr)
df %>% filter(is.na(score))
df %>% filter(!is.na(score))

df_nomiss <- df %>% filter(!is.na(score))
mean(df_nomiss$score)

# 결측치있는 행 모두 제거하기
df_nomiss <- df %>% filter(!is.na(score) & !is.na(sex))
df_nomiss

# 함수 이용해 결측치 제거하기 : na.omit(데이터프레임)
df_nomiss2 <- na.omit(df)
df_nomiss2
# -> 그런데 분석에 필요한 데이터까지 손실 될 가능성 유의 -> filter가 더 나을수도있음

# 통계함수 사용시 na제거 : na.rm=T
mean(df$score, na.rm=T)
sum(df$score, na.rm=T)

exam <- read.csv('./Data/csv_exam.csv')
exam[c(3,8,15),"math"] <- NA # -> 3,8,15행 math열에 NA할당
exam

exam %>% summarise(mean_math=mean(math)) # -> NA가 있기때문에 결과는 NA
exam %>% summarise(mean_math=mean(math,na.rm=T)) # -> 그럴땐 요롷게~
mean(exam$math,na.rm=T)

# 결측치를 평균값으로 대체하기
exam$math <- ifelse(is.na(exam$math),55,exam$math)
table(is.na(exam$math)) # -> false가 20개라는 뜻
exam # -> 결측치 없어짐

# 퀴즈 - mpg데이터에 임의로 결측치값 만들기
mpg <- as.data.frame(ggplot2::mpg)
mpg[c(65,124,131,153,212),'hwy'] <- NA
# >> drv별 hwy 평균이 어떻게 다른지(1. 결측치 먼저 확인)
table(is.na(mpg$drv)) # -> 여긴 없고
table(is.na(mpg$hwy)) # -> 여긴 5개 결측치 존재
# >> filter로 결측치 제외하고 hwy평균 구하기
mpg %>% filter(!is.na(hwy)) %>% group_by(drv) %>% 
  summarise(mean_hwy=mean(hwy))

# 이상치 제거
### 이상치: 정상범주에서 크게 벗어난 값 -> 분석 결과를 왜곡시킴
outlier <- data.frame(sex=c(1,2,1,3,2,1),
                      score=c(5,4,3,4,2,6))
outlier

table(outlier$sex)
table(outlier$score)

# 이상치를 결측치 처리한 뒤 통계함수 수행
outlier$sex <- ifelse(outlier$sex==3,NA,outlier$sex)
outlier

outlier$score <- ifelse(outlier$score >5,NA,outlier$score)
outlier

outlier %>% filter(!is.na(sex) & !is.na(score)) %>% 
  group_by(sex) %>% 
  summarise(mean_score=mean(score))

# boxplot으로 이상치 확인하기
mpg <- as.data.frame(ggplot2::mpg)
boxplot(mpg$hwy)
# boxplot 통계치 출력
boxplot(mpg$hwy)$stats

# 12~37 벗어나면 NA 할당
mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy >37, NA, mpg$hwy)
table(is.na(mpg$hwy))

mpg %>% group_by(drv) %>% 
  summarise(mean_hwy=mean(hwy,na.rm=T))

# 퀴즈 - mpg데이터에 이상치 만들기
mpg <- as.data.frame(ggplot2::mpg)
mpg[c(10,14,58,93),'drv'] <- 'k'
mpg[c(29,43,129,203),'cty'] <- c(3,4,39,42)
# 퀴즈 - drv 이상치 확인후 처리(%in%포함)
table(mpg$drv)
mpg$drv <- ifelse(mpg$drv %in% c("4","f","r"),mpg$drv,NA)
table(mpg$drv)
# 퀴즈 - boxplot으로 cty 이상치 확인 후 처리
boxplot(mpg$cty)
boxplot(mpg$cty)$stats
mpg$cty <- ifelse(mpg$cty<9 | mpg$cty >26,NA,mpg$cty)
boxplot(mpg$cty)
# 퀴즈 - drv별로 cty 평균이 어떤지 확인
mpg %>% filter(!is.na(drv) & !is.na(cty)) %>% 
  group_by(drv) %>% 
  summarise(mean_hwy=mean(cty))