install.packages('foreign')
library(foreign) #SPSS파일 로드
library(dplyr) # 전처리
library(ggplot2) #시각화
library(readxl) #엑셀파일불러오기
# 데이터 준비하기(dataframe=T안하면 list형태로 불러와짐)
raw_welfare <- read.spss(file='./data/Koweps_hpc10_2015_beta1.sav',
                         to.data.frame=T)
# 복사본 만들기
welfare <- raw_welfare

# 데이터검토하기
head(welfare)
tail(welfare)
View(welfare)
dim(welfare)
str(welfare)
summary(welfare)
# 대규모 데이터 + 많은 변수 + 코드명 -> 전체 데이터 구조 파악 어려움
# => 변수명을 쉬운 단어로 바꾼 후 데이터 파악

# 변수명 바꾸기-rename
welfare <- rename(welfare,
                  sex=h10_g3,
                  birth=h10_g4,
                  marriage=h10_g10,
                  religion=h10_g11,
                  income=p1002_8aq1,
                  code_job=h10_eco9,
                  code_region=h10_reg7)

# 변수검토 - 성별
class(welfare$sex) # type확인
table(welfare$sex) # 이상치확인 -> 없음
# 이상치 결측 처리
welfare$sex <- ifelse(welfare$sex ==9,NA,welfare$sex)
# 결측치 확인
table(is.na(welfare$sex))

# 성별 항목 이름 부여(1,2 -> male,female)
welfare$sex <- ifelse(welfare$sex==1,'male','female')
table(welfare$sex)

# 간단한 그래프 제공하는 함수 - gplot
qplot(welfare$sex)

# 변수검토 - 월급
class(welfare$income)
summary(welfare$income)

qplot(welfare$income)
qplot(welfare$income) + xlim(0,1000)

# 이상치 결측처리
welfare$income <- ifelse(welfare$income %in% c(0,9999),NA,welfare$income)
table(is.na(welfare$income))

# 성별 월급 평균표 만들기
sex_income <- welfare %>% filter(!is.na(income)) %>% 
  group_by(sex) %>% summarise(mean_income=mean(income))

sex_income
ggplot(data=sex_income,aes(x=sex,y=mean_income)) + geom_col()

# 변수검토 - 나이,월급
# 나이와 월급의 관계 알아보기
class(welfare$birth)
# 이상치 확인
summary(welfare$birth)
qplot(welfare$birth)

# 결측치 확인 및 이상치 결측 처리
table(is.na(welfare$birth))

welfare$birth <- ifelse(welfare$birth ==9999,NA,welfare$birth)
table(is.na(welfare$birth))

# 파생변수 만들기 - 나이(데이터 수집 년도가 2015년임)
welfare$age <- 2015 - welfare$birth +1
summary(welfare$age)
qplot(welfare$age)

# 나이에 따른 월급 평균표 만들기
age_income <- welfare %>% filter(!is.na(income)) %>% 
  group_by(age) %>% summarise(mean_income=mean(income))
head(age_income)

# 그래프로 시각화
ggplot(data=age_income,aes(x=age,y=mean_income)) + geom_line()

# 연령대에 따른 월급 차이 분석하기
# 파생변수 만들기 - 연령대
welfare <- welfare %>% mutate(ageg=ifelse(age<30,'young',
                                          ifelse(age<=59,'middle','old')))

table(welfare$ageg)
qplot(welfare$ageg)

# 연령대별 월급 평균표 만들기
ageg_income <- welfare %>% filter(!is.na(income)) %>% 
  group_by(ageg) %>% summarise(mean_income=mean(income))
ageg_income

ggplot(data=ageg_income,aes(x=ageg,y=mean_income)) + geom_col()
# 그래프 정렬하여 시각화
ggplot(data=ageg_income,aes(x=ageg,y=mean_income)) + 
  geom_col()+
  scale_x_discrete(limits=c('young','middle','old'))

# 연령대 및 성별 월급 평균표 만들기
sex_income <- welfare %>% filter(!is.na(income)) %>% 
  group_by(ageg,sex) %>% summarise(mean_income=mean(income))

sex_income  
# 그래프 정렬, 범주별 색깔 시각화
ggplot(data=sex_income,aes(x=ageg,y=mean_income,fill=sex)) + 
  geom_col()+
  scale_x_discrete(limits=c('young','middle','old'))

# position=dodge로 그래프 분리하기
ggplot(data=sex_income,aes(x=ageg,y=mean_income,fill=sex)) + 
  geom_col(position='dodge')+
  scale_x_discrete(limits=c('young','middle','old'))


# 성별 연령별 월급 평균표 만들기
sex_age <- welfare %>% filter(!is.na(income)) %>% 
  group_by(age,sex) %>% summarise(mean_income=mean(income))
sex_age

# 그래프로 시각화, col 파라미터 추가
ggplot(data=sex_age,aes(x=age,y=mean_income,col=sex)) + geom_line()