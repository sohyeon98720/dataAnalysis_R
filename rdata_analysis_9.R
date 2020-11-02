# 지난번 데이터 이어서 실행 - 직업별 월급 차이 알아보기
class(welfare$code_job)
table(welfare$code_job)
# 직업분류코드 목록 불러오기
library(readxl)
list_job <- read_excel('./Data/Koweps_Codebook.xlsx',col_names=T,sheet=2)
head(list_job)
dim(list_job)
# welfare에 직업명 결합
library(dplyr)
welfare <- left_join(welfare, list_job, id = "code_job")
welfare %>% filter(!is.na(code_job)) %>% 
  select(code_job,job) %>% 
  head(10)

# 직업별 월급 차이 분석하기
job_income <- welfare %>% 
  filter(!is.na(job) & !is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income=mean(income))
head(job_income)

# 상위 10개 추출
top10 <- job_income %>% 
  arrange(desc(mean_income)) %>% 
  head(10)
top10

# 그래프 만들기
library(ggplot2)
ggplot(data=top10,aes(x=reorder(job,mean_income),y=mean_income)) + 
  geom_col() + coord_flip()

# 하위 10개 추출
bottom10 <- job_income %>% 
  arrange(mean_income) %>% 
  head(10)
bottom10

# 그래프 만들기
ggplot(data=bottom10,aes(x=reorder(job,-mean_income),
                         y=mean_income)) + 
  geom_col() + coord_flip() + ylim(0,850)


# 성별 직업 빈도 - 남성 직업빈도 상위 10개
job_male <- welfare %>% 
  filter(!is.na(job) & sex=='male') %>% 
  group_by(job) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% head(10)
job_male

# 성별 직업 빈도 - 여성 직업빈도 상위 10개
job_female <- welfare %>% 
  filter(!is.na(job) & sex=='female') %>% 
  group_by(job) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% head(10)
job_female

# 그래프 만들기 - 남성
ggplot(data=job_male,aes(x=reorder(job,n),y=n)) + 
  geom_col() + coord_flip()

# 그래프 만들기 - 여성
ggplot(data=job_female,aes(x=reorder(job,n),y=n)) + 
  geom_col() + coord_flip()

# 종교 유무에 따른 이혼율 - 종교
class(welfare$religion)
table(welfare$religion)

welfare$religion <- ifelse(welfare$religion==1,"yes","no")
table(welfare$religion)
qplot(welfare$religion)

# 종교 유무에 따른 이혼율 - 이혼
class(welfare$marriage)
table(welfare$marriage)

welfare$group_marriage <- ifelse(welfare$marriage==1,"marriage",
                                 ifelse(welfare$marriage==3,"divorce",NA))
table(welfare$group_marriage)
table(is.na(welfare$group_marriage))
qplot(welfare$group_marriage)

# 종교 유무에 따른 이혼율 표 만들기
religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(religion,group_marriage) %>% 
  summarise(n=n()) %>% 
  mutate(tot_group=sum(n)) %>% 
  mutate(pct=round(n/tot_group*100,1))
religion_marriage

# 위와 같은 결과지만 방식이 다른 코드(count이용)
religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  count(religion,group_marriage) %>% 
  group_by(religion) %>% 
  mutate(pct=round(n/sum(n)*100,1))
religion_marriage

# 이혼율 표 만들기
divorce <- religion_marriage %>% 
  filter(group_marriage=='divorce') %>% 
  select(religion,pct)
divorce
ggplot(data=divorce,aes(x=religion,y=pct)) + geom_col()

# 연령대별 이혼율 표 만들기
ageg_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(ageg,group_marriage) %>% 
  summarise(n=n()) %>% 
  mutate(tot_group=sum(n)) %>% 
  mutate(pct=round(n/tot_group*100,1))
ageg_marriage

# 위와 같은 결과지만 방식이 다른 코드(count이용)
ageg_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  count(ageg,group_marriage) %>% 
  group_by(ageg) %>% 
  mutate(pct=round(n/sum(n)*100,1))
ageg_marriage

# 연령대별 이혼율 그래프 만들기 - 초년제외,이혼추출
ageg_divorce <- ageg_marriage %>% 
  filter(ageg!='young' & group_marriage=='divorce') %>% 
  select(ageg,pct)
ageg_divorce

ggplot(data=ageg_divorce,aes(x=ageg,y=pct)) + geom_col()

# 연령대 및 종교 유무에 따른 이혼율 표 만들기
ageg_religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage) & ageg!='young') %>% 
  group_by(ageg,religion,group_marriage) %>% 
  summarise(n=n()) %>% 
  mutate(tot_group=sum(n)) %>% 
  mutate(pct=round(n/tot_group*100,1))
ageg_religion_marriage

# 위와 같은 결과지만 방식이 다른 코드(count이용)
ageg_religion_marriage <- welfare %>%
  filter(!is.na(group_marriage) & ageg != "young") %>%
  count(ageg, religion, group_marriage) %>%
  group_by(ageg, religion) %>%
  mutate(pct = round(n/sum(n)*100, 1))
ageg_religion_marriage

# 연령대 및 종교 유무별 이혼율 표 만들기
df_divorce <- ageg_religion_marriage %>% 
  filter(group_marriage=='divorce') %>% 
  select(ageg,religion,pct)
df_divorce

ggplot(data=df_divorce,aes(x=ageg,y=pct,fill=religion)) + 
  geom_col(position='dodge')

# 지역별 연령대 비율
class(welfare$code_region)
table(welfare$code_region)

# 지역 코드 목록 만들기
list_region <- data.frame(code_region=c(1:7),
                          region=c('서울','수도권(인청/경기)',
                                   '부산/경남/울산',
                                   '대구/경북',
                                   '대전/충남',
                                   '강원/충북',
                                   '광주/전남/전북/제주도'))
list_region

#welfare에 지역명 변수 추가
welfare <- left_join(welfare,list_region,id='code_region')
welfare %>% 
  select(code_region,region) %>% 
  head

# 지역별 연령대 비율 분석하기
region_ageg <- welfare %>% 
  group_by(region,ageg) %>% 
  summarise(n=n()) %>% 
  mutate(tot_group=sum(n)) %>% 
  mutate(pct=round(n/tot_group*100,2))
head(region_ageg)

# 위와 같은 결과지만 방식이 다른 코드(count이용)
region_ageg <- welfare %>%
  count(region, ageg) %>%
  group_by(region) %>%
  mutate(pct = round(n/sum(n)*100, 2))

# 그래프 만들기
ggplot(data=region_ageg,aes(x=region,y=pct,fill=ageg)) + 
  geom_col() + coord_flip()

# 막대 정렬하기 : 노년층 비율 높은 순
list_order_old <- region_ageg %>% 
  filter(ageg=='old') %>% 
  arrange(pct)
list_order_old

# 지역명 순서 변수 만들기
order <- list_order_old$region
order

ggplot(data=region_ageg,aes(x=region,y=pct,fill=ageg)) + 
  geom_col() + coord_flip() + scale_x_discrete(limits=order)

# 연령대 순으로 막대 색깔 나열하기
class(region_ageg$ageg)
levels(region_ageg$ageg)

region_ageg$ageg <- factor(region_ageg$ageg,
                           level=c('old','middle','young'))
class(region_ageg$ageg)
levels(region_ageg$ageg)

ggplot(data=region_ageg,aes(x=region,y=pct,fill=ageg)) + 
  geom_col() + coord_flip() + scale_x_discrete(limits=order)

