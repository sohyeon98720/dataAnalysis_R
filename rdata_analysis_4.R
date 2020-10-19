install.packages('dplyr')
library(dplyr)
exam <- read.csv('./Data/csv_exam.csv')
exam

# 조건에 맞는 데이터 추출하기 - filter(ctrl + shift + m)
exam %>% filter(class==1)

exam %>% filter(class==1 & math>=50)

exam %>% filter(math>=90 | english>=90)

exam %>% filter(class %in% c(1,3,5))

class1 <- exam %>% filter(class==1)
class2 <- exam %>% filter(class==2)
mean(class1$math)
mean(class2$math)

# mpg 데이터로 분석문제 해결하기
# 1. 자동차 배기량에 따라 고속도로 연비가 다른지 알아보려한다. displ이 4이하인
# 자동차와 5이상인 자동차 중 어떤차의 hwy가 평균적으로 높은지 알아보세요
mpg <- as.data.frame(ggplot2::mpg)

Mpg_4 <- mpg %>% filter(displ<=4)
Mpg_5 <- mpg %>% filter(displ>=5)

mean(Mpg_4$hwy)
mean(Mpg_5$hwy)
# -> Mpg_4가 더 높음

# 2. 자동차 제조회사에 따라 도시 연비가 다른지 알아보려한다.
# audi와 toyota중 어느 manufacturer의 cty가 평균적으로 높은지 알아보세요
mpg_audi <- mpg %>% filter(manufacturer=='audi')
mpg_toyota <- mpg %>% filter(manufacturer=='toyota')

mean(mpg_audi$cty)
mean(mpg_toyota$cty)
# -> mpg_toyota가 더 높음

# 3. chevrolet, ford, honda 자동자의 고속도로 연비 평균을 구하려한다.
# 자동차를 추출한 뒤 hwy전체 평균을 구하시오
mpg_new <- mpg %>% filter(manufacturer %in% c('chevrolet','ford','honda'))
mean(mpg_new$hwy)

# 필요한 변수만 추출하기 - select (ctrl + shift + m이용)
exam %>% select(math)
exam %>% select(class,math,english)
# 제외
exam %>% select(-math)
# filter + select
exam %>% filter(class==1) %>% select(english)

# 1. mpg 데이터에서 class,cty를 추출해 새로운 데이터 만들기
mpg_double_c <- mpg %>% select(class,cty)
mpg_double_c

# 2. 위 dataframe에서 class가 suv인 차와 compact인 차 중 cty가 더 높은 차는?
mpg_suv <- mpg_double_c %>% filter(class=='suv')
mpg_compact <- mpg_double_c %>% filter(class=='compact')

mean(mpg_suv$cty)
mean(mpg_compact$cty)
# -> compact가 더 높음

# 정렬하기
exam %>% arrange(math) # 오름차순
exam %>% arrange(desc(math)) # 내림차순

# 1. audi자동차 중 hwy순으로 1~5위 출력
mpg %>% filter(manufacturer=='audi') %>% arrange(desc(hwy)) %>% head(5)


# 파생변수 추가하기 - mutate
exam %>% mutate(total=math + english + science) %>% head
exam %>% mutate(total=math + english + science,
                mean=(math + english + science)/3) %>% 
  head

# mutate + ifelse
exam %>% mutate(test=ifelse(science >=60,"pass","fail")) %>% head

# mutate + dplyr코드(arrange)
exam %>% mutate(total=math + english + science) %>% 
  arrange(desc(total)) %>% 
  head

# 1. mpg데이터 복사본을 만들고 cty와 hwy를 더한 합산연비변수 추가
mpg_new <- mpg
mpg_new <- mpg_new %>% mutate(total=cty+hwy)
mpg_new %>% head

# 2. 합산연비변수를 2로나눠 평균연비변수 추가
mpg_new <- mpg_new %>% mutate(mean=total/2)
mpg_new %>% head

# 3. 평균연비변수가 가장 높은 자동차 세종의 데이터 출력
mpg_new %>% arrange(desc(mean)) %>% head(3)

# 1~3합쳐서 하나의 구문으로 만들기
mpg %>% mutate(total=cty+hwy,
               mean=total/2) %>% 
  arrange(desc(mean)) %>% head(3)


# 집단별로 요약하기 - group_by, summarise
exam %>% summarise(mean_math=mean(math))

exam %>% group_by(class) %>% 
  summarise(mean_math=mean(math))

exam %>% group_by(class) %>% 
  summarise(mean_math=mean(math),
            sum_math=sum(math),
            medain_math=median(math),
            n=n()) #학생 수(빈도)

# 각 집단별로 다시 집단 나누기
mpg %>% group_by(manufacturer,drv) %>% 
  summarise(mean_cty=mean(cty)) %>% 
  head(10)

# 다양한 함수 조합 - 회사별로 suv자동차의 
# 도시 및 고속도로 통합연비 평균을 구해 내림차순 정렬 후 1~5위 출력
mpg %>% group_by(manufacturer) %>% 
  filter(class=='suv') %>% 
  mutate(tot=(cty+hwy)/2) %>% # 얘는 도시 및 고속도로 통합연비 평균
  summarise(mean_tot=mean(tot)) %>% # 얘는 회사별 tot평균
  arrange(desc(mean_tot)) %>% 
  head(5)

# 어떤 회사에서 compact차종을 많이 생상하는지
mpg %>% filter(class=='compact') %>% 
  group_by(manufacturer) %>% 
  summarise(count=n()) %>% 
  arrange(desc(count))

# 데이터 합치기(가로) - join
test1 <- data.frame(id=c(1,2,3,4,5),
                    midterm=c(60,80,70,90,85))
test2 <- data.frame(id=c(1,2,3,4,5),
                    final=c(70,83,65,95,80))
test1
test2

# id기준으로 합치기
total <- left_join(test1,test2,by='id')
total

# 다른 예시
name <- data.frame(class=c(1,2,3,4,5),
                   teacher=c('kim','lee','park','choi','jung'))
name
exam_new <- left_join(exam,name,by='class')
exam_new

# 데이터 합치기(세로) - bind_rows
group_a <- data.frame(id=c(1,2,3,4,5),
                      test=c(60,80,70,90,85))
group_b <- data.frame(id=c(6,7,8,9,10),
                      test=c(70,83,65,95,80))
group_all <- bind_rows(group_a,group_b)
group_all

# 퀴즈
fuel <- data.frame(fl=c('c','d','e','p','r'),
                   price_fl=c(2.35,2.38,2.11,2.76,2.22),
                   stringsAsFactors = F)
fuel

# 1. mpg에는 fl은 있지만 연료가격변수는 없다. fuel을 이용해 추가하라
mpg <- left_join(mpg,fuel,by='fl')
mpg

# 2. model,fl,price_fl추출하여 앞부분 5행 출력
mpg %>% select(model,fl,price_fl) %>% head(5)