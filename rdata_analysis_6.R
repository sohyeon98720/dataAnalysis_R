# gplot()은 기능 단순. ggplot()은 세부 조작기능 많음
# 산점도 그래프(scatter plot)
mpg <- as.data.frame(ggplot2::mpg)
library(ggplot2)
ggplot(data=mpg, aes(x=displ,y=hwy))
ggplot(data=mpg, aes(x=displ,y=hwy))+geom_point()
# 축범위 조정(xlim,ylim)
ggplot(data=mpg, aes(x=displ,y=hwy))+geom_point()+xlim(3,6)
ggplot(data=mpg, aes(x=displ,y=hwy))+geom_point()+
  xlim(3,6)+
  ylim(10,30)

# 퀴즈 - mpg, midwest 데이터이용하기
# (mpg)x-cty, y-hwy 인 scatterplot 만들기
ggplot(data=mpg,aes(x=cty,y=hwy)) + geom_point()
# (midwest)x-poptotal, y-popasian인 scatter plot + 조건
# 조건: 전체인구 50만명 이하 & 아시아인 인구 1만명 이하인 지역만 표시
ggplot(data=midwest,aes(x=poptotal, y=popasian))+
  geom_point()+xlim(0,500000)+ylim(0,10000)

# 막대그래프(bar chart)
library(dplyr)
df_mpg <- mpg %>% group_by(drv) %>% summarise(mean_hwy=mean(hwy))
df_mpg
ggplot(data=df_mpg, aes(x=drv,y=mean_hwy)) + geom_col()
# barplot + 정렬
ggplot(data=df_mpg,aes(x=reorder(drv,-mean_hwy),y=mean_hwy))+geom_col()
#빈도(값의 갯수)로 barplot그리기
ggplot(data=mpg,aes(x=drv))+geom_bar() #x는 범주형변수
ggplot(data=mpg,aes(x=hwy))+geom_bar() #x는 연속형변수

#평균막대그래프 - 평균표를 먼저 만든 후 그래프 생성 - geom_col()
#빈도막대그래프 - 원래데이터로 바로 그래프 생성 - geom_bar()

# 퀴즈 - mpg데이터 이용
# suv차종을 대상으로 평균cty가 가장 높은 회사 5곳을 막대그래프로 표현 + 높은순정렬
df_suv <- mpg %>% filter(class=='suv') %>% 
  group_by(manufacturer) %>% summarise(mean_cty=mean(cty)) %>% 
  arrange(desc(mean_cty)) %>% head(5)
df_suv
ggplot(data=df_suv,aes(x=reorder(manufacturer,-mean_cty),
                       y=mean_cty)) + geom_col()
# 어떤 class가 가장 많은지 빈도 막대 그래프
ggplot(data=mpg,aes(x=class)) + geom_bar()

# 선그래프 - line chart / time series chart
# time seires chart: 환율, 주가지수 등 경제지표가 시간에 따라 어떻게 변하는지 표현할 때 사용
ecomonics <- as.data.frame(ggplot2::economics)
View(ecomonics)
summary(ecomonics)
ggplot(data=economics,aes(x=date,y=unemploy))+geom_line()
# 퀴즈 - economics데이터 이용하기
# psavert(개인저축률)가 시간에 따라 어떻게 변해왔는지 그래프로 표현
ggplot(data=economics,aes(x=date,y=psavert))+geom_line()

# boxplot - 데이터의 분포를 상자모양으로 표현한 그래프
ggplot(data=mpg,aes(x=drv,y=hwy))+geom_boxplot()
# 박스밖의 점: 이상치, 아래부터 순서대로: min,Q1,median(Q2),Q3,max(75~100이내)

# 퀴즈 - mpg데이터 이용
# class가 compact,subcompact,suv인 자동차의 cty가 어떻게 다른지 boxplot으로 비교
class_mpg <- mpg %>% filter(class %in% c("compact","subcompact","suv"))
class_mpg
ggplot(data=class_mpg,aes(x=class,y=cty)) + geom_boxplot()

#ggplot에 대해 더 많은 기능을 알고싶다? -> help>>cheatsheets>>data visualization with ggplot2
#이미지 저장하는방법 -> 오른쪽 plots에서 export