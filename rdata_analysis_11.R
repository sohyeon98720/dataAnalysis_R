install.packages('ggiraphExtra')
library(ggiraphExtra)

str(USArrests)
head(USArrests)

library(tibble)
crime <- rownames_to_column(USArrests,var='state')
crime$state <- tolower(crime$state)
str(crime)

install.packages('maps')
library(maps)

# 미국 주 지도 데이터 준비하기
library(ggplot2)
states_map <- map_data('state')
str(states_map)

install.packages('mappproj')
library(mapproj)

ggChoropleth(data=crime, # 지도에 표현할 데이터
             aes(fill=Murder, # 색깔로 표현할 변수
                 map_id=state), # 지역 기준 변수
             map=states_map) # 지도 데이터

# 인터랙티브 단계 구분도 만들기
ggChoropleth(data = crime, # 지도에 표현할 데이터
             aes(fill = Murder, # 색깔로 표현할 변수
                 map_id = state), # 지역 기준 변수
             map = states_map, # 지도 데이터
             interactive = T) # 인터랙티브

# 대한민국 시도별 인구, 결핵 환자 수 단계 구분도 만들기

install.packages('stringi')
install.packages('devtools')
devtools::install_github("cardiomoon/kormaps2014")
library(kormaps2014)

str(changeCode(korpop1))

# 변수명 수정
library(dplyr)
korpop1 <- rename(korpop1,
                  pop=총인구_명,
                  name=행정구역별_읍면동)
str(changeCode(kormap1))

# 단계 구분도 만들기
ggChoropleth(data=korpop1,
             aes(fill=pop,
                 map_id=code,
                 tooltip=name),
             map=kormap1,
             interactive=T)

# 대한민국 시도별 결핵환자 수 단계 구분도 만들기
str(changeCode(tbc))
ggChoropleth(data=tbc, # 지도에 표현할 데이터
             aes(fill=NewPts, # 색깔로 표현할 변수
                 map_id=code, # 지역 기준 변수
                 tooltip=name), # 지도 위에 표시할 지역명
             map=kormap1, # 지도 데이터
             interacive=T) # 인터랙티브

