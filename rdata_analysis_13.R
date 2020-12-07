# t-검정, t-test
# compact 자동차와 suv 자동차의 도시 연비 t 검정
mpg <- as.data.frame(ggplot2::mpg)

library(dplyr)
mpg_diff <- mpg %>% 
  select(class, cty) %>% 
  filter(class %in% c('compact','suv'))

head(mpg_diff)
table(mpg_diff$class)

t.test(data=mpg_diff, cty~class, var.equal=T)
# cty는 비교할 값, class는 비교할 집단
# 여기서는 집단 간 분산이 같다고 가정
# 각 집단의 cty 평균결과가 산출됨

# 일반 휘발유와 고급 휘발유의 도시 연비 t 검정
mpg_diff2 <- mpg %>% 
  select(fl, cty) %>% 
  filter(fl %in% c('r','p')) # regular, premuim

table(mpg_diff2$fl)

t.test(data=mpg_diff2, cty~fl, var.equal=T)
# p-value가 0.2875 -> 실제로는 별 차이가 없는데 우연에 의해
# 이런 차이가 관찰될 확률이 28.75%라는 의미
# => 고급 휘발유 자동차의 도시 연비 평균이 0.6정도 높지만
# 이런 정도의 차이는 우연히 발생했을 가능성이 크다고 해석

# 12주차 건강검진 데이터에서 성별에 따른 신장에 차이가 있는지 분석
library(readr)
GJ_2002 <- read_csv('C:/Users/sohyeon/r_data_12weeks/GJ_2002_SAMPLE.CSV',locale=locale(encoding='euc-kr'))
summary(GJ_2002)

height_diff <- GJ_2002 %>% 
  select(SEX, HEIGHT) %>% 
  filter(SEX %in% c('1','2'))

head(height_diff)
table(height_diff)

t.test(data=height_diff, HEIGHT~SEX, var.equal=T)


# 상관분석 - 두 변수의 관계성 분석
# 0~1사이값이고 1에 가까울수록 관련성이 크다는 의미
# 양수면 정비례, 음수면 반비례

# 실업자 수와 개인 소비 지출의 상관관계
economics <- as.data.frame(ggplot2::economics)

cor.test(economics$unemploy, economics$pce)
# 0.614 -> 뚜렷한 양의 선형관계

# 12주차 건강검진 데이터에서 신장과 체중이 얼마나 관계가 있는지 분석
HE_WE <- GJ_2002 %>% 
  select(HEIGHT, WEIGHT)

head(HE_WE)

# 컬럼을 지정한 상관분석
cor(HE_WE$HEIGHT, HE_WE$WEIGHT, method='pearson')
# 데이터 전체를 지정한 상관분석
cor(HE_WE, method='pearson')
# p값을 확인하기 위한 상관분석
cor.test(HE_WE$HEIGHT, HE_WE$WEIGHT)


# 상관행렬 히트맵 만들기
# 상관행렬: 여러 변수 간 상관관계를 행렬로 나타낸 표
head(mtcars)

# 상관행렬 생성
car_cor <- cor(mtcars)
# 소수점 셋째자리에서 반올림해서 출력
round(car_cor, 2)
# => mpg(연비), cyl(실린더 수)의 상관계수가 -0.85이므로
# 연비가 높을수록 실린더 수가 적은 경향이 있음

# => cyl(실린더 수)와 wt(무게)의 상관계수가 0.78이므로
# 실린더 수가 많을수록 자동차가 무거운 경향이 있음

install.packages('corrplot')
library(corrplot)

corrplot(car_cor)
# => 상관계수가 클수록 원의 크기가 크고 색이 진함
# 상관계수가 양수면 파랑, 음수면 빨강강

corrplot(car_cor, method='number')

# 다양한 파라미터 지정하기
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

corrplot(car_cor,
         method='color', # 색깔로 표현
         col=col(200), # 색상 200개 선정
         type='lower', # 왼쪽아래 행렬만 표시
         order='hclust', # 유사한 상관계수끼리 군집화
         addCoef.col='black', # 상관계수 색깔
         tl.col='black', # 변수명 색깔
         tl.srt=45, # 변수명 45도 기울임
         diag=F) # 대각행렬 제외
