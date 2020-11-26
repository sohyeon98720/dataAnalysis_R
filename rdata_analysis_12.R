install.packages('plotly')
library(plotly)
library(ggplot2)

# 인터랙티브 산점도 그래프
p <- ggplot(data=mpg, aes(x=displ, y=hwy, col=drv)) + geom_point()
ggplotly(p)

# 인터랙티브 막대 그래프
p <- ggplot(data=diamonds, aes(x=cut, fill=clarity)) + geom_bar(position='dodge')
ggplotly(p)

# 인터랙티프 시계열 그래프
install.packages('dygraphs')
library(dygraphs)

economics <- ggplot2::economics
head(economics)

library(xts)
eco <- xts(economics$unemploy, order.by=economics$date)
head(eco)

dygraph(eco)

# 날짜범위 선택 기능
dygraph(eco) %>% dyRangeSelector()

# 여러값 표현하기: 인터랙티브 시계열 그래프에 여러 값을 동시에 표현가능
# 실업자 수와 저축률을 그래프에 함께 표현

# 저축률
eco_a <- xts(economics$psavert, order.by=economics$date)
# 실업자 수
eco_b <- xts(economics$unemploy/1000, order.by=economics$date)

# 합치기
eco2 <- cbind(eco_a, eco_b) #데이터 결합
colnames(eco2) <- c('psavert', 'unemploy') # 변수명 바꾸기
head(eco2)

dygraph(eco2) %>% dyRangeSelector()


# 데이터 로드
# csv 데이터
# 엑셀 데이터
write_csv(r_excel, 'C:/Users/sohyeon/r_data_12weeks/r_txt.txt')
# XML 데이터
install.packages('XML')
library(XML)

url <-'http://openapi.seoul.go.kr:8088/716646734e6a696e3130375a66507755/xml/SebcPublicEnterpriseKor/1/1000/'
indata <- xmlToDataFrame(url)
View(indata)

# 1~2행과 1~3열 제거`
indata <- indata[-1:-2,-1:-3]
View(indata)

# JSON 데이터
install.packages('jsonlite')
library(jsonlite)

# URL에서 json데이터 가져오기
install.packages('curl')
library(curl)
data2 <- fromJSON('https://api.github.com/users/hadley/orgs')
data2 <- as.data.frame(data2)
colnames(data2)
View(data2)

# 파일에서 json데이터 가져오기
data <- fromJSON('C:/Users/sohyeon/r_data_12weeks/전국초등학교통학구역표준데이터.json')
names(data)
data <- as.data.frame(data$records)
View(data)

# 빅데이터(feather와 fst)
# 내장 데이터셋 확인
data()
View(mtcars)

# 1억개의 난수 생성
data <- data.frame(number=rnorm(n=100000000))
dim(data)

# 패키지 설치&로드
install.packages('feather')
library(feather)

# rdata 파일형식 쓰기
system.time({
  save(data, file='C:/Users/sohyeon/r_data_12weeks/time/data.RData')})

# csv 파일형식 쓰기
system.time({
  save(data, file='C:/Users/sohyeon/r_data_12weeks/time/data.csv')})

# feather 파일형식 쓰기
system.time({
  save(data, file='C:/Users/sohyeon/r_data_12weeks/time/data.feather')})

# fst 파일형식 쓰기
system.time({
  save(data, file='C:/Users/sohyeon/r_data_12weeks/time/data.fst')})


# rdata 파일형식 읽기
system.time({
  load('C:/Users/sohyeon/r_data_12weeks/time/data.RData')})

# csv 파일형식 읽기
system.time({
  data1 <- read.csv('C:/Users/sohyeon/r_data_12weeks/time/data.csv')})

# feather 파일형식 읽기
system.time({
  data2 <- read_feather('C:/Users/sohyeon/r_data_12weeks/time/data.feather')})

# fst 파일형식 읽기
system.time({
  data3 <- read.fst('C:/Users/sohyeon/r_data_12weeks/time/data.fst')})

