install.packages('plotly')
library(plotly)
library(ggplot2)

# ���ͷ�Ƽ�� ������ �׷���
p <- ggplot(data=mpg, aes(x=displ, y=hwy, col=drv)) + geom_point()
ggplotly(p)

# ���ͷ�Ƽ�� ���� �׷���
p <- ggplot(data=diamonds, aes(x=cut, fill=clarity)) + geom_bar(position='dodge')
ggplotly(p)

# ���ͷ�Ƽ�� �ð迭 �׷���
install.packages('dygraphs')
library(dygraphs)

economics <- ggplot2::economics
head(economics)

library(xts)
eco <- xts(economics$unemploy, order.by=economics$date)
head(eco)

dygraph(eco)

# ��¥���� ���� ���
dygraph(eco) %>% dyRangeSelector()

# ������ ǥ���ϱ�: ���ͷ�Ƽ�� �ð迭 �׷����� ���� ���� ���ÿ� ǥ������
# �Ǿ��� ���� ������� �׷����� �Բ� ǥ��

# �����
eco_a <- xts(economics$psavert, order.by=economics$date)
# �Ǿ��� ��
eco_b <- xts(economics$unemploy/1000, order.by=economics$date)

# ��ġ��
eco2 <- cbind(eco_a, eco_b) #������ ����
colnames(eco2) <- c('psavert', 'unemploy') # ������ �ٲٱ�
head(eco2)

dygraph(eco2) %>% dyRangeSelector()


# ������ �ε�
# csv ������
# ���� ������
write_csv(r_excel, 'C:/Users/sohyeon/r_data_12weeks/r_txt.txt')
# XML ������
install.packages('XML')
library(XML)

url <-'http://openapi.seoul.go.kr:8088/716646734e6a696e3130375a66507755/xml/SebcPublicEnterpriseKor/1/1000/'
indata <- xmlToDataFrame(url)
View(indata)

# 1~2��� 1~3�� ����`
indata <- indata[-1:-2,-1:-3]
View(indata)

# JSON ������
install.packages('jsonlite')
library(jsonlite)

# URL���� json������ ��������
install.packages('curl')
library(curl)
data2 <- fromJSON('https://api.github.com/users/hadley/orgs')
data2 <- as.data.frame(data2)
colnames(data2)
View(data2)

# ���Ͽ��� json������ ��������
data <- fromJSON('C:/Users/sohyeon/r_data_12weeks/�����ʵ��б����б���ǥ�ص�����.json')
names(data)
data <- as.data.frame(data$records)
View(data)

# ������(feather�� fst)
# ���� �����ͼ� Ȯ��
data()
View(mtcars)

# 1�ﰳ�� ���� ����
data <- data.frame(number=rnorm(n=100000000))
dim(data)

# ��Ű�� ��ġ&�ε�
install.packages('feather')
library(feather)

# rdata �������� ����
system.time({
  save(data, file='C:/Users/sohyeon/r_data_12weeks/time/data.RData')})

# csv �������� ����
system.time({
  save(data, file='C:/Users/sohyeon/r_data_12weeks/time/data.csv')})

# feather �������� ����
system.time({
  save(data, file='C:/Users/sohyeon/r_data_12weeks/time/data.feather')})

# fst �������� ����
system.time({
  save(data, file='C:/Users/sohyeon/r_data_12weeks/time/data.fst')})


# rdata �������� �б�
system.time({
  load('C:/Users/sohyeon/r_data_12weeks/time/data.RData')})

# csv �������� �б�
system.time({
  data1 <- read.csv('C:/Users/sohyeon/r_data_12weeks/time/data.csv')})

# feather �������� �б�
system.time({
  data2 <- read_feather('C:/Users/sohyeon/r_data_12weeks/time/data.feather')})

# fst �������� �б�
system.time({
  data3 <- read.fst('C:/Users/sohyeon/r_data_12weeks/time/data.fst')})
