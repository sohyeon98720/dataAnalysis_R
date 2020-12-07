# t-����, t-test
# compact �ڵ����� suv �ڵ����� ���� ���� t ����
mpg <- as.data.frame(ggplot2::mpg)

library(dplyr)
mpg_diff <- mpg %>% 
  select(class, cty) %>% 
  filter(class %in% c('compact','suv'))

head(mpg_diff)
table(mpg_diff$class)

t.test(data=mpg_diff, cty~class, var.equal=T)
# cty�� ���� ��, class�� ���� ����
# ���⼭�� ���� �� �л��� ���ٰ� ����
# �� ������ cty ��հ���� �����

# �Ϲ� �ֹ����� ���� �ֹ����� ���� ���� t ����
mpg_diff2 <- mpg %>% 
  select(fl, cty) %>% 
  filter(fl %in% c('r','p')) # regular, premuim

table(mpg_diff2$fl)

t.test(data=mpg_diff2, cty~fl, var.equal=T)
# p-value�� 0.2875 -> �����δ� �� ���̰� ���µ� �쿬�� ����
# �̷� ���̰� ������ Ȯ���� 28.75%��� �ǹ�
# => ���� �ֹ��� �ڵ����� ���� ���� ����� 0.6���� ������
# �̷� ������ ���̴� �쿬�� �߻����� ���ɼ��� ũ�ٰ� �ؼ�

# 12���� �ǰ����� �����Ϳ��� ������ ���� ���忡 ���̰� �ִ��� �м�
library(readr)
GJ_2002 <- read_csv('C:/Users/sohyeon/r_data_12weeks/GJ_2002_SAMPLE.CSV',locale=locale(encoding='euc-kr'))
summary(GJ_2002)

height_diff <- GJ_2002 %>% 
  select(SEX, HEIGHT) %>% 
  filter(SEX %in% c('1','2'))

head(height_diff)
table(height_diff)

t.test(data=height_diff, HEIGHT~SEX, var.equal=T)


# ����м� - �� ������ ���輺 �м�
# 0~1���̰��̰� 1�� �������� ���ü��� ũ�ٴ� �ǹ�
# ����� �����, ������ �ݺ��

# �Ǿ��� ���� ���� �Һ� ������ �������
economics <- as.data.frame(ggplot2::economics)

cor.test(economics$unemploy, economics$pce)
# 0.614 -> �ѷ��� ���� ��������

# 12���� �ǰ����� �����Ϳ��� ����� ü���� �󸶳� ���谡 �ִ��� �м�
HE_WE <- GJ_2002 %>% 
  select(HEIGHT, WEIGHT)

head(HE_WE)

# �÷��� ������ ����м�
cor(HE_WE$HEIGHT, HE_WE$WEIGHT, method='pearson')
# ������ ��ü�� ������ ����м�
cor(HE_WE, method='pearson')
# p���� Ȯ���ϱ� ���� ����м�
cor.test(HE_WE$HEIGHT, HE_WE$WEIGHT)


# ������ ��Ʈ�� �����
# ������: ���� ���� �� ������踦 ��ķ� ��Ÿ�� ǥ
head(mtcars)

# ������ ����
car_cor <- cor(mtcars)
# �Ҽ��� ��°�ڸ����� �ݿø��ؼ� ���
round(car_cor, 2)
# => mpg(����), cyl(�Ǹ��� ��)�� �������� -0.85�̹Ƿ�
# ���� �������� �Ǹ��� ���� ���� ������ ����

# => cyl(�Ǹ��� ��)�� wt(����)�� �������� 0.78�̹Ƿ�
# �Ǹ��� ���� �������� �ڵ����� ���ſ� ������ ����

install.packages('corrplot')
library(corrplot)

corrplot(car_cor)
# => �������� Ŭ���� ���� ũ�Ⱑ ũ�� ���� ����
# �������� ����� �Ķ�, ������ ������

corrplot(car_cor, method='number')

# �پ��� �Ķ���� �����ϱ�
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

corrplot(car_cor,
         method='color', # ����� ǥ��
         col=col(200), # ���� 200�� ����
         type='lower', # ���ʾƷ� ��ĸ� ǥ��
         order='hclust', # ������ ���������� ����ȭ
         addCoef.col='black', # ������ ����
         tl.col='black', # ������ ����
         tl.srt=45, # ������ 45�� �����
         diag=F) # �밢��� ����