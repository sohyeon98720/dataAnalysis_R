df <- data.frame(sex=c('M','F',NA,'M','F'),
                 score=c(5,4,3,4,NA))
df
# ����ġ Ȯ�� �Լ� -> dis.na(����)
is.na(df)

# ����ġ �� ���
table(is.na(df))
table(is.na(df$sex))
table(is.na(df$score))

# ����ġ�� �ִ� �����ʹ� ��� �� �հ� ������ �ȵ�
mean(df$score) # -> na
sum(df$score) # -> na

# ����ġ�ִ� �� �����ϱ�
library(dplyr)
df %>% filter(is.na(score))
df %>% filter(!is.na(score))

df_nomiss <- df %>% filter(!is.na(score))
mean(df_nomiss$score)

# ����ġ�ִ� �� ��� �����ϱ�
df_nomiss <- df %>% filter(!is.na(score) & !is.na(sex))
df_nomiss

# �Լ� �̿��� ����ġ �����ϱ� : na.omit(������������)
df_nomiss2 <- na.omit(df)
df_nomiss2
# -> �׷��� �м��� �ʿ��� �����ͱ��� �ս� �� ���ɼ� ���� -> filter�� �� ������������

# ����Լ� ���� na���� : na.rm=T
mean(df$score, na.rm=T)
sum(df$score, na.rm=T)

exam <- read.csv('./Data/csv_exam.csv')
exam[c(3,8,15),"math"] <- NA # -> 3,8,15�� math���� NA�Ҵ�
exam

exam %>% summarise(mean_math=mean(math)) # -> NA�� �ֱ⶧���� ����� NA
exam %>% summarise(mean_math=mean(math,na.rm=T)) # -> �׷��� ����~
mean(exam$math,na.rm=T)

# ����ġ�� ��հ����� ��ü�ϱ�
exam$math <- ifelse(is.na(exam$math),55,exam$math)
table(is.na(exam$math)) # -> false�� 20����� ��
exam # -> ����ġ ������

# ���� - mpg�����Ϳ� ���Ƿ� ����ġ�� �����
mpg <- as.data.frame(ggplot2::mpg)
mpg[c(65,124,131,153,212),'hwy'] <- NA
# >> drv�� hwy ����� ��� �ٸ���(1. ����ġ ���� Ȯ��)
table(is.na(mpg$drv)) # -> ���� ����
table(is.na(mpg$hwy)) # -> ���� 5�� ����ġ ����
# >> filter�� ����ġ �����ϰ� hwy��� ���ϱ�
mpg %>% filter(!is.na(hwy)) %>% group_by(drv) %>% 
  summarise(mean_hwy=mean(hwy))

# �̻�ġ ����
### �̻�ġ: ������ֿ��� ũ�� ��� �� -> �м� ����� �ְ��Ŵ
outlier <- data.frame(sex=c(1,2,1,3,2,1),
                      score=c(5,4,3,4,2,6))
outlier

table(outlier$sex)
table(outlier$score)

# �̻�ġ�� ����ġ ó���� �� ����Լ� ����
outlier$sex <- ifelse(outlier$sex==3,NA,outlier$sex)
outlier

outlier$score <- ifelse(outlier$score >5,NA,outlier$score)
outlier

outlier %>% filter(!is.na(sex) & !is.na(score)) %>% 
  group_by(sex) %>% 
  summarise(mean_score=mean(score))

# boxplot���� �̻�ġ Ȯ���ϱ�
mpg <- as.data.frame(ggplot2::mpg)
boxplot(mpg$hwy)
# boxplot ���ġ ���
boxplot(mpg$hwy)$stats

# 12~37 ����� NA �Ҵ�
mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy >37, NA, mpg$hwy)
table(is.na(mpg$hwy))

mpg %>% group_by(drv) %>% 
  summarise(mean_hwy=mean(hwy,na.rm=T))

# ���� - mpg�����Ϳ� �̻�ġ �����
mpg <- as.data.frame(ggplot2::mpg)
mpg[c(10,14,58,93),'drv'] <- 'k'
mpg[c(29,43,129,203),'cty'] <- c(3,4,39,42)
# ���� - drv �̻�ġ Ȯ���� ó��(%in%����)
table(mpg$drv)
mpg$drv <- ifelse(mpg$drv %in% c("4","f","r"),mpg$drv,NA)
table(mpg$drv)
# ���� - boxplot���� cty �̻�ġ Ȯ�� �� ó��
boxplot(mpg$cty)
boxplot(mpg$cty)$stats
mpg$cty <- ifelse(mpg$cty<9 | mpg$cty >26,NA,mpg$cty)
boxplot(mpg$cty)
# ���� - drv���� cty ����� ��� Ȯ��
mpg %>% filter(!is.na(drv) & !is.na(cty)) %>% 
  group_by(drv) %>% 
  summarise(mean_hwy=mean(cty))