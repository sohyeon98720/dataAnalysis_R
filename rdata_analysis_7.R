install.packages('foreign')
library(foreign) #SPSS���� �ε�
library(dplyr) # ��ó��
library(ggplot2) #�ð�ȭ
library(readxl) #�������Ϻҷ�����
# ������ �غ��ϱ�(dataframe=T���ϸ� list���·� �ҷ�����)
raw_welfare <- read.spss(file='./data/Koweps_hpc10_2015_beta1.sav',
                         to.data.frame=T)
# ���纻 �����
welfare <- raw_welfare

# �����Ͱ����ϱ�
head(welfare)
tail(welfare)
View(welfare)
dim(welfare)
str(welfare)
summary(welfare)
# ��Ը� ������ + ���� ���� + �ڵ�� -> ��ü ������ ���� �ľ� �����
# => �������� ���� �ܾ�� �ٲ� �� ������ �ľ�

# ������ �ٲٱ�-rename
welfare <- rename(welfare,
                  sex=h10_g3,
                  birth=h10_g4,
                  marriage=h10_g10,
                  religion=h10_g11,
                  income=p1002_8aq1,
                  code_job=h10_eco9,
                  code_region=h10_reg7)

# �������� - ����
class(welfare$sex) # typeȮ��
table(welfare$sex) # �̻�ġȮ�� -> ����
# �̻�ġ ���� ó��
welfare$sex <- ifelse(welfare$sex ==9,NA,welfare$sex)
# ����ġ Ȯ��
table(is.na(welfare$sex))

# ���� �׸� �̸� �ο�(1,2 -> male,female)
welfare$sex <- ifelse(welfare$sex==1,'male','female')
table(welfare$sex)

# ������ �׷��� �����ϴ� �Լ� - gplot
qplot(welfare$sex)

# �������� - ����
class(welfare$income)
summary(welfare$income)

qplot(welfare$income)
qplot(welfare$income) + xlim(0,1000)

# �̻�ġ ����ó��
welfare$income <- ifelse(welfare$income %in% c(0,9999),NA,welfare$income)
table(is.na(welfare$income))

# ���� ���� ���ǥ �����
sex_income <- welfare %>% filter(!is.na(income)) %>% 
  group_by(sex) %>% summarise(mean_income=mean(income))

sex_income
ggplot(data=sex_income,aes(x=sex,y=mean_income)) + geom_col()

# �������� - ����,����
# ���̿� ������ ���� �˾ƺ���
class(welfare$birth)
# �̻�ġ Ȯ��
summary(welfare$birth)
qplot(welfare$birth)

# ����ġ Ȯ�� �� �̻�ġ ���� ó��
table(is.na(welfare$birth))

welfare$birth <- ifelse(welfare$birth ==9999,NA,welfare$birth)
table(is.na(welfare$birth))

# �Ļ����� ����� - ����(������ ���� �⵵�� 2015����)
welfare$age <- 2015 - welfare$birth +1
summary(welfare$age)
qplot(welfare$age)

# ���̿� ���� ���� ���ǥ �����
age_income <- welfare %>% filter(!is.na(income)) %>% 
  group_by(age) %>% summarise(mean_income=mean(income))
head(age_income)

# �׷����� �ð�ȭ
ggplot(data=age_income,aes(x=age,y=mean_income)) + geom_line()

# ���ɴ뿡 ���� ���� ���� �м��ϱ�
# �Ļ����� ����� - ���ɴ�
welfare <- welfare %>% mutate(ageg=ifelse(age<30,'young',
                                          ifelse(age<=59,'middle','old')))

table(welfare$ageg)
qplot(welfare$ageg)

# ���ɴ뺰 ���� ���ǥ �����
ageg_income <- welfare %>% filter(!is.na(income)) %>% 
  group_by(ageg) %>% summarise(mean_income=mean(income))
ageg_income

ggplot(data=ageg_income,aes(x=ageg,y=mean_income)) + geom_col()
# �׷��� �����Ͽ� �ð�ȭ
ggplot(data=ageg_income,aes(x=ageg,y=mean_income)) + 
  geom_col()+
  scale_x_discrete(limits=c('young','middle','old'))

# ���ɴ� �� ���� ���� ���ǥ �����
sex_income <- welfare %>% filter(!is.na(income)) %>% 
  group_by(ageg,sex) %>% summarise(mean_income=mean(income))

sex_income  
# �׷��� ����, ���ֺ� ���� �ð�ȭ
ggplot(data=sex_income,aes(x=ageg,y=mean_income,fill=sex)) + 
  geom_col()+
  scale_x_discrete(limits=c('young','middle','old'))

# position=dodge�� �׷��� �и��ϱ�
ggplot(data=sex_income,aes(x=ageg,y=mean_income,fill=sex)) + 
  geom_col(position='dodge')+
  scale_x_discrete(limits=c('young','middle','old'))


# ���� ���ɺ� ���� ���ǥ �����
sex_age <- welfare %>% filter(!is.na(income)) %>% 
  group_by(age,sex) %>% summarise(mean_income=mean(income))
sex_age

# �׷����� �ð�ȭ, col �Ķ���� �߰�
ggplot(data=sex_age,aes(x=age,y=mean_income,col=sex)) + geom_line()