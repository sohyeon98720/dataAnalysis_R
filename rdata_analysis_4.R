install.packages('dplyr')
library(dplyr)
exam <- read.csv('./Data/csv_exam.csv')
exam

# ���ǿ� �´� ������ �����ϱ� - filter(ctrl + shift + m)
exam %>% filter(class==1)

exam %>% filter(class==1 & math>=50)

exam %>% filter(math>=90 | english>=90)

exam %>% filter(class %in% c(1,3,5))

class1 <- exam %>% filter(class==1)
class2 <- exam %>% filter(class==2)
mean(class1$math)
mean(class2$math)

# mpg �����ͷ� �м����� �ذ��ϱ�
# 1. �ڵ��� ��ⷮ�� ���� ���ӵ��� ���� �ٸ��� �˾ƺ����Ѵ�. displ�� 4������
# �ڵ����� 5�̻��� �ڵ��� �� ����� hwy�� ��������� ������ �˾ƺ�����
mpg <- as.data.frame(ggplot2::mpg)

Mpg_4 <- mpg %>% filter(displ<=4)
Mpg_5 <- mpg %>% filter(displ>=5)

mean(Mpg_4$hwy)
mean(Mpg_5$hwy)
# -> Mpg_4�� �� ����

# 2. �ڵ��� ����ȸ�翡 ���� ���� ���� �ٸ��� �˾ƺ����Ѵ�.
# audi�� toyota�� ��� manufacturer�� cty�� ��������� ������ �˾ƺ�����
mpg_audi <- mpg %>% filter(manufacturer=='audi')
mpg_toyota <- mpg %>% filter(manufacturer=='toyota')

mean(mpg_audi$cty)
mean(mpg_toyota$cty)
# -> mpg_toyota�� �� ����

# 3. chevrolet, ford, honda �ڵ����� ���ӵ��� ���� ����� ���Ϸ��Ѵ�.
# �ڵ����� ������ �� hwy��ü ����� ���Ͻÿ�
mpg_new <- mpg %>% filter(manufacturer %in% c('chevrolet','ford','honda'))
mean(mpg_new$hwy)

# �ʿ��� ������ �����ϱ� - select (ctrl + shift + m�̿�)
exam %>% select(math)
exam %>% select(class,math,english)
# ����
exam %>% select(-math)
# filter + select
exam %>% filter(class==1) %>% select(english)

# 1. mpg �����Ϳ��� class,cty�� ������ ���ο� ������ �����
mpg_double_c <- mpg %>% select(class,cty)
mpg_double_c

# 2. �� dataframe���� class�� suv�� ���� compact�� �� �� cty�� �� ���� ����?
mpg_suv <- mpg_double_c %>% filter(class=='suv')
mpg_compact <- mpg_double_c %>% filter(class=='compact')

mean(mpg_suv$cty)
mean(mpg_compact$cty)
# -> compact�� �� ����

# �����ϱ�
exam %>% arrange(math) # ��������
exam %>% arrange(desc(math)) # ��������

# 1. audi�ڵ��� �� hwy������ 1~5�� ���
mpg %>% filter(manufacturer=='audi') %>% arrange(desc(hwy)) %>% head(5)


# �Ļ����� �߰��ϱ� - mutate
exam %>% mutate(total=math + english + science) %>% head
exam %>% mutate(total=math + english + science,
                mean=(math + english + science)/3) %>% 
  head

# mutate + ifelse
exam %>% mutate(test=ifelse(science >=60,"pass","fail")) %>% head

# mutate + dplyr�ڵ�(arrange)
exam %>% mutate(total=math + english + science) %>% 
  arrange(desc(total)) %>% 
  head

# 1. mpg������ ���纻�� ����� cty�� hwy�� ���� �ջ꿬�񺯼� �߰�
mpg_new <- mpg
mpg_new <- mpg_new %>% mutate(total=cty+hwy)
mpg_new %>% head

# 2. �ջ꿬�񺯼��� 2�γ��� ��տ��񺯼� �߰�
mpg_new <- mpg_new %>% mutate(mean=total/2)
mpg_new %>% head

# 3. ��տ��񺯼��� ���� ���� �ڵ��� ������ ������ ���
mpg_new %>% arrange(desc(mean)) %>% head(3)

# 1~3���ļ� �ϳ��� �������� �����
mpg %>% mutate(total=cty+hwy,
               mean=total/2) %>% 
  arrange(desc(mean)) %>% head(3)


# ���ܺ��� ����ϱ� - group_by, summarise
exam %>% summarise(mean_math=mean(math))

exam %>% group_by(class) %>% 
  summarise(mean_math=mean(math))

exam %>% group_by(class) %>% 
  summarise(mean_math=mean(math),
            sum_math=sum(math),
            medain_math=median(math),
            n=n()) #�л� ��(��)

# �� ���ܺ��� �ٽ� ���� ������
mpg %>% group_by(manufacturer,drv) %>% 
  summarise(mean_cty=mean(cty)) %>% 
  head(10)

# �پ��� �Լ� ���� - ȸ�纰�� suv�ڵ����� 
# ���� �� ���ӵ��� ���տ��� ����� ���� �������� ���� �� 1~5�� ���
mpg %>% group_by(manufacturer) %>% 
  filter(class=='suv') %>% 
  mutate(tot=(cty+hwy)/2) %>% # ��� ���� �� ���ӵ��� ���տ��� ���
  summarise(mean_tot=mean(tot)) %>% # ��� ȸ�纰 tot���
  arrange(desc(mean_tot)) %>% 
  head(5)

# � ȸ�翡�� compact������ ���� �����ϴ���
mpg %>% filter(class=='compact') %>% 
  group_by(manufacturer) %>% 
  summarise(count=n()) %>% 
  arrange(desc(count))

# ������ ��ġ��(����) - join
test1 <- data.frame(id=c(1,2,3,4,5),
                    midterm=c(60,80,70,90,85))
test2 <- data.frame(id=c(1,2,3,4,5),
                    final=c(70,83,65,95,80))
test1
test2

# id�������� ��ġ��
total <- left_join(test1,test2,by='id')
total

# �ٸ� ����
name <- data.frame(class=c(1,2,3,4,5),
                   teacher=c('kim','lee','park','choi','jung'))
name
exam_new <- left_join(exam,name,by='class')
exam_new

# ������ ��ġ��(����) - bind_rows
group_a <- data.frame(id=c(1,2,3,4,5),
                      test=c(60,80,70,90,85))
group_b <- data.frame(id=c(6,7,8,9,10),
                      test=c(70,83,65,95,80))
group_all <- bind_rows(group_a,group_b)
group_all

# ����
fuel <- data.frame(fl=c('c','d','e','p','r'),
                   price_fl=c(2.35,2.38,2.11,2.76,2.22),
                   stringsAsFactors = F)
fuel

# 1. mpg���� fl�� ������ ���ᰡ�ݺ����� ����. fuel�� �̿��� �߰��϶�
mpg <- left_join(mpg,fuel,by='fl')
mpg

# 2. model,fl,price_fl�����Ͽ� �պκ� 5�� ���
mpg %>% select(model,fl,price_fl) %>% head(5)