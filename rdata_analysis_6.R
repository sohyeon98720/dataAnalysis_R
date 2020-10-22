# gplot()�� ��� �ܼ�. ggplot()�� ���� ���۱�� ����
# ������ �׷���(scatter plot)
mpg <- as.data.frame(ggplot2::mpg)
library(ggplot2)
ggplot(data=mpg, aes(x=displ,y=hwy))
ggplot(data=mpg, aes(x=displ,y=hwy))+geom_point()
# ����� ����(xlim,ylim)
ggplot(data=mpg, aes(x=displ,y=hwy))+geom_point()+xlim(3,6)
ggplot(data=mpg, aes(x=displ,y=hwy))+geom_point()+
  xlim(3,6)+
  ylim(10,30)

# ���� - mpg, midwest �������̿��ϱ�
# (mpg)x-cty, y-hwy �� scatterplot �����
ggplot(data=mpg,aes(x=cty,y=hwy)) + geom_point()
# (midwest)x-poptotal, y-popasian�� scatter plot + ����
# ����: ��ü�α� 50���� ���� & �ƽþ��� �α� 1���� ������ ������ ǥ��
ggplot(data=midwest,aes(x=poptotal, y=popasian))+
  geom_point()+xlim(0,500000)+ylim(0,10000)

# ����׷���(bar chart)
library(dplyr)
df_mpg <- mpg %>% group_by(drv) %>% summarise(mean_hwy=mean(hwy))
df_mpg
ggplot(data=df_mpg, aes(x=drv,y=mean_hwy)) + geom_col()
# barplot + ����
ggplot(data=df_mpg,aes(x=reorder(drv,-mean_hwy),y=mean_hwy))+geom_col()
#��(���� ����)�� barplot�׸���
ggplot(data=mpg,aes(x=drv))+geom_bar() #x�� ����������
ggplot(data=mpg,aes(x=hwy))+geom_bar() #x�� ����������

#��ո���׷��� - ���ǥ�� ���� ���� �� �׷��� ���� - geom_col()
#�󵵸���׷��� - ���������ͷ� �ٷ� �׷��� ���� - geom_bar()

# ���� - mpg������ �̿�
# suv������ ������� ���cty�� ���� ���� ȸ�� 5���� ����׷����� ǥ�� + ����������
df_suv <- mpg %>% filter(class=='suv') %>% 
  group_by(manufacturer) %>% summarise(mean_cty=mean(cty)) %>% 
  arrange(desc(mean_cty)) %>% head(5)
df_suv
ggplot(data=df_suv,aes(x=reorder(manufacturer,-mean_cty),
                       y=mean_cty)) + geom_col()
# � class�� ���� ������ �� ���� �׷���
ggplot(data=mpg,aes(x=class)) + geom_bar()

# ���׷��� - line chart / time series chart
# time seires chart: ȯ��, �ְ����� �� ������ǥ�� �ð��� ���� ��� ���ϴ��� ǥ���� �� ���
ecomonics <- as.data.frame(ggplot2::economics)
View(ecomonics)
summary(ecomonics)
ggplot(data=economics,aes(x=date,y=unemploy))+geom_line()
# ���� - economics������ �̿��ϱ�
# psavert(���������)�� �ð��� ���� ��� ���ؿԴ��� �׷����� ǥ��
ggplot(data=economics,aes(x=date,y=psavert))+geom_line()

# boxplot - �������� ������ ���ڸ������ ǥ���� �׷���
ggplot(data=mpg,aes(x=drv,y=hwy))+geom_boxplot()
# �ڽ����� ��: �̻�ġ, �Ʒ����� �������: min,Q1,median(Q2),Q3,max(75~100�̳�)

# ���� - mpg������ �̿�
# class�� compact,subcompact,suv�� �ڵ����� cty�� ��� �ٸ��� boxplot���� ��
class_mpg <- mpg %>% filter(class %in% c("compact","subcompact","suv"))
class_mpg
ggplot(data=class_mpg,aes(x=class,y=cty)) + geom_boxplot()

#ggplot�� ���� �� ���� ����� �˰��ʹ�? -> help>>cheatsheets>>data visualization with ggplot2
#�̹��� �����ϴ¹�� -> ������ plots���� export