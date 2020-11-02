# ������ ������ �̾ ���� - ������ ���� ���� �˾ƺ���
class(welfare$code_job)
table(welfare$code_job)
# �����з��ڵ� ��� �ҷ�����
library(readxl)
list_job <- read_excel('./Data/Koweps_Codebook.xlsx',col_names=T,sheet=2)
head(list_job)
dim(list_job)
# welfare�� ������ ����
library(dplyr)
welfare <- left_join(welfare, list_job, id = "code_job")
welfare %>% filter(!is.na(code_job)) %>% 
  select(code_job,job) %>% 
  head(10)

# ������ ���� ���� �м��ϱ�
job_income <- welfare %>% 
  filter(!is.na(job) & !is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income=mean(income))
head(job_income)

# ���� 10�� ����
top10 <- job_income %>% 
  arrange(desc(mean_income)) %>% 
  head(10)
top10

# �׷��� �����
library(ggplot2)
ggplot(data=top10,aes(x=reorder(job,mean_income),y=mean_income)) + 
  geom_col() + coord_flip()

# ���� 10�� ����
bottom10 <- job_income %>% 
  arrange(mean_income) %>% 
  head(10)
bottom10

# �׷��� �����
ggplot(data=bottom10,aes(x=reorder(job,-mean_income),
                         y=mean_income)) + 
  geom_col() + coord_flip() + ylim(0,850)


# ���� ���� �� - ���� ������ ���� 10��
job_male <- welfare %>% 
  filter(!is.na(job) & sex=='male') %>% 
  group_by(job) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% head(10)
job_male

# ���� ���� �� - ���� ������ ���� 10��
job_female <- welfare %>% 
  filter(!is.na(job) & sex=='female') %>% 
  group_by(job) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% head(10)
job_female

# �׷��� ����� - ����
ggplot(data=job_male,aes(x=reorder(job,n),y=n)) + 
  geom_col() + coord_flip()

# �׷��� ����� - ����
ggplot(data=job_female,aes(x=reorder(job,n),y=n)) + 
  geom_col() + coord_flip()

# ���� ������ ���� ��ȥ�� - ����
class(welfare$religion)
table(welfare$religion)

welfare$religion <- ifelse(welfare$religion==1,"yes","no")
table(welfare$religion)
qplot(welfare$religion)

# ���� ������ ���� ��ȥ�� - ��ȥ
class(welfare$marriage)
table(welfare$marriage)

welfare$group_marriage <- ifelse(welfare$marriage==1,"marriage",
                                 ifelse(welfare$marriage==3,"divorce",NA))
table(welfare$group_marriage)
table(is.na(welfare$group_marriage))
qplot(welfare$group_marriage)

# ���� ������ ���� ��ȥ�� ǥ �����
religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(religion,group_marriage) %>% 
  summarise(n=n()) %>% 
  mutate(tot_group=sum(n)) %>% 
  mutate(pct=round(n/tot_group*100,1))
religion_marriage

# ���� ���� ������� ����� �ٸ� �ڵ�(count�̿�)
religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  count(religion,group_marriage) %>% 
  group_by(religion) %>% 
  mutate(pct=round(n/sum(n)*100,1))
religion_marriage

# ��ȥ�� ǥ �����
divorce <- religion_marriage %>% 
  filter(group_marriage=='divorce') %>% 
  select(religion,pct)
divorce
ggplot(data=divorce,aes(x=religion,y=pct)) + geom_col()

# ���ɴ뺰 ��ȥ�� ǥ �����
ageg_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(ageg,group_marriage) %>% 
  summarise(n=n()) %>% 
  mutate(tot_group=sum(n)) %>% 
  mutate(pct=round(n/tot_group*100,1))
ageg_marriage

# ���� ���� ������� ����� �ٸ� �ڵ�(count�̿�)
ageg_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  count(ageg,group_marriage) %>% 
  group_by(ageg) %>% 
  mutate(pct=round(n/sum(n)*100,1))
ageg_marriage

# ���ɴ뺰 ��ȥ�� �׷��� ����� - �ʳ�����,��ȥ����
ageg_divorce <- ageg_marriage %>% 
  filter(ageg!='young' & group_marriage=='divorce') %>% 
  select(ageg,pct)
ageg_divorce

ggplot(data=ageg_divorce,aes(x=ageg,y=pct)) + geom_col()

# ���ɴ� �� ���� ������ ���� ��ȥ�� ǥ �����
ageg_religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage) & ageg!='young') %>% 
  group_by(ageg,religion,group_marriage) %>% 
  summarise(n=n()) %>% 
  mutate(tot_group=sum(n)) %>% 
  mutate(pct=round(n/tot_group*100,1))
ageg_religion_marriage

# ���� ���� ������� ����� �ٸ� �ڵ�(count�̿�)
ageg_religion_marriage <- welfare %>%
  filter(!is.na(group_marriage) & ageg != "young") %>%
  count(ageg, religion, group_marriage) %>%
  group_by(ageg, religion) %>%
  mutate(pct = round(n/sum(n)*100, 1))
ageg_religion_marriage

# ���ɴ� �� ���� ������ ��ȥ�� ǥ �����
df_divorce <- ageg_religion_marriage %>% 
  filter(group_marriage=='divorce') %>% 
  select(ageg,religion,pct)
df_divorce

ggplot(data=df_divorce,aes(x=ageg,y=pct,fill=religion)) + 
  geom_col(position='dodge')

# ������ ���ɴ� ����
class(welfare$code_region)
table(welfare$code_region)

# ���� �ڵ� ��� �����
list_region <- data.frame(code_region=c(1:7),
                          region=c('����','������(��û/���)',
                                   '�λ�/�泲/���',
                                   '�뱸/���',
                                   '����/�泲',
                                   '����/���',
                                   '����/����/����/���ֵ�'))
list_region

#welfare�� ������ ���� �߰�
welfare <- left_join(welfare,list_region,id='code_region')
welfare %>% 
  select(code_region,region) %>% 
  head

# ������ ���ɴ� ���� �м��ϱ�
region_ageg <- welfare %>% 
  group_by(region,ageg) %>% 
  summarise(n=n()) %>% 
  mutate(tot_group=sum(n)) %>% 
  mutate(pct=round(n/tot_group*100,2))
head(region_ageg)

# ���� ���� ������� ����� �ٸ� �ڵ�(count�̿�)
region_ageg <- welfare %>%
  count(region, ageg) %>%
  group_by(region) %>%
  mutate(pct = round(n/sum(n)*100, 2))

# �׷��� �����
ggplot(data=region_ageg,aes(x=region,y=pct,fill=ageg)) + 
  geom_col() + coord_flip()

# ���� �����ϱ� : ����� ���� ���� ��
list_order_old <- region_ageg %>% 
  filter(ageg=='old') %>% 
  arrange(pct)
list_order_old

# ������ ���� ���� �����
order <- list_order_old$region
order

ggplot(data=region_ageg,aes(x=region,y=pct,fill=ageg)) + 
  geom_col() + coord_flip() + scale_x_discrete(limits=order)

# ���ɴ� ������ ���� ���� �����ϱ�
class(region_ageg$ageg)
levels(region_ageg$ageg)

region_ageg$ageg <- factor(region_ageg$ageg,
                           level=c('old','middle','young'))
class(region_ageg$ageg)
levels(region_ageg$ageg)

ggplot(data=region_ageg,aes(x=region,y=pct,fill=ageg)) + 
  geom_col() + coord_flip() + scale_x_discrete(limits=order)
