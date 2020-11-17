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

# �̱� �� ���� ������ �غ��ϱ�
library(ggplot2)
states_map <- map_data('state')
str(states_map)

install.packages('mappproj')
library(mapproj)

ggChoropleth(data=crime, # ������ ǥ���� ������
             aes(fill=Murder, # ����� ǥ���� ����
                 map_id=state), # ���� ���� ����
             map=states_map) # ���� ������

# ���ͷ�Ƽ�� �ܰ� ���е� �����
ggChoropleth(data = crime, # ������ ǥ���� ������
             aes(fill = Murder, # ����� ǥ���� ����
                 map_id = state), # ���� ���� ����
             map = states_map, # ���� ������
             interactive = T) # ���ͷ�Ƽ��

# ���ѹα� �õ��� �α�, ���� ȯ�� �� �ܰ� ���е� �����

install.packages('stringi')
install.packages('devtools')
devtools::install_github("cardiomoon/kormaps2014")
library(kormaps2014)

str(changeCode(korpop1))

# ������ ����
library(dplyr)
korpop1 <- rename(korpop1,
                  pop=���α�_��,
                  name=����������_���鵿)
str(changeCode(kormap1))

# �ܰ� ���е� �����
ggChoropleth(data=korpop1,
             aes(fill=pop,
                 map_id=code,
                 tooltip=name),
             map=kormap1,
             interactive=T)

# ���ѹα� �õ��� ����ȯ�� �� �ܰ� ���е� �����
str(changeCode(tbc))
ggChoropleth(data=tbc, # ������ ǥ���� ������
             aes(fill=NewPts, # ����� ǥ���� ����
                 map_id=code, # ���� ���� ����
                 tooltip=name), # ���� ���� ǥ���� ������
             map=kormap1, # ���� ������
             interacive=T) # ���ͷ�Ƽ��
