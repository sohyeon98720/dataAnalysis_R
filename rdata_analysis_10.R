install.packages('rJava')
install.packages('memoise')
install.packages('KoNLP') # �� ��Ű���� cran���� ������ ���¶� ��ġ�Ұ�
install.packages('multilinguer')
library(multilinguer)
install.packages(c("stringr", "hash", "tau", "Sejong", "RSQLite", "devtools"), type ="binary")
install.packages("https://cran.r-project.org/src/contrib/Archive/KoNLP/KoNLP_0.80.2.tar.gz", repos = NULL, type = "source", upgrade = "never", INSTALL_opts=c("--no-multiarch"))
# ���⼭ error �߻� -> r�� ��ġ�� ��ο� scala-library-2.11.8.jar ���� ����
library(KoNLP)
library(dplyr)
#library(rJava)
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre-9.0.4/")
Sys.getenv('JAVA_HOME')

useNIADic() # KoNLP���� �����ϴ� NIA����. ���¼� �м��� �ʿ�

# ������ �غ�
txt <- readLines('./Data/hiphop.txt')
head(txt)

# Ư������ ����
install.packages('stringr')
library(stringr)
# \\w�� Ư�����ڸ� �ǹ��ϴ� ����ǥ����.
txt <- str_replace_all(txt,'\\W',' ')

# ���� ���� ���� �ܾ� �˾ƺ���
# ��������
extractNoun("���ѹα��� ����� �ѹݵ��� �� �μӵ����� �Ѵ�")
nouns <- extractNoun(txt)
# ������ ����list�� ���ڿ� ���ͷ� ��ȯ, �ܾ� ��ǥ ����
wordcount <- table(unlist(nouns))
wordcount
# ������ ���������� ��ȯ
df_word <- as.data.frame(wordcount, stringsAsFactors =F)
# ������ ����
df_word <- rename(df_word,
                  word=Var1,
                  freq=Freq)
# �� ���� �̻� �ܾ� ����
df_word <- filter(df_word, nchar(word) >= 2)
# �󵵼� ���� ����� ���� 20�� �ܾ�(����)����
top_20 <- df_word %>% 
  arrange(desc(freq)) %>% 
  head(20)
top_20

# ����Ŭ���� �����
install.packages('wordcloud')
library(wordcloud)
library(RColorBrewer) # ���� ������ ǥ���ϴµ� ����� ��Ű��

pal <- brewer.pal(8,'Dark2') # dark2�����Ͽ��� 8�� ��������

set.seed(1234)
wordcloud(words=df_word$word, # �ܾ�
          freq=df_word$freq, # ��
          min.freq=2, # �ּ� �ܾ��
          max.words=200, # ǥ���ܾ� ��
          random.order=F, # ���� �ܾ� �߾� ��ġ
          rot.per = .1, # ȸ���ܾ� ����
          scale=c(4,0.3), # �ܾ� ũ�� ����
          colors=pal) # ���� ���

# �ܾ� ���� �ٲٱ�
pal <- brewer.pal(9,"Blues")[5:9] # ���� ��� ����
set.seed(1234) # ���� ����
wordcloud(words = df_word$word, # �ܾ�
          freq = df_word$freq, # ��
          min.freq = 2, # �ּ� �ܾ� ��
          max.words = 200, # ǥ�� �ܾ� ��
          random.order = F, # ���� �ܾ� �߾� ��ġ
          rot.per = .1, # ȸ�� �ܾ� ����
          scale = c(4, 0.3), # �ܾ� ũ�� ����
          colors = pal) # ���� ��Ϸ�

# ������ Ʈ�� �ؽ�Ʈ ���̴�
# ������ �ε�
twitter <- read.csv('./Data/twitter.csv',
                    header=T,
                    stringsAsFactors = F,
                    fileEncoding = 'UTF-8')
# ������ ����
twitter <- rename(twitter,
                  no=��ȣ,
                  id=�����̸�,
                  date=�ۼ���,
                  tw=����)
# Ư������ ����
twitter$tw <- str_replace_all(twitter$tw,'\\W',' ')
head(twitter$tw)

# Ʈ���Ϳ��� ��������
nouns <- extractNoun(twitter$tw)
# ������ ���� list�� ���ڿ����ͷ� ��ȯ, �ܾ ��ǥ ����
wordcount <- table(unlist(nouns))
#������ ���������� ��ȯ
df_word <- as.data.frame(wordcount, stringsAsFactors = F)
# ������ ����
df_word <- rename(df_word,
                  word=Var1,
                  freq=Freq)
# �α��� �̻����ε� �ܾ� ����, �� ���� 20�� �ܾ� ����
df_word <- filter(df_word, nchar(word) >=2)
top_20 <- df_word %>% arrange(desc(freq)) %>% head(20)
top_20

# �ܾ� �� ���� �׷��� �����
library(ggplot2)

order <- arrange(top_20,freq)$word # �� ���� ���� ����
ggplot(data=top_20,aes(x=word,y=freq)) + 
  ylim(0,2500) +
  geom_col() + 
  coord_flip() + 
  scale_x_discrete(limit=order) + # �� ���� ���� ���� ���� ����
  geom_text(aes(label=freq),hjust=-0.3) # �� ǥ��

# ����Ŭ���� �����
pal <- brewer.pal(8,'Dark2')
set.seed(1234)
wordcloud(words=df_word$word,
          freq=df_word$freq,
          min.freq=10,
          max_words=200,
          random.order=F,
          rot.per=.1,
          scale=c(6,0.2),
          colors=pal)

pal <- brewer.pal(9,'Blues')[5:9]
set.seed(1234)
wordcloud(words=df_word$word,
          freq=df_word$freq,
          min.freq=10,
          max.words=200,
          random.order=F,
          rot.per=.1,
          sclae=c(6,0.2),
          colors=pal)