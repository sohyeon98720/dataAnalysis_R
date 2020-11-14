install.packages('rJava')
install.packages('memoise')
install.packages('KoNLP') # 이 패키지는 cran에서 삭제된 상태라 설치불가
install.packages('multilinguer')
library(multilinguer)
install.packages(c("stringr", "hash", "tau", "Sejong", "RSQLite", "devtools"), type ="binary")
install.packages("https://cran.r-project.org/src/contrib/Archive/KoNLP/KoNLP_0.80.2.tar.gz", repos = NULL, type = "source", upgrade = "never", INSTALL_opts=c("--no-multiarch"))
# 여기서 error 발생 -> r이 설치된 경로에 scala-library-2.11.8.jar 파일 복사
library(KoNLP)
library(dplyr)
#library(rJava)
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre-9.0.4/")
Sys.getenv('JAVA_HOME')

useNIADic() # KoNLP에서 지원하는 NIA사전. 형태소 분석시 필요

# 데이터 준비
txt <- readLines('./Data/hiphop.txt')
head(txt)

# 특수문자 제거
install.packages('stringr')
library(stringr)
# \\w는 특수문자를 의미하는 정규표현식.
txt <- str_replace_all(txt,'\\W',' ')

# 가장 많이 사용된 단어 알아보기
# 명사추출
extractNoun("대한민국의 영토는 한반도와 그 부속도서로 한다")
nouns <- extractNoun(txt)
# 추출한 명사list를 문자열 벡터로 변환, 단어 빈도표 생성
wordcount <- table(unlist(nouns))
wordcount
# 데이터 프레임으로 변환
df_word <- as.data.frame(wordcount, stringsAsFactors =F)
# 변수명 수정
df_word <- rename(df_word,
                  word=Var1,
                  freq=Freq)
# 두 글자 이상 단어 추출
df_word <- filter(df_word, nchar(word) >= 2)
# 빈도수 높은 순대로 상위 20개 단어(명사)추출
top_20 <- df_word %>% 
  arrange(desc(freq)) %>% 
  head(20)
top_20

# 워드클라우드 만들기
install.packages('wordcloud')
library(wordcloud)
library(RColorBrewer) # 글자 색깔을 표현하는데 사용할 패키지

pal <- brewer.pal(8,'Dark2') # dark2색상목록에서 8개 색상추출

set.seed(1234)
wordcloud(words=df_word$word, # 단어
          freq=df_word$freq, # 빈도
          min.freq=2, # 최소 단어빈도
          max.words=200, # 표현단어 수
          random.order=F, # 고빈도 단어 중앙 배치
          rot.per = .1, # 회전단어 비율
          scale=c(4,0.3), # 단어 크기 범위
          colors=pal) # 색깔 목록

# 단어 색상 바꾸기
pal <- brewer.pal(9,"Blues")[5:9] # 색상 목록 생성
set.seed(1234) # 난수 고정
wordcloud(words = df_word$word, # 단어
          freq = df_word$freq, # 빈도
          min.freq = 2, # 최소 단어 빈도
          max.words = 200, # 표현 단어 수
          random.order = F, # 고빈도 단어 중앙 배치
          rot.per = .1, # 회전 단어 비율
          scale = c(4, 0.3), # 단어 크기 범위
          colors = pal) # 색상 목록록

# 국정원 트윗 텍스트 마이닝
# 데이터 로드
twitter <- read.csv('./Data/twitter.csv',
                    header=T,
                    stringsAsFactors = F,
                    fileEncoding = 'UTF-8')
# 변수명 수정
twitter <- rename(twitter,
                  no=번호,
                  id=계정이름,
                  date=작성일,
                  tw=내용)
# 특수문자 제거
twitter$tw <- str_replace_all(twitter$tw,'\\W',' ')
head(twitter$tw)

# 트위터에서 명사추출
nouns <- extractNoun(twitter$tw)
# 추출한 명사 list를 문자열벡터로 변환, 단어별 빈도표 새성
wordcount <- table(unlist(nouns))
#데이터 프레임으로 변환
df_word <- as.data.frame(wordcount, stringsAsFactors = F)
# 변수명 수정
df_word <- rename(df_word,
                  word=Var1,
                  freq=Freq)
# 두글자 이상으로된 단어 추출, 빈도 상위 20개 단어 추출
df_word <- filter(df_word, nchar(word) >=2)
top_20 <- df_word %>% arrange(desc(freq)) %>% head(20)
top_20

# 단어 빈도 막대 그래프 만들기
library(ggplot2)

order <- arrange(top_20,freq)$word # 빈도 순서 변수 생성
ggplot(data=top_20,aes(x=word,y=freq)) + 
  ylim(0,2500) +
  geom_col() + 
  coord_flip() + 
  scale_x_discrete(limit=order) + # 빈도 순서 변수 기준 막대 정렬
  geom_text(aes(label=freq),hjust=-0.3) # 빈도 표시

# 워드클라우드 만들기
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
