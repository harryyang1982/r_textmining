library(tidyverse)

# 공백 처리

mytext <- c("software environment", "software  environment", "software\tenvironment")
mytext
str_split(mytext, ' ')

map_int(str_split(mytext, " "), length)

map(str_split(mytext, " "), str_length)

mytext.nowhitespace <- mytext %>% str_replace_all("[[:space:]]{1,}", " ")
mytext.nowhitespace

map_int(mytext.nowhitespace %>% str_split(' '), length)
map(mytext.nowhitespace %>% str_split(' '), str_length)

# 대소문자 통일

mytext <- "The 45th President of the United States, Donald Trump, states that he knows how to play trump with the ormer president"
myword <- mytext %>% str_extract_all(boundary("word")) %>% unlist(.)
myword

table(myword)

myword <- myword %>% str_replace("Trump", "Trump_unique_") %>% 
  str_replace("States", "States_unique_")

myword %>% tolower %>% table()

# 숫자 자료 제거

mytext <- c("He is one of statisticians agreeing that R is the No. 1 statistical software.", "He is one of statisticians agreeing that R is the No. one statistical software.")
str_split(mytext, " ")

mytext2 <- mytext %>% str_replace_all("[[:digit:]]{1,}[[:space:]]{1,}", "") %>% 
  str_split(" ")
mytext2

mytext2 %>% .[[1]] %>% str_c(collapse=" ")
mytext2 %>% .[[2]] %>% str_c(collapse=" ")

mytext3 <- mytext %>% str_replace_all("[[:digit:]]{1,}[[:space:]]{1,}", "_number_ ") %>% 
  str_split(" ")
mytext3

mytext3 %>% .[[1]] %>% str_c(collapse=" ")
mytext3 %>% .[[2]] %>% str_c(collapse=" ")

# 문장부호 및 특수문자 제거

mytext <- "Baek et al. (2014) argued that the state of default-setting is critical for people to protect their own personal privacy of the Internet."
mytext %>% str_split("\\. ")

mytext %>% str_split(" ")

mytext2 <- mytext %>% str_replace_all("-", " ") %>% 
  str_replace_all("[[:upper:]]{1}[[:alpha:]]{1,}[[:space:]](et al\\.)[[:space:]]\\([[:digit:]]{4}\\)", "_reference_") %>% 
  str_replace_all("\\.[[:space:]]{0,}", "")

mytext2

# 불용단어 제거
mytext <- c("She is an actor", "She is the actor")
mystopwords <- "(\\ba )|(\\ban )|(\\bthe )"
mytext %>% str_replace_all(mystopwords, "")

library(tm)

length(stopwords('en'))
stopwords('SMART')

# 어근 동일화 처리

my_stemmer_func <- function(mytextobject, mystemmer, mystemmed) {
  mytext <- mytextobject %>% str_replace_all("(\\bam )|(\\bare )|(\\bis )|(\\bwas )|(\\bwere )|(\\bbe )", "be ")
  mytext
}

mytext <- c("I am a boy. You are a boy. He might be a boy.")
mytext_stem <- my_stemmer_func(mytext)
mytext_stem
table(str_split(mytext, " "))
table(str_split(mytext_stem, " "))

# n그램 적용

mytext <- "The United States compriss fifty states. In the United States, each state has its own laws. However, federal law overrides state law in the United States."
myword <- mytext %>% str_extract_all(boundary("word")) %>% unlist()

length(table(myword))
sum(table(myword))

mytext_2gram <- mytext %>% str_replace_all("\\bUnited States", "United_States")
myword2 <- mytext_2gram %>% str_extract_all(boundary("word")) %>% unlist()
length(table(myword2))
sum(table(myword2))

mytext.3gram <- mytext %>% str_replace_all("\\b(t|T)he United States", "The_United_States")
myword3 <- mytext.3gram %>% str_extract_all(boundary("word")) %>% unlist()
myword3 %>% table %>% length
myword3 %>% table %>% sum

# 코퍼스 구성
library(tm)

my_text_location <- "ymbaek_papers"
mypaper <- VCorpus(DirSource(my_text_location))
mypaper
summary(mypaper)

mypaper[2]
mypaper[[2]]

mypaper[[2]]$content
mypaper[[2]]$meta
meta(mypaper[[2]], tag='author') <- "Y. M. Baek"
mypaper[[2]]$meta

# 코퍼스 전처리
myfunc <- function(x) {
  x %>% str_extract_all("[[:alnum:]]{1,}[[:punct:]]{1}?[[:alnum:]]{1,}")
}

mypuncts <- lapply(mypaper, myfunc)
table(unlist(mypuncts))

myfunc <- function(x) {
  x %>% str_extract_all("[[:digit:]]{1,}")
}
mydigits <- lapply(mypaper, myfunc)
table(unlist(mydigits))

myfunc <- function(x) {
  x %>% str_extract_all("[[:upper:]]{1}[[:alpha:]]{1,}")
}
myuppers <- lapply(mypaper, myfunc)
table(unlist(myuppers))

# 본격적인 전처리
mycorpus <- tm_map(mypaper, removeNumbers)

mytempfunc <- function(myobject, oldexp, newexp){
  newobject <- myobject %>% tm_map(content_transformer(function(x, pattern) str_replace_all(x, pattern, newexp)), oldexp)
  newobject
}

# 예외처리
mycorpus <- mytempfunc(mycorpus, "-collar", "collar")
mycorpus <- mycorpus %>% mytempfunc("\\b((c|C)o-)", "co")
mycorpus <- mycorpus %>% mytempfunc("\\b((c|C)ross-)", "cross")
mycorpus <- mycorpus %>% mytempfunc("e\\.g\\.", "for example")
mycorpus <- mycorpus %>% mytempfunc("i\\.e\\.", "that is")
mycorpus <- mycorpus %>% mytempfunc("\\'s", "")
mycorpus <- mycorpus %>% mytempfunc("s'", "s")
mycorpus <- mycorpus %>% mytempfunc("ICD-", "ICD")
mycorpus <- mycorpus %>% mytempfunc("\\b((i|I)nter-)", "inter")
mycorpus <- mycorpus %>% mytempfunc("K-pop", "Kpop")
mycorpus <- mycorpus %>% mytempfunc("\\b((m|M)eta-)", "meta")
mycorpus <- mycorpus %>% mytempfunc("\\b((o|O)pt-)", "opt")
mycorpus <- mycorpus %>% mytempfunc("\\b((p|P)ost-)", "post")
mycorpus <- mycorpus %>% mytempfunc("-end", "end")
mycorpus <- mycorpus %>% mytempfunc("\\b((w|W)ithin-)", "within")
mycorpus <- mycorpus %>% mytempfunc("=", "is equal to")
mycorpus <- mycorpus %>% mytempfunc("and/or", "and or")
mycorpus <- mycorpus %>% mytempfunc("his/her", "his her")
mycorpus <- mycorpus %>% mytempfunc("-", " ")

mycorpus <- mycorpus %>% tm_map(removePunctuation)

#공란 처리
mycorpus <- tm_map(mycorpus, stripWhitespace)

# 소문자로 교체
mycorpus <- tm_map(mycorpus, content_transformer(tolower))

# stopwords 삭제
mycorpus <- mycorpus %>% tm_map(removeWords, words=stopwords("SMART"))

# 어근 동일화 알고리즘 적용
mycorpus <- mycorpus %>% tm_map(stemDocument, language="en")

# 전처리 적용 이전

mycharfunc <- function(x) {x %>% str_extract_all(".")}
mywordfunc <- function(x) {x %>% str_extract_all(boundary("word"))}

mychar <- lapply(mypaper, mycharfunc)
myuniquechar0 <- length(table(unlist(mychar)))
mytotalchar0 <- mychar %>% unlist %>% table %>% sum
myword <- lapply(mypaper, mywordfunc)
myuniqueword0 <- myword %>% unlist %>% table %>% length
mytotalword0 <- myword %>% unlist %>% table %>% sum

# 전처리 적용 이후
mychar <- lapply(mycorpus, mycharfunc)
myuniquechar1 <- mychar %>% unlist %>% table %>% length
mytotalchar1 <- mychar %>% unlist %>% table %>% sum
myword <- lapply(mycorpus, mywordfunc)
myuniqueword1 <- myword %>% unlist %>% table %>% length
mytotalword1 <- myword %>% unlist %>% table %>% sum

results.comparing <- rbind(
  c(myuniquechar0, myuniquechar1),
  c(mytotalchar0, mytotalchar1),
  c(myuniqueword0, myuniqueword1),
  c(mytotalword0, mytotalword1))

colnames(results.comparing) <- c("before", "after")
rownames(results.comparing) <- c("고유문자 수", "총 문자 수", "고유단어 수", "총 단어 수")
results.comparing

