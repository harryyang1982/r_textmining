library(tidyverse)
library(KoNLP)

obj1 <- 1:4
obj2 <- 6:10

obj3 <- list(obj1, obj2)
obj3

mylist <- list(obj1, obj2, obj3)
mylist

mylist[[3]][1]
mylist[[3]][[1]]

mylist[[3]][[1]][2]
mylist[[3]][[1]][[2]]

myvector <- c(1:6, 'a')
mylist <- list(1:6, 'a')
mylist
myvector

unlist(mylist)
unlist(mylist) == myvector

mean(mylist[[1]][1:6])
mean(unlist(mylist)[1:6])

# 텍스트형 자료

name1 <- "Donald"
myspace <- " "
name2 <- "Trump"
list(name1, myspace, name2)

unlist(list(name1, myspace, name2))

# 메타 데이터 만들기 attr

name <- c("갑", "을", "병", "정")
gender <- c(2, 1, 1, 2)
mydata <- tibble(name, gender)
attr(mydata$name, "what the variable means") <- "응답자의 이름"
mydata$name
attr(mydata$gender, "what the variable means") <- "응답자의 성별"
myvalues <- gender
myvalues

# for (i in 1:length(gender)) {
#   myvalues[i] <- ifelse(gender[i] == 1, "남성", "여성")
# }
# myvalues  

1:length(gender) %>% 
  map_chr(function(i) ifelse(gender[i] == 1, "남성", "여성")) -> myvalues
myvalues

attr(mydata$gender, "what the value means") <- myvalues
mydata$gender

attr(mydata$gender, "what the value means")

mydata$gender_character <- attr(mydata$gender, "what the value means")
mydata

# apply

mylist <- list(1:4, 6:10, list(1:4, 6:10))
lapply(mylist[[3]], mean)

map(mylist[[3]], mean)
lapply(mylist, mean)

lapply(mylist[c(1, 2, c(1, 2))], mean)
map(mylist[c(1, 2, c(1, 2))], mean)

map_dfc(mylist[c(1, 2, c(1, 2))], sum)
map_dbl(mylist[c(1, 2, c(1, 2))], sum)
unlist(lapply(mylist[c(1, 2, c(1, 2))], sum))

wordlist <- c("the", "is", "a", "the")
doc1freq <- c(3, 4, 2, 4)
doc2freq <- rep(1, 4)

tapply(doc1freq, wordlist, length)
tapply(doc2freq, wordlist, length)
tapply(doc1freq, wordlist, sum)
tapply(doc2freq, wordlist, sum)

sent1 <- c("earth", "to", "earth")
sent2 <- c("ashes", "to", "ashes")
sent3 <- c("dust", "to", "dust")

myfreq <- c(rep(1, length(sent1)), rep(1, length(sent2)), rep(1, length(sent3)))
myfreq
tapply(myfreq, c(sent1, sent2, sent3), sum)
