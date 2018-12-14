library(tidyverse)

R_wiki <- "R is a programming language and free software environment for statistical computing and graphics supported by the R Foundation for Statistical Computing. The R language is widely used among statisticians and data miners for developing statistical software and data analysis. Polls, data mining surveys and studies of scholarly literature databases, show substantial increases in popularity in recent years. As of December 2018, R ranks 16th in the TIOBE index, a measure of popularity of programming languages.

A GNU package, source code for the R software environment is written primarily in C, Fortran and R itself and is freely available under the GNU General Public License. Pre-compiled binary versions are provided for various operating systems. Although R has a command line interface, there are several graphical user interfaces, such as RStudio, an Integrated development environment."

str_extract(R_wiki, "software environment")
str_extract_all(R_wiki, "software environment")

myextract <- str_extract_all(R_wiki, "[[:upper:]]{1}[[:alpha:]]{0,}")
myextract

table(myextract)

R_wiki %>% 
  str_locate("software environment")

R_wiki %>% 
  str_locate_all("software environment")

mylocate <- R_wiki %>% 
  str_locate_all("[[:upper:]]{1}[[:alpha:]]{0,}")
mylocate

dim(mylocate[[1]])

mydata <- as.tibble(mylocate[[1]])
mydata

myextract <- R_wiki %>% str_extract_all("[[:upper:]]{1}[[:alpha:]]{0,}")
mydata$myword <- myextract[[1]]
mydata

R_wiki %>% str_replace("software environment", "software_environment")
R_wiki %>% str_replace_all("software environment", "software_environment")

temp <- str_replace_all(R_wiki, "software environment", "software_environment")
table(str_extract_all(R_wiki, "software_environment|software|environment"))
table(str_extract_all(temp, "software_environment|software|environment"))

temp <- R_wiki %>% str_replace_all("R\\b", "R_computer.language_") %>% 
  str_replace_all("C\\b", "C_computer.language_")

table(str_extract_all(temp, "[[:alnum:]]{1}_computer.language_"))

# str_split / str_split_fixed

R_wiki_para <- R_wiki %>% str_split("\n")
R_wiki_para

R_wiki_sent <- R_wiki_para %>% .[[1]] %>% str_split("\\.")
R_wiki_sent

my2sentences <- unlist(R_wiki_sent)[c(4, 7)]
my2sentences
mylength1 <- length(unlist(str_split(my2sentences[1], " ")))
mylength2 <- length(unlist(str_split(my2sentences[2], " ")))
mylength1; mylength2

myfixed.short <- str_split_fixed(my2sentences, " ", 5)
myfixed.short

myfixed.long <- str_split_fixed(my2sentences, " ", 13)
myfixed.long

map_int(R_wiki_sent, function(i) length(unlist(str_split(unlist(i), " ")))) -> length.sentences
length.sentences

# unlist(str_split(unlist(R_wiki_sent[1]), " "))

max.length.sentences <- max(length.sentences)
max.length.sentences

sent.word.matrix <- str_split_fixed(unlist(R_wiki_sent), " ", max.length.sentences)
sent.word.matrix

mydata <- data.frame(sent.word.matrix)
mydata

rownames(mydata) <- str_c('sent', 1:length(unlist(R_wiki_sent)), sep=".")
colnames(mydata) <- str_c("word", 1:max.length.sentences, sep=".")
mydata

mydata[, 1]
mydata[3, 1:10]

# str_count

str_count(R_wiki, "R")
str_count(R_wiki_para[[1]], "R")
str_count(unlist(R_wiki_sent), "R")

str_count(unlist(R_wiki_sent), "R.{1,}stat[[:lower:]]{1,}")

unlist(R_wiki_sent)[1:2]
str_count(unlist(R_wiki_sent), "R.{1,}(s|S)tat[[:alpha:]]{1,}")
str_extract_all(unlist(R_wiki_sent)[1], "R.{1,}(s|S)tat[[:alpha:]]{1,}")

# str_sub
R_wiki_sent %>% unlist(.) %>% .[1] %>% str_sub(1, 30)

# str_dup
str_dup("software", 3)
str_c(rep("software", 3), collapse="")

str_dup("software", 3) == str_c(rep("software", 3), collapse="")

# str_length

str_length(unlist(R_wiki_sent))
nchar(unlist(R_wiki_sent))

# str_pad / str_trim

name <- c("Joe", "Jack", "Jackie", "Jefferson")
donation <- c("$1", "$111", "$11111", "$1111111")
mydata <- tibble(name, donation)
mydata

name2 <- str_pad(mydata$name, width=15, side='right', pad=' ')
donation2 <- str_pad(mydata$donation, width=15, side='both', pad='~')
mydata2 <- tibble(name2, donation2)
mydata2

str_length(mydata$name)
str_length(mydata2$name2)

name3 <- str_trim(mydata2$name2, side='right')
donation3 <- str_trim(str_replace_all(mydata2$donation2, "~", " "), side='both')
mydata3 <- tibble(name3, donation3)
all(mydata == mydata3)
mydata3

# str_c

str_c(unlist(R_wiki_sent), collapse='. ')

str_c(unlist(R_wiki_sent), collapse='. ') == paste(unlist(R_wiki_sent), collapse='. ')
