library(tidyverse)

nchar("Korea")
nchar("Korea", type='bytes')
nchar("한국")
nchar("한국", type="bytes")

nchar("Korea")
nchar("Korea ")
nchar("Korea\t")
nchar("Korea\t", type="bytes")

nchar("Korea, Republic of")
nchar("Korea,
      Republic of")
nchar("Korea, \nRepublic of")

mysentence <- "Learning R is so interesting"
mysentence %>% str_split(pattern=" ")

mywords <- mysentence %>% 
  strsplit(split=" ")
str_split(mywords[[1]][5], pattern="")

mywords %>% 
  map(function(i) str_split(i, pattern = "")) %>% .[[1]]-> myletters

paste(myletters[[1]], collapse="")

mywords2 <- list(rep(NA, 5))

map(myletters, function(i) str_c(i, collapse="")) -> mywords2
mywords2

str_c(mywords2, collapse = " ")

R_wiki <- "R is a programming language and free software environment for statistical computing and graphics supported by the R Foundation for Statistical Computing. The R language is widely used among statisticians and data miners for developing statistical software and data analysis. Polls, data mining surveys and studies of scholarly literature databases, show substantial increases in popularity in recent years. As of December 2018, R ranks 16th in the TIOBE index, a measure of popularity of programming languages.

A GNU package, source code for the R software environment is written primarily in C, Fortran and R itself and is freely available under the GNU General Public License. Pre-compiled binary versions are provided for various operating systems. Although R has a command line interface, there are several graphical user interfaces, such as RStudio, an Integrated development environment."

R_wiki_para <- str_split(R_wiki, pattern="\n")

R_wiki_para

R_wiki_sent <- str_split(R_wiki_para[[1]], pattern="\\. ")
R_wiki_sent

R_wiki_word <- list(NA, NA)
map(R_wiki_sent, function(i) str_split(i, pattern = " ")) -> R_wiki_word

R_wiki_word[[1]][[2]][3]

# regexpr(), gregexpr(), regexec() : 위치정보 인덱싱

mysentence <- "Learning R is so interesting"
regexpr("ing", mysentence)

loc_begin <- as.vector(regexpr("ing", mysentence))
loc_begin

loc_length <- as.vector(attr(regexpr("ing", mysentence), "match.length"))
loc_length

loc_end <- loc_begin + loc_length - 1
loc_end

gregexpr("ing", mysentence)

length(gregexpr("ing", mysentence)[[1]])
loc.begin <- as.vector(gregexpr("ing", mysentence)[[1]])
loc.begin

loc.length <- as.vector(attr(gregexpr("ing", mysentence)[[1]], 'match.length'))

loc.end <- loc.begin + loc.length - 1
loc.end

regexpr("interesting", mysentence)

regexec("interestin(g)", mysentence)

regexec("so (interestin(g))", mysentence)

mysentences <- unlist(R_wiki_sent)
regexpr("software", mysentences)

gregexpr("software", mysentences)

mytemp <- regexpr("software", mysentences)
mytemp

my_begin <- as.vector(mytemp)
my_begin[my_begin == -1] <- NA
my_begin

my_end <- my_begin + as.vector(attr(mytemp, "match.length")) - 1
my_end

mylocs <- matrix(NA, nrow=length(my_begin), ncol=2)
mylocs

colnames(mylocs) <- c("begin", "end")
rownames(mylocs) <- str_c("sentence", 1:length(my_begin), sep=".")
mylocs

bind_cols(list(my_begin, my_end))

# grepl
grep("software", mysentences)
grepl("software", mysentences)

# sub("ing", "ING", mysentence)
# gsub("ing", "ING", mysentence)
# gsub("ing", "ING", mysentences)
# ==> str_replace_all / str_replace

sent1 <- R_wiki_sent[[1]][1]
new_sent1 <- sent1 %>% str_replace_all("R Foundation for Statistical Computing", "R_Foundation_for_Statistical_Computing")
new_sent1

sum(table(str_split(sent1, pattern=" ")))
sum(table(str_split(new_sent1, pattern=" ")))

drop_sent1 <- new_sent1 %>% str_replace_all("and |by |for |the ", "")
drop_sent1
sum(table(str_split(drop_sent1, pattern = " ")))

# regmatches() / substr() 원하는 표현만 추출하거나 원하는 위치 텍스트만 선별

mypattern <- regexpr("ing", mysentence)
mypattern
regmatches(mysentence, mypattern)

mypattern <- gregexpr("ing", mysentence)
regmatches(mysentence, mypattern)

mysentence %>% str_view("ing")

mypattern <- regexpr("ing", mysentence)
regmatches(mysentence, mypattern, invert=TRUE)

mypattern <- gregexpr("ing", mysentence)
regmatches(mysentence, mypattern, invert=TRUE)

str_split(mysentence, pattern="ing")

mysentence %>% str_replace_all("ing", "")
substr(mysentences, 1, 30)

# 정규표현식 연습

my2sentence <- c("Learning R is so interesting", "He is a fascinating singer")
mypattern0 <- gregexpr("ing", my2sentence)
regmatches(my2sentence, mypattern0)

mypattern1 <- gregexpr("[[:alpha:]](ing)", my2sentence)
regmatches(my2sentence, mypattern1)

mypattern2 <- gregexpr("[[:alpha:]]{1, }(ing)", my2sentence)
mypattern2 <- gregexpr("[[:alpha:]]+(ing)", my2sentence)
regmatches(my2sentence, mypattern2)

mypattern3 <- gregexpr("[[:alpha:]]+(ing)\\b", my2sentence)
regmatches(my2sentence, mypattern3)

mypattern <- gregexpr("[[:alpha:]]+(ing)\\b", mysentences)
myings <- regmatches(mysentences, mypattern)
table(unlist(myings))

mypattern <- gregexpr("[[:alpha:]]+(ing)\\b", tolower(mysentences))
myings <- regmatches(tolower(mysentences), mypattern)
table(unlist(myings))

mypattern <- gregexpr("(stat)[[:alpha:]]+", tolower(mysentences))
regmatches(tolower(mysentences), mypattern)

mysentences %>% tolower() %>% str_extract_all(pattern = "(stat)[[:alpha:]]+", simplify = T)

# 대문자 갯수

my_uppers <- mysentences %>% str_extract_all(pattern = "[[:upper:]]") %>% unlist(.[]) %>% table()

# 소문자 갯수

my_lowers <- mysentences %>% str_extract_all(pattern = "[[:lower:]]") %>% unlist(.[]) %>% table()

my_uppers
my_lowers

my_alphas <- mysentences %>% toupper() %>% str_extract_all(pattern = "[[:alpha:]]") %>% unlist(.[]) %>% table()

my_alphas[my_alphas == max(my_alphas)]

length(my_alphas)
sum(my_alphas)

my_alphas %>% 
  as_tibble() %>% 
  rename(alphabet = ".",
         count = n) %>% 
  ggplot(aes(x = alphabet,
             y = count, fill = alphabet)) +
  geom_col() +
  geom_hline(yintercept = median(my_alphas)) +
  labs(x = "알파벳", y = "빈도수")
