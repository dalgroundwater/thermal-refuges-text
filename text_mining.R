####Objective: compare state of Oregon regulations vs.  New Brunswick province regs

##Frequency table for a selected list of terms: water temperature, thermal refuge, refugia, coldwater refuge, salmonid, trout, salmon


setwd('C:\\Users\\fmejia\\1_Francine\\1_Francine from C\\SESYNC\\SESYNC proposal coldwater refuges\\Data for class\\Oregon')

getwd()

###This code extracts text from pdf documents"

install.packages("pdftools")
library(pdftools)

files <- list.files(pattern = "pdf$")

opinions <- lapply(files, pdf_text)  ###The pdftools function for extracting text is pdf_text

length(opinions)


####using tm package

install.packages("tm")
library(tm)
corp <- Corpus(URISource(files),
               readerControl = list(reader = readPDF))

dtm <- DocumentTermMatrix(corp)
inspect(dtm)  ##too many words are prepositions, I want to search for nouns!


###cleaning files: getting rid of white spaces, endings and stopwords


install.packages('magrittr')

library(magrittr)
dtm_words <- corp %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace)  %>%
 # tm_map(stemDocument) %>% ### didn't use so words don't lose ending like temperature
  tm_map(removeWords, stopwords("english"))


dtmw <- DocumentTermMatrix(dtm_words)
inspect(dtmw)  # checking what matrix looks like


##finding frequent terms to trim length of words

library(tidytext)
library(dplyr)
dtt <- tidy(dtmw)
words <- dtt %>%
  group_by(term) %>%
  summarise(
    n = n(),
    total = sum(count)) %>%
  mutate(nchar = nchar(term))

### Plotting the number of characters

library(ggplot2)
ggplot(words, aes(x = nchar)) +
  geom_histogram(binwidth = 1)


####### trimmingthe number of characters in words

dtt_trimmed <- words %>%
  filter(
    nchar < 16,
    n > 1,
    total > 3) %>%
  select(term) %>%
  inner_join(dtt)

dtm_trimmed <- dtt_trimmed %>%
  cast_dtm(document, term, count)

dtm_trimmed


### develop word associations with key words such temperatur and refugia  
#####using tm package finding associations --code from class

word_assoc <- findAssocs(dtm_trimmed, 'temperature', 0.95)
word_assoc <- data.frame(
  word = names(word_assoc[[1]]),
  assoc = word_assoc,
  row.names = NULL)

###visualization of word association

library(ggwordcloud)
ggplot(word_assoc,
       aes(label = word, size = temperature)) +
  geom_text_wordcloud_area()



####code lifted from internet### using quanteda package

devtools::install_github("kbenoit/quanteda", quiet = TRUE)
require(quanteda)
data(files, package = "tm") ## this line is not working..
mycorpus <- corpus(dtm_words)
textp<- tokens(mycorpus, what="word")
kwic(textp, "temperatur")




