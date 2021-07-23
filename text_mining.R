####Objective: compare state of Oregon regulations vs.  New Brunswick province regs

##Frequency table for a selected list of terms: water temperature, thermal refuge, refugia, coldwater refuge, salmonid, trout, salmon


#setwd('C:\\Users\\fmejia\\1_Francine\\1_Francine from C\\SESYNC\\SESYNC proposal coldwater refuges\\Data for class\\Oregon')
working <-'C:\\Users\\fmejia\\1_Francine\\1_Francine from C\\SESYNC\\SESYNC proposal coldwater refuges\\Data for class\\Oregon'
#getwd()

install.packages("installr")
require(installr)
updateR()

###This code extracts text from pdf documents"

install.packages("pdftools")
library(pdftools)

files <- list.files(pattern = "pdf$")

pdf_corpus <- lapply(files, pdf_text)  ###The pdftools function for extracting text is pdf_text

length(pdf_corpus)

##### Using quanteda package

#install.packages("quanteda")
#install.packages("quanteda.textstats")

# devtools packaged required to install readtext from Github 
devtools::install_github("quanteda/readtext") 
install.packages("read.text") # not available for R version 3.5.0
library(quanteda)


library(magrittr) # give us the pipe '%>%'
library(ggplot2)
#install.packages("quanteda.textstats")
library("quanteda.textstats")


library(tm)
corp <- VCorpus(DirSource(working),
               readerControl = list(reader = readPDF))
summary(corp)

pdf_corpus <- corpus(corp)


summary(pdf_corpus)


# remove stopwords, punctuation, number, URLs, and separators
wordsquant <- pdf_corpus %>%  
  tokens(remove_punct = TRUE,
         remove_symbols = TRUE,
         remove_numbers = TRUE,
         remove_url = TRUE,
         remove_separators = TRUE)

# remove stopwords and stem words, and make everything lowercase
wordsquant <- files_words %>%
  tokens_select(stopwords('english'), selection='remove') %>% 
  tokens_wordstem() %>% 
  tokens_tolower()

# Document-Term Matrix
term_matrix <- dfm(wordsquant)

# term frequency

pdf_freq <- textstat_frequency(term_matrix, n = 20) 


# plot term frequency
ggplot(pdf_freq, aes(x = rank, y = frequency)) +
  geom_point() +
  labs(x = "Frequency rank", y = "Term frequency")


# plot number of characters per word
## Long form
library(tidytext)
library(dplyr)

dtt <- tidy(term_matrix)
words <- dtt %>%
  group_by(term) %>%
  summarise(
    n = n(),
    total = sum(count)) %>%
  mutate(nchar = nchar(term))

#####
ggplot(words, aes(x = nchar)) +
  geom_histogram(binwidth = 1)

## Short form
dtt_trimmed <- words %>%
  filter(
    nchar < 16,
    n > 1,
    total > 3) %>%
  select(term) %>%
  inner_join(dtt)

words2 <- dtt_trimmed %>%
  group_by(term) %>%
  summarise(
    n = n(),
    total = sum(count)) %>%
  mutate(nchar = nchar(term))

##Plot - short form
ggplot(words2, aes(x = nchar)) +
  geom_histogram(binwidth = 1)


# Find number of occurrences of term using kwic() function (kwic = keyword-in-context)
# NOTE: we're using the terms that were stemmed and stopwords removed
nrow(kwic(wordsquant, pattern = "temperatur"))  # just the word 'temperatur'
nrow(kwic(wordsquant, pattern = "cold*")) # any word starting with 'cold'
nrow(kwic(wordsquant, pattern = "^coral('s){0,1}$", valuetype = "regex")) # use regex  --need to do this

# using words from tokenized corpus for dispersion
#install.packages("quanteda.textplots")
library("quanteda.textplots")
textplot_xray(kwic(wordsquant, pattern = "temperatur")) + 
  ggtitle("Lexical dispersion")

## get tokens

toks <- tokens(wordsquant)


##creating your own dictionary
dict <- dictionary(list(temperature = c("temperatur*"),
                        coldwater = c("cold*"), 
                        salmonids = c("salmon*", "brown", "cutthroat", "trout", "rainbow", "bull", "atlantic", "brook"),
                        refuge = c("refug*")))
print(dict)

dict_toks <- tokens_lookup(toks, dictionary = dict)
print(dict_toks)

dfmat <- dfm(toks)

dfmatdict <- dfm_lookup(dfmat, dictionary = dict, levels = 1)
print(dfmatdict, max_ndoc=14)

tstat_freq <- textstat_frequency(dfmatdict, n = 4, groups=14)
head(tstat_freq, 14)

dfmatdict %>% 
  textstat_frequency(n = 14) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()

dfmatdict %>% 
  textstat_frequency(n = 14) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_bar(stat="identity", width=0.2, color="cyan", fill="cyan")+
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()





######wordcloud
set.seed(132)
textplot_wordcloud(dfmatdict)




##Topic Modeling - latent dirichlet allocation (similar to principal component analysis)  ---need to work on this!!!!
#install.packages("topicmodels")
library(topicmodels)

seed = 12345
fit = LDA(dfmatdict k = 5, control = list(seed=seed))
#K- 5 means there will be 5 topics; it has to be specified 












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

#dtmw <- tm_map(dtm_words,content_transformer(tolower)) #Error in .tolower(txt) : invalid input 'âˆ’ð‘ð‘í µy' in 'utf8towcs'


f <- content_transformer(function(x, pattern) gsub(pattern, "", x))
dtm_words<- tm_map(dtm_words, f, "[!\"#$%&'*+,./)(:;<=>?@[\\^`{|}~]")





dtm_words <- DocumentTermMatrix(dtm_words)
inspect(dtm_words)  # checking what matrix looks like


## not sure how to have more than 10 terms

dtmd<- inspect (DocumentTermMatrix(corp, list(dictionary = c("temperature", "refugia", "coldwater", "salmonids", "refuge", "cold-water", "trout","brown", "cutthroat", "brook", "rainbow", 

"thermal", "salmon", "pool", "riffles", "river", "wetland", "seep", "groundwater"))))


dtmd2 <- dtmd %>%
  cast_dtm(document, term, count)



dttd <- tidy(dtmd)
dictionarywords <- dttd %>%
  group_by(term) %>%
  summarise(
    n = n(),
    total = sum(count)) %>%
  mutate(nchar = nchar(term))


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







