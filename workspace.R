##Text mining: Kathryn Smith testing

##Load data 
###This code extracts text from pdf documents"

library(pdftools)

setwd('C:/Users/kathr/OneDrive/Documents/thermal-refuges-text/data/oregon')

getwd()

files <- list.files(pattern = "pdf$")

opinions <- lapply(files, pdf_text)  ###The pdftools function for extracting text is pdf_text

length(opinions)

lapply(opinions, length)

####using tm package

library(tm)
corp <- Corpus(URISource(files),
               readerControl = list(reader = readPDF))

opinions.dtm <- DocumentTermMatrix(corp)


##Cleaning text
# Let's try a different R package to work with your data in dataframe format
library(quanteda)

final <- corpus(corp)

###cleaning files: getting rid of white spaces, endings and stopwords
# load more packages that are useful
library(magrittr) # give us the pipe '%>%'
library(ggplot2)

# remove stopwords, punctuation, number, URLs, and separators
tweets_words <- final %>%  
  tokens(remove_punct = TRUE,
         remove_symbols = TRUE,
         remove_numbers = TRUE,
         remove_url = TRUE,
         remove_separators = TRUE)

# remove stopwords and stem words, and make everything lowercase
twt_words <- tweets_words %>%
  tokens_select(stopwords('english'), selection='remove') %>% 
  tokens_wordstem() %>% 
  tokens_tolower()

# Document-Term Matrix
term_matrix <- dfm(twt_words)

# term frequency
library("quanteda.textstats")
tweet_freq <- textstat_frequency(term_matrix, n = 15) 

# plot term frequency
ggplot(tweet_freq, aes(x = rank, y = frequency)) +
  geom_point() +
  labs(x = "Frequency rank", y = "Term frequency")

# check out the vignette: https://quanteda.io/articles/pkgdown/replication/digital-humanities.html

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

##Plot - long form
library(ggplot2)

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
nrow(kwic(twt_words, pattern = "temperature"))  # just the word 'coral'
nrow(kwic(twt_words, pattern = "temperature*")) # any word starting with 'coral'
nrow(kwic(twt_words, pattern = "^temperature('s){0,1}$", valuetype = "regex")) # use regex

# using words from tokenized corpus for dispersion
library("quanteda.textplots")
textplot_xray(kwic(twt_words, pattern = "temperature")) + 
  ggtitle("Lexical dispersion")

# correlations - ignore the warning message!
dfm_weight(term_matrix, scheme = "prop") %>% 
  textstat_simil(selection = c("temperature", "fish"), method = "correlation", margin = "features") %>%
  as.matrix() %>%
  head(5)

cor_data_df <- dfm_weight(term_matrix, scheme = "prop") %>% 
  dfm_keep(pattern = c("reef", "squid")) %>% 
  convert(to = "data.frame")

# sample 1000 replicates and create data frame
n <- 1000
samples <- data.frame(
  cor_sample = replicate(n, cor(sample(cor_data_df$reef), cor_data_df$squid)),
  id_sample = 1:n
)

# plot distribution of resampled correlations
ggplot(data = samples, aes(x = cor_sample, y = ..density..)) +
  geom_histogram(colour = "black", binwidth = 0.01) +
  geom_density(colour = "red") +
  labs(x = "Correlation Coefficient", y = NULL,
       title = "Histogram of Random Correlation Coefficients with Normal Curve")

##Topic Modeling - latent dirichlet allocation (similar to principal component analysis)
library(topicmodels)

seed = 12345
fit = LDA(term_matrix, k = 5, control = list(seed=seed))
#K- 5 means there will be 5 topics; it has to be specified 

library(ggwordcloud)

topics <- tidy(fit) %>%
  filter(beta > 0.004)

ggplot(topics, aes(size = beta, label = term)) +
  geom_text_wordcloud_area(rm_outside = TRUE) +
  facet_wrap(vars(topic))












####Objective: compare state of Oregon regulations vs.  New Brunswick province regs

##Frequency table for a selected list of terms: water temperature, thermal refuge, refugia, coldwater refuge, salmonid, trout, salmon

library(pdftools)

setwd('C:/Users/kathr/OneDrive/Documents/thermal-refuges-text/data/oregon')

getwd()

files <- list.files(pattern = "pdf$")

opinions <- lapply(files, pdf_text)  ###The pdftools function for extracting text is pdf_text

length(opinions)

####using tm package

library(tm)
corp <- Corpus(URISource(files),
               readerControl = list(reader = readPDF))

dtm <- DocumentTermMatrix(corp)
inspect(dtm)  ##too many words are prepositions, I want to search for nouns!


###cleaning files: getting rid of white spaces, endings and stopwords

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


dtmd <- inspect (DocumentTermMatrix(corp, list(dictionary = c("temperature", "refugia", "coldwater", "salmonids", "refuge", "cold-water", "salmon", "trout"))))


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
dtt <- tidy(dtm)
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



####code lifted from internet### using quanteda package

data(files, package = "tm") ## this line is not working..
mycorpus <- corpus(dtm_words)
textp <- tokens(final, what="word")
kwic(textp, "temperature")


findFreqTerms(dtm, lowfreq = 100, highfreq = Inf)


dtm <- dfm(final, tolower = TRUE, stem = TRUE, 
           remove_punct = TRUE, remove = stopwords("english"))

myDict <- dictionary(list(fish = c("fish", "salmon", "trout"),
                          temperature = c("temperature"),
                          coldwater = c("coldwater", "cold-water", "cold water")))

dict_dtm <- dfm_lookup(dtm, myDict)

dict_dtm
tail(dict_dtm)


corpus <- Corpus(DirSource('C:/Users/kathr/OneDrive/Documents/thermal-refuges-text/data/oregon'))

clean_corpus <- function(corpus_to_use){
  library(magrittr)
  library(tm)
  corpus_to_use %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("en")) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, c("etc","ie", "eg", stopwords("english")))
      }

corpus_improvements <- clean_corpus(corpus)

find_freq_terms_fun <- function(corpus_in){
  doc_term_mat <- TermDocumentMatrix(corpus_in)
  freq_terms <- findFreqTerms(doc_term_mat)[1:max(doc_term_mat$nrow)]
  terms_grouped <-
    doc_term_mat[freq_terms,] %>%
    as.matrix() %>%
    rowSums() %>%
    data.frame(Term=freq_terms, Frequency = .) %>%
    arrange(desc(Frequency)) %>%
    mutate(prop_term_to_total_terms=Frequency/nrow(.))
   return(data.frame(terms_grouped))
 }


dtm <- DocumentTermMatrix(corpus)



improvement_freq_terms <- data.frame(find_freq_terms_fun(corpus_improvements))
