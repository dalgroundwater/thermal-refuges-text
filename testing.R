##Text Mining - Kathryn Testing

#Setting Oregon working directory in thermal refuges text repository 
setwd('C:/Users/kathr/OneDrive/Documents/thermal-refuges-text/data/oregon')
working <- 'C:/Users/kathr/OneDrive/Documents/thermal-refuges-text/data/oregon'
getwd() #Double checks the working directory

#Load data 
library(pdftools) #Used to load in pdf documents
files <- list.files(pattern = "pdf$") #Recognizes documents that have ".pdf" at the end

opinions <- lapply(files, pdf_text) #The pdftools function for extracting text is pdf_text

length(opinions) #Shows how many documents have been read in

#Start
my_corpus <- VCorpus(DirSource(working),
                  readerControl = list(reader = readPDF))

summary(my_corpus)

meta(my_corpus[[1]]) #Shows the metadata for the first pdf document

f <- content_transformer(function(x, pattern) gsub(pattern, "", x)) #Setting a function to remove special characters

corpus_filtered <- my_corpus %>%
                   tm_map(removePunctuation) %>%
                   tm_map(removeNumbers) %>%
                   tm_map(content_transformer(tolower)) %>%
                   tm_map(removeWords, stopwords("english")) %>%
                   tm_map(stripWhitespace) %>%
                   tm_map(stemDocument)


mystring <- str_replace_all(string = corpus_filtered, pattern = "[^[:alnum:]]", replacement = " ") 


library(stringr)


mystring <- str_remove_all(mystring, "[^[:alnum:]]")
mystring

dtm1 <- DocumentTermMatrix(corpus_filtered) #Document Term Matrix
inspect(dtm1)







termFreq <- colSums(as.matrix(dtm1))
head(termFreq)

tf <- data.frame(term = names(termFreq), freq = termFreq)
tf <- tf[order(-tf[,2]),]
head(tf)

f <- content_transformer(function(x, pattern) gsub(pattern, "", x))
dtm_words<- tm_map(my_corpus, f, "[!\"#$%&'*+,./)(:;<=>?@[\\^`{|}~]")





# inspects chapters 1:5, terms 10:17
inspect(dtm_potter) 
dtm_potter
words_frequency <- colSums(as.matrix(dtm_potter)) 
# verify that the terms are still equal to dtm_potter
length(words_frequency)
# create sort order (descending) for matrix
ord <- order(words_frequency, decreasing=TRUE)
library(knitr)       # used to make kable tables
# get the top 10 words by frequency of appeearance
words_frequency[head(ord, 10)] %>% 
  kable()

# Return unoredered frequent terms that appeared more than 200, but less than infinity.
findFreqTerms(dtm_potter, lowfreq = 200, highfreq = Inf)
# Find words that are correlated with "ron" with a coefficient > .70
findAssocs(dtm_potter, "temperature", .85) %>% 
  kable()

# convert our "clean" corpus into a tfidf weighted dtm
DocumentTermMatrix(corp, control = list(weighting = weightTfIdf)) -> dtm_potter_tfidf

# View details of tfidf weighted dtm
dtm_potter_tfidf

# convert dtm into a df
df_potter <- tidy(dtm_potter)

# take the product of tf and idf and create new column labeled "tf_idf". Graph it. 
bind_tf_idf(df_potter, term_col = term, document_col = document, n_col = count) %>% 
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(term, levels = rev(unique(term))),
         chapter = factor(document, levels = 1:17)) %>%  
  group_by(document) %>% 
  top_n(6, wt = tf_idf) %>% 
  ungroup() %>% 
  ggplot(aes(word, tf_idf, fill = document)) +
  geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
  labs(title = "Highest tf-idf words in Philosopher's Stone by Chapter",
       x = "Words", y = "tf-idf") +
  facet_wrap(~chapter, ncol = 2, scales = "free") +
  coord_flip()
