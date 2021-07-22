##Text mining

##Load data 
###This code extracts text from pdf documents"

library(pdftools)

files <- list.files(path = 'C:/Users/kathr/OneDrive/Documents/thermal-refuges-text/data', pattern = "pdf$")

opinions <- lapply(files, pdf_text)  ###The pdftools function for extracting text is pdf_text

length(opinions)

##Cleaning text
# Let's try a different R package to work with your data in dataframe format
library(quanteda)
# NOTE: You must have unique document names, so each row/tweet must have a unique "name".  
tweet_corpus <- corpus(cp$tweettext, docnames = rownames(cp))
# in this case I've used the names of the rows (1, 2, 3, etc.) as the unique names.
# now have a look on the new corpus object
summary(tweet_corpus)

# load more packages that are useful
library(magrittr) # give us the pipe '%>%'
library(ggplot2)

# remove stopwords, punctuation, number, URLs, and separators
tweets_words <- tweet_corpus %>%  
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
nrow(kwic(twt_words, pattern = "coral"))  # just the word 'coral'
nrow(kwic(twt_words, pattern = "coral*")) # any word starting with 'coral'
nrow(kwic(twt_words, pattern = "^coral('s){0,1}$", valuetype = "regex")) # use regex

# using words from tokenized corpus for dispersion
library("quanteda.textplots")
textplot_xray(kwic(twt_words, pattern = "coral")) + 
  ggtitle("Lexical dispersion")

# correlations - ignore the warning message!
dfm_weight(term_matrix, scheme = "prop") %>% 
  textstat_simil(selection = c("reef", "squid"), method = "correlation", margin = "features") %>%
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

