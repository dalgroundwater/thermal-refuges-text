####Objective: compare state of Oregon regulations vs.  New Brunswick province regs

##Frequency table for a selected list of terms: water temperature, thermal refuge, refugia, coldwater refuge, salmonid, trout, salmon


setwd('C:\\Users\\fmejia\\1_Francine\\1_Francine from C\\SESYNC\\SESYNC proposal coldwater refuges\\Data for class')

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


####lifted from internet###

devtools::install_github("kbenoit/quanteda", quiet = TRUE)
require(quanteda)
data(files, package = "tm") ## this line is not working..
mycorpus <- corpus(files)
textp<- tokens(mycorpus, what="word")
kwic(textp, "water temperature")


