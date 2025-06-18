

####Section: setup the required packages ####
library(httr)
library(pdftools)
library(here)
library(tm)
library(wordcloud2)

####Section: Read the pdf of the proposal from Google Drive ####
file_id <- "1QzkCYul01leMi-z8WqIHOR0jP2pvVCMl"
url <- paste0("https://drive.google.com/uc?export=download&id=", file_id)

temp_file <- tempfile(fileext = ".pdf")
GET(url, write_disk(temp_file, overwrite = TRUE))

pdf_text_raw <- pdftools::pdf_text(temp_file)

####Section: Preprocess the text 
# Combine all pages into one string
text <- paste(pdf_text_raw, collapse = " ")

# Create a corpus
corpus <- tm::Corpus(VectorSource(text))

# Clean the text
corpus <- tm::tm_map(corpus, content_transformer(tolower))
corpus <- tm::tm_map(corpus, removeNumbers)
corpus <- tm::tm_map(corpus, removePunctuation)
corpus <- tm::tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm::tm_map(corpus, stripWhitespace)

####Section: Creat and plot wordcloud ####

dtm <- tm::TermDocumentMatrix(corpus)
matrix <- as.matrix(dtm)
word_freqs <- sort(rowSums(matrix), decreasing = TRUE)
df <- data.frame(word = names(word_freqs), freq = word_freqs)

# Create the word cloud
wordcloud2(df)
