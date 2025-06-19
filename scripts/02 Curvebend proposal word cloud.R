
# ---- 01: Header ---------------------------------------------------------
# Purpose: Plot a word cloud for the Curvebend proposal
# Author: Han Olff
# Started: 2025-06-18
# Input: Curvebend full proposal
browseURL("https://drive.google.com/file/d/1QzkCYul01leMi-z8WqIHOR0jP2pvVCMl/view?usp=drive_link")
# Output: word cloud figure based on word frequencies in the proposal 
# Requirements: R 4.4.1, rtools 4.4, for library versions see file renv.lock 


# ---- 02 Restore environment and load libraries --------------------------
# restore your library versions to be the same as your collaborators
renv::restore() 
# Load necessary libraries
library(httr)
library(pdftools)
library(here)
library(tm)
library(wordcloud2)
library(tidyverse)
library(htmlwidgets)
library(webshot2)

# create a folder for output figures for this script (but note that this is ignored by git for syncing)
# create a folder for output figures for this script (but note that this is ignored by git for syncing)
if (!dir.exists("figures")) dir.create("figures")
if (!dir.exists("figures/02")) dir.create("figures/02")


# ---- 03: Read the pdf of the proposal from Google Drive -----------------
file_id <- "1QzkCYul01leMi-z8WqIHOR0jP2pvVCMl"
url <- paste0("https://drive.google.com/uc?export=download&id=", file_id)

temp_file <- tempfile(fileext = ".pdf")
GET(url, write_disk(temp_file, overwrite = TRUE))

pdf_text_raw <- pdftools::pdf_text(temp_file)


# ---- 04: Preprocess the text --------------------------------------------

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


# ----05: Create and plot the word cloud ----------------------------------

dtm <- tm::TermDocumentMatrix(corpus)
matrix <- as.matrix(dtm)
word_freqs <- sort(rowSums(matrix), decreasing = TRUE)
df <- data.frame(word = names(word_freqs), freq = word_freqs)
# Exclude non-informative words
excluded_words <- c("will", "project", "proposal","different","fig","also","can","plan","study","nwaorc",
                    "tenured","three","new","work","best","confidential","curvebend")
# Filter the data frame
df_filtered <- df %>%
  filter(!word %in% excluded_words)

# Create the word cloud
FigScript02_wordcloud<-wordcloud2(df_filtered)
FigScript02_wordcloud

# save as png file

# Save HTML temporarily
html_path <- tempfile(fileext = ".html")
htmlwidgets::saveWidget(FigScript02_wordcloud, html_path, selfcontained = TRUE)

# Save as a PNG (true single file)
webshot2::webshot(html_path, here::here("figures/02/FigScript02_wordcloud.png"), vwidth = 1920, vheight = 1200)
