# install.packages("wordcloud")
# install.packages("RColorBrewer")
source("lib/csv_reader.R")
# Load required libraries
library(tm)
library(textTinyR)
library(SnowballC)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(pheatmap)

# Create a text corpus
create_corpus <- function(texts_vector = c()) {
  # Check if text data is provided
  if (length(texts_vector) == 0) {
    stop("No text data provided")
  }
  # Check if the input is a character vector; if not, it must be a vector of text data
  else if (texts_vector %in% c('PRACTICAL', 'CREATIVE')) {
    # Set the input file path
    file_path <- 'data/text_data/extracted_behavior_pattern_data.csv'
    # Get the final submissions from the CSV file
    filter_conditions <- list(
      paste0("task_type == '", texts_vector, "'")
    )
    df <- csv_reader(file_path, filter_conditions = filter_conditions, remove_duplicates = TRUE)
    texts_vector <- as.vector(df$final_submission)
  }

  # Create a text corpus
  corpus <- VCorpus(VectorSource(texts_vector))

  # Preprocess the text data
  expand_contractions <- function(text) {
    text <- gsub("’ll", " will", text)
    text <- gsub("’re", " are", text)
    text <- gsub("’ve", " have", text)
    text <- gsub("n’t", " not", text)
  #   text <- gsub("’d", " would", text) # 可根據語境改成 "had"
    text <- gsub("’m", " am", text)
  #   text <- gsub("’s", " is", text) # 可能會影響所有格 (e.g., "dog’s" -> "dog is")
  #   text <- gsub("’", "", text) # 最後移除剩餘的 smart quote
    return(text)
  }

  remove_emojis <- function(text) {
    gsub("[\\p{So}\\p{Cn}]", "", text, perl = TRUE)
  }

  # Preprocess steps
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, content_transformer(expand_contractions)) # 展開縮寫
  # corpus <- tm_map(corpus, content_transformer(remove_selected_punctuation))
  corpus <- tm_map(corpus, removePunctuation) # may have meaning
  corpus <- tm_map(corpus, content_transformer(remove_emojis))
  corpus <- tm_map(corpus, removeNumbers) # may have meaning
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, stripWhitespace)
  # corpus <- tm_map(corpus, stemDocument) # causes error

  # Print the final preprocessed text
  # cat(str(corpus))

  return(corpus)
}

# 建立大語料庫的 DTM (TF)
create_dtm_tf <- function(corpus) {
  dtm <- DocumentTermMatrix(corpus)
  return(dtm)
}

# 建立大語料庫的 DTM (TF-IDF)
create_dtm_tfidf <- function(corpus) {
  dtm <- DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf))
  return(dtm)
}

# Bag-of-Words Vectorizer
bow_vectorize <- function(tokens, all_tokens) {
  sapply(all_tokens, function(token) sum(tokens == token))
}

# One-hot Encoding Vectorizer
one_hot_vectorize <- function(tokens, all_tokens) {
  sapply(all_tokens, function(token) ifelse(token %in% tokens, 1, 0))
}

# TF-IDF Vectorizer
tfidf_vectorize <- function(texts, dtm) {
  # Create a document-term matrix only with input texts
  # dtm <- DocumentTermMatrix(
  #   Corpus(VectorSource(texts)),
  #   control = list(weighting = weightTfIdf)
  # )

  # 轉換文本為語料庫
  corpus <- create_corpus(texts)
  # 建立新的 DTM，並確保詞彙與大語料庫詞集一致
  dtm_new <- DocumentTermMatrix(
    corpus,
    control = list(dictionary = Terms(dtm),
    weighting = weightTfIdf)
  )

  tfidf_matrix <- as.matrix(dtm_new)
  return(tfidf_matrix)
}

# Remove stopwords
remove_stopwords <- function(tokens) {
  stopwords_list <- stopwords("en")
  tokens <- tokens[!tokens %in% stopwords_list]
  return(tokens)
}

# Remove selected punctuations
remove_selected_punctuation <- function(text) {
  # 刪除大部分標點, except "-", "/"
  text <- gsub("[!\"#$%&'()*+,./:;<=>?@[\\]^_`{|}~]", "", text, perl = TRUE)
  return(text)
}

# Tokenization methods
tokenize <- function(text, method = "word") {
  # text <- remove_selected_punctuation(text)
  text <- tolower(text)

  if (method == "word") {
    tokens <- unlist(strsplit(text, "\\W+"))
  } else if (method == "character") {
    tokens <- unlist(strsplit(text, ""))
  } else if (method == "bigram") {
    words <- unlist(strsplit(text, "\\W+"))  # 先將文本按空格拆成詞
    tokens <- if (length(words) > 1) unlist(lapply(1:(length(words) - 1), function(i) paste(words[i], words[i+1]))) else character(0)
  } else if (method == "trigram") {
    words <- unlist(strsplit(text, "\\W+"))  # 先將文本按空格拆成詞
    tokens <- if (length(words) > 2) unlist(lapply(1:(length(words) - 2), function(i) paste(words[i], words[i+1], words[i+2]))) else character(0)
  }else {
    stop("Unknown tokenization method")
  }
  return(tokens)
}

# Text Vectorizer
text_vectorizer <- function(text1, text2, tokenize_method = "word", vectorize_method = "bow", task_type = "PRACTICAL") {
  # Tokenize texts
  tokens1 <- remove_stopwords(tokenize(text1, tokenize_method))
  tokens2 <- remove_stopwords(tokenize(text2, tokenize_method))

  # Merge all tokens
  all_tokens <- unique(c(tokens1, tokens2))

  # Different vectorize methods
  if (vectorize_method == "bow") {
    vec1 <- bow_vectorize(tokens1, all_tokens)
    vec2 <- bow_vectorize(tokens2, all_tokens)
  } else if (vectorize_method == "one_hot") {
    vec1 <- one_hot_vectorize(tokens1, all_tokens)
    vec2 <- one_hot_vectorize(tokens2, all_tokens)
  } else if (vectorize_method == "tfidf") {
    # Create a document-term matrix with the entire corpus
    corpus <- create_corpus(texts_vector = task_type)
    dtm <- create_dtm_tfidf(corpus)

    tfidf_matrix <- tfidf_vectorize(c(text1, text2), dtm)
    vec1 <- tfidf_matrix[1, ]
    vec2 <- tfidf_matrix[2, ]
  } else {
    stop("Unknown vectorize method")
  }

  return(list(vec1, vec2))
}

# Example usage
text1 <- "The cat sits on the mat."
text2 <- "The dog sits on the mat."

# result <- text_vectorizer(text1, text2, tokenize_method = "word", vectorize_method = "tfidf")
# print(result)