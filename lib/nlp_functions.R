# Load required libraries
library(tm)
library(textTinyR)

# Bag-of-Words Vectorizer
bow_vectorize <- function(tokens, all_tokens) {
  sapply(all_tokens, function(token) sum(tokens == token))
}

# One-hot Encoding Vectorizer
one_hot_vectorize <- function(tokens, all_tokens) {
  sapply(all_tokens, function(token) ifelse(token %in% tokens, 1, 0))
}

# TF-IDF Vectorizer
tfidf_vectorize <- function(texts) {
  dtm <- DocumentTermMatrix(Corpus(VectorSource(texts)),
                control = list(weighting = weightTfIdf))
  tfidf_matrix <- as.matrix(dtm)
  return(tfidf_matrix)
}

# Tokenization methods
tokenize <- function(text, method = "word") {
  text <- tolower(text)
  if (method == "word") {
    tokens <- unlist(strsplit(text, "\\W+"))
  } else if (method == "character") {
    tokens <- unlist(strsplit(text, ""))
  } else if (method == "bigram") {
    tokens <- unlist(lapply(1:(nchar(text) - 1), function(i) substr(text, i, i+1)))
  } else if (method == "trigram") {
    tokens <- unlist(lapply(1:(nchar(text) - 2), function(i) substr(text, i, i+2)))
  } else {
    stop("Unknown tokenization method")
  }
  return(tokens)
}

# Remove stopwords
remove_stopwords <- function(tokens) {
  stopwords_list <- stopwords("en")
  tokens <- tokens[!tokens %in% stopwords_list]
  return(tokens)
}

# Text Vectorizer
text_vectorizer <- function(text1, text2, tokenize_method = "word", vectorize_method = "bow") {

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
    tfidf_matrix <- tfidf_vectorize(c(text1, text2))
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

result <- text_vectorizer(text1, text2, tokenize_method = "word", vectorize_method = "bow")
# print(result)