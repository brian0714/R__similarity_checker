# install.packages("wordcloud")
# install.packages("RColorBrewer")
# install.packages("udpipe")
source("lib/csv_reader.R")
# Load required libraries
library(tm)
library(textTinyR) # for ngram_as_strings
library(SnowballC) # for stemming
library(ggplot2)
library(wordcloud)
library(RColorBrewer) # for wordcloud
library(pheatmap) # for heatmap
library(udpipe) # for POS taggng

# Create a text corpus
create_corpus <- function(
  texts_vector = c(),
  return_user_id = FALSE,
  POS = FALSE
) {
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
    df <- csv_reader(
      file_path,
      filter_conditions = filter_conditions,
      show_col_types = FALSE,
      remove_duplicates = TRUE
    )
    texts_vector <- as.vector(df$final_submission)
  }

  # 使用 `udpipe` 進行 POS 標註
  filter_pos <- function(text, POS) {
    if (POS == FALSE) return(text)  # 不過濾，直接回傳原文本

    # 選擇語言模型 (English-only so far)
    ud_model <- udpipe_load_model("model/english-ewt-ud-2.5-191206.udpipe")

    # 進行詞性標註
    annotated <- udpipe_annotate(ud_model, x = text)
    annotated <- as.data.frame(annotated)

    # 選擇特定詞性
    if (!POS %in% c("NOUN", "VERB", "ADJ")) {
      cat("未知的 POS 選擇，請使用 'NOUN', 'VERB', 'ADJ'")
      return(text)
    } else {
      filtered_words <- annotated$lemma[annotated$upos == POS]
    }

    # 將篩選後的詞彙組合成文本
    return(paste(filtered_words, collapse = " "))
  }

  # 過濾 POS
  if (POS != FALSE) {
    texts_vector <- sapply(texts_vector, filter_pos, POS = POS)
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

  if (return_user_id) {
    return(list(corpus, df$user_id))
  } else {
    return(corpus)
  }
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
tokenize <- function(
  text,
  method = "word",
  remove_punc = FALSE,
  remove_sw = FALSE
) {
  if (remove_punc) {
    text <- remove_selected_punctuation(text)
  }
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

  if (remove_sw) {
    tokens <- remove_stopwords(tokens)
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

# test for cosine similarity
# vec1 <- result[[1]]
# vec2 <- result[[2]]
# # Calculate the Cosine Similarity distance
# distance <- sqrt(sum((vec1 - vec2) ^ 2))
# similarity <- 1 / (1 + distance)
# print(similarity)