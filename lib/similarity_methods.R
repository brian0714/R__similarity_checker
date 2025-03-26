# Load required libraries
library(tm)
library(stringdist)
library(proxy)
library(textTinyR)
library(gmp)
source("lib/nlp_functions.R")

# Euclidean Similarity
euclidean_similarity <- function(text1, text2, tokenize_method = "word", vectorize_method = "bow") {
  # Use the custom text_vectorizer function to generate vectors
  vectors <- text_vectorizer(text1, text2, tokenize_method, vectorize_method)
  vec1 <- vectors[[1]]
  vec2 <- vectors[[2]]

  # Ensure the vectors have the same length
  if (length(vec1) != length(vec2)) {
    stop("Vectors must have the same length")
  }

  # Calculate the Euclidean distance
  distance <- sqrt(sum((vec1 - vec2) ^ 2))

  # Calculate similarity based on the distance
  similarity <- 1 / (1 + distance)
  return(similarity)
}

# Jaccard Similarity
jaccard_similarity <- function(text1, text2, ngram = NULL) {
  words1 <- tokenize(text1)
  words2 <- tokenize(text2)

  if (!is.null(ngram)) {
    set1 <- unique(textTinyR::ngram_as_strings(words1, ngram))
    set2 <- unique(textTinyR::ngram_as_strings(words2, ngram))
  } else {
    set1 <- unique(words1)
    set2 <- unique(words2)
  }

  intersection <- length(intersect(set1, set2))
  union <- length(union(set1, set2))

  return(intersection / union)
}

# Cosine Similarity
cosine_similarity <- function(text1, text2, tokenize_method = "word") {
  freq1 <- table(tokenize(
    text1,
    method = tokenize_method,
    remove_punc = TRUE
  ))
  freq2 <- table(tokenize(
    text2,
    method = tokenize_method,
    remove_punc = TRUE
  ))
  # cat("Frequency 1:", freq1, "\n")
  # cat("Frequency 2:", freq2, "\n")

  # 建立完整詞彙表 (不只是 common words)
  full_vocab <- union(names(freq1), names(freq2))
  # cat("Full Vocab:", full_vocab, "\n")

  # 確保向量完整，對齊所有詞，沒有的詞補 0
  vec1 <- as.numeric(freq1[full_vocab])
  vec2 <- as.numeric(freq2[full_vocab])

  # 將 NA (因為某些詞在某個文本中缺失) 轉為 0
  vec1[is.na(vec1)] <- 0
  vec2[is.na(vec2)] <- 0

  # cat("Vector 1:", vec1, "\n")
  # cat("Vector 2:", vec2, "\n")

  # 計算 cosine similarity by tfidf
  # result <- text_vectorizer(
  #   text1,
  #   text2,
  #   tokenize_method = tokenize_method,
  #   vectorize_method = "tfidf")
  # vec1 <- result[[1]]
  # vec2 <- result[[2]]

  similarity <- sum(vec1 * vec2) / (sqrt(sum(vec1 ^ 2)) * sqrt(sum(vec2 ^ 2)))
  return(round(similarity, 2))
}

# Levenshtein Distance
levenshtein_distance <- function(text1, text2, tokenized=FALSE) {
  if (tokenized) {
    # Tokenize text1 and text2
    tokens1 <- tokenize(text1, remove_punc=TRUE, remove_sw=TRUE)
    tokens2 <- tokenize(text2, remove_punc=TRUE, remove_sw=TRUE)

    # 轉換 tokens 為單一字符串，並以空格連接
    tokenized_text1 <- paste(tokens1, collapse = " ")
    tokenized_text2 <- paste(tokens2, collapse = " ")

    # 計算 Levenshtein Distance
    distance <- stringdist::stringdist(tokenized_text1, tokenized_text2, method = "lv")
  }
  else {
    distance <- stringdist::stringdist(text1, text2, method = "lv")
  }
  return(distance)
}

normalized_levenshtein_distance <- function(text1, text2, tokenized=FALSE) {
  max_len <- max(nchar(text1), nchar(text2))
  if (max_len == 0) return(0)
  return(levenshtein_distance(text1, text2, tokenized = tokenized) / max_len)
}

# Hamming Distance
hamming_distance <- function(text1, text2) {
  if (nchar(text1) != nchar(text2)) {
    stop("Strings must be of equal length")
  }

  return(sum(tokenize(text1, method = "word") != tokenize(text2, method = "word")))
}

normalized_hamming_distance <- function(text1, text2) {
  return(hamming_distance(text1, text2) / nchar(text1))
}

# Overlap Coefficient
overlap_coefficient <- function(text1, text2) {
  set1 <- unique(tokenize(text1, method = "word"))
  set2 <- unique(tokenize(text2, method = "word"))

  intersection <- length(intersect(set1, set2))
  return(intersection / min(length(set1), length(set2)))
}

# Winnowing Algorithm (simplified for R)
hash_tokens <- function(tokens) {
  # Filter out empty strings or invalid tokens
  tokens <- tokens[tokens != "" & !is.na(tokens)]

  # Perform hash conversion
  hashes <- sapply(tokens, function(token) {
    hash_value <- tryCatch({
      # Try calculating the hash value
      digest_value <- digest::digest(token, algo = "sha1")
      substr(digest_value, 1, 8)  # Keep the first 8 characters of the hash value
    }, error = function(e) {
      # If an error occurs, print the invalid token and return NA
      cat("Invalid token:", token, "\n")
      NA
    })

    # Convert the hash to a big integer using gmp
    hash_numeric <- as.bigz(paste0("0x", hash_value))  # Convert hex string to big integer
    if (is.na(hash_numeric)) {
      cat("Failed to convert hash value (non-numeric):", hash_value, " for token: ", token, "\n")
    }
    as.numeric(hash_numeric)  # Convert big integer to numeric if possible
  })

  # Check if there are any NAs and remove them
  invalid_tokens <- tokens[is.na(hashes)]

  if (length(invalid_tokens) > 0) {
    cat("Invalid tokens:", paste(invalid_tokens, collapse = ", "), "\n")
  }

  # Return valid hash values
  hashes <- hashes[!is.na(hashes)]
  return(hashes)
}

k_grams <- function(hashes, k) {
  return(embed(hashes, k)[, k:1, drop = FALSE])
}

fingerprints <- function(k_grams, w) {
  window_min <- apply(embed(k_grams, w), 1, min)
  return(unique(window_min))
}

winnowing <- function(doc1, doc2, k = 1, w = 2) {
  tokens1 <- tokenize(
    text = doc1,
    remove_punc = TRUE,
    remove_sw = TRUE
  )
  tokens2 <- tokenize(
    text = doc2,
    remove_punc = TRUE,
    remove_sw = TRUE
  )

  hashes1 <- hash_tokens(tokens1)
  hashes2 <- hash_tokens(tokens2)

  k_grams1 <- k_grams(hashes1, k)
  k_grams2 <- k_grams(hashes2, k)

  fingerprints1 <- fingerprints(k_grams1, w)
  fingerprints2 <- fingerprints(k_grams2, w)

  if (length(fingerprints1) == 0 || length(fingerprints2) == 0) return(0)

  # Overlap coefficient (Containment similarity)
  matches <- length(intersect(fingerprints1, fingerprints2))
  return(matches / min(length(fingerprints1), length(fingerprints2)))

  # Jacard similarity
  # union <- length(union(fingerprints1, fingerprints2))
  # intersection <- length(intersect(fingerprints1, fingerprints2))
  # return(intersection / union)
}

winnowing_by_char <- function(doc1, doc2, k = 5, w = NULL) {
  library(digest)
  library(gmp)

  if (is.null(w)) w <- k + 1  # 預設 w = k + 1

  # 切出連續 k 字元片段
  get_char_kgrams <- function(text, k) {
    n <- nchar(text)
    if (n < k) return(character(0))
    sapply(1:(n - k + 1), function(i) substr(text, i, i + k - 1))
  }

  # 將每個 k-gram 做 hash
  hash_tokens <- function(tokens) {
    tokens <- tokens[tokens != "" & !is.na(tokens)]

    hashes <- sapply(tokens, function(token) {
      digest_value <- tryCatch({
        digest::digest(token, algo = "sha1")
      }, error = function(e) {
        cat("Invalid token:", token, "\n")
        return(NA)
      })

      if (is.na(digest_value)) return(NA)

      hash_numeric <- tryCatch({
        as.bigz(paste0("0x", substr(digest_value, 1, 8)))
      }, error = function(e) {
        cat("Failed to convert hash:", digest_value, " Token:", token, "\n")
        return(NA)
      })

      as.numeric(hash_numeric)
    })

    hashes[!is.na(hashes)]
  }

  # 計算 fingerprints
  fingerprints <- function(hashes, w) {
    n <- length(hashes)
    if (n < w) return(numeric(0))

    window_mins <- sapply(1:(n - w + 1), function(i) {
      min(hashes[i:(i + w - 1)])
    })

    unique(window_mins)
  }

  # 主流程
  kgrams1 <- get_char_kgrams(doc1, k)
  kgrams2 <- get_char_kgrams(doc2, k)

  hashes1 <- hash_tokens(kgrams1)
  hashes2 <- hash_tokens(kgrams2)

  if (length(hashes1) < w || length(hashes2) < w) return(0)

  fp1 <- fingerprints(hashes1, w)
  fp2 <- fingerprints(hashes2, w)

  if (length(fp1) == 0 || length(fp2) == 0) return(0)

  matches <- length(intersect(fp1, fp2))
  similarity <- matches / min(length(fp1), length(fp2))

  return(similarity)
}


# Usage examples
## Case 1 - Similar texts
text1 <- "I like to read to improve myself."
text2 <- "I love to read to improve myself."

# Case 2 - Similar meanings but different texts
# text1 = "During weekends, I like to read books."
# text2 = "I love to read books on Saturday and Sunday."

## Case 3 - Different texts
# text1 = "The research is about similarity calculation."
# text2 = "Multiple methods are based on NLP."

# cat("Jaccard Similarity:", jaccard_similarity(text1, text2), "\n")
# cat("Cosine Similarity:", cosine_similarity(text1, text2, tokenize_method = "word"), "\n")
# cat("Cosine Similarity with bigram:", cosine_similarity(text1, text2, tokenize_method = "bigram"), "\n")
# cat("Cosine Similarity with trigram:", cosine_similarity(text1, text2, tokenize_method = "trigram"), "\n")
# cat("Levenshtein Similarity:", 1 - normalized_levenshtein_distance(text1, text2), "\n")
# cat("Winnowing Similarity:", winnowing(text1, text2, k = 2, w = 3), "\n")
# cat("Winnowing Similarity by character:", winnowing_by_char(text1, text2, k = 5), "\n")
# cat("Euclidean Similarity:", euclidean_similarity(text1, text2), "\n")
# cat("Overlap Coefficient:", overlap_coefficient(text1, text2), "\n")

# # text1, text2 must in same length
# cat("Hamming Similarity:", 1 - normalized_hamming_distance(text1, text2), "\n")
