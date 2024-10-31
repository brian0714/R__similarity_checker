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
  words1 <- unlist(strsplit(text1, "\\s+"))
  words2 <- unlist(strsplit(text2, "\\s+"))

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
cosine_similarity <- function(text1, text2) {
  freq1 <- table(unlist(strsplit(text1, "\\s+")))
  freq2 <- table(unlist(strsplit(text2, "\\s+")))

  common <- intersect(names(freq1), names(freq2))
  if (length(common) == 0) return(0)

  vec1 <- freq1[common]
  vec2 <- freq2[common]

  similarity <- sum(vec1 * vec2) / (sqrt(sum(vec1 ^ 2)) * sqrt(sum(vec2 ^ 2)))
  return(round(similarity, 2))
}

# Levenshtein Distance
levenshtein_distance <- function(text1, text2) {
  return(stringdist::stringdist(text1, text2, method = "lv"))
}

normalized_levenshtein_distance <- function(text1, text2) {
  max_len <- max(nchar(text1), nchar(text2))
  return(levenshtein_distance(text1, text2) / max_len)
}

# Hamming Distance
hamming_distance <- function(text1, text2) {
  if (nchar(text1) != nchar(text2)) {
    stop("Strings must be of equal length")
  }

  return(sum(unlist(strsplit(text1, "")) != unlist(strsplit(text2, ""))))
}

normalized_hamming_distance <- function(text1, text2) {
  return(hamming_distance(text1, text2) / nchar(text1))
}

# Overlap Coefficient
overlap_coefficient <- function(text1, text2) {
  set1 <- unique(unlist(strsplit(text1, "\\s+")))
  set2 <- unique(unlist(strsplit(text2, "\\s+")))

  intersection <- length(intersect(set1, set2))
  return(intersection / min(length(set1), length(set2)))
}

# Winnowing Algorithm (simplified for R)
winnow_tokenize <- function(text) {
  return(tolower(unlist(strsplit(text, "\\W+"))))
}

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
  # tokens1 <- winnow_tokenize(doc1)
  # tokens2 <- winnow_tokenize(doc2)
  tokens1 <- tokenize(doc1)
  tokens2 <- tokenize(doc2)

  hashes1 <- hash_tokens(tokens1)
  hashes2 <- hash_tokens(tokens2)

  k_grams1 <- k_grams(hashes1, k)
  k_grams2 <- k_grams(hashes2, k)

  fingerprints1 <- fingerprints(k_grams1, w)
  fingerprints2 <- fingerprints(k_grams2, w)

  if (length(fingerprints1) == 0 || length(fingerprints2) == 0) return(0)

  matches <- length(intersect(fingerprints1, fingerprints2))
  return(matches / min(length(fingerprints1), length(fingerprints2)))
}

# Usage examples
## Case 1 - Similar texts
# text1 <- "I like to read."
# text2 <- "I love to read."

# Case 2 - Similar meanings but different texts
text1 = "During weekends, I like to read books."
text2 = "I love to read books on Saturday and Sunday."

## Case 3 - Different texts
# text1 = "The research is about similarity calculation."
# text2 = "Multiple methods are based on NLP."

# cat("Jaccard Similarity:", jaccard_similarity(text1, text2), "\n")
# cat("Cosine Similarity:", cosine_similarity(text1, text2), "\n")
# cat("Levenshtein Similarity:", 1 - normalized_levenshtein_distance(text1, text2), "\n")
# cat("Winnowing Similarity:", winnowing(text1, text2), "\n")
# cat("Euclidean Similarity:", euclidean_similarity(text1, text2), "\n")
# cat("Overlap Coefficient:", overlap_coefficient(text1, text2), "\n")
