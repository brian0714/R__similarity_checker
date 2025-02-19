# Load required libraries
source("lib/csv_reader.R")
library(tm)
library(SnowballC)

# Set the input file path
file_path <- 'data/text_data/extracted_behavior_pattern_data.csv'
TASK_TYPE <- "PRACTICAL" # "PRACTICAL" or "CREATIVE"

# Define multiple filter conditions as strings
filter_conditions <- list(
#   "use_ai == 0",
  paste0("task_type == '", TASK_TYPE, "'")
)

# Call the function
df <- csv_reader(file_path, filter_conditions = filter_conditions, remove_duplicates = TRUE)
final_submission <- as.vector(df$final_submission)


# Create a text corpus
corpus <- VCorpus(VectorSource(final_submission))

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

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(expand_contractions)) # 展開縮寫
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(remove_emojis))
# corpus <- tm_map(corpus, removeNumbers) # may have meaning
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)
# corpus <- tm_map(corpus, stemDocument) # causes error


# Create a term-document matrix
tdm <- TermDocumentMatrix(corpus)
tdm_matrix <- as.matrix(tdm)
colnames(tdm_matrix) <- df$user_id
print(tdm_matrix)

# Save the term-document matrix to a CSV file
output_dir <- "output/R_output/CSV_output/tdm"
datetime <- format(Sys.time(), "%Y%m%d%H%M")
csv_file_path <- paste0(output_dir, "/", TASK_TYPE, "_tdm_", datetime, ".csv")
write.csv(tdm_matrix, file = csv_file_path, row.names = TRUE, fileEncoding = "UTF-8")
