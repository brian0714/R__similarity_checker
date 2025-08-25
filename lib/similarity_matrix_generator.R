# Load necessary libraries
library(dplyr)
library(glue)
source("lib/csv_reader.R")
source("lib/rmd_data_loader.R")
source("lib/make_unique_ids.R")
source("lib/similarity_methods.R")
source("lib/nlp_functions.R")
source("lib/csv_writer.R")
source("lib/matrix_visualization.R")

# Define the compare_matrix_generator function in R
# Generate a similarity matrix from a CSV file.
compare_matrix_generator <- function(
  input_file_path,
  filter_conditions = list(),
  output_dir = "output/R_output/CSV_output",
  methods = c("cosine")
) {
  start_time <- Sys.time()

  # Step 1: Check and read the data (檢查並讀取資料)
  if (!file.exists(input_file_path)) {
    stop("❌ Error: Input file does not exist.")
  }
  temp_df <- read.csv(input_file_path, stringsAsFactors = FALSE)

  if (all(c("user_id", "final_submission") %in% names(temp_df))) {
    df <- csv_reader(input_file_path, filter_conditions = filter_conditions)
    doc_ids <- df$user_id
    final_submissions <- df$final_submission
    cat("🗂 Format detected: user_id + final_submission\n")
    rmd <- FALSE
  } else if (all(c("doc_id", "text") %in% names(temp_df))) {
    rmd <- TRUE
    df <- load_doc_table(input_file_path)
    doc_ids <- df$doc_id
    final_submissions <- df$text
    cat("🗂 Format detected: doc_id + text\n")
  } else {
    stop("❌ Error: CSV must contain either ('user_id', 'final_submission') or ('doc_id', 'text') columns.")
  }

  cat("The size of the df:", nrow(df), "\n")

  # Step 1-2: Check if there are duplicate doc_ids, if so, handle them
  # Step 1-2: 檢查是否有重複的 doc_ids，若有則處理
  if (any(duplicated(doc_ids))) {
    cat("⚠️ Detected duplicate doc_ids. Proceeding to remove duplicates...\n")

    result <- make_unique_ids(doc_ids)
    unique_doc_ids_with_suffices <- result$unique_ids
    duplicate_doc_ids <- result$duplicate_ids

    non_duplicate_indices <- !(doc_ids %in% duplicate_doc_ids)
    unique_doc_ids <- doc_ids[non_duplicate_indices]
    final_submissions <- final_submissions[non_duplicate_indices]

    cat("Removed duplicates. Remaining size of the df:", length(unique_doc_ids), "\n")

    if (any(duplicated(unique_doc_ids))) {
      stop("❌ Error: Duplicate doc_ids remain after processing.")
    } else {
      cat("✅ Unique doc_ids after processing:\n", unique_doc_ids, "\n")
    }
  } else {
    unique_doc_ids <- doc_ids
    # cat("✅ No duplicate doc_ids found.\n")
  }

  # Step 2: Initialize the similarity matrix (初始化相似度矩陣)
  available_methods <- c(
    "cosine", "cosine_bigram", "cosine_trigram",
    "euclidean", "jaccard", "levenshtein", "overlap",
    "winnowing", "winnowing_by_char"
  )
  invalid_methods <- setdiff(methods, available_methods)
  if (length(invalid_methods) > 0) {
    stop("❌ Invalid method(s): ", paste(invalid_methods, collapse = ", "))
  }

  n <- length(final_submissions)
  similarities <- list()
  for (method in methods) {
    similarities[[method]] <- matrix(NA, n, n)
  }

  cat("🔧 Enabled methods:", paste(methods, collapse = ", "), "\n")

  # Step 3: Compute Similarity 計算相似度
  for (i in 1:(n - 1)) {
    cat("🔄 Processing document", i, "of", n, "\n")
    # Progress bar (進度條)
    # pb <- txtProgressBar(min = 0, max = n, style = 3)
    # setTxtProgressBar(pb, i)
    # Sys.sleep(0.1)  # Simulate time progress 模擬計算時間

    for (j in (i + 1):n) {
      text_i <- final_submissions[i]
      text_j <- final_submissions[j]

      if ("cosine" %in% methods) {
        val <- cosine_similarity(text_i, text_j)
        similarities[["cosine"]][i, j] <- val
        similarities[["cosine"]][j, i] <- val
      }
      if ("cosine_bigram" %in% methods) {
        val <- cosine_similarity(text_i, text_j, tokenize_method = "bigram")
        similarities[["cosine_bigram"]][i, j] <- val
        similarities[["cosine_bigram"]][j, i] <- val
      }
      if ("cosine_trigram" %in% methods) {
        val <- cosine_similarity(text_i, text_j, tokenize_method = "trigram")
        similarities[["cosine_trigram"]][i, j] <- val
        similarities[["cosine_trigram"]][j, i] <- val
      }
      if ("euclidean" %in% methods) {
        val <- euclidean_similarity(text_i, text_j)
        similarities[["euclidean"]][i, j] <- val
        similarities[["euclidean"]][j, i] <- val
      }
      if ("jaccard" %in% methods) {
        val <- jaccard_similarity(text_i, text_j)
        similarities[["jaccard"]][i, j] <- val
        similarities[["jaccard"]][j, i] <- val
      }
      if ("levenshtein" %in% methods) {
        val <- 1 - levenshtein_distance(text_i, text_j, normalized = TRUE, rmd = rmd)
        similarities[["levenshtein"]][i, j] <- val
        similarities[["levenshtein"]][j, i] <- val
      }
      if ("overlap" %in% methods) {
        val <- overlap_coefficient(text_i, text_j)
        similarities[["overlap"]][i, j] <- val
        similarities[["overlap"]][j, i] <- val
      }
      if ("winnowing" %in% methods) {
        val <- winnowing(text_i, text_j, k = 3, w = 4)
        similarities[["winnowing"]][i, j] <- val
        similarities[["winnowing"]][j, i] <- val
      }
      if ("winnowing_by_char" %in% methods) {
        val <- winnowing_by_char(text_i, text_j, k = 5)
        similarities[["winnowing_by_char"]][i, j] <- val
        similarities[["winnowing_by_char"]][j, i] <- val
      }
    }
  }

  # Step 4: Export the similarity matrix (匯出相似度矩陣)
  dfs <- csv_writer(
    unique_doc_ids,
    similarities,
    output_dir
  )

  # Step 5: Display the processing time (顯示處理時間)
  process_time <- round(difftime(Sys.time(), start_time, units = "secs"), 2)
  cat("\n⏱️ Process Time:", process_time, "s\n")

  # Step 6: Plot heatmap 繪製熱圖
  datetime <- format(Sys.time(), "%Y%m%d%H%M")

  for (similarity_name in names(dfs)) {
    df <- dfs[[similarity_name]]
    output_path <- paste0("output/viz/heatmap/", similarity_name, "_heatmap_", datetime, ".png")
    plot_similarity_heatmap(df = df, output_path = output_path)
  }

  return(similarities)
}


# Test the function
file_path <- 'data/text_data/extracted_behavior_pattern_data.csv'
TASK_TYPE <- "PRACTICAL" # "PRACTICAL" or "CREATIVE"

# Define multiple filter conditions as strings
filter_conditions <- list(
  # "use_ai == 1",
  paste0("task_type == '", TASK_TYPE, "'")
)

# Call the function with the file path and filter conditions
# similarities <- compare_matrix_generator(
#   input_file_path = file_path,
#   filter_conditions = filter_conditions,
#   output_dir = glue("output/R_output/CSV_output/{TASK_TYPE}_similarity_matrices")
# )
