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
compare_matrix_generator <- function(
  input_file_path,
  filter_conditions = list(),
  output_dir = "output/R_output/CSV_output"
  ) {
  start_time <- Sys.time()

  # Step 1: Read the CSV file and extract ID and text columns based on available format
  # Check if the input file exists
  if (!file.exists(input_file_path)) {
    stop("❌ Error: Input file does not exist.")
  }
  # Read the CSV file
  temp_df <- read.csv(input_file_path, stringsAsFactors = FALSE)

  if (all(c("user_id", "final_submission") %in% names(temp_df))) {
    df <- csv_reader(input_file_path, filter_conditions = filter_conditions)
    doc_ids <- df$user_id
    final_submissions <- df$final_submission
    cat("🗂 Format detected: user_id + final_submission\n")
  } else if (all(c("doc_id", "text") %in% names(temp_df))) {
    df <- load_doc_table(csv_path)
    doc_ids <- df$doc_id
    final_submissions <- df$text
    cat("🗂 Format detected: doc_id + text\n")
  } else {
    stop("❌ Error: CSV must contain either ('user_id', 'final_submission') or ('doc_id', 'text') columns.")
  }

  cat("The size of the df:", nrow(df), "\n")


  # Step 1-2: Make doc_ids unique by adding suffixes to duplicates
  result <- make_unique_ids(doc_ids)
  # Get unique_ids and duplicate_ids separately
  unique_doc_ids_with_suffices <- result$unique_ids
  duplicate_doc_ids <- result$duplicate_ids

  # Filter out duplicate doc_ids from doc_ids and final_submissions
  non_duplicate_indices <- !(doc_ids %in% duplicate_doc_ids)
  unique_doc_ids <- doc_ids[non_duplicate_indices]
  final_submissions <- final_submissions[non_duplicate_indices]
  cat("Removed duplicates. Remaining size of the df:", length(unique_doc_ids), "\n")

  # Check if unique_doc_ids has duplicates after processing
  if (any(duplicated(unique_doc_ids))) {
    stop("Error: Duplicate doc_ids remain after processing.")
  } else {
    cat("Unique doc_ids after processing:\n", unique_doc_ids, "\n")
  }

  # Step 2: Prepare an empty list to store all similarity matrices
  similarities <- list(
    # "cosine_similarity" = list(),
    # "cosine_similarity_with_bigram" = list(),
    # "cosine_similarity_with_trigram" = list(),
    # "euclidean_similarity" = list(),
    # "jaccard_similarity" = list(),
    # "levenshtein_similarity" = list(),
    # "overlap_similarity" = list(),
    # "winnowing_similarity" = list(),
    "winnowing_similarity_by_char" = list()
  )

  # Step 3: Calculate the similarity between each text and generate the corresponding matrix
  # 初始化所有相似度矩陣
    n <- length(final_submissions)
    # cosine_similarity_matrix <- matrix(NA, n, n)
    # cosine_similarity_with_bigram_matrix <- matrix(NA, n, n)
    # cosine_similarity_with_trigram_matrix <- matrix(NA, n, n)
    # euclidean_similarity_matrix <- matrix(NA, n, n)
    # jaccard_similarity_matrix <- matrix(NA, n, n)
    # levenshtein_similarity_matrix <- matrix(NA, n, n)
    # overlap_similarity_matrix <- matrix(NA, n, n)
    # winnowing_similarity_matrix <- matrix(NA, n, n)
    winnowing_similarity_by_char_matrix <- matrix(NA, n, n)

    # 計算相似度並填入矩陣
    for (i in seq_along(final_submissions)) {
      for (j in seq_along(final_submissions)) {
        if (i != j) {
          # 計算不同的相似度
          # cosine_similarity_matrix[i, j] <- cosine_similarity(final_submissions[i], final_submissions[j])
          # cosine_similarity_with_bigram_matrix[i, j] <- cosine_similarity(final_submissions[i], final_submissions[j], tokenize_method = "bigram")
          # cosine_similarity_with_trigram_matrix[i, j] <- cosine_similarity(final_submissions[i], final_submissions[j], tokenize_method = "trigram")
          # euclidean_similarity_matrix[i, j] <- euclidean_similarity(final_submissions[i], final_submissions[j])
          # jaccard_similarity_matrix[i, j] <- jaccard_similarity(final_submissions[i], final_submissions[j])
          # levenshtein_similarity_matrix[i, j] <- 1 - normalized_levenshtein_distance(final_submissions[i], final_submissions[j])
          # overlap_similarity_matrix[i, j] <- overlap_coefficient(final_submissions[i], final_submissions[j])
          # winnowing_similarity_matrix[i, j] <- winnowing(final_submissions[i], final_submissions[j], k = 3, w = 4)
          winnowing_similarity_by_char_matrix[i, j] <- winnowing_by_char(final_submissions[i], final_submissions[j], k = 5)
        }
      }
    }

    # 將結果存入 similarities 列表中
    similarities <- list(
      # "cosine_similarity" = cosine_similarity_matrix,
      # "cosine_similarity_with_bigram" = cosine_similarity_with_bigram_matrix,
      # "cosine_similarity_with_trigram" = cosine_similarity_with_trigram_matrix
      # "euclidean_similarity" = euclidean_similarity_matrix,
      # "jaccard_similarity" = jaccard_similarity_matrix,
      # "levenshtein_similarity" = levenshtein_similarity_matrix,
      # "overlap_similarity" = overlap_similarity_matrix,
      # "winnowing_similarity" = winnowing_similarity_matrix,
      "winnowing_similarity_by_char" = winnowing_similarity_by_char_matrix
    )

  # Step 4: Write the similarity matrix to CSV
  # Check if the output directory exists, if not, create it
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("📁 Created output directory:", output_dir, "\n")
  }
  dfs <- csv_writer(
    unique_doc_ids,
    similarities,
    output_dir
  )

  # Calculate and round the process time
  process_time <- round(difftime(Sys.time(), start_time, units = "secs"), 2)
  cat("Process Time:", process_time, "s\n")

  # Step 5: Draw dendrogram and heatmap
  # 取得目前日期時間作為檔名的一部分（Format：YYYYMMDD%H%M）
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
