# Load necessary libraries
library(dplyr)
source("lib/csv_reader.R")
source("lib/make_unique_ids.R")
source("lib/similarity_methods.R")
source("lib/nlp_functions.R")
source("lib/data_loader.R")
source("lib/csv_writer.R")
source("lib/matrix_visualization.R")

# Define the compare_matrix_generator function in R
compare_matrix_generator <- function(input_file_path, filter_conditions) {
  start_time <- Sys.time()

  # Step 1: Read the CSV file and extract "user_id" and "final_submission"
  df <- csv_reader(input_file_path, filter_conditions = filter_conditions)
  user_ids <- df$user_id
  final_submissions <- df$final_submission
  cat("The size of the df:", nrow(df), "\n")

  # Step 1-2: Make user_ids unique by adding suffixes to duplicates
  unique_user_ids <- make_unique_ids(user_ids)
  # Check if unique_user_ids has duplicates after processing
  if (any(duplicated(unique_user_ids))) {
    stop("Error: Duplicate user_ids remain after processing.")
  }

  # Step 2: Prepare an empty list to store all similarity matrices
  similarities <- list(
    "cosine_similarity" = list(),
    # "euclidean_similarity" = list(),
    # "jaccard_similarity" = list(),
    # "levenshtein_similarity" = list(),
    # "overlap_similarity" = list(),
    "winnowing_similarity" = list()
  )

  # Step 3: Calculate the similarity between each text and generate the corresponding matrix
  # 初始化所有相似度矩陣
    n <- length(final_submissions)
    cosine_similarity_matrix <- matrix(NA, n, n)
    # euclidean_similarity_matrix <- matrix(NA, n, n)
    # jaccard_similarity_matrix <- matrix(NA, n, n)
    # levenshtein_similarity_matrix <- matrix(NA, n, n)
    # overlap_similarity_matrix <- matrix(NA, n, n)
    winnowing_similarity_matrix <- matrix(NA, n, n)

    # 計算相似度並填入矩陣
    for (i in seq_along(final_submissions)) {
      for (j in seq_along(final_submissions)) {
        if (i != j) {
          # 計算不同的相似度
          cosine_similarity_matrix[i, j] <- cosine_similarity(final_submissions[i], final_submissions[j])
          # euclidean_similarity_matrix[i, j] <- euclidean_similarity(final_submissions[i], final_submissions[j])
          # jaccard_similarity_matrix[i, j] <- jaccard_similarity(final_submissions[i], final_submissions[j])
          # levenshtein_similarity_matrix[i, j] <- 1 - normalized_levenshtein_distance(final_submissions[i], final_submissions[j])
          # overlap_similarity_matrix[i, j] <- overlap_coefficient(final_submissions[i], final_submissions[j])
          winnowing_similarity_matrix[i, j] <- winnowing(final_submissions[i], final_submissions[j], k = 3, w = 4)
        }
      }
    }

    # 將結果存入 similarities 列表中
    similarities <- list(
      "cosine_similarity" = cosine_similarity_matrix,
      # "euclidean_similarity" = euclidean_similarity_matrix,
      # "jaccard_similarity" = jaccard_similarity_matrix,
      # "levenshtein_similarity" = levenshtein_similarity_matrix,
      # "overlap_similarity" = overlap_similarity_matrix,
      "winnowing_similarity" = winnowing_similarity_matrix
    )

  # Step 4: Write the similarity matrix to CSV
  dfs <- csv_writer(unique_user_ids, similarities)

  # # Calculate and round the process time
  # process_time <- round(difftime(Sys.time(), start_time, units = "secs"), 2)
  # cat("Process Time:", process_time, "s\n")

  # Step 5: Draw dendrogram and heatmap
  # 取得目前日期時間作為檔名的一部分（Format：YYYYMMDD%H%M）
  datetime <- format(Sys.time(), "%Y%m%d%H%M")

  for (similarity_name in names(dfs)) {
    df <- dfs[[similarity_name]]
    output_path <- paste0("output/viz/", similarity_name, "_heatmap_", datetime, ".png")
    plot_similarity_heatmap(df = df, output_path = output_path)
  }

  return(similarities)
}

# Test the function
file_path <- 'data/text_data/extracted_behavior_pattern_data.csv'

# Define multiple filter conditions as strings
filter_conditions <- list(
  "use_ai == 1",
  "task_type == 'PRACTICAL'"
)

# Call the function with the file path and filter conditions
similarities <- compare_matrix_generator(input_file_path = file_path, filter_conditions = filter_conditions)
