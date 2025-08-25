source("lib/rmd_data_loader.R")
source("lib/similarity_matrix_generator.R")
source("lib/consensus_similarity_matrix_generator.R")
source("lib/matrix_visualization.R")
source("lib/test_kmeans.R")
library(glue)

# Function to export most similar documents from a similarity matrix
export_most_similar_docs <- function(
  matrix_csv_path,
  threshold = 0.8,
  output_path = NULL
) {
  if (!file.exists(matrix_csv_path)) {
    stop("❌ Matrix CSV file not found: ", matrix_csv_path)
  }

  # 讀取相似度矩陣
  sim_matrix <- read.csv(matrix_csv_path, row.names = 1, check.names = FALSE)

  # 初始化結果表格
  result <- data.frame(
    doc = rownames(sim_matrix),
    most_similar_doc = character(nrow(sim_matrix)),
    similarity = numeric(nrow(sim_matrix)),
    stringsAsFactors = FALSE
  )

  for (i in 1:nrow(sim_matrix)) {
    sims <- sim_matrix[i, ]
    sims[i] <- NA
    max_val <- max(sims, na.rm = TRUE)

    if (!is.na(max_val) && max_val >= threshold) {
      max_index <- which.max(sims)
      result$most_similar_doc[i] <- colnames(sim_matrix)[max_index]
      result$similarity[i] <- round(max_val, 4)
    } else {
      result$most_similar_doc[i] <- ""
      result$similarity[i] <- NA
    }
  }

  # --- 🧠 自動生成輸出路徑 ---
  if (is.null(output_path)) {
    input_parts <- strsplit(matrix_csv_path, "/")[[1]]
    # folder_name <- gsub("\\s+", "", tolower(input_parts[length(input_parts) - 1]))  # e.g., "week 8" → "week8"
    folder_name <- tolower(input_parts[length(input_parts) - 1])
    file_stem <- sub("_checker.*\\.csv$", "", basename(matrix_csv_path))  # e.g., "cosine_checker_20250424..." → "cosine"

    output_dir <- file.path("output/R_output/CSV_output/rmd_similar_pairs", folder_name)
    output_file <- paste0(file_stem, "_most_similar_docs.csv")
    output_path <- file.path(output_dir, output_file)

    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
        cat("📁 Created output directory:", output_dir, "\n")
    }
  }
  write.csv(result, output_path, row.names = FALSE)
  cat("✅ Most similar document mapping saved to:", output_path, "\n")

  return(result)
}

# Function to export similarity statistics from a similarity matrix
export_doc_similarity_stats <- function(matrix_csv_path, output_path = NULL) {
  if (!file.exists(matrix_csv_path)) {
    stop("❌ Matrix CSV file not found: ", matrix_csv_path)
  }

  # 讀取相似度矩陣
  sim_df <- read.csv(matrix_csv_path, row.names = 1, check.names = FALSE)
  sim_matrix <- as.matrix(sapply(sim_df, as.numeric))

  result <- data.frame(
    doc = rownames(sim_df),
    average_score = numeric(nrow(sim_matrix)),
    std_score = numeric(nrow(sim_matrix)),
    stringsAsFactors = FALSE
  )

  for (i in 1:nrow(sim_matrix)) {
    sims <- sim_matrix[i, ]
    sims[i] <- NA  # 移除自己
    result$average_score[i] <- round(mean(sims, na.rm = TRUE), 4)
    result$std_score[i] <- round(sd(sims, na.rm = TRUE), 4)
  }

  # 自動決定輸出位置
  if (is.null(output_path)) {
    input_parts <- strsplit(matrix_csv_path, "/")[[1]]
    folder_name <- tolower(input_parts[length(input_parts) - 1])
    file_stem <- sub("_checker.*\\.csv$", "", basename(matrix_csv_path))
    output_dir <- file.path("output/R_output/CSV_output/rmd_similarity_stats", folder_name)
    output_file <- paste0(file_stem, "_doc_similarity_stats.csv")
    output_path <- file.path(output_dir, output_file)

    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
      cat("📁 Created output directory:", output_dir, "\n")
    }
  }

  write.csv(result, output_path, row.names = FALSE)
  cat("✅ Document similarity stats saved to:", output_path, "\n")

  return(result)
}


# Merge most similar docs across different methods
merge_most_similar_docs <- function(input_dir, output_csv = "merged_most_similar_matrix.csv") {
  files <- list.files(input_dir, pattern = "_most_similar_docs\\.csv$", full.names = TRUE)

  merged_df <- NULL

  for (file in files) {
    method_name <- basename(file)
    method_name <- sub("_most_similar_docs\\.csv$", "", method_name)
    method_name <- gsub("[()]", "", method_name)  # remove parentheses if any

    df <- read.csv(file, stringsAsFactors = FALSE)
    df <- df[, c("doc", "most_similar_doc")]
    colnames(df)[2] <- method_name

    if (is.null(merged_df)) {
      merged_df <- df
    } else {
      merged_df <- merge(merged_df, df, by = "doc", all = TRUE)
    }
  }

  output_path <- file.path(input_dir, output_csv)
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  write.csv(merged_df, output_path, row.names = FALSE)
  cat("✅ Merged matrix saved to:", file.path(input_dir, output_csv), "\n")
  return(merged_df)
}

# Merge similarity statistics from each doc across different methods
merge_similarity_stats <- function(
    input_dir,
    score_type = c("average_score", "std_score"),
    output_path = NULL,
    output_csv = NULL # "merged_doc_similarity_stats.csv"
) {
  score_type <- match.arg(score_type)

  files <- list.files(input_dir, pattern = "_doc_similarity_stats\\.csv$", full.names = TRUE)
  if (length(files) == 0) stop("❌ No *_doc_similarity_stats.csv files found in the directory.")

  merged_df <- NULL

  for (file in files) {
    method_name <- basename(file)
    method_name <- sub("_doc_similarity_stats\\.csv$", "", method_name)
    method_name <- gsub("[()]", "", method_name)

    df <- read.csv(file, stringsAsFactors = FALSE)
    if (!all(c("doc", score_type) %in% names(df))) {
      warning("⚠️ Missing required columns in: ", file)
      next
    }

    df <- df[, c("doc", score_type)]
    colnames(df)[2] <- method_name

    if (is.null(merged_df)) {
      merged_df <- df
    } else {
      merged_df <- merge(merged_df, df, by = "doc", all = TRUE)
    }
  }

  # ⬇️ 自動產出檔名（含 score_type）
  if (is.null(output_csv)) {
    output_csv <- paste0("merged_doc_similarity_stats_", score_type, ".csv")
  }

  if (is.null(output_path)) {
    output_path <- file.path(input_dir, output_csv)
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  }

  write.csv(merged_df, output_path, row.names = FALSE)
  cat("✅ Merged similarity stats saved to:", output_path, "\n")
  return(merged_df)
}

# test
WEEK = "week 8"
datetime = format(Sys.time(), "%Y%m%d%H%M")
SIM_METHOD = "jaccard"

# Build Raw document table
# csv_path <- build_raw_doc_table(glue("data/code_data/{WEEK}"))

# Generate similarity matrix
# similarities <- compare_matrix_generator(
#   input_file_path =  glue("output/R_output/CSV_output/rmd_doc_table/{WEEK}/raw_doc_table.csv"),
#   output_dir = glue("output/R_output/CSV_output/rmd_similarity_matrices/{WEEK}"),
#   methods = c("jaccard", "levenshtein") # c("cosine", "overlap")
# )

# Pairwise similarity matrix to find most similar documents
# - Cosine
# export_most_similar_docs(
#   matrix_csv_path = "output/R_output/CSV_output/rmd_similarity_matrices/week 10/cosine_checker_202505081044.csv",
#   threshold = 0.8
# )
# export_doc_similarity_stats(
#     matrix_csv_path = "output/R_output/CSV_output/rmd_similarity_matrices/week 8/cosine_checker_202504240519.csv"
# )

# - Levenshtein (no preprocessed)
# export_most_similar_docs(
#   matrix_csv_path = "output/R_output/CSV_output/rmd_similarity_matrices/week 12/levenshtein_checker_202505291125.csv",
#   threshold = 0.8
# )
# export_doc_similarity_stats(
#   matrix_csv_path = "output/R_output/CSV_output/rmd_similarity_matrices/week 8/levenshtein(no preprocess)_checker_.csv"
# )

# - Levenshtein
# export_most_similar_docs(
#   matrix_csv_path = "output/R_output/CSV_output/rmd_similarity_matrices/week 11/levenshtein_checker_202505291132.csv",
#   threshold = 0.8
# )
# export_doc_similarity_stats(
#   matrix_csv_path = "output/R_output/CSV_output/rmd_similarity_matrices/week 8/levenshtein_checker_202504240610.csv"
# )

# - Jaccard
# export_most_similar_docs(
#   matrix_csv_path = "output/R_output/CSV_output/rmd_similarity_matrices/week 14/levenshtein_checker_202505291130.csv",
#   threshold = 0.8
# )
# export_doc_similarity_stats(
#   matrix_csv_path = "output/R_output/CSV_output/rmd_similarity_matrices/week 8/jaccard_checker_202504240613.csv"
# )

# - Overlap
# export_most_similar_docs(
#   matrix_csv_path = "output/R_output/CSV_output/rmd_similarity_matrices/week 10/overlap_checker_202505081044.csv",
#   threshold = 0.8
# )
# export_doc_similarity_stats(
#   matrix_csv_path = "output/R_output/CSV_output/rmd_similarity_matrices/week 8/overlap_checker_202504240614.csv"
# )

# - Winnowing
# export_most_similar_docs(
#   matrix_csv_path = "output/R_output/CSV_output/rmd_similarity_matrices/week 8/winnowing_by_char_checker_202504240728.csv",
#   threshold = 0.8
# )
# export_most_similar_docs(
#   matrix_csv_path = "output/R_output/CSV_output/rmd_similarity_matrices/week 8/winnowing_by_char_checker_202504240728.csv",
#   threshold = 0.6,
#   output_path = "output/R_output/CSV_output/rmd_similar_pairs/week8/test/winnowing_by_char(>0.6)_most_similar_docs.csv"
# )
# export_doc_similarity_stats(
#   matrix_csv_path = "output/R_output/CSV_output/rmd_similarity_matrices/week 8/winnowing_by_char_checker_202504240728.csv"
# )

# - Winnowing
# export_most_similar_docs(
#   matrix_csv_path = "output/R_output/CSV_output/rmd_similarity_matrices/week 8/winnowing_checker_202504240737.csv",
#   threshold = 0.8
# )
# export_most_similar_docs(
#   matrix_csv_path = "output/R_output/CSV_output/rmd_similarity_matrices/week 8/winnowing_checker_202504240737.csv",
#   threshold = 0.6,
#   output_path = "output/R_output/CSV_output/rmd_similar_pairs/week8/test/winnowing(>0.6)_most_similar_docs.csv"
# )
# export_doc_similarity_stats(
#   matrix_csv_path = "output/R_output/CSV_output/rmd_similarity_matrices/week 8/winnowing_checker_202504240737.csv"
# )

# Merge most similar docs across different methods
# merged <- merge_most_similar_docs(glue("output/R_output/CSV_output/rmd_similar_pairs/{WEEK}"))
# head(merged)
# 合併所有 average_score
# merge_similarity_stats(
#     glue("output/R_output/CSV_output/rmd_similarity_stats/{WEEK}"),
#     score_type = "average_score"
# )
# 或合併所有 std_score
# merge_similarity_stats(
#     glue("output/R_output/CSV_output/rmd_similarity_stats/{WEEK}"),
#     score_type = "std_score"
# )

# Plot box plot of similarity score distribution
plot_all_similarity_boxplots(glue("output/R_output/CSV_output/rmd_similarity_matrices/{WEEK}"))



# Create consensus similarity matrix
# cosine_sim_matrix <- "output/R_output/CSV_output/rmd_similarity_matrices/week 8/cosine_checker_202504240519.csv"
# levenshtein_sim_matrix <- "output/R_output/CSV_output/rmd_similarity_matrices/week 8/levenshtein_checker_202504240610.csv"
# levenshtein_no_preprocess_sim_matrix <- "output/R_output/CSV_output/rmd_similarity_matrices/week 8/levenshtein(no preprocess)_checker_.csv"
# jaccard_sim_matrix <- "output/R_output/CSV_output/rmd_similarity_matrices/week 8/jaccard_checker_202504240613.csv"
# overlap_sim_matrix <- "output/R_output/CSV_output/rmd_similarity_matrices/week 8/overlap_checker_202504240614.csv"
# winnowing_sim_matrix <- "output/R_output/CSV_output/rmd_similarity_matrices/week 8/winnowing_checker_202504240737.csv"
# winnowing_by_char_sim_matrix <- "output/R_output/CSV_output/rmd_similarity_matrices/week 8/winnowing_by_char_checker_202504240728.csv"

# 指定讀取的相似度矩陣 CSV 檔案路徑
# file_paths <- c(
#     cosine_sim_matrix,
#     levenshtein_sim_matrix,
#     levenshtein_no_preprocess_sim_matrix,
#     jaccard_sim_matrix,
#     overlap_sim_matrix,
#     winnowing_sim_matrix,
#     winnowing_by_char_sim_matrix
# )

# consensus_matrix <- consensus_similarity_matrix_generator(
#   file_paths,
#   weights = "average",
#   output_path = glue("output/R_output/CSV_output/rmd_similarity_matrices/week 8/consensus_matrix/consensus_matrix_average.csv")
# )

# export_most_similar_docs(
#   matrix_csv_path = "output/R_output/CSV_output/rmd_similarity_matrices/week 8/consensus_matrix/consensus_matrix_average.csv",
#   threshold = 0.6,
#   output_path = "output/R_output/CSV_output/rmd_similar_pairs/week8/consensus_most_similar_docs.csv"
# )

# plot_similarity_boxplot(
#   file_path = "output/R_output/CSV_output/rmd_similarity_matrices/week 8/consensus_matrix/consensus_matrix_average.csv",
#   title = "Consensus (average weight) Similarity Score Distribution",
#   save_path = "output/viz/boxplot (similarity scores)/consensus_matrix_average_boxplot.png"
# )

# Test K means
# Similarity matrix
# sim_matrix <- "output/R_output/CSV_output/rmd_similarity_matrices/week 8/jaccard_checker_202504240613.csv"

# 3️⃣ 使用 Elbow Method 確定最佳 k
# output_path <- glue("output/viz/sse_curve/RMD/{SIM_METHOD}_sse_elbow_plot_{datetime}.png")
# optimal_k <- elbow_method(file_path=sim_matrix, output_path, max_k = 70)
# cat("Optimal k (elbow point):", optimal_k, "\n")

# 4️⃣ 執行 K-Means 並輸出分群結果
# kmeans_results <- kmeans_clustering(sim_matrix, optimal_k)
# clusters <- kmeans_results$cluster_groups
# representative_docs <- kmeans_results$representative_docs

# 5️⃣ 將 cluster 寫入 JSON
# output_name <- glue("RMD_{SIM_METHOD}_clusters")

# json_file_path <- write_list_to_json(
#   clusters,
#   output_dir = glue("output/R_output/json_output/RMD_clusters/"),
#   output_name = output_name
# )

# 6️⃣ 分析 cluster 的 JSON 檔案 (No task type)
# analyze_clusters_from_json(
#     json_file_path,
#     TASK_TYPE=TASK_TYPE,
#     representative_docs=representative_docs
# )
