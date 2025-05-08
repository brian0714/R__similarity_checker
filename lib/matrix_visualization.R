# install.packages("dendextend")
# install.packages("cluster")
# install.packages("factoextra")

source("lib/csv_reader.R")
source("lib/cluster_to_json_writer.R")
# Load necessary libraries
library(pheatmap)
library(dendextend)
library(cluster)
library(ggplot2)
library(glue)
# library(factoextra)

# 讀取 CSV 或使用現有的相似度矩陣
read_similarity_matrix <- function(file_path) {
  # 如果輸入是檔案路徑
  if (is.character(file_path)) {
    # 讀取 CSV 檔案
    matrix_data <- read.csv(file_path, stringsAsFactors = FALSE)

    # 將非 user_id 欄位轉成數值
    matrix_data_clean <- apply(matrix_data[,-1], 2, as.numeric)
    matrix_data_clean <- as.matrix(matrix_data_clean)

    # 設定列與欄名稱
    rownames(matrix_data_clean) <- matrix_data$user_id
    colnames(matrix_data_clean) <- matrix_data$user_id

    # 處理缺失或無效值
    if (any(is.na(matrix_data_clean) | is.nan(matrix_data_clean) | is.infinite(matrix_data_clean))) {
      matrix_data_clean[is.na(matrix_data_clean) | is.nan(matrix_data_clean) | is.infinite(matrix_data_clean)] <- 0
      # cat("Warning in reading matrix: Missing or invalid values detected and replaced with 0.\n")
    }

  } else if (is.matrix(file_path)) {
    # 直接使用提供的數值矩陣
    matrix_data_clean <- file_path
  } else {
    stop("Error: file_path must be either a file path (character) or a matrix.")
  }

  return(matrix_data_clean)
}

# Heatmap plot function
plot_similarity_heatmap <- function(file_path = NULL, df = NULL, output_path) {
  if (!is.null(file_path)) {
    # Load the matrix from CSV if file_path is provided
    matrix_data <- read.csv(file_path)
    View(matrix_data)
    # Remove user_id column for the heatmap
    matrix_data_clean <- as.matrix(matrix_data[,-1])
    # Set row and column names to user_id
    rownames(matrix_data_clean) <- matrix_data$user_id
    colnames(matrix_data_clean) <- matrix_data$user_id
  } else if (!is.null(df)) {
    # Use the provided dataframe directly
    matrix_data_clean <- as.matrix(df[,-1])
    # Set row and column names to user_id
    rownames(matrix_data_clean) <- df$user_id
    colnames(matrix_data_clean) <- df$user_id
  } else {
    stop("Error: Either 'file_path' or 'df' must be provided.")
  }

  # Plot heatmap and save to output_path
  pheatmap(matrix_data_clean, cluster_rows = TRUE, cluster_cols = TRUE, filename = output_path)
}

# Dendrogram plot function
plot_dendrogram <- function(file_path, method = "average", output_path) {
    # Load the matrix from CSV if file_path is provided
    matrix_data <- read.csv(file_path)
    # 將資料轉換為矩陣並移除 user_id 欄位
    matrix_data_clean <- as.matrix(matrix_data[,-1])
    rownames(matrix_data_clean) <- matrix_data$user_id
    colnames(matrix_data_clean) <- matrix_data$user_id

    # 計算距離矩陣
    dist_matrix <- dist(matrix_data_clean)

    # 使用指定的 linkage 方法進行階層式聚類
    hc <- hclust(dist_matrix, method = method)

    # 將聚類結果轉換為樹狀圖物件
    dend <- as.dendrogram(hc)

    # 繪製樹狀圖
    png(output_path, width = 800, height = 600)
    plot(dend, main = paste("Dendrogram using", method, "linkage"))
    dev.off()
}

# 計算 Silhouette score 並繪製最佳 k 值的圖表
calculate_silhouette_scores <- function(file_path, output_path, method = "average", max_k = 10) {
    # 讀取 CSV 或使用現有的相似度矩陣
    matrix_data_clean <- read_similarity_matrix(file_path)

    # 計算距離矩陣
    # different from dist(matrix_data_clean) as it calculates the Euclidean distance
    dist_matrix <- as.dist(1 - matrix_data_clean)

    # 初始化儲存 Silhouette scores
    silhouette_scores <- numeric(max_k - 1)

    # 使用 hclust 創建階層式聚類
    hc <- hclust(dist_matrix, method = method)

    # 計算每個 k 的 Silhouette score
    for (k in 2:max_k) {
        clustering <- cutree(hc, k = k)  # 只將 k 傳遞給 cutree
        silhouette <- silhouette(clustering, dist_matrix)
        silhouette_scores[k - 1] <- mean(silhouette[, 3])  # Silhouette score 的第三欄是 score
    }

    # 繪製 Silhouette scores plot
    png(output_path, width = 800, height = 600)
    plot(2:max_k, silhouette_scores, type = "b", col = "blue", pch = 4,
         xlab = "k", ylab = "Silhouette Score",
         main = "Silhouette method for Optimal k")
    dev.off()  # 關閉圖形設備以保存圖片

    # 回傳最佳的 k 值
    best_k <- which.max(silhouette_scores) + 1
    return(best_k)
}

# Elbow method function for determining optimal k (適用於相似度矩陣)
elbow_method <- function(file_path, output_path, max_k = 10, seed = 123) {
  # 設定隨機種子，確保每次結果一致
  set.seed(seed)

  # 讀取 CSV 或使用現有的相似度矩陣
  matrix_data_clean <- read_similarity_matrix(file_path)

  # 轉換相似度為距離矩陣
  distance_matrix <- as.dist(1 - matrix_data_clean)

  # 使用 MDS 降維
  feature_matrix <- cmdscale(distance_matrix, k = 5)

  # 計算 SSE
  sse <- numeric(max_k - 1)
  for (k in 2:max_k) {
    kmeans_result <- kmeans(feature_matrix, centers = k, nstart = 25)
    sse[k - 1] <- kmeans_result$tot.withinss
  }

  # 繪圖
  png(output_path, width = 800, height = 600)
  plot(2:max_k, sse, type = "b", col = "blue", pch = 4,
       xlab = "k", ylab = "SSE (Sum of Squared Errors)",
       main = paste("Elbow Method for Optimal k (k range: 2 to", max_k, ")"))

  # 找 elbow point
  elbow_k <- which.min(diff(diff(sse))) + 2
  points(elbow_k, sse[elbow_k - 1], col = "red", pch = 19, cex = 1.5)
  text(elbow_k, sse[elbow_k - 1], labels = paste("Elbow at k =", elbow_k), pos = 4, col = "red")

  dev.off()

  return(elbow_k)
}

# Dendrogram plot function with cutree visualization
plot_dendrogram_with_cut <- function(file_path, task_type, method = "average", k = 4, output_path) {
  # 讀取資料
  matrix_data <- read.csv(file_path)
  matrix_data_clean <- as.matrix(matrix_data[,-1])
  rownames(matrix_data_clean) <- matrix_data$user_id
  colnames(matrix_data_clean) <- matrix_data$user_id

  dist_matrix <- dist(matrix_data_clean)
  hc <- hclust(dist_matrix, method = method)
  dend <- as.dendrogram(hc)
  clusters <- cutree(hc, k = k)

  # 取得 dendrogram 左到右的順序
  ordered_ids <- rownames(matrix_data_clean)[hc$order]

  # 將每群內的 ID 按照圖中的順序排序
  cluster_list <- lapply(1:k, function(i) {
    ids <- names(clusters[clusters == i])
    ids[order(match(ids, ordered_ids))]
  })

  # 根據每群在圖中最早出現的 ID 位置，決定整體順序
  first_positions <- sapply(cluster_list, function(ids) {
    min(match(ids, ordered_ids))
  })

  # 重新排序整體 cluster list
  cluster_list <- cluster_list[order(first_positions)]

  # 顯示每個群組及其平均字數
  cat("Cluster members (ordered by dendrogram):\n")
  for (i in seq_along(cluster_list)) {
    members <- cluster_list[[i]]
    cat("Cluster", i, "( size =", length(members), "):", members, "\n")

    document_length_list <- numeric(0)
    for (j in members) {
      filter_conditions <- list(
        paste0("user_id == ", j),
        paste0("task_type == '", task_type, "'")
      )
      row <- csv_reader(show_col_types = FALSE, filter_conditions = filter_conditions)
      submission <- as.character(row$final_submission)
      document_length_list <- c(document_length_list, length(unlist(strsplit(submission, " "))))
    }

    average_word_size <- mean(document_length_list)
    cat("Cluster", i, "average word size:", average_word_size, "\n")
  }

  # 繪圖
  png(output_path, width = 800, height = 600)
  plot(dend, main = paste("Dendrogram using", method, "linkage with", k, "clusters"))
  rect.hclust(hc, k = k, border = "red")
  dev.off()

  return(cluster_list)
}

# Boxplot function for similarity scores
plot_similarity_boxplot <- function(
  file_path,
  title = "Similarity Score Distribution",
  save_path = NULL
) {
  # 讀取資料
  matrix_data <- read.csv(file_path)
  sim_matrix <- as.matrix(matrix_data[,-1])

  if (!is.matrix(sim_matrix)) {
    stop("❌ Input must be a matrix.")
  }

  n <- nrow(sim_matrix)
  if (n != ncol(sim_matrix)) {
    stop("❌ Similarity matrix must be square (N x N).")
  }

  # 移除對角線（自己 vs 自己 = 1 或 NA）
  all_scores <- c()
  for (i in 1:n) {
    scores <- sim_matrix[i, ]
    scores[i] <- NA
    all_scores <- c(all_scores, scores)
  }

  all_scores <- na.omit(all_scores)

  # 畫圖
  boxplot(
    all_scores,
    main = title,
    ylab = "Similarity Score",
    col = "skyblue",
    border = "darkblue",
    ylim = c(0, 1)
  )

  if (!is.null(save_path)) {
    png(filename = save_path, width = 600, height = 400)
    boxplot(
      all_scores,
      main = title,
      ylab = "Similarity Score",
      col = "skyblue",
      border = "darkblue"
    )
    dev.off()
    cat("✅ Saved boxplot to:", save_path, "\n")
  }

  # 回傳 summary 統計
  return(summary(all_scores))
}

plot_all_similarity_boxplots <- function(input_dir, output_dir = "output/viz/boxplot (similarity scores)") {
  files <- list.files(input_dir, pattern = "_checker_.*\\.csv$", full.names = TRUE)

  for (file_path in files) {
    filename <- basename(file_path)
    method <- sub("_checker_.*", "", filename)  # e.g. cosine_checker_20250424... → cosine
    title <- glue("{method} Similarity Score Distribution")
    save_path <- file.path(output_dir, paste0(method, "_similarity_score_boxplot.png"))

    plot_similarity_boxplot(file_path, title, save_path)
  }
}


# Example usage
TASK_TYPE <- "PRACTICAL" # "PRACTICAL" or "CREATIVE"
SIM_METHOD <- "winnowing_by_char"  # "jaccard" or "overlap" or "winnowing" or "winnowing_by_char" or "cosine" or "levenshtein"

# Case 1: Using file_path
# CREATIVE
# file_path <- "output/R_output/CSV_output/CREATIVE_similarity_matrices/cosine_similarity_checker_202502280632.csv"
# file_path <- glue("output/R_output/CSV_output/CREATIVE_similarity_matrices/winnowing_similarity_checker_202502281048.csv")
# file_path <- "output/R_output/CSV_output/CREATIVE_similarity_matrices/winnowing_similarity_by_char_checker_202503270642.csv"
# file_path <- "output/R_output/CSV_output/CREATIVE_similarity_matrices/jaccard_similarity_checker_202502281040.csv"
# file_path <- "output/R_output/CSV_output/CREATIVE_similarity_matrices/overlap_similarity_checker_202502281040.csv"
# file_path <- "output/R_output/CSV_output/CREATIVE_similarity_matrices/levenshtein_similarity_checker_202502281040.csv"

# PRACTICAL
# file_path <- "output/R_output/CSV_output/PRACTICAL_similarity_matrices/cosine_similarity_checker_202502281029.csv"
# file_path <- "output/R_output/CSV_output/PRACTICAL_similarity_matrices/levenshtein_similarity_checker_202502281037.csv"
# file_path <- "output/R_output/CSV_output/PRACTICAL_similarity_matrices/jaccard_similarity_checker_202502281037.csv"
# file_path <- "output/R_output/CSV_output/PRACTICAL_similarity_matrices/overlap_similarity_checker_202502281037.csv"
# file_path <- "output/R_output/CSV_output/PRACTICAL_similarity_matrices/winnowing_similarity_checker_202502281052.csv"
file_path <- "output/R_output/CSV_output/PRACTICAL_similarity_matrices/winnowing_by_char_similarity_checker_202503271559.csv"


# 範例使用，繪製相似度熱度圖
# output_path <- "output/viz/heatmap/winnowing_similarity_heatmap.png"
# plot_similarity_heatmap(file_path = file_path, output_path = output_path)

# 範例使用，使用不同的連結方法繪製樹狀圖並指定不同的 linkage method 的繪圖產出位置
# output_path_average <- "output/viz/dendrogram/winnowing_dendrogram_average.png"
# output_path_single <- "output/viz/dendrogram/winnowing_dendrogram_single.png"
# output_path_complete <- "output/viz/dendrogram/winnowing_dendrogram_complete.png"
# plot_dendrogram(file_path, method = "average", output_path = output_path_average)
# plot_dendrogram(file_path, method = "single", output_path = output_path_single)
# plot_dendrogram(file_path, method = "complete", output_path = output_path_complete)

# 範例使用，計算 Silhouette scores 並繪製最佳 k 值的圖表
# output_path <- "output/viz/silhouette_scores/winnowing_silhouette_scores_plot.png"
# output_path <- "output/viz/silhouette_scores/cosine_silhouette_scores_plot.png"
# optimal_k <- calculate_silhouette_scores(file_path, output_path, method = "average", max_k = 10)
# cat("Optimal k (Silhouette scores):", optimal_k, "\n")

# 範例使用，計算 Elbow method 並繪製最佳 k 值的圖表
# output_path <- "output/viz/sse_curve/winnowing_sse_elbow_plot.png"
# output_path <- "output/viz/sse_curve/cosine_sse_elbow_plot.png"
# optimal_k <- elbow_method(file_path, output_path, max_k = 10)
# cat("Optimal k (elbow point):", optimal_k, "\n")

# 範例使用，繪製帶有切割結果的樹狀圖
# output_path <- "output/viz/dendrogram/winnowing_dendrogram_with_cut.png"
output_path <- glue("output/viz/dendrogram/{TASK_TYPE}/{TASK_TYPE}_{SIM_METHOD}_dendrogram_with_cut.png")
optimal_k <- 8
# clusters <- plot_dendrogram_with_cut(
#   file_path,
#   task_type = TASK_TYPE,
#   method = "average",
#   k = optimal_k,
#   output_path = output_path
# )

# 將 cluster 寫入 JSON
output_name <- glue("{TASK_TYPE}_HC_{SIM_METHOD}_clusters")
# json_file_path <- write_list_to_json(
#   clusters,
#   output_dir = glue("output/R_output/json_output/{TASK_TYPE}_clusters/HC"),
#   output_name = output_name
# )


# Case 2: Using df directly
# Assuming df is a pre-loaded data frame with similar structure
# output_path <- "output/viz/heatmap_dend_output.png"
# df = ??
# plot_similarity_heatmap(df = df, output_path = output_path)
