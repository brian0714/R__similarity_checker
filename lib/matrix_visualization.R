# install.packages("dendextend")
# install.packages("cluster")

# Load necessary libraries
library(pheatmap)
library(dendextend)
library(cluster)

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
    # Load the matrix from CSV if file_path is provided
    matrix_data <- read.csv(file_path)

    # 移除 user_id 欄位，並將其轉換為矩陣格式
    matrix_data_clean <- as.matrix(matrix_data[,-1])
    rownames(matrix_data_clean) <- matrix_data$user_id
    colnames(matrix_data_clean) <- matrix_data$user_id

    # 計算距離矩陣
    dist_matrix <- dist(matrix_data_clean)

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

# Elbow method function for determining optimal k
elbow_method <- function(file_path, output_path, max_k = 10) {
  # Load the matrix from CSV if file_path is provided
  matrix_data <- read.csv(file_path)

  # 移除 user_id 欄位，並將其轉換為矩陣格式
  matrix_data_clean <- as.matrix(matrix_data[,-1])
  rownames(matrix_data_clean) <- matrix_data$user_id
  colnames(matrix_data_clean) <- matrix_data$user_id

  # 檢查並處理缺失值或無效值
  if (any(is.na(matrix_data_clean) | is.nan(matrix_data_clean) | is.infinite(matrix_data_clean))) {
    matrix_data_clean[is.na(matrix_data_clean) | is.nan(matrix_data_clean) | is.infinite(matrix_data_clean)] <- 0
    cat("Warning: Missing or invalid values detected and replaced with 0.\n")
  }

  # 計算每個 k 的 SSE
  sse <- numeric(max_k - 1)
  for (k in 2:max_k) {
    kmeans_result <- kmeans(matrix_data_clean, centers = k, nstart = 25)
    sse[k - 1] <- kmeans_result$tot.withinss  # SSE
  }

  # 繪製 Elbow curve 並標示 elbow point
  png(output_path, width = 800, height = 600)
  plot(2:max_k, sse, type = "b", col = "blue", pch = 4,
       xlab = "k", ylab = "SSE (Sum of Squared Errors)",
       main = "Elbow Method for Optimal k")

  # 找到 elbow point 的 k 值（SSE 的變化開始減緩）
  elbow_k <- which.min(diff(diff(sse))) + 2
  points(elbow_k, sse[elbow_k - 1], col = "red", pch = 19, cex = 1.5)
  text(elbow_k, sse[elbow_k - 1], labels = paste("Elbow at k =", elbow_k), pos = 4, col = "red")

  dev.off()  # 關閉圖形設備
  return(elbow_k)
}

# Dendrogram plot function with cutree visualization
plot_dendrogram_with_cut <- function(file_path, method = "average", k = 4, output_path) {
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

  # 繪製樹狀圖並標示分群結果
  png(output_path, width = 800, height = 600)
  plot(dend, main = paste("Dendrogram using", method, "linkage with", k, "clusters"))

  # 在樹狀圖上顯示切割結果
  rect.hclust(hc, k = k, border = "red")

  dev.off()
}

# Example usage
# Case 1: Using file_path
file_path <- "output/R_output/CSV_output/winnowing_similarity_similarity_checker_202410311316.csv"

# 範例使用，繪製相似度熱度圖
output_path <- "output/viz/heatmap/winnowing_similarity_heatmap.png"
# plot_similarity_heatmap(file_path = file_path, output_path = output_path)

# 範例使用，指定不同的 linkage method
output_path_average <- "output/viz/dendrogram/winnowing_dendrogram_average.png"
output_path_single <- "output/viz/dendrogram/winnowing_dendrogram_single.png"
output_path_complete <- "output/viz/dendrogram/winnowing_dendrogram_complete.png"

# 使用不同的連結方法繪製樹狀圖
# plot_dendrogram(file_path, method = "average", output_path = output_path_average)
# plot_dendrogram(file_path, method = "single", output_path = output_path_single)
# plot_dendrogram(file_path, method = "complete", output_path = output_path_complete)

# 範例使用，計算 Silhouette scores 並繪製最佳 k 值的圖表
output_path <- "output/viz/silhouette_scores/winnowing_silhouette_scores_plot.png"
optimal_k <- calculate_silhouette_scores(file_path, output_path, method = "average", max_k = 10)
cat("Optimal k (Silhouette scores):", optimal_k, "\n")

# 範例使用，計算 Elbow method 並繪製最佳 k 值的圖表
output_path <- "output/viz/sse_curve/winnowing_sse_elbow_plot.png"
optimal_k <- elbow_method(file_path, output_path, max_k = 10)
cat("Optimal k (elbow point):", optimal_k, "\n")

# 範例使用，繪製帶有切割結果的樹狀圖
output_path <- "output/viz/dendrogram/winnowing_dendrogram_with_cut.png"
plot_dendrogram_with_cut(file_path, method = "average", k = optimal_k, output_path = output_path)

# Case 2: Using df directly
# Assuming df is a pre-loaded data frame with similar structure
# output_path <- "output/viz/heatmap_dend_output.png"
# df = ??
# plot_similarity_heatmap(df = df, output_path = output_path)
