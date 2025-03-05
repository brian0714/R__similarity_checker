source("lib/nlp_functions.R")
source("lib/matrix_visualization.R")
source("lib/cluster_to_json_writer.R")
source("lib/cluster_stat.R")

precompute_tfidf <- function(task_type) {
  # 建立 corpus
  result <- create_corpus(
    texts_vector = task_type,
    return_user_id = TRUE,
  )
  corpus <- result[[1]]
  user_id <- result[[2]]

  # 建立 TF-IDF 矩陣
  dtm <- DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf))
  tfidf_matrix <- as.matrix(dtm)

  # 讓 `rownames(tfidf_matrix)` 儲存對應文本
  rownames(tfidf_matrix) <- seq_along(user_id)

  return(tfidf_matrix)
}

# 計算 Cosine Similarity 矩陣
compute_cosine_similarity_matrix <- function(tfidf_matrix) {
  norm_matrix <- sqrt(rowSums(tfidf_matrix^2))
  sim_matrix <- (tfidf_matrix %*% t(tfidf_matrix)) / (norm_matrix %o% norm_matrix)
  diag(sim_matrix) <- 1  # 確保對角線為 1
  return(sim_matrix)
}

kmeans_clustering <- function(sim_matrix, optimal_k, dim_reduce = 5) {
  # 1️⃣ 轉換相似度矩陣為距離矩陣
  distance_matrix <- 1 - sim_matrix

  # 2️⃣ 使用 MDS (降維)
  feature_matrix <- cmdscale(as.dist(distance_matrix), k = dim_reduce)

  # 3️⃣ 執行 K-Means
  kmeans_result <- kmeans(feature_matrix, centers = optimal_k, nstart = 25)

  # 4️⃣ 取得群組標籤
  clusters <- kmeans_result$cluster

  # 5️⃣ 建立群組 (確保輸出為 JSON-friendly 的 `list of lists`)
  cluster_groups <- split(rownames(sim_matrix), clusters)
  # 移除 cluster 的 index 避免變成 key
  cluster_groups_json <- unname(lapply(cluster_groups, as.character))

  # 6️⃣ 計算每個群組的代表性文本
  representative_docs <- list()

  for (k in 1:optimal_k) {
    # 取得該群組的樣本索引
    cluster_indices <- which(clusters == k)

    # 取得該群組的特徵點
    cluster_points <- feature_matrix[cluster_indices, , drop = FALSE]

    # 計算每個點與該群中心的歐幾里得距離
    center <- kmeans_result$centers[k, ]  # 取得該群的中心
    distances <- rowSums((cluster_points - center)^2)  # 計算平方距離

    # 找出距離最小的文本
    closest_index <- cluster_indices[which.min(distances)]
    representative_docs[[as.character(k)]] <- rownames(sim_matrix)[closest_index]  # 確保 JSON Key 為字串
  }

  # 7️⃣ 構造 JSON-friendly 結果
  cluster_result <- list(
    cluster_groups = cluster_groups_json,  # ✅ JSON-friendly List of Lists
    representative_docs = representative_docs
  )

  return(cluster_result)  # ✅ 直接可寫入 JSON
}


# test
TASK_TYPE <- "PRACTICAL" # "PRACTICAL" or "CREATIVE"

# 1️⃣ 計算 TF-IDF 矩陣
tfidf_matrix <- precompute_tfidf(TASK_TYPE)
# print out head row of tfidf_matrix
# cat("Head of TF-IDF Matrix:\n")
# print(head(tfidf_matrix))

# 2️⃣ 計算 Cosine Similarity 矩陣
sim_matrix <- compute_cosine_similarity_matrix(tfidf_matrix)
cat("Cosine Similarity Matrix Size:", dim(sim_matrix)[1], "x", dim(sim_matrix)[2], "\n")

# 3️⃣ 使用 Elbow Method 確定最佳 k
output_path <- "output/viz/sse_curve/cosine_sse_elbow_plot.png"
optimal_k <- elbow_method(file_path=sim_matrix, output_path, max_k = 10)
cat("Optimal k (elbow point):", optimal_k, "\n")

# 4️⃣ 執行 K-Means 並輸出分群結果
kmeans_results <- kmeans_clustering(sim_matrix, optimal_k)
clusters <- kmeans_results$cluster_groups

# 5️⃣ 將 cluster 寫入 JSON
output_name <- paste0(TASK_TYPE, "_clusters_")
# json_file_path <- write_list_to_json(clusters, output_name=output_name)

# 6️⃣ 分析 cluster 的 JSON 檔案
json_file_path <- "output/R_output/json_output/PRACTICAL_clusters_202503060607.json"
analyze_clusters_from_json(json_file_path, TASK_TYPE=TASK_TYPE)
