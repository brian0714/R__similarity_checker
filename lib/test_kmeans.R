source("lib/nlp_functions.R")
source("lib/matrix_visualization.R")
source("lib/cluster_to_json_writer.R")
source("lib/cluster_stat.R")
# Load necessary libraries
library(ggplot2)
library(glue)

precompute_tfidf <- function(task_type) {
  # 建立 corpus
  result <- create_corpus(
    texts_vector = task_type,
    return_user_id = TRUE,
  )
  corpus <- result[[1]]
  user_id <- result[[2]]
#   cat("User ID:", user_id, "\n")

  # 建立 TF-IDF 矩陣
  dtm <- DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf))
  tfidf_matrix <- as.matrix(dtm)

  # 讓 `rownames(tfidf_matrix)` 儲存對應文本的 user_id
  rownames(tfidf_matrix) <- user_id

  return(tfidf_matrix)
}

# 計算 Cosine Similarity 矩陣
compute_cosine_similarity_matrix <- function(tfidf_matrix) {
  norm_matrix <- sqrt(rowSums(tfidf_matrix^2))
  sim_matrix <- (tfidf_matrix %*% t(tfidf_matrix)) / (norm_matrix %o% norm_matrix)
  diag(sim_matrix) <- 1  # 確保對角線為 1

  # **確保 rownames 繼承 user_id**
  rownames(sim_matrix) <- rownames(tfidf_matrix)
  colnames(sim_matrix) <- rownames(tfidf_matrix)

  return(sim_matrix)
}

kmeans_clustering <- function(sim_matrix, optimal_k, dim_reduce = 5) {
  sim_matrix <- read_similarity_matrix(file_path=sim_matrix)

  # 確保 `sim_matrix` 使用的是 `user_id`
  user_ids <- rownames(sim_matrix)

  # 1️⃣ 轉換相似度矩陣為距離矩陣
  distance_matrix <- 1 - sim_matrix

  # 2️⃣ 使用 MDS (降維)
  feature_matrix <- cmdscale(as.dist(distance_matrix), k = dim_reduce)

  # 3️⃣ 執行 K-Means
  set.seed(123)  # 確保每次結果相同
  kmeans_result <- kmeans(feature_matrix, centers = optimal_k, nstart = 25)

  # 4️⃣ 取得群組標籤
  clusters <- kmeans_result$cluster

  # 5️⃣ 建立群組 (確保輸出為 JSON-friendly 的 `list of lists`)
  cluster_groups <- split(user_ids, clusters)
  cluster_groups_json <- unname(lapply(cluster_groups, as.character))  # JSON-friendly

  # 6️⃣ 計算每個群組的代表性文本
  representative_docs <- list()
  for (k in 1:optimal_k) {
    # 取得該群組的樣本索引
    cluster_indices <- which(clusters == k)

    # 取得該群組的特徵點
    cluster_points <- feature_matrix[cluster_indices, , drop = FALSE]

    # 計算每個點與該群中心的距離
    center <- kmeans_result$centers[k, ]  # 取得該群的中心
    distances <- rowSums((cluster_points - center)^2)  # 計算平方距離

    # 找出距離最小的文本
    closest_index <- cluster_indices[which.min(distances)]
    representative_docs[[as.character(k)]] <- user_ids[closest_index]
    cat("Cluster", k, "representative doc:", user_ids[closest_index], "\n")
  }

  # 7️⃣ 構造 JSON-friendly 結果
  cluster_result <- list(
    cluster_groups = cluster_groups_json,
    representative_docs = representative_docs
  )

  return(cluster_result)
}

plot_kmeans_clusters <- function(sim_matrix, kmeans_result, TASK_TYPE, output_dir = "output/viz/kmeans_clusters/") {
    # 確保資料夾存在
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

    # ✅ 轉換相似度矩陣為距離矩陣
    distance_matrix <- 1 - sim_matrix

    # ✅ 使用 MDS 進行 2D 降維
    feature_matrix_2D <- cmdscale(as.dist(distance_matrix), k = 2)

    # ✅ 取得分群標籤
    clusters <- kmeans_result$cluster_groups

    # ✅ 構造繪圖 DataFrame
    plot_df <- data.frame(
        x = feature_matrix_2D[, 1],
        y = feature_matrix_2D[, 2],
        cluster = as.factor(unlist(lapply(1:length(clusters), function(k) rep(k, length(clusters[[k]])))))
    )

    # ✅ 繪製散點圖
    p <- ggplot(plot_df, aes(x = x, y = y, color = cluster)) +
        geom_point(size = 3, alpha = 0.7) +
        labs(title = "K-Means Clustering Visualization (2D MDS Projection)",
             x = "MDS Dimension 1",
             y = "MDS Dimension 2") +
        theme_minimal() +
        theme(legend.title = element_text(size = 12),
              legend.text = element_text(size = 10))

    # ✅ 儲存圖片
    datetime <- format(Sys.time(), "%Y%m%d%H%M")
    output_path <- glue("{output_dir}/{TASK_TYPE}_kmeans_clusters_{datetime}.png")
    ggsave(output_path, plot = p, width = 8, height = 6, dpi = 300)

    cat("✅ K-Means Cluster Plot saved at:", output_path, "\n")
}

# test
TASK_TYPE <- "PRACTICAL" # "PRACTICAL" or "CREATIVE"

# "consensus" or "jaccard" or "overlap" or "winnowing"
# "winnowing_by_char" or "cosine" or "levenshtein"
SIM_METHOD <- "consensus"

datetime <- format(Sys.time(), "%Y%m%d%H%M")

# 1️⃣ (if no similarity matrix) 計算 TF-IDF 矩陣
# tfidf_matrix <- precompute_tfidf(TASK_TYPE)
# print out head row of tfidf_matrix
# cat("Head of TF-IDF Matrix:\n")
# print(head(tfidf_matrix))

# 2️⃣ (if no similarity matrix) 計算 Cosine Similarity 矩陣
# sim_matrix <- compute_cosine_similarity_matrix(tfidf_matrix)
# cat("Cosine Similarity Matrix Size:", dim(sim_matrix)[1], "x", dim(sim_matrix)[2], "\n")

# 2️⃣ Load similarity matrix
# CREATIVE
# sim_matrix <- glue("output/R_output/CSV_output/CREATIVE_similarity_matrices/winnowing_similarity_checker_202502281048.csv")
# sim_matrix <- "output/R_output/CSV_output/CREATIVE_similarity_matrices/winnowing_similarity_by_char_checker_202503270642.csv"
# sim_matrix <- "output/R_output/CSV_output/CREATIVE_similarity_matrices/jaccard_similarity_checker_202502281040.csv"
# sim_matrix <- "output/R_output/CSV_output/CREATIVE_similarity_matrices/overlap_similarity_checker_202502281040.csv"

# PRACTICAL
# sim_matrix <- "output/R_output/CSV_output/PRACTICAL_similarity_matrices/cosine_similarity_checker_202502281029.csv"
# sim_matrix <- "output/R_output/CSV_output/PRACTICAL_similarity_matrices/levenshtein_similarity_checker_202502281037.csv"
# sim_matrix <- "output/R_output/CSV_output/PRACTICAL_similarity_matrices/jaccard_similarity_checker_202502281037.csv"
# sim_matrix <- "output/R_output/CSV_output/PRACTICAL_similarity_matrices/overlap_similarity_checker_202502281037.csv"
# sim_matrix <- "output/R_output/CSV_output/PRACTICAL_similarity_matrices/winnowing_similarity_checker_202502281052.csv"
# sim_matrix <- "output/R_output/CSV_output/PRACTICAL_similarity_matrices/winnowing_by_char_similarity_checker_202503271559.csv"

# PRACTICAL consensus matrix
sim_matrix <- "output/R_output/CSV_output/PRACTICAL_similarity_matrices/consensus_matrix/consensus_matrix_average.csv"

# 3️⃣ 使用 Elbow Method 確定最佳 k
output_path <- glue("output/viz/sse_curve/{TASK_TYPE}/{SIM_METHOD}_sse_elbow_plot_{datetime}.png")
optimal_k <- elbow_method(file_path=sim_matrix, output_path, max_k = 10)
cat("Optimal k (elbow point):", optimal_k, "\n")

# 4️⃣ 執行 K-Means 並輸出分群結果
kmeans_results <- kmeans_clustering(sim_matrix, optimal_k)
clusters <- kmeans_results$cluster_groups
representative_docs <- kmeans_results$representative_docs

# 5️⃣ 將 cluster 寫入 JSON
output_name <- glue("{TASK_TYPE}_{SIM_METHOD}_clusters")
# output_name <- paste0(TASK_TYPE, "_clusters_")

json_file_path <- write_list_to_json(
  clusters,
  output_dir = glue("output/R_output/json_output/{TASK_TYPE}_clusters/"),
  output_name = output_name)

# # 6️⃣ 分析 cluster 的 JSON 檔案
# json_file_path <- "output/R_output/json_output/CREATIVE_clusters_202503061509.json"
# json_file_path <- "output/R_output/json_output/PRACTICAL_clusters_202503061020.json"
analyze_clusters_from_json(
    json_file_path,
    TASK_TYPE=TASK_TYPE,
    representative_docs=representative_docs
)

# # 【Unused】 繪製並存儲 K-Means 分群結果
# plot_kmeans_clusters(sim_matrix, kmeans_results, TASK_TYPE)
