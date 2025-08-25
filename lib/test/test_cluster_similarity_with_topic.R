# Source external R scripts
source("lib/cluster_stat.R")
source("lib/similarity_methods.R")

# 讀取 cluster 相似度
analyze_cluster_similarity <- function(topic, TASK_TYPE, json_file_path) {
    # 讀取 cluster 數據
    filtered_dfs <- process_user_clusters(json_path = json_file_path)
    # cat("Filtered dataframes:", length(filtered_dfs), "\n")

    # 記錄時間戳記
    datetime <- format(Sys.time(), "%Y%m%d%H%M")

    # 初始化 topic words 存儲結果的列表
    data_list <- list()

    # 遍歷所有 clusters
    for (i in seq_along(filtered_dfs)) {
        cluster_i <- i
        cat("\nCluster", cluster_i, ":\n")

        df <- filtered_dfs[[i]] # 取得第 i 個 cluster 的數據

        # 檢查是否有 "final_submission" 欄位
        if (!"final_submission" %in% colnames(df)) {
            cat("Warning: No 'final_submission' column in Cluster", cluster_i, "\n")
            next
        }

        # 取得 final_submission 內容
        final_submissions <- df$final_submission

        # 初始化相似度存儲
        similarities <- numeric(length(final_submissions))

        # 計算每篇 submission 與 topic 的 cosine similarity
        for (j in seq_along(final_submissions)) {
            submission <- final_submissions[j]

            # 檢查是否為 NA 或 空值
            if (is.na(submission) || submission == "") {
                similarities[j] <- NA
            } else {
                # similarities[j] <- overlap_coefficient(topic, submission)
                similarities[j] <- winnowing(topic, submission, k = 3, w = 2)
                # similarities[j] <- cosine_similarity(topic, submission, tokenize_method="word")
                # similarities[j] <- normalized_levenshtein_distance(topic, submission, tokenized=TRUE)
            }
        }

        # 計算該 cluster 的平均相似度（忽略 NA 值）
        avg_similarity <- mean(similarities, na.rm = TRUE)
        cat("Average similarity for Cluster", cluster_i, ":", avg_similarity, "\n")

        # 儲存計算結果
        data_list[[as.character(cluster_i)]] <- list(
            cluster = cluster_i,
            avg_similarity = avg_similarity,
            similarities = similarities
        )
    }

    # 回傳計算結果
    return(data_list)
}


# 設定參數
TASK_TYPE <- "CREATIVE" # "PRACTICAL" 或 "CREATIVE"
json_file_path <- "output/R_output/json_output/CREATIVE_clusters/CREATIVE_clusters_202503061509.json"

# 取得 topics
# topic 1
# topics <- list(
#   "Cluster_1" = "Efficient and convenient biometric systems",
#   "Cluster_2" = "Futuristic, smart, and seamless travel experience",
#   "Cluster_3" = "Personalized and sustainable airport innovations",
#   "Cluster_4" = "Comfortable and accessible futuristic facilities",
#   "Cluster_5" = "Immersive and interactive digital environments",
#   "Cluster_6" = "Smooth and structured boarding process",
#   "Cluster_7" = "Premium comfort and cultural inclusivity",
#   "Cluster_8" = "Large-scale, high-tech futuristic design"
# )

# topic 2
# topics <- list(
#   "Cluster_1" = "Seamless and efficient biometric travel",
#   "Cluster_2" = "Futuristic and holographic enhancements",
#   "Cluster_3" = "Safety and human-like AI integration",
#   "Cluster_4" = "Fast and robotic-driven experience",
#   "Cluster_5" = "Advanced biometric and holographic technology",
#   "Cluster_6" = "Step-by-step guided travel process",
#   "Cluster_7" = "Diverse and modern travel options",
#   "Cluster_8" = "Personalized and immersive experiences"
# )

# topic 1 + topic 2
topics <- list(
  "Cluster_1" = "Seamless and efficient airport processes with biometric and automation technologies.",
  "Cluster_2" = "Futuristic, smart, and holographic elements enhancing the travel experience.",
  "Cluster_3" = "Personalized and advanced technology-driven experiences with a human touch.",
  "Cluster_4" = "High-tech, comfortable, and fast travel facilitated by futuristic automation.",
  "Cluster_5" = "Immersive, interactive, and visually stunning airport environments with biometric security.",
  "Cluster_6" = "Step-by-step procedural guidance ensuring smooth check-in and boarding.",
  "Cluster_7" = "Comfortable, culturally diverse, and futuristic airport amenities for travelers.",
  "Cluster_8" = "Innovative, large-scale, and diverse futuristic airport facilities with biometric integration."
)



# 遍歷 topics 並分析每個 cluster 的相似度
for (name in names(topics)) {
    topic <- topics[[name]]
    cat("\nProcessing:", name, "\n")

    cluster_results <- analyze_cluster_similarity(topic, TASK_TYPE, json_file_path)

    # 檢視回傳的數據
    # print(cluster_results)
}
