library(dplyr)
library(glue)
library(ggplot2)
library(purrr)
library(stringr)
library(tidyr)

source("lib/cluster_to_json_writer.R")

# Helper function: calculate Jaccard index
jaccard_index <- function(set1, set2) {
  intersect_len <- length(intersect(set1, set2))
  union_len <- length(union(set1, set2))
  if (union_len == 0) return(0)
  return(intersect_len / union_len)
}

# pairwise comparison of two clustering results
compare_clusterings <- function(cluster1, cluster2, cluster_method_names = NULL, output_dir = "output/R_output/CSV_output") {
  # read json file if provided
  if (is.character(cluster1) && file.exists(cluster1)) {
    cluster1 <- read_json_as_clusters(cluster1)
  }
  if (is.character(cluster2) && file.exists(cluster2)) {
    cluster2 <- read_json_as_clusters(cluster2)
  }

  # Prepare similarity matrix
  similarity_matrix <- matrix(0, nrow = length(cluster1), ncol = length(cluster2))

  # Set default names if not provided or invalid
  if (is.null(cluster_method_names) || length(cluster_method_names) != 2) {
    rownames(similarity_matrix) <- paste0("C", seq_along(cluster1))
    colnames(similarity_matrix) <- paste0("C", seq_along(cluster2))
  } else {
    row_prefix <- cluster_method_names[1]
    col_prefix <- cluster_method_names[2]
    rownames(similarity_matrix) <- paste0(row_prefix, "_C", seq_along(cluster1))
    colnames(similarity_matrix) <- paste0(col_prefix, "_C", seq_along(cluster2))
  }

  # Fill similarity matrix with Jaccard index
  for (i in seq_along(cluster1)) {
    for (j in seq_along(cluster2)) {
      similarity_matrix[i, j] <- jaccard_index(cluster1[[i]], cluster2[[j]])
    }
  }

  # Get best match for cluster1 to cluster2
  best_match_1_to_2 <- setNames(
    colnames(similarity_matrix)[apply(similarity_matrix, 1, which.max)],
    rownames(similarity_matrix)
  )

  # Get best match for cluster2 to cluster1
  best_match_2_to_1 <- setNames(
    rownames(similarity_matrix)[apply(similarity_matrix, 2, which.max)],
    colnames(similarity_matrix)
  )

  # Save matrix as CSV
  if (is.null(cluster_method_names) || length(cluster_method_names) != 2) {
    output_path <- glue("{output_dir}/C1_C2_compared_similarity_matrix.csv")
  } else {
    output_path <- glue("{output_dir}/{row_prefix}_{col_prefix}_compared_similarity_matrix.csv")
  }
  write.csv(similarity_matrix, file = output_path, row.names = TRUE)

  # Return as list
  return(list(
    similarity_matrix = similarity_matrix,
    best_match_from_1_to_2 = best_match_1_to_2,
    best_match_from_2_to_1 = best_match_2_to_1
  ))
}

# test pairwise comparing
# TASK_TYPE <- "PRACTICAL"

# result <- compare_clusterings(
#   cluster1 = "output/R_output/json_output/PRACTICAL_clusters/PRACTICAL_cosine_clusters_202504101352.json",
#   cluster2 = "output/R_output/json_output/PRACTICAL_clusters/PRACTICAL_jaccard_clusters_202504101418.json",
#   cluster_method_names = c("cosine", "jaccard"),
#   output_dir = glue("output/R_output/CSV_output/{TASK_TYPE}_cluster_compare_result")
# )

# # Check result
# # 查看回傳的結果
# print(result$similarity_matrix)
# print(result$best_match_from_1_to_2)
# print(result$best_match_from_2_to_1)

# Load JSON files as clusters
# 讀取多個 cluster 結果的 JSON 檔案並轉換為 clusterings
load_all_clusterings <- function(cluster_paths) {
  clusterings <- list()

  for (method in names(cluster_paths)) {
    path <- cluster_paths[[method]]
    # print(path)
    clusterings[[method]] <- read_json_as_clusters(path)
  }

  return(clusterings)
}

# Get the maximum cluster count for each clustering method
# 取得所有 clustering 的最大群數 (計算每個 clustering 方法的最大群數，並返回一個列表)
get_true_max_cluster_count <- function(clusterings) {
  method_cluster_counts <- purrr::map_dfr(clusterings, ~tibble(cluster_count = length(.x)), .id = "method")

  overall_max <- max(method_cluster_counts$cluster_count)

  return(list(
    max_by_method = method_cluster_counts,
    overall_max = overall_max
  ))
}

# List out all cluster alignments with Jaccard index filtering (by threshold)
# 多個 clusterings 全列出對應 + threshold 過濾
generate_all_cluster_alignments <- function(cluster_paths = NULL,
                                            clusterings = NULL,
                                            threshold = 0.3,
                                            output_dir = NULL) {
  # Load all clustering results
  # 載入所有 clustering 結果
  if (is.null(clusterings)) {
    if (is.null(cluster_paths)) {
      stop("Please provide either cluster_paths or clusterings.")
    } else {
      clusterings <- load_all_clusterings(cluster_paths)
    }
  } else {
    if (!is.null(cluster_paths)) {
      warning("Both cluster_paths and clusterings provided. Using clusterings.")
    }
  }
  method_names <- names(clusterings)
  all_matches <- list()

  for (i in seq_along(clusterings)) {
    method_i <- method_names[i]
    clusters_i <- clusterings[[i]]

    for (k in seq_along(clusters_i)) {
      source_cluster <- clusters_i[[k]]
      source_label <- paste0(method_i, "_C", k)

      for (j in seq_along(clusterings)) {
        if (j == i) next  # will not compare with itself (不與自身比對)

        method_j <- method_names[j]
        clusters_j <- clusterings[[j]]

        for (m in seq_along(clusters_j)) {
          target_cluster <- clusters_j[[m]]
          target_label <- paste0(method_j, "_C", m)

          score <- round(jaccard_index(source_cluster, target_cluster), 4)

          if (score >= threshold) {
            all_matches[[length(all_matches) + 1]] <- data.frame(
              Source_Cluster = source_label,
              Target_Method = method_j,
              Target_Cluster = paste0("C", m),
              Target_Full = target_label,
              Jaccard_Score = score
            )
          }
        }
      }
    }
  }

  result_df <- bind_rows(all_matches)

  # Write the result to a CSV file
  # 將結果寫入 CSV 檔案
  if (!is.null(output_dir)) {
    datetime <- format(Sys.time(), "%Y%m%d%H%M")
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    output_path <- glue("{output_dir}/cluster_alignment_full_{datetime}.csv")
    write.csv(result_df, file = output_path, row.names = FALSE)
  }

  return(result_df)
}

# Filter stable clusters based on Jaccard index threshold
# 篩選出 Jaccard_Score >= threshold 的穩定 cluster
get_stable_clusters <- function(alignment_df, threshold = 0.5) {
  stable_clusters <- alignment_df %>%
    filter(Jaccard_Score >= threshold) %>%

    # Compute the number of methods for each Source cluster
    # 計算每個 Source cluster 對應幾個獨立方法
    group_by(Source_Cluster) %>%
    summarise(
      n_methods = n_distinct(Target_Method),
      .groups = "drop"
    ) %>%

    # Extract method name and cluster number for sorting
    # 解析方法名稱與群號（用於排序）
    mutate(
      method = str_extract(Source_Cluster, "^[^_]+"),
      cluster_num = as.integer(str_extract(Source_Cluster, "(?<=_C)\\d+"))
    ) %>%

    arrange(method, cluster_num) %>%
    select(Source_Cluster, n_methods)

  return(stable_clusters)
}

# Plot Jaccard similarity distribution
# 畫 Jaccard 相似度分佈圖
plot_jaccard_distribution <- function(alignment_df, threshold = 0.3, save_path = "jaccard_distribution.png") {
  p <- ggplot(alignment_df, aes(x = Jaccard_Score)) +
    geom_histogram(binwidth = 0.05, fill = "#69b3a2", color = "white", boundary = 0) +
    geom_vline(xintercept = threshold, color = "red", linetype = "dashed", linewidth = 1) +
    labs(
      title = "Jaccard Similarity Score Distribution",
      subtitle = paste("Dashed red line = threshold at", threshold),
      x = "Jaccard Similarity",
      y = "Frequency"
    ) +
    theme_minimal()

  # 儲存圖表
  ggsave(filename = save_path, plot = p, width = 8, height = 5, dpi = 300)
  return(p)
}

# Reshape stable clusters into a matrix format
# 重新整理穩定的 cluster 結果
reshape_stable_matrix <- function(clusterings, stable_clusters_df = NULL, output_path = NULL) {
  if (is.null(stable_clusters_df)) {
    # 執行並指定 threshold
    result_df <- generate_all_cluster_alignments(
      clusterings = clusterings,
      threshold = 0,
    )

    # 篩選出 Jaccard_Score >= 0.3 的穩定 cluster
    stable_clusters_df <- get_stable_clusters(result_df, threshold = 0.3)
  }

  # 自動取得 max 群數
  max_info <- get_true_max_cluster_count(clusterings)
  max_number_of_clusters <- max_info$overall_max

  reshaped_df <- stable_clusters_df %>%
    # 解析 method 與 cluster number
    mutate(
      method = stringr::str_extract(Source_Cluster, "^.+(?=_C\\d+)"),
      cluster_num = paste0("C", as.integer(stringr::str_extract(Source_Cluster, "(?<=_C)\\d+")))
    ) %>%

    # 建立對應表（cluster_num × method）
    select(cluster_num, method, n_methods) %>%
    complete(
      cluster_num = paste0("C", 1:max_number_of_clusters),
      method,
      fill = list(n_methods = 0)
    ) %>%

    pivot_wider(
      names_from = method,
      values_from = n_methods,
      values_fn = max,     # 合併重複 (cluster_num, method)
      values_fill = 0      # 補 0
    ) %>%

    arrange(cluster_num)

  if (!is.null(output_path)) {
    readr::write_csv(reshaped_df, output_path)
  }

  return(reshaped_df)
}


# Test generate_all_cluster_alignments
TASK_TYPE <- "CREATIVE" # "PRACTICAL" or "CREATIVE"
datetime <- format(Sys.time(), "%Y%m%d%H%M")

# Creative Clusters
# cluster_paths <- list(
#   cosine = "output/R_output/json_output/CREATIVE_clusters/CREATIVE_cosine_clusters_202503061509.json",
#   levenshtein = "output/R_output/json_output/CREATIVE_clusters/CREATIVE_levenshtein_clusters_202503270411.json",
#   winnowing = "output/R_output/json_output/CREATIVE_clusters/CREATIVE_winnowing_clusters_202503270406.json",
#   winnowing_by_char = "output/R_output/json_output/CREATIVE_clusters/CREATIVE_winnowing_by_char_clusters_202503270645.json",
#   jaccard = "output/R_output/json_output/CREATIVE_clusters/CREATIVE_jaccard_clusters_202504101238.json",
#   overlap = "output/R_output/json_output/CREATIVE_clusters/CREATIVE_overlap_clusters_202504101241.json"
# )

# Practical Clusters
# cluster_paths <- list(
#   cosine = "output/R_output/json_output/PRACTICAL_clusters/PRACTICAL_cosine_clusters_202504101352.json",
#   levenshtein = "output/R_output/json_output/PRACTICAL_clusters/PRACTICAL_levenshtein_clusters_202504101404.json",
#   jaccard = "output/R_output/json_output/PRACTICAL_clusters/PRACTICAL_jaccard_clusters_202504101418.json",
#   overlap = "output/R_output/json_output/PRACTICAL_clusters/PRACTICAL_overlap_clusters_202504171140.json",
#   winnowing = "output/R_output/json_output/PRACTICAL_clusters/PRACTICAL_winnowing_clusters_202504171140.json",
#   winnowing_by_char = "output/R_output/json_output/PRACTICAL_clusters/PRACTICAL_winnowing_by_char_clusters_202504171140.json"
# )

# Creative HC Clusters
cluster_paths <- list(
  cosine = "output/R_output/json_output/CREATIVE_clusters/HC/CREATIVE_HC_cosine_clusters.json",
  levenshtein = "output/R_output/json_output/CREATIVE_clusters/HC/CREATIVE_HC_levenshtein_clusters.json",
  jaccard = "output/R_output/json_output/CREATIVE_clusters/HC/CREATIVE_HC_jaccard_clusters.json",
  overlap = "output/R_output/json_output/CREATIVE_clusters/HC/CREATIVE_HC_overlap_clusters.json",
  winnowing = "output/R_output/json_output/CREATIVE_clusters/HC/CREATIVE_HC_winnowing_clusters.json",
  winnowing_by_char = "output/R_output/json_output/CREATIVE_clusters/HC/CREATIVE_HC_winnowing_by_char_clusters.json"
)


# Practical HC Clusters
# cluster_paths <- list(
#   cosine = "output/R_output/json_output/PRACTICAL_clusters/HC/PRACTICAL_HC_cosine_clusters.json",
#   levenshtein = "output/R_output/json_output/PRACTICAL_clusters/HC/PRACTICAL_HC_levenshtein_clusters.json",
#   jaccard = "output/R_output/json_output/PRACTICAL_clusters/HC/PRACTICAL_HC_jaccard_clusters.json",
#   overlap = "output/R_output/json_output/PRACTICAL_clusters/HC/PRACTICAL_HC_overlap_clusters.json",
#   winnowing = "output/R_output/json_output/PRACTICAL_clusters/HC/PRACTICAL_HC_winnowing_clusters.json",
#   winnowing_by_char = "output/R_output/json_output/PRACTICAL_clusters/HC/PRACTICAL_HC_winnowing_by_char_clusters.json"
# )

# Set up a threshold and execute the alignment generation
# 執行並指定 threshold
result_df <- generate_all_cluster_alignments(
  cluster_paths = cluster_paths,
  threshold = 0,
  output_dir = glue("output/R_output/CSV_output/{TASK_TYPE}_cluster_compare_result")
)

# Check result
# 查看結果
print(result_df)

# Filter out stable clusters where Jaccard_Score >= 0.5
# 篩選出 Jaccard_Score >= 0.5 的穩定 cluster
stable_clusters <- get_stable_clusters(result_df, threshold = 0.3)
print(stable_clusters, n=Inf)

# Plot 畫圖
# plot_jaccard_distribution(
#   result_df,
#   threshold = 0.2,
#   save_path = glue("output/viz/jaccard_distribution_plot/{TASK_TYPE}_jaccard_distribution_plot_{datetime}.png")
# )

# Reshape stable clusters into a dataframe format
# 重新整理穩定的 cluster 結果
clusterings <- load_all_clusterings(cluster_paths)
reshaped_stable_df <- reshape_stable_matrix(
  clusterings,
  output_path = glue("output/R_output/CSV_output/{TASK_TYPE}_cluster_compare_result/stable_cluster_matrix/stable_cluster_matrix_{datetime}.csv")
)
