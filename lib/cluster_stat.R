source("lib/csv_reader.R")
source("lib/matrix_visualization.R")
source("lib/cluster_to_json_writer.R")
library(writexl)

# 定義函式以計算集群中的屬性統計
calculate_cluster_stats <- function(df, clusters) {
  cluster_stats <- list()

  for (i in seq_along(clusters)) {
    cluster_ids <- clusters[[i]]
    cluster_data <- df[df$user_id %in% cluster_ids, ]

    # 計算 task_type 中 "practical" 與 "creative" 的數量與比例
    task_type_count <- table(cluster_data$task_type)
    task_type_ratio <- prop.table(task_type_count)

    # 計算各個變數的 1 和 0 的數量與比例
    use_stats <- lapply(cluster_data[, c("use_ai", "use_all", "use_revise", "use_refine", "use_reject")], function(col) {
      count <- table(col)
      ratio <- prop.table(count)
      list(count = count, ratio = ratio)
    })

    # 存儲結果
    cluster_stats[[paste("Cluster", i)]] <- list(
      task_type = list(count = task_type_count, ratio = task_type_ratio),
      use_stats = use_stats
    )
  }

  return(cluster_stats)
}

# 定義函式以將集群使用統計寫入 Excel
write_cluster_use_stats_to_excel <- function(cluster_stats, output_dir="output/R_output/excel_output", output_name = "cluster_use_stats_") {
  # 初始化一個空列表來存放每個 Cluster 的數據框
  excel_sheets <- list()

  # 遍歷每個 Cluster
  for (cluster_name in names(cluster_stats)) {
    cluster_data <- cluster_stats[[cluster_name]]
    use_stats <- cluster_data$use_stats

    # 構建 use_stats 的數據框
    use_stats_df <- do.call(rbind, lapply(names(use_stats), function(stat_name) {
      stat <- use_stats[[stat_name]]
      if (length(stat$count) > 0) {
        # 如果有數據，轉換為數據框
        data.frame(
          Variable = stat_name,
          Value = names(stat$count),
          Count = as.numeric(stat$count),
          Ratio = as.numeric(stat$ratio),
          stringsAsFactors = FALSE
        )
      } else {
        # 如果沒有數據，返回一個空數據框
        data.frame(
          Variable = stat_name,
          Value = NA,
          Count = NA,
          Ratio = NA,
          stringsAsFactors = FALSE
        )
      }
    }))

    # 確保 use_stats_df 非空
    if (is.null(use_stats_df)) {
      use_stats_df <- data.frame(
        Variable = character(0),
        Value = character(0),
        Count = numeric(0),
        Ratio = numeric(0),
        stringsAsFactors = FALSE
      )
    }

    # 添加數據框到工作表
    excel_sheets[[cluster_name]] <- use_stats_df
  }

  # 寫入 Excel
  # 取得目前日期時間作為檔名的一部分（格式：YYYYMMDD%H%M）
  datetime <- format(Sys.time(), "%Y%m%d%H%M")
  excel_file_path <- paste0(output_dir, "/", output_name, datetime, ".xlsx")
  write_xlsx(excel_sheets, path = excel_file_path)
}

# Main script
# Set the input file path
file_path <- 'data/text_data/extracted_behavior_pattern_data.csv'
TASK_TYPE <- "CREATIVE" # "PRACTICAL" or "CREATIVE"

# Define multiple filter conditions as strings
filter_conditions <- list(
#   "use_ai == 0",
  paste0("task_type == '", TASK_TYPE, "'")
)

# Call the function
df <- csv_reader(file_path, filter_conditions = filter_conditions)

# Load the similarity data from CSV
# similarity_file_path <- "output/R_output/CSV_output/winnowing_similarity_similarity_checker_202411070948.csv"
# similarity_file_path <- "output/R_output/CSV_output/practical_cosine_similarity_similarity_checker_202501222332.csv"
similarity_file_path <- "output/R_output/CSV_output/creative_cosine_similarity_similarity_checker_202501230452.csv"


# 計算 Elbow method 並繪製最佳 k 值的圖表
output_path <- "output/viz/sse_curve/winnowing_sse_elbow_plot.png"
optimal_k <- elbow_method(similarity_file_path, output_path, max_k = 10)
cat("Optimal k (elbow point):", optimal_k, "\n")

# 繪製帶有切割結果的樹狀圖
output_path <- "output/viz/dendrogram/winnowing_dendrogram_with_cut.png"
# Use optimal_k from elbow method
# clusters <- plot_dendrogram_with_cut(file_path, method = "average", k = optimal_k, output_path = output_path)
# Use fixed k value (e.g. 10)
clusters <- plot_dendrogram_with_cut(similarity_file_path, task_type = TASK_TYPE, method = "average", k = 10, output_path = output_path)
cat("Clusters:")
print(clusters)

# 將 cluster 寫入 JSON
output_name <- paste0(TASK_TYPE, "_clusters_")
json_file_path <- write_list_to_json(clusters, output_name=output_name)
output_name <- paste0(TASK_TYPE, "_text_clusters_")
# replace_ids_with_submissions(json_file_path, file_path, task_type=TASK_TYPE, output_name=output_name)

# 執行函式
# cluster_stats <- calculate_cluster_stats(df, clusters)
# 檢視 cluster_stats 結果
# cat(strrep("=", 50), "\nCluster stats output:\n")
# print(cluster_stats)

# 將 cluster_stats 寫入 Excel
# output_name <- paste0(TASK_TYPE, "_cluster_use_stats_")
# write_cluster_use_stats_to_excel(cluster_stats, output_name=output_name)
# cat("Cluster use stats written to", output_dir, "\n")

