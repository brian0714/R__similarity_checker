source("lib/csv_reader.R")
source("lib/matrix_visualization.R")
source("lib/cluster_to_json_writer.R")
source("lib/term_document_matrix_generator.R")
# Load necessary libraries
library(writexl)
library(glue)

# Define the function to calculate cluster statistics
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

# Define a function to write cluster statistics to an Excel file
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

process_user_clusters <- function(
  json_path,
  csv_file_path="data/text_data/extracted_behavior_pattern_data.csv"
) {
    user_clusters <- read_json_as_clusters(json_path)  # 讀取 JSON
    filtered_data_list <- list()  # 用來存每個 cluster 的篩選結果

    for (i in seq_along(user_clusters)) {
        user_vector <- user_clusters[[i]]

        # 動態產生 `filter_conditions`
        filter_conditions <- list(paste0("user_id %in% c(", paste(user_vector, collapse = ", "), ")"))

        # 調用 `csv_reader()` 來篩選數據
        filtered_data <- csv_reader(
          csv_file_path,
          show_col_types = FALSE,
          filter_conditions = filter_conditions
        )

        # 存入 list（每個 cluster 一組數據）
        filtered_data_list[[i]] <- filtered_data
        # print(filtered_data)

        # 顯示進度
        # cat("完成處理 Cluster", i, "- user 數:", length(user_vector), "\n")
    }

    return(filtered_data_list)
}

# Analyze clusters from JSON file and generate statistics
analyze_clusters_from_json <- function(json_file_path, TASK_TYPE, representative_docs=NULL) {
  # Set the input Json file path (設定 JSON 檔案路徑)
  filtered_dfs <- process_user_clusters(json_path=json_file_path)
  # Execute Clustering Analysis 進行文本分析
  datetime <- format(Sys.time(), "%Y%m%d%H%M")

  # Create a large text corpus from nlp_functions.R
  large_corpus <- create_corpus(texts_vector = TASK_TYPE)
  # cat("Corpus Size:", length(corpus), "\n")
  # Generate the term-document matrix
  large_tdm_matrix <- generate_tdm(large_corpus)
  # Generate the term frequency data frame
  large_term_freq_df <- generate_term_freq_df(large_tdm_matrix)
  high_freq_terms <- large_term_freq_df$term[1:top_n]
  cat("High Frequency Terms:", high_freq_terms, "\n")

  for (i in seq_along(filtered_dfs)) {
    cluster_i <- i
    df <- filtered_dfs[[i]] # Get the data for the i-th cluster (取得第 i 個 cluster 的數據)
    final_submissions <- as.vector(df$final_submission)
    cat("Cluster", cluster_i, ":\n")

    # Create a text corpus from nlp_functions.R
    corpus <- create_corpus(texts_vector = final_submissions, POS = c("NOUN", "ADJ"))
    cat("Corpus Size:", length(corpus), "\n")
    # Generate the term-document matrix
    tdm_matrix <- generate_tdm(corpus)
    # Generate the term frequency data frame
    term_freq_df <- generate_term_freq_df(tdm_matrix)

    # Define the output directory for visualizations (定義資料夾路徑)
    dir_path_barplot <- glue("output/viz/term_count_barplot/{TASK_TYPE}/{datetime}/")
    dir_path_wordcloud <- glue("output/viz/wordcloud/{TASK_TYPE}/{datetime}/")

    # Ensure the directories exist (確保資料夾存在)
    dir.create(dir_path_barplot, recursive = TRUE, showWarnings = FALSE)
    dir.create(dir_path_wordcloud, recursive = TRUE, showWarnings = FALSE)

    # Plot a barplot of the top terms (e.g. top 20)
    # 繪製前 20 個最高頻詞 bar plot
    top_n <- 20
    output_path <- glue("{dir_path_barplot}/cluster_{cluster_i}_top_{top_n}_terms.png")
    top_terms <- plot_top_terms(term_freq_df, top_n = top_n, output_path = output_path)
    output_path <- glue("{dir_path_barplot}/cluster_{cluster_i}_filtered_top_{top_n}_terms.png")
    filtered_top_terms <- plot_top_terms(
      term_freq_df,
      top_n = top_n,
      output_path = output_path,
      remove_terms = high_freq_terms
    )

    # ✅ 若有提供代表性文件，則比對其文本是否包含 `top_terms`
    if (!is.null(representative_docs)) {
      doc <- representative_docs[[i]]  # 取得當前 Cluster 的代表性文件 ID
      cat("🔹 Representative Document for Cluster", cluster_i, ":", doc, "\n")

      # ✅ 動態篩選 `user_id` 對應的文本
      filter_conditions <- list(
        paste0("user_id == '", doc, "'")
      )

      # ✅ 使用 `csv_reader()` 取得該用戶的文本內容
      filtered_data <- csv_reader(
        show_col_types = FALSE,
        filter_conditions = filter_conditions
      )

      if (nrow(filtered_data) > 0) {
        text <- filtered_data$final_submission
        # 確保 `text` 是字符向量
        text_words <- unlist(strsplit(tolower(text), "\\W+"))

        # 比對 `top_terms` 是否出現在 `text_words`
        matched_terms <- intersect(top_terms, text_words)
        matched_filtered_terms <- intersect(filtered_top_terms, text_words)

        if (length(matched_terms) > 0) {
          cat("✅ Representative Document contains these top terms:", matched_terms, "\n")
        } else {
          cat("❌ No top terms found in Representative Document.\n")
        }
        if (length(matched_filtered_terms) > 0) {
          cat("✅ Representative Document contains these filtered top terms:", matched_filtered_terms, "\n")
        } else {
          cat("❌ No filtered top terms found in Representative Document.\n")
        }
      } else {
        cat("⚠️ No matching document found for user_id:", doc, "\n")
      }
    }

    # 繪製前 50 個最高頻詞 word cloud
    top_n <- 50
    output_path <- glue("{dir_path_wordcloud}/cluster_{cluster_i}_top_{top_n}_wordcloud.png")
    plot_wordcloud(term_freq_df, top_n = top_n, output_path = output_path)
  }
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
df <- csv_reader(file_path, filter_conditions = filter_conditions, show_col_types = FALSE)

# Load the similarity data from CSV
# similarity_file_path <- "output/R_output/CSV_output/practical_cosine_similarity_checker_202502200503.csv"
file_path <- "output/R_output/CSV_output/CREATIVE_similarity_matrices/cosine_similarity_checker_202502280632.csv"

# Compute
# 計算 Elbow method 並繪製最佳 k 值的圖表
output_path <- "output/viz/sse_curve/cosine_sse_elbow_plot.png"
# optimal_k <- elbow_method(similarity_file_path, output_path, max_k = 10)
# cat("Optimal k (elbow point):", optimal_k, "\n")

# Plot dendrogram with cut
# 繪製帶有切割結果的樹狀圖
output_path <- "output/viz/dendrogram/cosine_dendrogram_with_cut.png"
# Use optimal_k from elbow method
# clusters <- plot_dendrogram_with_cut(file_path, method = "average", k = optimal_k, output_path = output_path)
# Use fixed k value (e.g. 10)
# clusters <- plot_dendrogram_with_cut(
#   similarity_file_path,
#   task_type = TASK_TYPE,
#   method = "average",
#   k = optimal_k,
#   output_path = output_path)
# cat("Clusters:")
# print(clusters)

# 將 cluster 寫入 JSON
output_name <- paste0(TASK_TYPE, "_clusters_")
# json_file_path <- write_list_to_json(clusters, output_name=output_name)
# output_name <- paste0(TASK_TYPE, "_text_clusters_")
# replace_ids_with_submissions(json_file_path, file_path, task_type=TASK_TYPE, output_name=output_name)

# 執行函式輸出cluster_stats
# cluster_stats <- calculate_cluster_stats(df, clusters)
# 檢視 cluster_stats 結果
# cat(strrep("=", 50), "\nCluster stats output:\n")
# print(cluster_stats)

# 將 cluster_stats 寫入 Excel
# output_name <- paste0(TASK_TYPE, "_cluster_use_stats_")
# write_cluster_use_stats_to_excel(cluster_stats, output_name=output_name)
# cat("Cluster use stats written to", output_dir, "\n")

# 分析 cluster 的 JSON 檔案
# json_file_path <- "output/R_output/json_output/CREATIVE_clusters/CREATIVE_HC_cosine_clusters_202503271437.json"
# analyze_clusters_from_json(json_file_path, TASK_TYPE)
