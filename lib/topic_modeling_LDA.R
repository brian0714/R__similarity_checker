# 安裝並載入必要的套件
# install.packages("tm")  # 文本處理
# install.packages("topicmodels")  # LDA模型
# install.packages("tidytext")  # 文字分析
# install.packages("ggplot2")  # 繪圖
# install.packages("dplyr")  # 數據處理
# install.packages("LDAvis")  # LDA視覺化
# install.packages("servr")  # LDAvis 互動式視覺化

source("lib/cluster_stat.R")
# Load necessary libraries
library(tm)
library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)
library(LDAvis)
library(servr)

run_lda <- function(tdm, k = 2, top_n = 10, seed = 313) {
  # 轉換 TDM 為 DTM，因為 LDA 需要 DTM 格式
  dtm <- as.DocumentTermMatrix(tdm)

  # 訓練 LDA 模型
  lda_model <- LDA(dtm, k = k, control = list(seed = seed))

  # 獲取每個主題的前幾個關鍵詞
  top_terms <- terms(lda_model, top_n)

  # 獲取每篇文檔的主題分配
  topic_assignments <- data.frame(Doc_ID = seq_len(nrow(dtm)), Topic = topics(lda_model))

  # 回傳 LDA 模型、關鍵詞與主題分配
  return(list(lda_model = lda_model, top_terms = top_terms, topic_assignments = topic_assignments))
}
# lda_results <- run_lda(tdm, k = 3)
# 查看 LDA 模型
# lda_results$lda_model
# 查看每個主題的前10個關鍵詞
# lda_results$top_terms
# 查看文檔的主題分配
# head(lda_results$topic_assignments)

# 繪製 LDA 主題分析圖
plot_lda_topics <- function(
    lda_model,
    top_n = 10,
    output_path = "output/viz/topic_modeling_LDA/lda_topics.png") {

  # 轉換為可視化的格式
  lda_topics <- tidy(lda_model, matrix = "beta")

  # 繪製前10個詞彙的分佈
  top_terms <- lda_topics %>%
    group_by(topic) %>%
    slice_max(beta, n = top_n) %>%
    ungroup() %>%
    mutate(term = reorder_within(term, beta, topic))

  # 繪製 LDA 主題分析圖
  p <- ggplot(top_terms, aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free_y") +
    coord_flip() +
    labs(title = "LDA topic modeling", x = "Term", y = "Weight") +
    scale_x_reordered()

  # 儲存圖片
  ggsave(output_path, plot = p, width = 8, height = 6, dpi = 300)

  # 回傳 top_terms
  return(top_terms)
}

# test
TASK_TYPE <- "CREATIVE" # "PRACTICAL" or "CREATIVE"
json_file_path <- "output/R_output/json_output/CREATIVE_clusters/CREATIVE_clusters_202503061509.json"
# json_file_path <- "output/R_output/json_output/PRACTICAL_clusters/PRACTICAL_clusters_202503061429.json"

filtered_dfs <- process_user_clusters(json_path=json_file_path)
cat("Filtered dataframes:", length(filtered_dfs), "\n")
datetime <- format(Sys.time(), "%Y%m%d%H%M")

data_list <- list()  # 初始化 topic words 存儲結果的列表
for (i in seq_along(filtered_dfs)) {
    cluster_i <- i
    cat("Cluster", cluster_i, ":\n")

    df <- filtered_dfs[[i]] # 取得第 i 個 cluster 的數據
    final_submissions <- as.vector(df$final_submission)

    # Create a text corpus from nlp_functions.R
    corpus <- create_corpus(texts_vector = final_submissions, POS = "ADJ")
    cat("Corpus Size:", length(corpus), "\n")

    # 直接建立 Document-Term Matrix (DTM)
    dtm <- DocumentTermMatrix(corpus, control = list(weighting = weightTf))
    # 檢查 DTM 是否為空
    if (nrow(dtm) == 0 || ncol(dtm) == 0) {
        cat("Warning: DTM is empty for Cluster", cluster_i, "\n")
        next # 跳過此迭代
    }

    # LDA 主題建模
    lda_results <- run_lda(dtm, k = 2)
    lda_model <- lda_results$lda_model

    # 定義資料夾路徑
    dir_path_lda <- glue("output/viz/topic_modeling_LDA/{TASK_TYPE}/{datetime}")
    # 確保資料夾存在
    dir.create(dir_path_lda, recursive = TRUE, showWarnings = FALSE)
    # 定義輸出路徑
    output_path <- glue("{dir_path_lda}/cluster_{cluster_i}_lda_topics.png")

    # Plot LDA topics
    top_n <- 10
    top_terms_df <- plot_lda_topics(
        lda_model,
        top_n = top_n,
        output_path = output_path)
    cat("LDA topics plot saved at:", output_path, "\n")

    # **整理 `top_terms_df` 為符合格式的列表**
    topic_terms_list <- top_terms_df %>%
        group_by(topic) %>%
        summarise(terms = list(unique(term))) %>%  # 每個 topic 轉成詞彙列表
        pull(terms)  # 轉換為 R List 結構

    # **存入 data_list**
    data_list[[cluster_i]] <- topic_terms_list
}

# 將結果寫入 JSON 檔案
json_file_path <- write_list_to_json(
  data_list = data_list,
  output_dir = glue("output/R_output/json_output/{TASK_TYPE}_topic_words/"),
  output_name = glue("LDA_topics({top_n})_{datetime}.json")
)
cat("LDA topics data saved at:", json_file_path, "\n")
