# Load required libraries
source("lib/csv_reader.R")
source("lib/nlp_functions.R")
library(tm)
library(SnowballC)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(pheatmap)

generate_tdm <- function(corpus) {
  # Create a term-document matrix
  tdm <- TermDocumentMatrix(corpus)
  tdm_matrix <- as.matrix(tdm)
  # colnames(tdm_matrix) <- df$user_id
  cat("TDM Matrix Size:", dim(tdm_matrix)[1], "x", dim(tdm_matrix)[2], "\n")
  # print(tdm_matrix)

  # Save the term-document matrix to a CSV file
  output_dir <- "output/R_output/CSV_output/tdm"
  datetime <- format(Sys.time(), "%Y%m%d%H%M")
  csv_file_path <- paste0(output_dir, "/", TASK_TYPE, "_tdm_", datetime, ".csv")
  # write.csv(tdm_matrix, file = csv_file_path, row.names = TRUE, fileEncoding = "UTF-8")

  return(tdm_matrix)
}

generate_term_freq_df <- function(tdm_matrix) {
  # EDA: Organize the most frequent terms through the whole corpus
  term_freq <- sort(rowSums(tdm_matrix), decreasing = TRUE)
  term_freq_df <- data.frame(term = names(term_freq), freq = term_freq)

  return(term_freq_df)
}

# Example: Set the input file path
file_path <- 'data/text_data/extracted_behavior_pattern_data.csv'
TASK_TYPE <- "PRACTICAL" # "PRACTICAL" or "CREATIVE"
datetime <- format(Sys.time(), "%Y%m%d%H%M")

## Create a text corpus from nlp_functions.R
# corpus <- create_corpus(texts_vector = TASK_TYPE)
# cat("Corpus Size:", length(corpus), "\n")
## Generate the term-document matrix
# tdm_matrix <- generate_tdm(corpus)
## Generate the term frequency data frame
# term_freq_df <- generate_term_freq_df(tdm_matrix)

# EDA: Plot a barplot of the top terms
plot_top_terms <- function(term_freq_df, top_n = 10, output_path = "output/viz/term_count_barplot/top_10_terms_plot.png") {
    # 確保 top_n 不超過可用的詞數
    top_n <- min(top_n, nrow(term_freq_df))

    # 選取前 top_n 個高頻詞
    top_terms <- term_freq_df$term[1:top_n]

    # 繪製前 top_n 個高頻詞的長條圖
    p <- ggplot(term_freq_df[1:top_n,], aes(x = reorder(term, -freq), y = freq)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        labs(title = paste("Top", top_n, "Most Frequent Terms"),
             x = "Terms", y = "Frequency") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))  # 避免文字重疊

    # 匯出圖檔
    ggsave(output_path, plot = p, width = 8, height = 6, dpi = 300)

    # 回傳 top_n 文字 vector
    return(top_terms)
}

# 使用範例：只繪製前 10 個最高頻詞
top_n <- 10
output_path <- paste0("output/viz/term_count_barplot/", TASK_TYPE, "_top_", top_n, "_terms_", datetime, ".png")
# plot_top_terms(term_freq_df, top_n = top_n, output_path = output_path)

# EDA: Plot a word cloud
plot_wordcloud <- function(term_freq_df, top_n = NULL, output_path = "output/viz/wordcloud/wordcloud.png") {
    # 篩選前 top_n 個詞（如果提供 top_n）
    if (!is.null(top_n)) {
        top_n <- min(top_n, nrow(term_freq_df))  # 確保 top_n 不超過可用行數
        term_freq_df <- term_freq_df[1:top_n, ]  # 只取前 top_n 個高頻詞
    }

    # 開啟圖形裝置，避免 WordCloud 顯示問題
    png(output_path, width = 800, height = 600)

    # 繪製詞雲
    wordcloud(words = term_freq_df$term, freq = term_freq_df$freq, min.freq = 1,
              colors = brewer.pal(8, "Dark2"), scale = c(3, 0.5))

    dev.off()  # 關閉圖形裝置
    message("Word cloud saved to: ", output_path)
}
# 使用範例：只繪製前 50 個最高頻詞
top_n <- 50
output_path <- paste0("output/viz/wordcloud/", TASK_TYPE, "_top_", top_n,"_wordcloud_", datetime, ".png")
# plot_wordcloud(term_freq_df, top_n = top_n, output_path = output_path)

# EDA: Plot a heatmap of the term-document matrix
plot_tdm_heatmap <- function(tdm_matrix, output_path = "output/viz/heatmap/tdm/tdm_heatmap.png",
                             doc_range = NULL, min_freq = NULL) {
    # 過濾低頻詞（min_freq）
    if (!is.null(min_freq)) {
        term_sums <- rowSums(tdm_matrix)  # 計算每個詞的總頻率
        tdm_matrix <- tdm_matrix[term_sums >= min_freq, ]  # 只保留頻率 ≥ min_freq 的詞
    }

    # 限制文件數量（doc_range）
    if (!is.null(doc_range)) {
        start_doc <- max(1, doc_range[1])  # 確保範圍不小於 1
        end_doc <- min(ncol(tdm_matrix), doc_range[2])  # 確保不超過總文件數
        tdm_matrix <- tdm_matrix[, start_doc:end_doc]  # 取指定範圍的文件
    }

    # 確保 TDM 仍然有詞和文件（避免空矩陣錯誤）
    if (nrow(tdm_matrix) == 0 || ncol(tdm_matrix) == 0) {
        message("Warning: No data left after filtering. Heatmap not generated.")
        return()
    }

    # 開啟圖形裝置
    png(output_path, width = 1600, height = 1200)

    # 繪製熱圖
    pheatmap(tdm_matrix, cluster_rows = TRUE, cluster_cols = TRUE,
             main = "Heatmap of Term-Document Matrix")

    dev.off()  # 關閉圖形裝置
    message("Heatmap saved to: ", output_path)
}

# 使用範例：
# output_path <- paste0("output/viz/heatmap/tdm/", TASK_TYPE, "_TDM_heatmap_", datetime, ".png")
# output_path <- paste0("output/viz/heatmap/tdm/", TASK_TYPE, "_TDM_heatmap_r1.png")
# plot_tdm_heatmap(tdm_matrix,
#                 doc_range = c(1, 50),
#                 min_freq = 50,
#                 output_path = output_path)
# output_path <- paste0("output/viz/heatmap/tdm/", TASK_TYPE, "_TDM_heatmap_r2.png")
# plot_tdm_heatmap(tdm_matrix,
#                 doc_range = c(51, 100),
#                 min_freq = 50,
#                 output_path = output_path)
# output_path <- paste0("output/viz/heatmap/tdm/", TASK_TYPE, "_TDM_heatmap_r3.png")
# plot_tdm_heatmap(tdm_matrix,
#                 doc_range = c(101, 155),
#                 min_freq = 50,
#                 output_path = output_path)
