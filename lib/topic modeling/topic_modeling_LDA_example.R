library(tm)
library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)
library(LDAvis)
library(servr)

# 讀取文本數據 (這裡用內建的 'crude' 資料集做示範)
data("crude")
docs <- Corpus(VectorSource(crude))

# 文字前處理
docs <- tm_map(docs, content_transformer(tolower))  # 轉換為小寫
docs <- tm_map(docs, removePunctuation)  # 移除標點符號
docs <- tm_map(docs, removeNumbers)  # 移除數字
docs <- tm_map(docs, removeWords, stopwords("english"))  # 移除常見英語停用詞
docs <- tm_map(docs, stripWhitespace)  # 移除多餘空格

# 建立 Document-Term Matrix (DTM)
dtm <- DocumentTermMatrix(docs)

# 移除稀疏詞 (減少噪音，提高模型效率)
dtm <- removeSparseTerms(dtm, 0.99)

# 設定 LDA 模型的參數
k <- 3  # 設定主題數量
lda_model <- LDA(dtm, k = k, control = list(seed = 1234))

# 查看每個主題的前幾個關鍵詞
terms(lda_model, 10)

# 獲取每篇文檔的主題分配
topic_assignments <- data.frame(Doc_ID = seq_len(nrow(dtm)), Topic = topics(lda_model))
# 印出前幾筆結果
print(head(topic_assignments))

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
top_terms <- plot_lda_topics(lda_model)


# # LDAvis 視覺化
# lda_json <- LDAvis::createJSON(
#   phi = posterior(lda_model)$terms,
#   theta = posterior(lda_model)$topics,
#   doc.length = rowSums(as.matrix(dtm)),
#   vocab = colnames(as.matrix(dtm)),
#   term.frequency = colSums(as.matrix(dtm))
# )

# serVis(lda_json, open.browser = TRUE)
