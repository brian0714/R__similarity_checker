# Load necessary library
library(readr)

# Define the csv_writer function
csv_writer <- function(users, similarities) {
  # 定義輸出資料夾路徑
  output_dir <- "output/R_output/CSV_output"

  # 檢查並建立輸出資料夾
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # 取得目前日期時間作為檔名的一部分（格式：YYYYMMDD%H%M）
  datetime <- format(Sys.time(), "%Y%m%d%H%M")

  # 初始化列表來儲存每個相似度矩陣的 data frame
  dfs <- list()
  # 逐一處理每個相似度矩陣，並將其存為獨立的 CSV 檔案
  for (similarity_name in names(similarities)) {
    matrix <- similarities[[similarity_name]]

    # Print the length of users and the number of columns in matrix
    # cat("Length of users:", length(users), "\n")
    # cat("Number of columns in matrix:", ncol(matrix), "\n")

    # 檢查矩陣是否有效（不為 NULL 或空的）
    if (is.null(matrix) || length(matrix) == 0) {
      cat("Warning: Similarity matrix for", similarity_name, "is empty or null.\n")
      next  # 跳過該相似度矩陣
    }

    # Convert matrix to data frame and set row and column names
    df <- as.data.frame(matrix)
    colnames(df) <- users
    df <- cbind(user_id = users, df)  # Add user_id as the first column
    # str(df)

    # 印出前 5 行作為除錯訊息
    # cat("Preview of", similarity_name, "matrix:\n")
    # print(head(df, 5))

    # 定義 CSV 檔案路徑
    csv_file_path <- paste0(output_dir, "/", similarity_name, "_similarity_checker_", datetime, ".csv")

    # 將資料框寫入 CSV 檔案
    write.csv(df, file = csv_file_path, row.names = FALSE)

    # 成功訊息
    cat("CSV file - '", similarity_name, "_similarity_checker_", datetime, ".csv' has been created.\n")

    # 將當前 df 存入 dfs 列表中
    dfs[[similarity_name]] <- df
  }
  return(dfs)
}

# Test
# Given 3 users
users <- c("user_1", "user_2", "user_3")

# Given different similarity matrix
similarities <- list(
  "cosine_similarity" = matrix(c(NA, 0.75, 0.10,
                                 0.75, NA, 0.50,
                                 0.10, 0.50, NA),
                               nrow = 3, byrow = TRUE),
  "euclidean_similarity" = matrix(c(NA, 0.80, 0.65,
                                    0.80, NA, 0.55,
                                    0.65, 0.55, NA),
                                  nrow = 3, byrow = TRUE)
)

# csv_writer(users, similarities)