library(jsonlite)
library(glue)

# Function to write a list to a JSON file
write_list_to_json <- function(data_list,
                            output_dir="output/R_output/json_output",
                            pretty = TRUE, # Whether to format the JSON output for readability
                            output_name = "clusters_") {
  datetime <- format(Sys.time(), "%Y%m%d%H%M")
  # 確保資料夾存在
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # 定義 JSON 檔案路徑
  if (!grepl("\\.json$", output_name)) {  # 檢查 output_name 是否已經有 .json 結尾
    output_name <- glue("{output_name}{datetime}.json")
    json_file_path <- file.path(output_dir, paste0(output_name, datetime, ".json"))
  } else {
  json_file_path <- file.path(output_dir, output_name)
  }

  # Write the list to a JSON file
  write_json(data_list, json_file_path, pretty = pretty)
  # Print confirmation message
  cat("JSON file written to:", json_file_path, "\n")

  return(json_file_path)
}

# Function to load JSON and CSV, and replace IDs with final_submission
replace_ids_with_submissions <- function(json_path, csv_path, task_type, output_dir="output/R_output/json_output", output_name="text_clusters_") {
  # Step 1: Load the JSON
  clusters <- fromJSON(json_path)
#   cat("Loaded JSON structure:\n")
#   print(clusters)

  # Step 2: Load the CSV
  csv_data <- read.csv(csv_path, stringsAsFactors = FALSE)
#   cat("Loaded CSV structure:\n")
#   print(head(csv_data))

  # Step 3: Replace IDs in JSON with final_submission
  clusters_replaced <- lapply(clusters, function(cluster) {
    sapply(cluster, function(user_id) {
      # Find the corresponding row in CSV
      submission <- csv_data$final_submission[csv_data$user_id == user_id & csv_data$task_type == task_type]
      if (length(submission) > 0) {
        return(submission) # Replace with final_submission
      } else {
        return(NA) # If user_id not found
      }
    })
  })

  # Step 4: Save the modified JSON
  datetime <- format(Sys.time(), "%Y%m%d%H%M")
  json_file_path <- paste0(output_dir, "/", output_name, datetime, ".json")
  write_json(clusters_replaced, json_file_path, pretty = TRUE)
  cat("Replaced JSON written to:", json_file_path, "\n")
}

# 取 JSON 並轉換為 R list，每個元素是一個 user_id cluster
read_json_as_clusters <- function(json_path) {
    json_data <- fromJSON(json_path)
    cluster_list <- lapply(json_data, as.character)  # 確保是字符型向量
    return(cluster_list)
}