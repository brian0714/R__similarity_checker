library(tibble)
library(glue)
library(fs)

build_raw_doc_table <- function(input_dir, output_csv = NULL) {
  rmd_files <- list.files(path = input_dir, pattern = "\\.(R|r)md$", full.names = TRUE)

  # 回傳 list，若讀不到就給 NA 並記錄
  read_raw_text <- function(file_path) {
    tryCatch({
      paste(readLines(file_path, warn = FALSE), collapse = "\n")
    }, error = function(e) {
      warning(glue("❌ Failed to read: {file_path}"))
      return(NA)
    })
  }

  raw_texts <- sapply(rmd_files, read_raw_text)

  doc_table <- tibble(
    doc_id = seq_along(rmd_files),
    filename = basename(rmd_files),
    text = raw_texts
  )

  # 基本統計
  message("📦 Document count: ", nrow(doc_table))

  failed_rows <- which(is.na(doc_table$text) | doc_table$text == "")
  if (length(failed_rows) > 0) {
    warning("⚠️ Found ", length(failed_rows), " documents with empty or NA content:")
    print(doc_table[failed_rows, ])
  } else {
    message("✅ All documents loaded successfully.")
  }

  # 處理儲存路徑
  dir_name <- basename(normalizePath(input_dir))

  if (is.null(output_csv)) {
    output_dir <- glue("output/R_output/CSV_output/rmd_doc_table/{dir_name}")
    output_csv <- glue("{output_dir}/raw_doc_table.csv")
  } else {
    output_dir <- dirname(output_csv)
  }

  if (!dir_exists(output_dir)) {
    dir_create(output_dir, recursive = TRUE)
  }

  write.csv(doc_table, output_csv, row.names = FALSE)
  message("📄 Raw document table saved to: ", output_csv)

  return (output_csv)
}

load_doc_table <- function(csv_path) {
  if (!file.exists(csv_path)) {
    stop(glue("❌ File does not exist: {csv_path}"))
  }

  doc_table <- read.csv(csv_path, stringsAsFactors = FALSE)
  message("✅ Document table loaded: ", nrow(doc_table), " documents")

  if (any(is.na(doc_table$text) | doc_table$text == "")) {
    warning("⚠️ Document table contains empty or missing text fields.")
  }

  return(doc_table)
}


# Example usage
# csv_path <- build_raw_doc_table("data/code_data/week 8")

# load_doc_table(csv_path)
# load_doc_table("output/R_output/CSV_output/rmd_doc_table/week 8/raw_doc_table.csv")
