library(readr)
library(dplyr)
library(glue)
library(tibble)
source("lib/matrix_visualization.R")

# Function to generate consensus similarity matrix
consensus_similarity_matrix_generator <- function(file_paths, weights = "average", output_path = NULL) {
  if (length(file_paths) == 0) stop("No file paths provided.")

  # Load all matrices
  matrices <- lapply(file_paths, read_similarity_matrix)

  # Check if all matrices are the same dimension and names
  ref_names <- rownames(matrices[[1]])
  for (i in seq_along(matrices)) {
    if (!identical(rownames(matrices[[i]]), ref_names) || !identical(colnames(matrices[[i]]), ref_names)) {
      stop(glue::glue("Matrix at index {i} does not have matching row/column names."))
    }
  }

  # Determine weights
  if (is.character(weights) && weights == "average") {
    weights <- rep(1, length(matrices))
  } else if (length(weights) != length(matrices)) {
    stop("Length of weights does not match number of matrices.")
  }

  # normalize weights
  weights <- weights / sum(weights)

  # Compute consensus matrix
  consensus_mat <- matrices[[1]] * weights[1]
  for (i in 2:length(matrices)) {
    consensus_mat <- consensus_mat + matrices[[i]] * weights[i]
  }

  # Assign row/col names
  rownames(consensus_mat) <- ref_names
  colnames(consensus_mat) <- ref_names

  # Export to CSV if output_path provided
  if (!is.null(output_path)) {
    output_df <- as.data.frame(consensus_mat)
    output_df <- tibble::rownames_to_column(output_df, var = "user_id")
    write_csv(output_df, output_path)
  }

  return(consensus_mat)
}

# Example usage
TASK_TYPE = "CREATIVE"

# CREATIVE
cosine_sim_matrix <- "output/R_output/CSV_output/CREATIVE_similarity_matrices/cosine_similarity_checker_202502280632.csv"
levenshtein_sim_matrix <- "output/R_output/CSV_output/CREATIVE_similarity_matrices/levenshtein_similarity_checker_202502281040.csv"
winnowing_sim_matrix <- glue("output/R_output/CSV_output/CREATIVE_similarity_matrices/winnowing_similarity_checker_202502281048.csv")
winnowing_by_char_sim_matrix <- "output/R_output/CSV_output/CREATIVE_similarity_matrices/winnowing_similarity_by_char_checker_202503270642.csv"
jaccard_sim_matrix <- "output/R_output/CSV_output/CREATIVE_similarity_matrices/jaccard_similarity_checker_202502281040.csv"
overlap_sim_matrix <- "output/R_output/CSV_output/CREATIVE_similarity_matrices/overlap_similarity_checker_202502281040.csv"

# PRACTICAL
# cosine_sim_matrix <- "output/R_output/CSV_output/PRACTICAL_similarity_matrices/cosine_similarity_checker_202502281029.csv"
# levenshtein_sim_matrix <- "output/R_output/CSV_output/PRACTICAL_similarity_matrices/levenshtein_similarity_checker_202502281037.csv"
# jaccard_sim_matrix <- "output/R_output/CSV_output/PRACTICAL_similarity_matrices/jaccard_similarity_checker_202502281037.csv"
# overlap_sim_matrix <- "output/R_output/CSV_output/PRACTICAL_similarity_matrices/overlap_similarity_checker_202502281037.csv"
# winnowing_sim_matrix <- "output/R_output/CSV_output/PRACTICAL_similarity_matrices/winnowing_similarity_checker_202502281052.csv"
# winnowing_by_char_sim_matrix <- "output/R_output/CSV_output/PRACTICAL_similarity_matrices/winnowing_by_char_similarity_checker_202503271559.csv"

# 指定讀取的相似度矩陣 CSV 檔案路徑
file_paths <- c(
    cosine_sim_matrix,
    levenshtein_sim_matrix,
    jaccard_sim_matrix,
    overlap_sim_matrix,
    winnowing_sim_matrix,
    winnowing_by_char_sim_matrix
)

# 呼叫函數並平均加總所有相似度矩陣
# consensus_matrix <- consensus_similarity_matrix_generator(
#   file_paths,
#   weights = "average",
#   output_path = glue("output/R_output/CSV_output/{TASK_TYPE}_similarity_matrices/consensus_matrix/consensus_matrix_average.csv")
# )
