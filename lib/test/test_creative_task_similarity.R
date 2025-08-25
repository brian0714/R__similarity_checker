source("lib/test_rmd_plagiarism.R")

SIM_METHOD = "consensus"
# "consensus" or "jaccard" or "overlap" or "winnowing"
# or "winnowing_by_char" or "cosine" or "levenshtein"

sim_matrices <- list(
  cosine   = "output/R_output/CSV_output/CREATIVE_similarity_matrices/cosine_similarity_checker_202502280632.csv",
  levenshtein = "output/R_output/CSV_output/CREATIVE_similarity_matrices/levenshtein_similarity_checker_202502281040.csv",
  winnowing = "output/R_output/CSV_output/CREATIVE_similarity_matrices/winnowing_similarity_checker_202502281048.csv",
  winnowing_by_char = "output/R_output/CSV_output/CREATIVE_similarity_matrices/winnowing_similarity_by_char_checker_202503270642.csv",
  jaccard = "output/R_output/CSV_output/CREATIVE_similarity_matrices/jaccard_similarity_checker_202502281040.csv",
  overlap = "output/R_output/CSV_output/CREATIVE_similarity_matrices/overlap_similarity_checker_202502281040.csv",
  consensus = "output/R_output/CSV_output/CREATIVE_similarity_matrices/consensus_matrix/consensus_matrix_average.csv"
)

matrix_csv_path = sim_matrices[[SIM_METHOD]] # e.g. sim_matrices[["jaccard"]]
# export_most_similar_docs(
#   matrix_csv_path = matrix_csv_path,
#   threshold = 0.8
# )
# export_doc_similarity_stats(
#   matrix_csv_path = matrix_csv_path,
#   output_path = glue("output/R_output/CSV_output/CREATIVE_task_similarity_stats/{SIM_METHOD}_similarity_stats.csv")
# )

## 合併所有 average_score
merge_similarity_stats(
    glue("output/R_output/CSV_output/CREATIVE_task_similarity_stats"),
    score_type = "average_score"
)
## 或合併所有 std_score
merge_similarity_stats(
    glue("output/R_output/CSV_output/CREATIVE_task_similarity_stats"),
    score_type = "std_score"
)

# kmeans_results <- kmeans_clustering(
    # sim_matrix="output/R_output/CSV_output/CREATIVE_similarity_matrices/cosine_similarity_checker_202502280632.csv",
    # sim_matrix="output/R_output/CSV_output/CREATIVE_similarity_matrices/levenshtein_similarity_checker_202502281040.csv",
    # sim_matrix="output/R_output/CSV_output/CREATIVE_similarity_matrices/winnowing_similarity_checker_202502281048.csv",
    # sim_matrix="output/R_output/CSV_output/CREATIVE_similarity_matrices/winnowing_similarity_by_char_checker_202503270642.csv",
    # sim_matrix="output/R_output/CSV_output/CREATIVE_similarity_matrices/jaccard_similarity_checker_202502281040.csv",
    # sim_matrix="output/R_output/CSV_output/CREATIVE_similarity_matrices/overlap_similarity_checker_202502281040.csv",
    # sim_matrix="output/R_output/CSV_output/CREATIVE_similarity_matrices/consensus_matrix/consensus_matrix_average.csv",
    # optimal_k=1
# )
# clusters <- kmeans_results$cluster_groups
# representative_docs <- kmeans_results$representative_docs