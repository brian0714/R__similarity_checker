source("lib/test_rmd_plagiarism.R")

SIM_METHOD = "overlap"
# "consensus" or "jaccard" or "overlap" or "winnowing"
# or "winnowing_by_char" or "cosine" or "levenshtein"

# export_most_similar_docs(
#   matrix_csv_path = "output/R_output/CSV_output/rmd_similarity_matrices/week 10/jaccard_checker_202505081039.csv",
#   threshold = 0.8
# )
# export_doc_similarity_stats(
#   matrix_csv_path = "output/R_output/CSV_output/PRACTICAL_similarity_matrices/overlap_similarity_checker_202502281037.csv",
#   output_path = glue("output/R_output/CSV_output/task_similarity_stats/{SIM_METHOD}_similarity_stats.csv")
# )

## 合併所有 average_score
# merge_similarity_stats(
#     glue("output/R_output/CSV_output/task_similarity_stats"),
#     score_type = "average_score"
# )
## 或合併所有 std_score
# merge_similarity_stats(
#     glue("output/R_output/CSV_output/task_similarity_stats"),
#     score_type = "std_score"
# )

kmeans_results <- kmeans_clustering(
    # sim_matrix="output/R_output/CSV_output/PRACTICAL_similarity_matrices/cosine_similarity_checker_202502281029.csv",
    # sim_matrix="output/R_output/CSV_output/PRACTICAL_similarity_matrices/levenshtein_similarity_checker_202502281037.csv",
    # sim_matrix="output/R_output/CSV_output/PRACTICAL_similarity_matrices/winnowing_similarity_checker_202502281052.csv",
    # sim_matrix="output/R_output/CSV_output/PRACTICAL_similarity_matrices/winnowing_by_char_similarity_checker_202503271559.csv",
    # sim_matrix="output/R_output/CSV_output/PRACTICAL_similarity_matrices/jaccard_similarity_checker_202502281037.csv",
    # sim_matrix="output/R_output/CSV_output/PRACTICAL_similarity_matrices/overlap_similarity_checker_202502281037.csv",
    sim_matrix="output/R_output/CSV_output/PRACTICAL_similarity_matrices/consensus_matrix/consensus_matrix_average.csv",
    optimal_k=1
)
# clusters <- kmeans_results$cluster_groups
representative_docs <- kmeans_results$representative_docs
