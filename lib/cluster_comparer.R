library(dplyr)
library(purrr)
library(stringr)
library(glue)
source("lib/cluster_to_json_writer.R")

# Helper function: calculate Jaccard index
jaccard_index <- function(set1, set2) {
  intersect_len <- length(intersect(set1, set2))
  union_len <- length(union(set1, set2))
  if (union_len == 0) return(0)
  return(intersect_len / union_len)
}

# Main function
compare_clusterings <- function(cluster1, cluster2, cluster_method_names = NULL, output_dir = "output/R_output/CSV_output") {
  # read json file if provided
  if (is.character(cluster1) && file.exists(cluster1)) {
    cluster1 <- read_json_as_clusters(cluster1)
  }
  if (is.character(cluster2) && file.exists(cluster2)) {
    cluster2 <- read_json_as_clusters(cluster2)
  }

  # Prepare similarity matrix
  similarity_matrix <- matrix(0, nrow = length(cluster1), ncol = length(cluster2))

  # Set default names if not provided or invalid
  if (is.null(cluster_method_names) || length(cluster_method_names) != 2) {
    rownames(similarity_matrix) <- paste0("C", seq_along(cluster1))
    colnames(similarity_matrix) <- paste0("C", seq_along(cluster2))
  } else {
    row_prefix <- cluster_method_names[1]
    col_prefix <- cluster_method_names[2]
    rownames(similarity_matrix) <- paste0(row_prefix, "_C", seq_along(cluster1))
    colnames(similarity_matrix) <- paste0(col_prefix, "_C", seq_along(cluster2))
  }

  # Fill similarity matrix with Jaccard index
  for (i in seq_along(cluster1)) {
    for (j in seq_along(cluster2)) {
      similarity_matrix[i, j] <- jaccard_index(cluster1[[i]], cluster2[[j]])
    }
  }

  # Get best match for cluster1 to cluster2
  best_match_1_to_2 <- setNames(
    colnames(similarity_matrix)[apply(similarity_matrix, 1, which.max)],
    rownames(similarity_matrix)
  )

  # Get best match for cluster2 to cluster1
  best_match_2_to_1 <- setNames(
    rownames(similarity_matrix)[apply(similarity_matrix, 2, which.max)],
    colnames(similarity_matrix)
  )

  # Save matrix as CSV
  if (is.null(cluster_method_names) || length(cluster_method_names) != 2) {
    output_path <- glue("{output_dir}/C1_C2_compared_similarity_matrix.csv")
  } else {
    output_path <- glue("{output_dir}/{row_prefix}_{col_prefix}_compared_similarity_matrix.csv")
  }
  write.csv(similarity_matrix, file = output_path, row.names = TRUE)

  # Return as list
  return(list(
    similarity_matrix = similarity_matrix,
    best_match_from_1_to_2 = best_match_1_to_2,
    best_match_from_2_to_1 = best_match_2_to_1
  ))
}



# test
TASK_TYPE <- "PRACTICAL"

result <- compare_clusterings(
    cluster1 = "output/R_output/json_output/PRACTICAL_clusters/PRACTICAL_cosine_clusters_202504101352.json",
    cluster2 = "output/R_output/json_output/PRACTICAL_clusters/PRACTICAL_jaccard_clusters_202504101418.json",
    cluster_method_names = c("cosine", "jaccard"),
    output_dir = glue("output/R_output/CSV_output/{TASK_TYPE}_cluster_compare_result"))

# 查看回傳的結果
print(result$similarity_matrix)
print(result$best_match_from_1_to_2)
print(result$best_match_from_2_to_1)