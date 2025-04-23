source("lib/rmd_data_loader.R")
source("lib/similarity_matrix_generator.R")
library(glue)

# csv_path <- build_raw_doc_table("data/code_data/week 8")

similarities <- compare_matrix_generator(
  input_file_path =  "output/R_output/CSV_output/rmd_doc_table/week 8/raw_doc_table.csv",
  output_dir = glue("output/R_output/CSV_output/rmd_similarity_matrices/week 8")
)