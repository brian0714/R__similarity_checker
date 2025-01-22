library(jsonlite)

# Function to write a list to a JSON file
write_list_to_json <- function(data_list,
                            output_dir="output/R_output/json_output",
                            pretty = TRUE) {
  datetime <- format(Sys.time(), "%Y%m%d%H%M")
  json_file_path <- paste0(output_dir, "/clusters_", datetime, ".json")
  # Write the list to a JSON file
  write_json(data_list, json_file_path, pretty = pretty)
  # Print confirmation message
  cat("JSON file written to:", json_file_path, "\n")

  return(json_file_path)
}

# Function to load JSON and CSV, and replace IDs with final_submission
replace_ids_with_submissions <- function(json_path, csv_path, output_dir="output/R_output/json_output") {
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
      submission <- csv_data$final_submission[csv_data$user_id == user_id & csv_data$task_type == "PRACTICAL"]
      if (length(submission) > 0) {
        return(submission) # Replace with final_submission
      } else {
        return(NA) # If user_id not found
      }
    })
  })

  # Step 4: Save the modified JSON
  datetime <- format(Sys.time(), "%Y%m%d%H%M")
  json_file_path <- paste0(output_dir, "/text_clusters_", datetime, ".json")
  write_json(clusters_replaced, json_file_path, pretty = TRUE)
  cat("Replaced JSON written to:", json_file_path, "\n")
}
