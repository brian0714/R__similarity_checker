# Function to handle duplicate user_ids by adding suffixes
make_unique_ids <- function(user_ids) {
  # Convert user_ids to character to ensure proper handling
  user_ids <- as.character(user_ids)

  # Initialize occurrence tracker as an empty list
  occurrence_tracker <- list()
  unique_ids <- character(length(user_ids))  # Initialize empty vector for unique IDs
  duplicate_ids <- character()  # Initialize vector to store duplicate IDs

  # Iterate through user_ids and add suffix if necessary
  for (i in seq_along(user_ids)) {
    id <- user_ids[i]

    # Check and handle duplicates
    if (!(id %in% names(occurrence_tracker))) {
      occurrence_tracker[[id]] <- 1  # First occurrence
      unique_ids[i] <- id
    } else {
      occurrence_tracker[[id]] <- occurrence_tracker[[id]] + 1
      unique_id <- paste0(id, "_", occurrence_tracker[[id]])  # Add suffix for duplicates
      unique_ids[i] <- unique_id
      duplicate_ids <- c(duplicate_ids, id)  # Track duplicates
      cat("Processed duplicate ID:", unique_id, "\n")  # Print each processed duplicate
    }
  }

  # Check if there are still duplicates after processing
  if (any(duplicated(unique_ids))) {
    cat("Duplicate user_ids found after processing:\n", unique_ids[duplicated(unique_ids)], "\n")
    stop("Error: Duplicate user_ids remain after processing.")
  }

  # Return both unique IDs and duplicate IDs
  return(list(unique_ids = unique_ids, duplicate_ids = unique(duplicate_ids)))
}

# Test the function with sample user_ids
# user_ids <- c("user1", "user2", "user1", "user3", "user2", "user2")
# result <- make_unique_ids(user_ids)
# # Get unique_ids and duplicate_ids separately
# unique_ids <- result$unique_ids
# duplicate_ids <- result$duplicate_ids

# # Print the results
# cat("Unique IDs:\n", unique_ids, "\n")
# cat("Duplicate IDs:\n", duplicate_ids, "\n")