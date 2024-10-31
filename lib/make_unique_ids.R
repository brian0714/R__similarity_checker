# Function to handle duplicate user_ids by adding suffixes
make_unique_ids <- function(user_ids) {
  # Convert user_ids to character to ensure proper handling
  user_ids <- as.character(user_ids)

  # Initialize occurrence tracker as an empty list
  occurrence_tracker <- list()

  # Iterate through user_ids and add suffix if necessary
  unique_ids <- sapply(user_ids, function(id) {
    # If the user_id is not in the tracker, initialize it
    if (!(id %in% names(occurrence_tracker))) {
      occurrence_tracker[[id]] <- 1  # First occurrence
      return(id)
    } else {
      occurrence_tracker[[id]] <- occurrence_tracker[[id]] + 1
      return(paste0(id, "_", occurrence_tracker[[id]]))  # Add suffix for duplicates
    }
  })

  # Check if there are still duplicates after processing
  if (any(duplicated(unique_ids))) {
    cat("Duplicate user_ids found after processing:\n", unique_ids[duplicated(unique_ids)], "\n")
    stop("Error: Duplicate user_ids remain after processing.")
  }

  return(unique_ids)
}