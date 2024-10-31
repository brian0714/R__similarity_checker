# Load necessary libraries
library(dplyr)
library(rlang)
library(readr)

# Read GenAI CSV data and apply filters
csv_reader <- function(csv_file_path, filter_conditions = list(), num_rows = NULL) {

  # Read CSV data
  data <- read_csv(csv_file_path)

  # Transform user_id as integer and sorting in ascending order
  data <- data %>%
    mutate(user_id = as.integer(user_id),
           use_ai = as.numeric(use_ai),
           use_all = as.numeric(use_all),
           use_revise = as.numeric(use_revise),
           use_refine = as.numeric(use_refine),
           use_reject = as.numeric(use_reject)
           ) %>%

    arrange(user_id)

  # Applying multiple filter conditions (if any)
  if (length(filter_conditions) > 0) {
    # Loop through each condition and apply using rlang's eval and parse
    for (condition in filter_conditions) {
      data <- data %>% filter(!!parse_expr(condition))
    }
  }

  # Return head "n" of rows as filtered data if specified num_rows
  if (!is.null(num_rows)) {
    data <- head(data, num_rows)
  }

  return(data)
}

# Test the function
# Set the input file path
file_path <- 'data/text_data/extracted_behavior_pattern_data.csv'

# Define multiple filter conditions as strings
filter_conditions <- list(
  "use_ai == 0",
  "task_type == 'PRACTICAL'"
)

# Call the function
df <- csv_reader(file_path, filter_conditions = filter_conditions)

# Show the number of rows
print(nrow(df))