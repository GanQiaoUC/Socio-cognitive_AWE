#before running this, please download the corpus here and use the csv files: https://github.com/terryyutian/KLiCKe-Corpus
#reference: Tian, Y., Crossley, S. A., & Van Waes, L. (2025). The KLiCKe Corpus: Keystroke Logging in Compositions for Knowledge Evaluation. Journal of Writing Research

# Load required libraries
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(purrr)

# Function to analyze a single keystroke logging file
analyze_keystroke_file <- function(file_path) {
  # Create a tryCatch to handle potential errors in processing
  tryCatch({
    # Read the CSV file
    data <- read.csv(file_path)
    
    # Ensure proper column types
    data$DownEventID <- as.integer(data$DownEventID)
    data$UpEventID <- as.integer(data$UpEventID)
    data$DownTime <- as.numeric(data$DownTime)
    data$UpTime <- as.numeric(data$UpTime)
    data$ActionTime <- as.numeric(data$ActionTime)
    data$CursorPosition <- as.integer(data$CursorPosition)
    data$PauseTime <- as.numeric(data$PauseTime)
    data$WordCount <- as.integer(data$WordCount)
    data$Activity <- as.factor(data$Activity)
    
    # Extract filename for identification
    file_name <- basename(file_path)
    
    # Calculate P-bursts (pauses exceeding 2 seconds)
    # A P-burst is a string of actions between pauses that exceed 2 seconds
    data$new_p_burst <- data$PauseTime >= 2000
    data$p_burst_id <- cumsum(data$new_p_burst)
    
    p_bursts <- data %>%
      filter(Activity %in% c("Input", "Remove/Cut", "Replace", "Paste")) %>%
      group_by(p_burst_id) %>%
      summarize(
        burst_length = sum(ifelse(Activity %in% c("Input", "Replace", "Paste"), 1, 0)),
        .groups = "drop"
      )
    
    mean_p_burst_length <- mean(p_bursts$burst_length, na.rm = TRUE)
    
    # Calculate R-bursts (insertions or deletions exceeding 2 seconds)
    data$is_revision <- data$Activity %in% c("Remove/Cut", "Replace")
    data$new_r_burst <- c(FALSE, diff(as.integer(data$is_revision)) != 0)
    data$r_burst_id <- cumsum(data$new_r_burst)
    
    r_bursts <- data %>%
      filter(Activity %in% c("Input", "Remove/Cut", "Replace", "Paste")) %>%
      group_by(r_burst_id) %>%
      summarize(
        burst_length = sum(ifelse(Activity %in% c("Input", "Replace", "Paste"), 1, 0)),
        .groups = "drop"
      )
    
    mean_r_burst_length <- mean(r_bursts$burst_length, na.rm = TRUE)
    
    # Calculate mean pause lengths (pauses exceeding 200ms)
    all_pauses <- data$PauseTime[data$PauseTime >= 200]
    mean_pause_length <- mean(all_pauses, na.rm = TRUE) / 1000  # Convert to seconds
    
    # Calculate within-word pauses
    within_word_indices <- numeric(0)
    
    # Use tryCatch here to handle potential errors in lag() function
    tryCatch({
      # Assume within-word pauses occur when we're not at word boundaries
      within_word_indices <- which(data$Activity %in% c("Input", "Replace", "Paste") & 
                                     !str_detect(data$TextChange, "\\s") & 
                                     data$PauseTime >= 200)
      
      # Exclude first character of each word since that would be between-word
      word_start_indices <- which(data$WordCount > lag(data$WordCount, default = 0))
      within_word_indices <- setdiff(within_word_indices, word_start_indices)
    }, error = function(e) {
      warning(paste("Error calculating within-word pauses for", file_name, ":", e$message))
    })
    
    mean_within_word_pause <- ifelse(length(within_word_indices) > 0, 
                                     mean(data$PauseTime[within_word_indices], na.rm = TRUE) / 1000,
                                     NA)
    
    # Calculate between-word pauses
    between_word_indices <- numeric(0)
    
    # Use tryCatch here to handle potential errors in lag() function
    tryCatch({
      # Assume between-word pauses occur before the first character of a word
      between_word_indices <- which(data$Activity %in% c("Input", "Replace", "Paste") & 
                                      (data$WordCount > lag(data$WordCount, default = 0) | 
                                         str_detect(data$TextChange, "\\s")) &
                                      data$PauseTime >= 200)
    }, error = function(e) {
      warning(paste("Error calculating between-word pauses for", file_name, ":", e$message))
    })
    
    mean_between_word_pause <- ifelse(length(between_word_indices) > 0,
                                      mean(data$PauseTime[between_word_indices], na.rm = TRUE) / 1000,
                                      NA)
    
    # Calculate deletion metrics
    deletion_rows <- data %>% filter(Activity == "Remove/Cut")
    deletion_count <- nrow(deletion_rows)
    
    mean_deletion_length <- 0
    if(deletion_count > 0) {
      # Group consecutive deletions
      deletion_rows$deletion_group <- c(0, cumsum(diff(deletion_rows$DownEventID) > 1))
      deletion_groups <- deletion_rows %>% 
        group_by(deletion_group) %>% 
        summarize(chars_deleted = n(), .groups = "drop")
      
      mean_deletion_length <- mean(deletion_groups$chars_deleted, na.rm = TRUE)
    }
    
    # Total characters produced (counting all productive actions)
    total_chars <- sum(data$Activity %in% c("Input", "Replace", "Paste")) + sum(data$Activity == "Remove/Cut")
    
    # Proportion of deletions
    prop_deletions <- ifelse(total_chars > 0, sum(data$Activity == "Remove/Cut") / total_chars, 0)
    
    # Calculate insertion metrics (Input, Replace, and Paste are insertions)
    insertion_rows <- data %>% filter(Activity %in% c("Input", "Replace", "Paste"))
    insertion_count <- nrow(insertion_rows)
    
    mean_insertion_length <- 0
    if(insertion_count > 0) {
      # Group consecutive insertions
      insertion_rows$insertion_group <- c(0, cumsum(diff(insertion_rows$DownEventID) > 1))
      insertion_groups <- insertion_rows %>% 
        group_by(insertion_group) %>% 
        summarize(chars_inserted = n(), .groups = "drop")
      
      mean_insertion_length <- mean(insertion_groups$chars_inserted, na.rm = TRUE)
    }
    
    # Proportion of insertions
    prop_insertions <- ifelse(total_chars > 0, insertion_count / total_chars, 0)
    
    # Calculate paste operations specifically
    paste_rows <- data %>% filter(Activity == "Paste")
    paste_count <- nrow(paste_rows)
    
    # Mean paste length
    paste_length <- 0
    if(paste_count > 0) {
      paste_groups <- paste_rows %>% 
        group_by(DownEventID) %>% 
        summarize(chars_pasted = n(), .groups = "drop")
      paste_length <- mean(paste_groups$chars_pasted, na.rm = TRUE)
    }
    
    # Calculate interval variance
    # First, divide the writing process into 10 equal intervals
    max_time <- max(data$UpTime, na.rm = TRUE)
    min_time <- min(data$DownTime, na.rm = TRUE)
    total_time <- max_time - min_time
    
    interval_variance <- 0
    
    if(total_time > 0) {
      interval_size <- total_time / 10
      
      data$interval <- ceiling((data$DownTime - min_time) / interval_size)
      data$interval[data$interval == 0] <- 1  # Ensure no zero intervals
      data$interval[data$interval > 10] <- 10  # Cap at 10 in case of rounding errors
      
      # Calculate production rate for each interval
      interval_rates <- data %>%
        filter(Activity %in% c("Input", "Replace", "Paste")) %>%
        group_by(interval) %>%
        summarize(
          chars_produced = n(),
          .groups = "drop"
        ) %>%
        arrange(interval)
      
      # Fill in missing intervals with zero production
      all_intervals <- data.frame(interval = 1:10)
      interval_rates <- merge(all_intervals, interval_rates, by = "interval", all.x = TRUE)
      interval_rates$chars_produced[is.na(interval_rates$chars_produced)] <- 0
      
      # Calculate the standard deviation of production rates
      interval_variance <- sd(interval_rates$chars_produced, na.rm = TRUE)
    }
    
    # Return all calculated metrics as a dataframe row
    result <- data.frame(
      file_name = file_name,
      mean_p_burst_length = mean_p_burst_length,
      mean_r_burst_length = mean_r_burst_length,
      mean_pause_length = mean_pause_length,
      mean_within_word_pause = mean_within_word_pause,
      mean_between_word_pause = mean_between_word_pause,
      mean_deletion_length = mean_deletion_length,
      proportion_deletions = prop_deletions,
      mean_insertion_length = mean_insertion_length,
      proportion_insertions = prop_insertions,
      mean_paste_length = paste_length,
      paste_count = paste_count,
      interval_variance = interval_variance
    )
    
    return(result)
  }, error = function(e) {
    # Return a data frame with file name and NA values for all metrics
    warning(paste("Error processing file", file_path, ":", e$message))
    return(data.frame(
      file_name = basename(file_path),
      mean_p_burst_length = NA,
      mean_r_burst_length = NA,
      mean_pause_length = NA,
      mean_within_word_pause = NA,
      mean_between_word_pause = NA,
      mean_deletion_length = NA,
      proportion_deletions = NA,
      mean_insertion_length = NA,
      proportion_insertions = NA,
      mean_paste_length = NA,
      paste_count = NA,
      interval_variance = NA
    ))
  })
}

# Function to process all CSV files in a folder
process_keystroke_folder <- function(folder_path, output_file = "keystroke_analysis_results.csv") {
  # Get list of all CSV files in the folder
  csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  # Process each file and collect results
  results_list <- list()
  errors_list <- character()
  
  for (file in csv_files) {
    # Print progress
    cat("Processing", basename(file), "\n")
    
    # Try to analyze the file
    tryCatch({
      result <- analyze_keystroke_file(file)
      results_list[[length(results_list) + 1]] <- result
    }, error = function(e) {
      # Record the error
      errors_list <<- c(errors_list, paste("Error with file", basename(file), ":", e$message))
    })
  }
  
  # Combine results if any were successful
  if (length(results_list) > 0) {
    results <- bind_rows(results_list)
    
    # Write results to CSV
    write.csv(results, output_file, row.names = FALSE)
    cat("Analysis complete. Results saved to", output_file, "\n")
    
    # Report any errors
    if (length(errors_list) > 0) {
      error_file <- paste0("keystroke_analysis_errors_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
      writeLines(errors_list, error_file)
      cat("Errors encountered with", length(errors_list), "files. Details saved to", error_file, "\n")
    }
    
    return(results)
  } else {
    stop("No files were successfully processed. Check error messages.")
  }
}

results <- process_keystroke_folder("your/path")
