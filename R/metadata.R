#' Get Project Metadata
#'
#' Retrieve project metadata by Project Accession(s).
#'
#' @param project_accessions Character vector of project accession IDs
#' @return Data frame containing project metadata
#' @export
#' @examples
#' metadata <- get_project_metadata(c("PRJDB13875", "PRJDB16847"))
#' print(metadata)
get_project_metadata <- function(project_accessions) {
  if (!is.character(project_accessions) || length(project_accessions) == 0) {
    stop("project_accessions must be a non-empty character vector")
  }

  # Ensure project_accessions is always a list/array for JSON serialization
  body <- list(project_accessions = as.list(project_accessions))
  response <- make_request("/metadatas/project", method = "POST", body = body)
  result <- parse_response(response)

  # Convert to data frame for metadata (contains long text fields)
  if (is.null(result$data) || length(result$data) == 0) {
    return(data.frame())
  }

  # Convert list to data frame
  result <- do.call(rbind, lapply(result$data, function(x) {
    # Handle NULL values by converting to NA
    x <- lapply(x, function(val) ifelse(is.null(val), NA, val))
    as.data.frame(x, stringsAsFactors = FALSE)
  }))

  return(result)
}

#' Get Sample Metadata
#'
#' Retrieve sample metadata by Run ID(s) with batch processing for large datasets.
#'
#' @param runs Character vector of sample run IDs
#' @param show_progress Whether to show progress messages (default: TRUE)
#' @return Data frame containing sample metadata
#' @export
#' @examples
#' metadata <- get_sample_metadata(c("DRR396974", "DRR396975"))
#' print(metadata)
#'
#' # For large datasets, batch processing is automatic
#' large_metadata <- get_sample_metadata(large_run_list)
get_sample_metadata <- function(runs, show_progress = TRUE) {
  if (!is.character(runs) || length(runs) == 0) {
    stop("runs must be a non-empty character vector")
  }

  total_runs <- length(runs)
  batch_size <- 4000  # Fixed batch size

  # If total runs is small enough, process all at once
  if (total_runs <= batch_size) {
    if (show_progress) {
      cat("Processing", total_runs, "samples in single batch...\n")
    }

    body <- list(runs = as.list(runs))
    response <- make_request("/metadatas/sample", method = "POST", body = body)
    result <- parse_response(response)

    if (is.null(result$data) || length(result$data) == 0) {
      return(data.frame())
    }

    # Convert list to data frame
    result <- do.call(rbind, lapply(result$data, function(x) {
      x <- lapply(x, function(val) ifelse(is.null(val), NA, val))
      as.data.frame(x, stringsAsFactors = FALSE)
    }))

    if (show_progress) {
      cat("Completed processing", total_runs, "samples.\n")
    }

    return(result)
  }

  # For large datasets, process in batches
  if (show_progress) {
    cat("Processing", total_runs, "samples in batches of", batch_size, "...\n")
  }

  # Calculate number of batches
  num_batches <- ceiling(total_runs / batch_size)
  all_results <- list()

  for (i in 1:num_batches) {
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- min(i * batch_size, total_runs)
    batch_runs <- runs[start_idx:end_idx]

    if (show_progress) {
      cat("Batch", i, "/", num_batches, ": Processing samples", start_idx, "to", end_idx, "...\n")
    }

    tryCatch({
      body <- list(runs = as.list(batch_runs))
      response <- make_request("/metadatas/sample", method = "POST", body = body)
      result <- parse_response(response)

      if (!is.null(result$data) && length(result$data) > 0) {
        # Convert list to data frame
        batch_df <- do.call(rbind, lapply(result$data, function(x) {
          x <- lapply(x, function(val) ifelse(is.null(val), NA, val))
          as.data.frame(x, stringsAsFactors = FALSE)
        }))
        all_results[[i]] <- batch_df
      } else {
        all_results[[i]] <- data.frame()
      }

      if (show_progress) {
        cat("Batch", i, "/", num_batches, ": Completed successfully.\n")
      }

    }, error = function(e) {
      if (show_progress) {
        cat("Batch", i, "/", num_batches, ": Error -", e$message, "\n")
      }
      all_results[[i]] <- data.frame()
    })

    # Add a small delay between batches to avoid overwhelming the server
    if (i < num_batches) {
      Sys.sleep(0.5)
    }
  }

  # Combine all results
  if (show_progress) {
    cat("Combining results from all batches...\n")
  }

  # Filter out empty data frames
  valid_results <- all_results[sapply(all_results, nrow) > 0]

  if (length(valid_results) == 0) {
    if (show_progress) {
      cat("No valid data retrieved from any batch.\n")
    }
    return(data.frame())
  }

  # Combine all data frames
  final_result <- do.call(rbind, valid_results)

  if (show_progress) {
    cat("Successfully processed", nrow(final_result), "samples out of", total_runs, "requested.\n")
  }

  return(final_result)
}
