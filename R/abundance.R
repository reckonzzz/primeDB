#' Get Project Abundance Data
#'
#' Get abundance data for project samples. First retrieves sample list from projects, then gets abundance data in batches.
#'
#' @param db Reference database type ("gg" or "silva")
#' @param rank Taxonomic rank ("phylum", "class", "order", "family", "genus", "species")
#' @param type Abundance type ("absolute" or "relative")
#' @param project_accessions Character vector of project accession IDs
#' @param show_progress Whether to show progress messages (default: TRUE)
#' @return Matrix with samples as rows and taxa as columns
#' @export
#' @examples
#' abundance <- get_abundance_project("gg", "phylum", "absolute", c("PRJDB13875"))
#' print(head(abundance))
get_abundance_project <- function(db, rank, type, project_accessions, show_progress = TRUE) {
  if (!db %in% c("gg", "silva")) {
    stop("db must be either 'gg' or 'silva'")
  }
  if (!rank %in% c("phylum", "class", "order", "family", "genus", "species")) {
    stop("rank must be one of: phylum, class, order, family, genus, species")
  }
  if (!type %in% c("absolute", "relative")) {
    stop("type must be either 'absolute' or 'relative'")
  }
  if (!is.character(project_accessions) || length(project_accessions) == 0) {
    stop("project_accessions must be a non-empty character vector")
  }

  if (show_progress) {
    cat("Getting sample list from", length(project_accessions), "project(s)...\n")
  }

  # Get sample list from projects
  sample_list <- get_project_samples(project_accessions)
  
  # Handle different return formats from get_project_samples
  if (is.list(sample_list)) {
    # Multiple projects - combine all sample lists
    all_runs <- unlist(sample_list)
  } else {
    # Single project - sample_list is already a vector
    all_runs <- sample_list
  }
  
  if (length(all_runs) == 0) {
    if (show_progress) {
      cat("No samples found in the specified projects.\n")
    }
    return(matrix(nrow = 0, ncol = 0))
  }
  
  if (show_progress) {
    cat("Found", length(all_runs), "samples. Getting abundance data...\n")
  }
  
  # Get abundance data using the sample-based function
  return(get_abundance_sample(db, rank, type, all_runs, show_progress))
}

#' Get Sample Abundance Data
#'
#' Get abundance data for specific samples with batch processing for large datasets.
#'
#' @param db Reference database type ("gg" or "silva")
#' @param rank Taxonomic rank ("phylum", "class", "order", "family", "genus", "species")
#' @param type Abundance type ("absolute" or "relative")
#' @param runs Character vector of sample run IDs
#' @param show_progress Whether to show progress messages (default: TRUE)
#' @return Matrix with samples as rows and taxa as columns
#' @export
#' @examples
#' abundance <- get_abundance_sample("gg", "phylum", "absolute", c("DRR396974", "DRR396975"))
#' print(head(abundance))
get_abundance_sample <- function(db, rank, type, runs, show_progress = TRUE) {
  if (!db %in% c("gg", "silva")) {
    stop("db must be either 'gg' or 'silva'")
  }
  if (!rank %in% c("phylum", "class", "order", "family", "genus", "species")) {
    stop("rank must be one of: phylum, class, order, family, genus, species")
  }
  if (!type %in% c("absolute", "relative")) {
    stop("type must be either 'absolute' or 'relative'")
  }
  if (!is.character(runs) || length(runs) == 0) {
    stop("runs must be a non-empty character vector")
  }

  total_runs <- length(runs)
  batch_size <- 1000  # Fixed batch size
  
  # If total runs is small enough, process all at once
  if (total_runs <= batch_size) {
    if (show_progress) {
      cat("Processing", total_runs, "samples in single batch...\n")
    }
    
    endpoint <- paste0("/abundances/sample/", db, "/", rank, "/", type)
    body <- list(runs = c(runs, " "))
    
    # No timeout for abundance requests
    response <- make_request(endpoint, method = "POST", body = body, timeout = 60 * 60 * 24)
    csv_content <- httr::content(response, "text", encoding = "UTF-8")
    
    if (csv_content == "" || is.null(csv_content)) {
      return(matrix(nrow = 0, ncol = 0))
    }
    
    # Read CSV content
    con <- textConnection(csv_content)
    data <- read.csv(con, row.names = 1, check.names = FALSE)
    close(con)
    
    # Convert to matrix
    abundance_matrix <- as.matrix(data)
    
    if (show_progress) {
      cat("Completed processing", total_runs, "samples.\n")
    }
    
    return(abundance_matrix)
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
      endpoint <- paste0("/abundances/sample/", db, "/", rank, "/", type)
      body <- list(runs = c(batch_runs, " "))
      
      # No timeout for abundance requests
      response <- make_request(endpoint, method = "POST", body = body, timeout = 60 * 60 * 24)
      csv_content <- httr::content(response, "text", encoding = "UTF-8")
      
      if (csv_content != "" && !is.null(csv_content)) {
        # Read CSV content
        con <- textConnection(csv_content)
        data <- read.csv(con, row.names = 1, check.names = FALSE)
        close(con)
        
        # Convert to matrix
        batch_matrix <- as.matrix(data)
        all_results[[i]] <- batch_matrix
      } else {
        all_results[[i]] <- matrix(nrow = 0, ncol = 0)
      }
      
      if (show_progress) {
        cat("Batch", i, "/", num_batches, ": Completed successfully.\n")
      }
      
    }, error = function(e) {
      if (show_progress) {
        cat("Batch", i, "/", num_batches, ": Error -", e$message, "\n")
      }
      all_results[[i]] <- matrix(nrow = 0, ncol = 0)
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
  
  # Filter out empty matrices
  valid_results <- all_results[sapply(all_results, function(x) nrow(x) > 0 && ncol(x) > 0)]
  
  if (length(valid_results) == 0) {
    if (show_progress) {
      cat("No valid data retrieved from any batch.\n")
    }
    return(matrix(nrow = 0, ncol = 0))
  }
  
  # Combine all matrices by rows (samples)
  final_result <- do.call(rbind, valid_results)
  
  if (show_progress) {
    cat("Successfully processed", nrow(final_result), "samples out of", total_runs, "requested.\n")
  }
  
  return(final_result)
}

#' Get Cached Abundance Data
#'
#' Get cached abundance data files for all runs with download progress monitoring. Data may be large, so it is recommended to download from the PrimeDB website.
#'
#' @param db Reference database type ("gg" or "silva")
#' @param rank Taxonomic rank ("phylum", "class", "order", "family", "genus", "species")
#' @param type Abundance type ("absolute" or "relative")
#' @param show_progress Whether to show download progress (default: TRUE)
#' @return Matrix with samples as rows and taxa as columns
#' @export
#' @examples
#' \dontrun{
#' abundance <- get_abundance_cache("gg", "phylum", "absolute")
#' print(head(abundance))
#' }
get_abundance_cache <- function(db, rank, type, show_progress = TRUE) {
  if (!db %in% c("gg", "silva")) {
    stop("db must be either 'gg' or 'silva'")
  }
  if (!rank %in% c("phylum", "class", "order", "family", "genus", "species")) {
    stop("rank must be one of: phylum, class, order, family, genus, species")
  }
  if (!type %in% c("absolute", "relative")) {
    stop("type must be either 'absolute' or 'relative'")
  }

  endpoint <- paste0("/abundances/cache/", db, "/", rank, "/", type)
  url <- paste0(api_base_url(), endpoint)
  
  if (show_progress) {
    cat("Downloading cached abundance data...\n")
    cat("This may take a while for large datasets.\n")
  }
  
  # Create a temporary file to store the download
  temp_file <- tempfile(fileext = ".csv")
  
  # Download with progress monitoring
  if (show_progress) {
    # Use httr::GET with write_disk for progress monitoring
    response <- httr::GET(url, 
                         httr::write_disk(temp_file, overwrite = TRUE),
                         httr::timeout(60 * 60 * 24),
                         httr::progress())
  } else {
    # Download without progress display
    response <- httr::GET(url, 
                         httr::write_disk(temp_file, overwrite = TRUE),
                         httr::timeout(60 * 60 * 24))
  }
  
  # Check for HTTP errors
  if (httr::http_error(response)) {
    unlink(temp_file)  # Clean up temp file
    content <- httr::content(response, "text", encoding = "UTF-8")
    content <- trimws(content)
    parsed <- jsonlite::fromJSON(content, simplifyVector = FALSE)
    if (!is.null(parsed$success) && !parsed$success) {
      if (!is.null(parsed$message)) {
        stop(parsed$message)
      } else if (!is.null(parsed$error)) {
        stop(parsed$error)
      } else {
        stop("Resource not found")
      }
    } else {
      stop("Resource not found")
    }
  }
  
  # Get file size for progress information
  file_size <- file.size(temp_file)
  if (show_progress) {
    cat("Download completed. File size:", format(file_size, units = "auto"), "\n")
    cat("Reading CSV data...\n")
  }
  
  # Read CSV content from file
  data <- read.csv(temp_file, row.names = 1, check.names = FALSE)
  
  # Clean up temp file
  unlink(temp_file)
  
  # Convert to matrix
  abundance_matrix <- as.matrix(data)
  
  if (show_progress) {
    cat("Successfully loaded", nrow(abundance_matrix), "samples and", ncol(abundance_matrix), "taxa.\n")
  }
  
  return(abundance_matrix)
}
