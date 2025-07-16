#' Get Sample List
#' 
#' Get all sample run list.
#' 
#' @return Matrix containing sample information with columns: run, projectAccession
#' @export
#' @examples
#' samples <- get_samples()
#' print(dim(samples))
get_samples <- function() {
  response <- make_request("/samples/list")
  result <- parse_response(response)

  # Convert the list data to a matrix using the generic function
  if (length(result) > 0) {
    result <- convert_list_to_matrix(result, "samples", simple_fields = c("run", "projectAccession"))
  } else {
    # Return empty matrix if no data
    result <- matrix(nrow = 0, ncol = 2, dimnames = list(NULL, c("run", "projectAccession")))
  }

  return(result)
}

#' Get Sample Project
#' 
#' Get project accession for specific sample(s).
#' 
#' @param sample_run Sample Run ID(s) - can be a single string or vector of strings
#' @return For single sample: character string containing project accession. For multiple samples: matrix with columns 'run' and 'projectAccession'
#' @export
#' @examples
#' project <- get_sample_project("DRR396974")
#' print(project)
#' projects <- get_sample_project(c("DRR396974", "DRR396975"))
#' print(projects)
get_sample_project <- function(sample_run) {
  if (!is.character(sample_run)) {
    stop("sample_run must be a character string or vector of character strings")
  }
  
  # single sample
  if (length(sample_run) == 1) {
  endpoint <- paste0("/samples/", sample_run, "/project")
    response <- make_request(endpoint)
    result <- parse_response(response)
    # return projectAccession
    return(if (!is.null(result$projectAccession)) result$projectAccession else character(0))
  }

  # multiple samples
  results <- matrix(nrow = length(sample_run), ncol = 2, 
                   dimnames = list(NULL, c("run", "projectAccession")))
  
  for (i in seq_along(sample_run)) {
    endpoint <- paste0("/samples/", sample_run[i], "/project")
    response <- make_request(endpoint)
    result <- parse_response(response)
    
    results[i, "run"] <- sample_run[i]
    results[i, "projectAccession"] <- if (!is.null(result$projectAccession)) result$projectAccession else NA_character_
  }
  
  return(results)
}

#' Get Sample Statistics
#' 
#' Get detailed metadata information for a specific sample.
#' 
#' @param sample_run Sample Run ID
#' @return List containing sample metadata
#' @export
#' @examples
#' \dontrun{
#' stats <- get_sample_stats("DRR396974")
#' print(stats)
#' }
get_sample_stats <- function(sample_run) {
  if (!is.character(sample_run) || length(sample_run) != 1) {
    stop("sample_run must be a single character string")
  }
  
  endpoint <- paste0("/samples/", sample_run, "/stats")
  response <- make_request(endpoint)
  result <- parse_response(response)

  # Sample stats endpoint doesn't have nested list fields that need conversion
  # The data is already in the correct format
  return(result)
}
