#' Get Phenotype List
#' 
#' Get all phenotypes with their IDs and names, including phenotype ID, phenotype name.
#' 
#' @return Matrix containing phenotype information with columns: phenotypeId, phenotypeName
#' @export
#' @examples
#' phenotypes <- get_phenotypes()
#' print(dim(phenotypes))
get_phenotypes <- function() {
  response <- make_request("/phenotypes/list")
  result <- parse_response(response)
  
  # Convert the list data to a matrix using the generic function
  if (length(result) > 0) {
    result <- convert_list_to_matrix(result, "phenotypes")
  } else {
    # Return empty matrix if no data
    result <- matrix(nrow = 0, ncol = 2, dimnames = list(NULL, c("phenotypeId", "phenotypeName")))
  }
  
  return(result)
}

#' Get Phenotype Projects
#' 
#' Get all project accessions for a specific phenotype.
#' 
#' @param phenotype_id Phenotype ID (integer)
#' @return Vector containing project accessions for the phenotype
#' @export
#' @examples
#' projects <- get_phenotype_projects(1)
#' print(projects)
get_phenotype_projects <- function(phenotype_id) {
  if (!is.numeric(phenotype_id) || length(phenotype_id) != 1) {
    stop("phenotype_id must be a single integer")
  }
  
  endpoint <- paste0("/phenotypes/", phenotype_id, "/project")
  response <- make_request(endpoint)
  result <- parse_response(response)
  
  # Convert projectAccessions to vector
  if (!is.null(result$data$projectAccessions)) {
    result$data$projectAccessions <- convert_list_to_matrix(
      result$data$projectAccessions, 
      "projectAccessions"
    )
  }
  
  return(unlist(result$projectAccessions))
}

#' Get Phenotype Samples
#' 
#' Get all sample runs for a specific phenotype.
#' 
#' @param phenotype_id Phenotype ID (integer)
#' @return Vector containing sample run IDs for the phenotype
#' @export
#' @examples
#' samples <- get_phenotype_samples(1)
#' print(samples)
get_phenotype_samples <- function(phenotype_id) {
  if (!is.numeric(phenotype_id) || length(phenotype_id) != 1) {
    stop("phenotype_id must be a single integer")
  }
  
  endpoint <- paste0("/phenotypes/", phenotype_id, "/sample")
  response <- make_request(endpoint)
  result <- parse_response(response)
  
  # Convert runs to vector
  if (!is.null(result$data$runs)) {
    result$data$runs <- convert_list_to_matrix(
      result$data$runs, 
      "runs"
    )
  }
  
  return(unlist(result$runs))
}

#' Get Phenotype Statistics
#' 
#' Get detailed statistics for a specific phenotype.
#' 
#' @param phenotype_id Phenotype ID (integer)
#' @return List containing phenotype statistics
#' @export
#' @examples
#' \dontrun{
#' stats <- get_phenotype_stats(1)
#' print(stats)
#' }
get_phenotype_stats <- function(phenotype_id) {
  if (!is.numeric(phenotype_id) || length(phenotype_id) != 1) {
    stop("phenotype_id must be a single integer")
  }
  
  endpoint <- paste0("/phenotypes/", phenotype_id, "/stats")
  response <- make_request(endpoint)
  result <- parse_response(response)
  
  # Use the generic function to process all list fields
  list_conversions <- list(
    "samples$runs" = "simple",
    "projects$details" = "matrix",
    "countries$distribution" = "matrix",
    "systems$distribution" = "matrix",
    "bodySites$distribution" = "matrix",
    "variableRegions$distribution" = "matrix",
    "sequencingTypes$distribution" = "matrix"
  )
  
  return(process_response(result, list_conversions))
} 