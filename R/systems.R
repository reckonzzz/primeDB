#' Get System List
#' 
#' Get all system information, including system ID, system name, and short name.
#' 
#' @return Matrix containing system information with columns: systemId, systemName, shortName
#' @export
#' @examples
#' systems <- get_systems()
#' print(systems)
get_systems <- function() {
  response <- make_request("/systems/list")
  result <- parse_response(response)
  
  # Convert the list data to a matrix using the generic function
  if (length(result) > 0) {
    result <- convert_list_to_matrix(result, "systems")
  } else {
    # Return empty matrix if no data
    result <- matrix(nrow = 0, ncol = 3, dimnames = list(NULL, c("systemId", "systemName", "shortName")))
  }
  
  return(result)
}

#' Get System Projects
#' 
#' Get all project accessions for a specific system.
#' 
#' @param short_name System short name (e.g., "gut", "respiratory")
#' @return Vector containing project accessions for the system
#' @export
#' @examples
#' projects <- get_system_projects("respiratory")
#' print(projects)
get_system_projects <- function(short_name) {
  if (!is.character(short_name) || length(short_name) != 1) {
    stop("short_name must be a single character string")
  }
  
  endpoint <- paste0("/systems/", short_name, "/project")
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

#' Get System Samples
#' 
#' Get all sample runs for a specific system.
#' 
#' @param short_name System short name (e.g., "gut", "respiratory")
#' @return Vector containing sample run IDs for the system
#' @export
#' @examples
#' samples <- get_system_samples("respiratory")
#' print(samples)
get_system_samples <- function(short_name) {
  if (!is.character(short_name) || length(short_name) != 1) {
    stop("short_name must be a single character string")
  }
  
  endpoint <- paste0("/systems/", short_name, "/sample")
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

#' Get System Statistics
#' 
#' Get detailed statistics for a specific system.
#' 
#' @param short_name System short name (e.g., "gut", "respiratory")
#' @return List containing system statistics
#' @export
#' @examples
#' \dontrun{
#' stats <- get_system_stats("respiratory")
#' print(stats$statistics$totalSamples)
#' }
get_system_stats <- function(short_name) {
  if (!is.character(short_name) || length(short_name) != 1) {
    stop("short_name must be a single character string")
  }
  
  endpoint <- paste0("/systems/", short_name, "/stats")
  response <- make_request(endpoint)
  result <- parse_response(response)
  
  # Use the generic function to process all list fields
  list_conversions <- list(
    "samples$runs" = "simple",
    "projects$details" = "matrix",
    "countries$distribution" = "matrix",
    "bodySites$distribution" = "matrix",
    "phenotypes$distribution" = "matrix",
    "variableRegions$distribution" = "matrix",
    "sequencingTypes$distribution" = "matrix"
  )
  
  return(process_response(result, list_conversions))
} 