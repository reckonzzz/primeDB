#' Get Body Site List
#' 
#' Get all body site information, including body site ID, body site name, and system.
#' 
#' @return Matrix containing body site information with columns: bodySiteId, bodySiteName, system
#' @export
#' @examples
#' bodysites <- get_bodysites()
#' print(dim(bodysites))
#' print(head(bodysites))
get_bodysites <- function() {
  response <- make_request("/bodysites/list")
  result <- parse_response(response)
  
  # Convert the list data to a matrix using the generic function
  if (length(result) > 0) {
    result <- convert_list_to_matrix(result, "bodysites")
  } else {
    # Return empty matrix if no data
    result <- matrix(nrow = 0, ncol = 3, dimnames = list(NULL, c("bodySiteId", "bodySiteName", "system")))
  }
  
  return(result)
}

#' Get Body Site Samples
#' 
#' Get all sample runs for a specific body site.
#' 
#' @param body_site_id Body site ID (integer)
#' @return Vector containing sample run IDs for the body site
#' @export
#' @examples
#' samples <- get_bodysite_samples(2)
#' print(samples)
get_bodysite_samples <- function(body_site_id) {
  if (!is.numeric(body_site_id) || length(body_site_id) != 1) {
    stop("body_site_id must be a single integer")
  }
  
  endpoint <- paste0("/bodysites/", body_site_id, "/sample")
  response <- make_request(endpoint)
  result <- parse_response(response)
  
  # Use the generic function to convert runs to matrix
  if (!is.null(result$data$runs)) {
    result$data$runs <- convert_list_to_matrix(
      result$data$runs, 
      "runs"
    )
  }
  
  return(unlist(result$runs))
}

#' Get Body Site Projects
#' 
#' Get all project accessions for a specific body site.
#' 
#' @param body_site_id Body site ID (integer)
#' @return Vector containing project accessions for the body site
#' @export
#' @examples
#' projects <- get_bodysite_projects(2)
#' print(projects)
get_bodysite_projects <- function(body_site_id) {
  if (!is.numeric(body_site_id) || length(body_site_id) != 1) {
    stop("body_site_id must be a single integer")
  }
  
  endpoint <- paste0("/bodysites/", body_site_id, "/project")
  response <- make_request(endpoint)
  result <- parse_response(response)
  
  # Use the generic function to convert projectAccessions to vector
  if (!is.null(result$data$projectAccessions)) {
    result$data$projectAccessions <- convert_list_to_matrix(
      result$data$projectAccessions, 
      "projectAccessions"
    )
  }
  
  return(unlist(result$projectAccessions))
}

#' Get Body Site System
#' 
#' Get system information for a specific body site.
#' 
#' @param body_site_id Body site ID (integer)
#' @return Character of the full system name for the body site
#' @export
#' @examples
#' system <- get_bodysite_system(6)
#' print(system)
get_bodysite_system <- function(body_site_id) {
  if (!is.numeric(body_site_id) || length(body_site_id) != 1) {
    stop("body_site_id must be a single integer")
  }
  
  endpoint <- paste0("/bodysites/", body_site_id, "/system")
  response <- make_request(endpoint)
  result <- parse_response(response)
  return(result$system)
}

#' Get Body Site Statistics
#' 
#' Get detailed statistics for a specific body site.
#' 
#' @param body_site_id Body site ID (integer)
#' @return List containing body site statistics
#' @export
#' @examples
#' \dontrun{
#' stats <- get_bodysite_stats(6)
#' print(stats)
#' }
get_bodysite_stats <- function(body_site_id) {
  if (!is.numeric(body_site_id) || length(body_site_id) != 1) {
    stop("body_site_id must be a single integer")
  }
  
  endpoint <- paste0("/bodysites/", body_site_id, "/stats")
  response <- make_request(endpoint)
  result <- parse_response(response)
  
  # Use the generic function to process all list fields
  list_conversions <- list(
    "samples$runs" = "simple",
    "projects$details" = "matrix",
    "countries$distribution" = "matrix",
    "systems$distribution" = "matrix",
    "phenotypes$distribution" = "matrix",
    "variableRegions$distribution" = "matrix",
    "sequencingTypes$distribution" = "matrix"
  )
  
  return(process_response(result, list_conversions))
} 