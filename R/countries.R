#' Get Country List
#' 
#' Get all country information, including country ID, country name, and continent.
#' 
#' @return Matrix containing country information with columns: countryId, countryName, continent
#' @export
#' @examples
#' countries <- get_countries()
#' print(dim(countries))
get_countries <- function() {
  response <- make_request("/countries/list")
  result <- parse_response(response)
  
  # Convert the list data to a matrix using the generic function
  if (length(result) > 0) {
    result <- convert_list_to_matrix(result, "countries")
  } else {
    # Return empty matrix if no data
    result <- matrix(nrow = 0, ncol = 3, dimnames = list(NULL, c("countryId", "countryName", "continent")))
  }
  
  return(result)
}

#' Get Country Projects
#' 
#' Get all project accessions for a specific country.
#' 
#' @param country_id Country ID (integer)
#' @return Vector containing project accessions for the country
#' @export
#' @examples
#' projects <- get_country_projects(1)
#' print(projects)
get_country_projects <- function(country_id) {
  if (!is.numeric(country_id) || length(country_id) != 1) {
    stop("country_id must be a single integer")
  }
  
  endpoint <- paste0("/countries/", country_id, "/project")
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

#' Get Country Samples
#' 
#' Get all sample runs for a specific country.
#' 
#' @param country_id Country ID (integer)
#' @return Vector containing sample run IDs for the country
#' @export
#' @examples
#' samples <- get_country_samples(1)
#' print(samples)
get_country_samples <- function(country_id) {
  if (!is.numeric(country_id) || length(country_id) != 1) {
    stop("country_id must be a single integer")
  }
  
  endpoint <- paste0("/countries/", country_id, "/sample")
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

#' Get Country Statistics
#' 
#' Get detailed statistics for a specific country.
#' 
#' @param country_id Country ID (integer)
#' @return List containing country statistics
#' @export
#' @examples
#' \dontrun{
#' stats <- get_country_stats(1)
#' print(stats$statistics$totalSamples)
#' }
get_country_stats <- function(country_id) {
  if (!is.numeric(country_id) || length(country_id) != 1) {
    stop("country_id must be a single integer")
  }
  
  endpoint <- paste0("/countries/", country_id, "/stats")
  response <- make_request(endpoint)
  result <- parse_response(response)
  
  # Use the generic function to process all list fields
  list_conversions <- list(
    "samples$runs" = "simple",
    "projects$details" = "matrix",
    "systems$distribution" = "matrix",
    "bodySites$distribution" = "matrix",
    "phenotypes$distribution" = "matrix",
    "variableRegions$distribution" = "matrix",
    "sequencingTypes$distribution" = "matrix"
  )
  
  return(process_response(result, list_conversions))
} 