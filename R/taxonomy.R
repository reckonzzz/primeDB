#' Get Taxonomy List
#' 
#' Get all taxa list for a specific database.
#' 
#' @param db Database type ("gg" or "silva")
#' @param rank Taxonomic rank (optional)
#' @return Matrix containing taxonomy information with columns: taxonId, taxonName, rank, parentId, fullName
#' @export
#' @examples
#' taxa <- get_taxonomy_list("gg", "phylum")
#' print(taxa)
get_taxonomy_list <- function(db, rank = 'phylum') {
  if (!db %in% c("gg", "silva")) {
    stop("db must be either 'gg' or 'silva'")
  }
  
  endpoint <- paste0("/taxa/", db, "/list")
  if (!is.null(rank)) {
    endpoint <- paste0(endpoint, "?rank=", rank)
  }
  
  response <- make_request(endpoint)
  result <- parse_response(response)
  
  # Convert the list data to a matrix using the generic function
  if (length(result$data) > 0) {
    result <- convert_list_to_matrix(result$data, "taxonomy")
  } else {
    # Return empty matrix if no data
    result <- matrix(nrow = 0, ncol = 5, dimnames = list(NULL, c("taxonId", "taxonName", "rank", "parentId", "fullName")))
  }
  
  return(result)
}

#' Get Taxonomy Projects
#' 
#' Get all project accessions for a specific taxon.
#' 
#' @param db Database type ("gg" or "silva")
#' @param taxon_id Taxon ID (integer)
#' @return Vector containing project accessions for the taxon
#' @export
#' @examples
#' projects <- get_taxonomy_projects("gg", 15)
#' print(projects)
get_taxonomy_projects <- function(db, taxon_id) {
  if (!db %in% c("gg", "silva")) {
    stop("db must be either 'gg' or 'silva'")
  }
  if (!is.numeric(taxon_id) || length(taxon_id) != 1) {
    stop("taxon_id must be a single integer")
  }
  
  endpoint <- paste0("/taxa/", db, "/", taxon_id, "/project")
  response <- make_request(endpoint)
  result <- parse_response(response)
  
  # Convert projectAccessions to vector
  if (!is.null(result$data$projectAccessions)) {
    result$data$projectAccessions <- convert_list_to_matrix(
      result$data$projectAccessions, 
      "projectAccessions"
    )
  }
  
  return(unlist(result$data$projectAccessions))
}

#' Get Taxonomy Samples
#' 
#' Get all sample runs for a specific taxon.
#' 
#' @param db Database type ("gg" or "silva")
#' @param taxon_id Taxon ID (integer)
#' @return Vector containing sample run IDs for the taxon
#' @export
#' @examples
#' samples <- get_taxonomy_samples("gg", 15)
#' print(samples)
get_taxonomy_samples <- function(db, taxon_id) {
  if (!db %in% c("gg", "silva")) {
    stop("db must be either 'gg' or 'silva'")
  }
  if (!is.numeric(taxon_id) || length(taxon_id) != 1) {
    stop("taxon_id must be a single integer")
  }
  
  endpoint <- paste0("/taxa/", db, "/", taxon_id, "/sample")
  response <- make_request(endpoint)
  result <- parse_response(response)
  
  # Convert runs to vector
  if (!is.null(result$data$runs)) {
    result$data$runs <- convert_list_to_matrix(
      result$data$runs, 
      "runs"
    )
  }
  
  return(unlist(result$data$runs))
}

#' Get Taxonomy Statistics
#' 
#' Get detailed statistics for a specific taxon.
#' 
#' @param db Database type ("gg" or "silva")
#' @param taxon_id Taxon ID (integer)
#' @return List containing taxonomy statistics
#' @export
#' @examples
#' \dontrun{
#' stats <- get_taxonomy_stats("gg", 15)
#' print(stats)
#' }
get_taxonomy_stats <- function(db, taxon_id) {
  if (!db %in% c("gg", "silva")) {
    stop("db must be either 'gg' or 'silva'")
  }
  if (!is.numeric(taxon_id) || length(taxon_id) != 1) {
    stop("taxon_id must be a single integer")
  }
  
  endpoint <- paste0("/taxa/", db, "/", taxon_id, "/stats")
  response <- make_request(endpoint)
  result <- parse_response(response)
  
  # Use the generic function to process all list fields
  list_conversions <- list(
    "samples$runs" = "simple",
    "projects$details" = "matrix",
    "countries$distribution" = "matrix",
    "systems$distribution" = "matrix",
    "bodySites$distribution" = "matrix",
    "phenotypes$distribution" = "matrix",
    "variableRegions$distribution" = "matrix",
    "sequencingTypes$distribution" = "matrix"
  )
  
  return(process_response(result, list_conversions))
}

#' Get Taxonomy Parent
#' 
#' Get parent taxon information for a specific taxon.
#' 
#' @param db Database type ("gg" or "silva")
#' @param taxon_id Taxon ID (integer)
#' @return List containing taxonomy parent information
#' @export
#' @examples
#' parent <- get_taxonomy_parent("gg", 15)
#' print(parent)
get_taxonomy_parent <- function(db, taxon_id) {
  if (!db %in% c("gg", "silva")) {
    stop("db must be either 'gg' or 'silva'")
  }
  if (!is.numeric(taxon_id) || length(taxon_id) != 1) {
    stop("taxon_id must be a single integer")
  }
  
  endpoint <- paste0("/taxa/", db, "/", taxon_id, "/parent")
  response <- make_request(endpoint)
  result <- parse_response(response)

  # Handle NULL values by converting them to NA
  parent_data <- result$data$parent
  taxonId <- ifelse(is.null(parent_data$taxonId), NA, parent_data$taxonId)
  taxonName <- ifelse(is.null(parent_data$taxonName), NA, parent_data$taxonName)
  rank <- ifelse(is.null(parent_data$rank), NA, parent_data$rank)
  parentId <- ifelse(is.null(parent_data$parentId), NA, parent_data$parentId)
  fullName <- ifelse(is.null(parent_data$fullName), NA, parent_data$fullName)

  result <- matrix(c(taxonId, taxonName, rank, parentId, fullName), byrow = TRUE, nrow = 1, ncol = 5, dimnames = list(NULL, c("taxonId", "taxonName", "rank", "parentId", "fullName")))

  return(result)
}

#' Get Taxonomy Children
#' 
#' Get all child taxa for a specific taxon.
#' 
#' @param db Database type ("gg" or "silva")
#' @param taxon_id Taxon ID (integer)
#' @return List containing taxonomy children information
#' @export
#' @examples
#' children <- get_taxonomy_children("gg", 15)
#' print(children)
get_taxonomy_children <- function(db, taxon_id) {
  if (!db %in% c("gg", "silva")) {
    stop("db must be either 'gg' or 'silva'")
  }
  if (!is.numeric(taxon_id) || length(taxon_id) != 1) {
    stop("taxon_id must be a single integer")
  }
  
  endpoint <- paste0("/taxa/", db, "/", taxon_id, "/children")
  response <- make_request(endpoint)
  result <- parse_response(response)
  result <- convert_list_to_matrix(result$data$children, "children")
  return(result)
} 