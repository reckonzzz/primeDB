#' Get Project List
#' 
#' Get all project accession list.
#' 
#' @return Character vector containing project accessions
#' @export
#' @examples
#' projects <- get_projects()
#' print(length(projects))
get_projects <- function() {
  response <- make_request("/projects/list")
  result <- parse_response(response)

  # Convert the list data to a vector using the generic function
  if (length(result) > 0) {
    result <- convert_list_to_matrix(result, "projectAccessions")
  } else {
    # Return empty vector if no data
    result <- character(0)
  }

  return(result)
}

#' Get Project Samples
#' 
#' Get all sample runs for specific project(s).
#' 
#' @param project_accession Project Accession ID(s) - can be a single string or vector of strings
#' @return Character vector containing sample run IDs for a single project or a list of vectors containing sample run IDs for multiple projects
#' @export
#' @examples
#' samples <- get_project_samples("PRJDB13875")
#' print(length(samples))
#' samples <- get_project_samples(c("PRJDB13875", "PRJDB16847"))
#' print(length(samples))
get_project_samples <- function(project_accession) {
  if (!is.character(project_accession)) {
    stop("project_accession must be a character string or vector of character strings")
  }
  
  # single project
  if (length(project_accession) == 1) {
  endpoint <- paste0("/projects/", project_accession, "/sample")
    response <- make_request(endpoint)
    result <- parse_response(response)
    # return runs vector
    return(if (!is.null(result$data)) unlist(result$data) else character(0))
  }

  # multiple projects
  res_list <- vector("list", length(project_accession))
  names(res_list) <- rep(NA_character_, length(project_accession))
  for (i in seq_along(project_accession)) {
    endpoint <- paste0("/projects/", project_accession[i], "/sample")
    response <- make_request(endpoint)
    result <- parse_response(response)
    # named by projectAccession, content is runs vector
    pname <- if (!is.null(result$projectAccession)) result$projectAccession else project_accession[i]
    res_list[[i]] <- if (!is.null(result$data)) unlist(result$data) else character(0)
    names(res_list)[i] <- pname
  }
  return(res_list)
}

#' Get Project Statistics
#' 
#' Get detailed statistics for a specific project.
#' 
#' @param project_accession Project Accession ID
#' @return List containing project statistics
#' @export
#' @examples
#' \dontrun{
#' stats <- get_project_stats("PRJDB13875")
#' print(stats)
#' }
get_project_stats <- function(project_accession) {
  if (!is.character(project_accession) || length(project_accession) != 1) {
    stop("project_accession must be a single character string")
  }
  
  endpoint <- paste0("/projects/", project_accession, "/stats")
  response <- make_request(endpoint)
  result <- parse_response(response)

  # Use the generic function to process all list fields
  list_conversions <- list(
    "countries$distribution" = "matrix",
    "phenotypes$distribution" = "matrix",
    "systems$distribution" = "matrix",
    "bodySites$distribution" = "matrix",
    "variableRegions$distribution" = "matrix",
    "sequencingTypes$distribution" = "matrix"
  )

  return(process_response(result, list_conversions))
}
