#' Example Usage of Generic List Conversion Functions
#' 
#' This file demonstrates how to use the generic functions
#' `convert_list_to_matrix` and `process_response`
#' to handle different types of API response list structures.
#' 
#' @examples
#' \dontrun{
#' # Example 1: Simple list conversion (like runs)
#' response <- make_request("/some/endpoint")
#' result <- parse_response(response)
#' 
#' # Convert runs to matrix with specific fields
#' if (!is.null(result$data$runs)) {
#'   result$data$runs <- convert_list_to_matrix(
#'     result$data$runs,
#'     "runs",
#'     simple_fields = c("run", "projectAccession")
#'   )
#' }
#' 
#' # Example 2: Object list conversion (like details, distribution)
#' if (!is.null(result$data$details)) {
#'   result$data$details <- convert_list_to_matrix(
#'     result$data$details,
#'     "details"
#'   )
#' }
#' 
#' # Example 3: Using process_response for multiple conversions
#' list_conversions <- list(
#'   runs = c("run", "projectAccession"),
#'   details = "matrix",
#'   distribution = "matrix",
#'   projectAccessions = "simple"
#' )
#' 
#' processed_result <- process_response(result, list_conversions)
#' }
#' 
#' # Example 4: For stats endpoints with multiple list fields
#' # This is useful for endpoints like /bodysites/:id/stats
#' # that return multiple list structures
#' 
#' process_stats_response <- function(response) {
#'   list_conversions <- list(
#'     runs = c("run", "projectAccession"),
#'     details = "matrix",
#'     distribution = "matrix",
#'     projectAccessions = "simple"
#'   )
#'   
#'   return(process_response(response, list_conversions))
#' }
#' 
#' # Example 5: For simple list endpoints
#' # This is useful for endpoints that return simple lists
#' 
#' process_simple_list_response <- function(response, field_name) {
#'   if (!is.null(response$data[[field_name]])) {
#'     response$data[[field_name]] <- convert_list_to_matrix(
#'       response$data[[field_name]],
#'       field_name
#'     )
#'   }
#'   return(response)
#' }
#' 
#' # Example 6: For object list endpoints
#' # This is useful for endpoints that return lists of objects
#' 
#' process_object_list_response <- function(response, field_name) {
#'   if (!is.null(response$data[[field_name]])) {
#'     response$data[[field_name]] <- convert_list_to_matrix(
#'       response$data[[field_name]],
#'       field_name
#'     )
#'   }
#'   return(response)
#' }