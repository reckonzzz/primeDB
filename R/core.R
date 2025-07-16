#' PRIME API Base URL
#' 
#' @return Character string containing the base URL for PRIME API
api_base_url <- function() {
  "https://primedb.sjtu.edu.cn/api/v1"
}

#' Make HTTP Request to PRIME API
#' 
#' @param endpoint API endpoint path
#' @param method HTTP method (GET, POST)
#' @param body Request body for POST requests
#' @param timeout Request timeout in seconds
#' @return API response
make_request <- function(endpoint, method = "GET", body = NULL, timeout = 30) {
  url <- paste0(api_base_url(), endpoint)
  
  if (method == "GET") {
    response <- httr::GET(url, httr::timeout(timeout))
  } else if (method == "POST") {
    response <- httr::POST(url, 
                          body = jsonlite::toJSON(body, auto_unbox = TRUE),
                          httr::content_type("application/json"),
                          httr::timeout(timeout))
  } else {
    stop("Unsupported HTTP method: ", method)
  }
  
  return(response)
}

#' Parse API Response
#' 
#' @param response HTTP response object
#' @return Parsed JSON response
parse_response <- function(response) {
  if (httr::http_error(response)) {
    content <- httr::content(response, "text", encoding = "UTF-8")
    content <- trimws(content)  # 去除前后空白
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
  content <- httr::content(response, "text", encoding = "UTF-8")
  content <- trimws(content)
  parsed <- jsonlite::fromJSON(content, simplifyVector = FALSE)
  if (!is.null(parsed$success) && !parsed$success) {
    stop(parsed$message)
  }
  # 去除success和message字段
  parsed$success <- NULL
  parsed$message <- NULL
  
  # 如果结果包含count字段，去掉count并直接返回数据字段的内容
  if (!is.null(parsed$count)) {
    # 找到除了count之外的其他字段
    data_fields <- names(parsed)[names(parsed) != "count"]
    if (length(data_fields) == 1) {
      # 如果只有一个数据字段，直接返回该字段的内容
      return(parsed[[data_fields[1]]])
    } else if (length(data_fields) > 1) {
      # 如果有多个数据字段，去掉count后返回其余字段
      parsed$count <- NULL
      return(parsed)
    }
  }
  
  # 如果只剩下一个list元素，直接返回该list
  non_null <- parsed[!sapply(parsed, is.null)]
  if (length(non_null) == 1 && is.list(non_null[[1]])) {
    return(non_null[[1]])
  }
  return(parsed)
}

#' Health Check
#' 
#' Check API service status and database connectivity.
#' 
#' @return List containing health check information
#' @export
#' @examples
#' health <- health_check()
#' print(health$status)
health_check <- function() {
  response <- make_request("/health")
  return(parse_response(response))
}

#' Get Database Statistics
#' 
#' Get comprehensive database statistics and overview.
#' 
#' @return List containing database statistics
#' @export
#' @examples
#' stats <- get_stats()
#' print(stats$data$basicStats$totalProjects)
get_stats <- function() {
  response <- make_request("/stats")
  result <- parse_response(response)
  
  # Convert distribution data to matrices
  if (!is.null(result$data$distributions)) {
    # Convert each distribution field to matrix
    distribution_fields <- c("systems", "bodySites", "countries", "phenotypes", 
                           "variableRegions", "sequencingTypes", "continents", 
                           "instruments", "hostSex")
    
    for (field in distribution_fields) {
      if (!is.null(result$data$distributions[[field]])) {
        result$data$distributions[[field]] <- convert_list_to_matrix(
          result$data$distributions[[field]], 
          field
        )
      }
    }
  }
  
  return(result)
}

#' Convert API Response Lists to Matrix Format
#' 
#' A utility function to convert API response lists to matrix format.
#' Handles different types of list structures:
#' - Simple lists (e.g., runs) are converted to vectors
#' - Object lists (e.g., details, distribution) are converted to matrices
#' 
#' @param data_list The list data from API response
#' @param list_name Name of the list field for error messages
#' @param simple_fields Character vector of field names that should be treated as simple lists
#' @return Converted data (vector for simple lists, matrix for object lists)
convert_list_to_matrix <- function(data_list, list_name = "data", simple_fields = NULL) {
  # Return NULL if data_list is NULL or empty
  if (is.null(data_list) || length(data_list) == 0) {
    return(NULL)
  }
  
  # Check if this is a simple list (all elements are atomic)
  is_simple_list <- all(sapply(data_list, function(x) {
    is.atomic(x) && length(x) == 1
  }))
  
  # If it's a simple list and matches simple_fields, convert to vector
  if (is_simple_list && !is.null(simple_fields)) {
    # Check if all elements have the same structure
    first_element <- data_list[[1]]
    if (is.list(first_element) && all(names(first_element) %in% simple_fields)) {
      # Convert to matrix first, then to vector if appropriate
      result_matrix <- do.call(rbind, lapply(data_list, function(x) {
        unlist(x[simple_fields])
      }))
      
      # If only one column, return as vector
      if (ncol(result_matrix) == 1) {
        return(as.vector(result_matrix))
      } else {
        # Set column names
        colnames(result_matrix) <- simple_fields
        return(result_matrix)
      }
    }
  }
  
  # For object lists, convert to matrix
  if (is.list(data_list[[1]]) && length(data_list[[1]]) > 0) {
    # Get all unique field names from all objects
    all_fields <- unique(unlist(lapply(data_list, names)))
    
    # Convert to matrix
    result_matrix <- do.call(rbind, lapply(data_list, function(x) {
      # Create a vector with all fields, filling missing values with NA
      values <- rep(NA, length(all_fields))
      names(values) <- all_fields
      
      # Fill in available values
      for (field in names(x)) {
        if (field %in% all_fields) {
          values[field] <- x[[field]]
        }
      }
      
      return(values)
    }))
    
    # Set column names
    colnames(result_matrix) <- all_fields
    return(result_matrix)
  }
  
  # For simple atomic lists, return as vector
  if (is_simple_list) {
    return(unlist(data_list))
  }
  
  # If none of the above conditions match, return as is
  warning(paste("Could not determine conversion method for", list_name, "- returning as is"))
  return(data_list)
}

#' Process API Response with List Conversions
#' 
#' Process an API response and convert specified list fields to matrix/vector format.
#' 
#' @param response The parsed API response
#' @param list_conversions List specifying which fields to convert and how
#'   - For simple lists: list(field_name = "simple")
#'   - For object lists: list(field_name = "matrix")
#'   - For specific simple fields: list(field_name = c("field1", "field2"))
#' @return Processed response with converted fields
process_response <- function(response, list_conversions = NULL) {
  if (is.null(list_conversions)) {
    return(response)
  }
  
  # Process nested fields in the response
  for (field_name in names(list_conversions)) {
    conversion_type <- list_conversions[[field_name]]
    
    # Handle nested structures like samples$runs, projects$details, etc.
    if (grepl("\\$", field_name)) {
      parts <- strsplit(field_name, "\\$")[[1]]
      parent_field <- parts[1]
      child_field <- parts[2]
      
      if (!is.null(response[[parent_field]]) && !is.null(response[[parent_field]][[child_field]])) {
        if (conversion_type == "simple") {
          response[[parent_field]][[child_field]] <- convert_list_to_matrix(
            response[[parent_field]][[child_field]], 
            child_field
          )
        } else if (conversion_type == "matrix") {
          response[[parent_field]][[child_field]] <- convert_list_to_matrix(
            response[[parent_field]][[child_field]], 
            child_field
          )
        } else if (is.character(conversion_type)) {
          response[[parent_field]][[child_field]] <- convert_list_to_matrix(
            response[[parent_field]][[child_field]], 
            child_field,
            simple_fields = conversion_type
          )
        }
      }
    } else {
      # Handle direct fields in data
      if (!is.null(response$data) && !is.null(response$data[[field_name]])) {
        if (conversion_type == "simple") {
          response$data[[field_name]] <- convert_list_to_matrix(
            response$data[[field_name]], 
            field_name
          )
        } else if (conversion_type == "matrix") {
          response$data[[field_name]] <- convert_list_to_matrix(
            response$data[[field_name]], 
            field_name
          )
        } else if (is.character(conversion_type)) {
          response$data[[field_name]] <- convert_list_to_matrix(
            response$data[[field_name]], 
            field_name,
            simple_fields = conversion_type
          )
        }
      }
    }
  }
  
  return(response)
} 