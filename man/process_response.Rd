% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/core.R
\name{process_response}
\alias{process_response}
\title{Process API Response with List Conversions}
\usage{
process_response(response, list_conversions = NULL)
}
\arguments{
\item{response}{The parsed API response}

\item{list_conversions}{List specifying which fields to convert and how
\itemize{
\item For simple lists: list(field_name = "simple")
\item For object lists: list(field_name = "matrix")
\item For specific simple fields: list(field_name = c("field1", "field2"))
}}
}
\value{
Processed response with converted fields
}
\description{
Process an API response and convert specified list fields to matrix/vector format.
}
