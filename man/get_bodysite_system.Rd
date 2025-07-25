% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bodysites.R
\name{get_bodysite_system}
\alias{get_bodysite_system}
\title{Get Body Site System}
\usage{
get_bodysite_system(body_site_id)
}
\arguments{
\item{body_site_id}{Body site ID (integer)}
}
\value{
Character of the full system name for the body site
}
\description{
Get system information for a specific body site.
}
\examples{
system <- get_bodysite_system(6)
print(system)
}
