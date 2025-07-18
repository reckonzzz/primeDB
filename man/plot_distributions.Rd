% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualization.R
\name{plot_distributions}
\alias{plot_distributions}
\title{Plot All Distribution Pie Charts}
\usage{
plot_distributions(stats_data, plot_names = NULL)
}
\arguments{
\item{stats_data}{Statistics data list containing multiple distribution fields}

\item{plot_names}{Optional vector of distribution names to plot (default: all available)}
}
\value{
A combined ggplot2 object if patchwork is available, otherwise a list of ggplot2 objects
}
\description{
This function automatically plots pie charts for all distribution fields in the statistics data.
}
\examples{
data <- get_project_stats('PRJDB13875')
plot_distributions(data)
plot_distributions(data, plot_names = c('countries', 'systems'))

}
