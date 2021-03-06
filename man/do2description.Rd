% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/do2description.R
\name{do2description}
\alias{do2description}
\title{Disease Ontology (DO) to Description}
\usage{
do2description(do.id)
}
\arguments{
\item{do.id}{A vector of DO IDs}
}
\value{
A \code{\link{data.frame}} with DO IDs and their descriptions.
}
\description{
Disease Ontology (DO) to Description
}
\examples{
do.ids <- sample_data$DO$ID[1:3]
do2description(do.ids)
}
