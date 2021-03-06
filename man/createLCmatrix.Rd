% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getOntoClust.R
\name{createLCmatrix}
\alias{createLCmatrix}
\title{Create a binary matrix of LC members}
\usage{
createLCmatrix(lc, includeOutlierLC = FALSE)
}
\arguments{
\item{lc}{Output of \code{\link{getLC}}}

\item{includeOutlierLC}{Include the ontology terms that are not part of a link community. Default FALSE}
}
\value{
Binary LC membership \code{\link{matrix}}. Rows: ontology terms, columns: link communities.
}
\description{
Create a binary matrix of LC members
}
\examples{
\dontrun{
ontology.id <- sample_data$GOBP$ID[1:100]
network <- createOntologyNetwork(ontology.id, method = "jaccard", weighted = FALSE)
lc <- getLC(network)
createLCmatrix(lc)
}
}
