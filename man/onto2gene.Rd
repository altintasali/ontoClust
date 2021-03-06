% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/onto2gene.R
\name{onto2gene}
\alias{onto2gene}
\title{Ontology ID to gene IDs}
\usage{
onto2gene(id, organism = "hsa")
}
\arguments{
\item{id}{ID of any ontology term}

\item{organism}{Organism identifier (required for GO annotations). Default is "hsa". Currently human (\code{hsa}), mouse (\code{mmu}) and rat (\code{rno}) are supported. Full list of organism IDs can be found [here](https://www.genome.jp/kegg/catalog/org_list.html).}
}
\value{
A \code{\link{list}} object containing gene IDs for each ontology ID
}
\description{
Ontology ID to gene IDs
}
\examples{
\dontrun{
id <- lapply(sample_data, function(x){head(x$ID)})
id <- do.call(c, id)
onto2gene(id)
}
}
