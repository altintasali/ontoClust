% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/heatmapOntoClust.R
\name{heatmapOntoClust}
\alias{heatmapOntoClust}
\title{Create a heatmap of ontology clusters}
\usage{
heatmapOntoClust(
  oc,
  clusterColors = NULL,
  cluster_cols = FALSE,
  rowNames = c("id", "description")[2],
  cellwidth = 8,
  cellheight = 8,
  treeheight_row = 30,
  treeheight_col = 30,
  legend = FALSE,
  filename = NA,
  silent = FALSE,
  ...
)
}
\arguments{
\item{oc}{Output from \code{\link{getOntoClust}}}

\item{clusterColors}{A character \code{\link{vector}} with length of clusters (rows). If NULL (default), \code{\link{rainbow}} colors will be used.}

\item{cluster_cols}{Logical to cluster link communities (columns).}

\item{rowNames}{Display text for ontologies. It can be either
\describe{
\item{id}{Ontology ID}
\item{description}{Description of the ontology ID (default)}
}}

\item{cellwidth}{Width each cell. Default is 8.}

\item{cellheight}{Height each cell. Default is 8.}

\item{treeheight_row}{Height of the tree on rows (ontology clusters). Default is 30.}

\item{treeheight_col}{Height of the tree on columns (link communities). Default is 30.}

\item{legend}{Legend for black/white colors. Default is FALSE.}

\item{filename}{Default NULL. If provided with .pdf or .png extensions, function generates a PDF or PNG output file.}

\item{silent}{Default FALSE. If TRUE, the function does not plot the heatmap to the graphics device.}

\item{...}{Additional parameters for \code{\link{pheatmap}}}
}
\value{
\code{\link{pheatmap}} object
}
\description{
Create a heatmap of ontology clusters
}
\examples{
ontology.id <- sample_data$GOBP$ID[1:50]
network <- createOntologyNetwork(ontology.id, method = "jaccard", weighted = FALSE)
lc <- getLC(network)
oc <- getOntoClust(lc)
heatmapOntoClust(oc)
}
