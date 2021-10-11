#' Get clusters of link communitites
#'
#' @param lc Output of \code{\link{getLC}}
#' @param minClusterSize Minimum number of members in a cluster. Default is 3.
#'
#' @return A \code{\link{list}} object with
#' \describe{
#' \item{mat}{Binary matrix. Rows: ontologies, columns: link communities}
#' \item{ontoClust}{data.frame with columns of ontology IDs and link communities}
#' \item{dist}{Distance matrix of \code{mat}}
#' \item{hc}{\code{\link{hclust} object}
#' }
#' @export
#'
#' @importFrom dynamicTreeCut cutreeDynamic
#' @examples
#' \dontrun{
#' ontology.id <- sample_data$GOBP$ID[1:100]
#' network <- createOntologyNetwork(ontology.id, method = "jaccard", weighted = FALSE)
#' lc <- getLC(network)
#' oc <- getOntoClust(lc)
#' }
getOntoClust <- function(lc, minClusterSize = 3, verbose = TRUE){
  ##------------------------------------------------------------------------
  #TODO: make class and report less
  ##------------------------------------------------------------------------

  ##------------------------------------------------------------------------
  ## Verbosity
  ##------------------------------------------------------------------------
  if(verbose){
    verb <- 2
  }else{
    verb <- 0
  }

  ##------------------------------------------------------------------------
  ## Create binary matrix
  ##------------------------------------------------------------------------
  mat <- reshape2::dcast(lc$nodeclusters, node ~ cluster)
  rowname <- mat[,1]
  mat <- mat[,-1]
  mat[!is.na(mat)] <- 1
  mat[is.na(mat)] <- 0
  mat <- apply(mat, 2, as.numeric)
  rownames(mat) <- rowname
  mat <- mat[, unique(lc$nodeclusters$cluster)]

  ##------------------------------------------------------------------------
  ## Calculate clusters
  ##------------------------------------------------------------------------
  distmat <- dist(mat, method = "binary")
  hc <- hclust(distmat, method = "ward.D2")

  clusts <- cutreeDynamic(dendro = hc,
                          minClusterSize = minClusterSize,
                          distM =  as.matrix(distmat),
                          method = c("hybrid","tree")[1],
                          verbose = verb)
  names(clusts) <- rownames(as.matrix(distmat))

  out <- list(mat = mat,
              ontoClust = clusts,
              dist = distmat,
              hc = hc)

  return(out)
}
