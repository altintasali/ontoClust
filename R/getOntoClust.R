#' Get clusters of link communities
#'
#' @param lc Output of \code{\link{getLC}}
#' @param minClusterSize Minimum number of members in a cluster. Default is 3.
#' @param verbose Print detailed output. Default is TRUE.
#'
#' @return A \code{\link{list}} object with
#' \item{\code{mat}}{Binary matrix. Rows: ontologies, columns: link communities}
#' \item{\code{ontoClust}}{data.frame with columns of ontology IDs and link communities}
#' \item{\code{dist}}{Distance matrix of \code{mat}}
#' \item{\code{hc}}{\code{\link{hclust}} object}
#'
#' @export
#'
#' @importFrom dynamicTreeCut cutreeDynamic
#' @importFrom data.table as.data.table setnames setkey `:=`
#'
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
  mat <- createLCmatrix(lc)

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


  ##------------------------------------------------------------------------
  ## Create output
  ##------------------------------------------------------------------------
  res <- as.data.table(lc$nodeclusters)
  res <- res[, paste(get("cluster"), collapse = ","), by = "node"]

  clust_df <- data.frame(node = names(clusts), clust = clusts)
  res <- merge(res, clust_df, by = "node")
  setnames(res, c("ontology", "linkcommunity", "cluster"))
  setkey(res, "cluster")
  res[, eval("description") := go2description(get("ontology"))[,2]]

  out <- list(result = as.data.frame(res),
              link_communities = lc$nodeclusters,
              ontology_clusters = clusts,
              LCmatrix = mat,
              lc = lc,
              dist = distmat,
              hc = hc)

  return(out)
}


#' Create a binary matrix of LC members
#'
#' @param lc Output of \code{\link{getLC}}
#' @param includeOutlierLC Include the ontology terms that are not part of a link community. Default FALSE
#'
#' @return Binary LC membership \code{\link{matrix}}. Rows: ontology terms, columns: link communities.
#' @importFrom reshape2 dcast
#'
#' @examples
#' \dontrun{
#' ontology.id <- sample_data$GOBP$ID[1:100]
#' network <- createOntologyNetwork(ontology.id, method = "jaccard", weighted = FALSE)
#' lc <- getLC(network)
#' createLCmatrix(lc)
#' }

createLCmatrix <- function(lc, includeOutlierLC = FALSE){
  mat <- reshape2::dcast(lc$nodeclusters, node ~ cluster)
  rowname <- mat[,1]
  mat <- mat[,-1]
  mat[!is.na(mat)] <- 1
  mat[is.na(mat)] <- 0
  mat <- apply(mat, 2, as.numeric)
  rownames(mat) <- rowname
  mat <- mat[, unique(lc$nodeclusters$cluster)]

  # TODO: includeOutlierLC
  # if(noLCmember){
  #   notLinked <- dt1$GOdefinition[!dt1$GOdefinition %in% rowname] %>% unique
  #   mat_notLinked <- matrix(data = 0, nrow = length(notLinked), ncol = ncol(mat))
  #   rownames(mat_notLinked) <- notLinked
  #   mat <- rbind(mat, mat_notLinked)
  #
  # }
  return(mat)
}
