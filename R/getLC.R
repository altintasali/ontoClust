#' Get Link Communities
#'
#' @param network A \code{\link{data.frame}} representing network with columns "source", "target" and "weight" (optional).
#' @param hcmethod Hierarchical clustering method. See \code{\link{hclust}} for details. Recommended to use "single" for unweighted networks and "ward.D2" for weighted networks.
#' @param verbose Print detailed output. Default is TRUE.
#'
#' @return An object of class \code{\link{linkcomm}}. See \code{\link{getLinkCommunities}} for details.
#' @export
#'
#' @importFrom linkcomm getLinkCommunities
#'
#' @examples
#' \dontrun{
#' ontology.id <- sample_data$GOBP$ID[1:100]
#' network <- createOntologyNetwork(ontology.id, method = "jaccard", weighted = FALSE)
#' lc <- getLC(network)
#' lc
#' }
getLC <- function(network, hcmethod = "single", verbose = TRUE){
  lc <- getLinkCommunities(network, hcmethod, plot = FALSE, verbose = verbose)
  return(lc)
}
