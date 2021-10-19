#' Create ontology network
#' @description Creates a network of ontology terms based on the \code{method} provided.
#'
#' @param ontology.id A character \code{\link{vector}} of ontology ID from supported ontology databases.
#' @param weighted Weight calculated by \code{method}. If FALSE (default), no weight is calculated.
#' @param method Calculated similarity between 2 ontology terms based on the \code{method}. Available methods are:
#' \describe{
#' \item{jaccard}{Jaccard Index}
#' \item{intersect}{Number of intersecting genes}
#' }
#' @param organism Organism identifier (required for GO annotations). Default is "hsa". Currently human (\code{hsa}), mouse (\code{mmu}) and rat (\code{rno}) are supported. Full list of organism IDs can be found [here](https://www.genome.jp/kegg/catalog/org_list.html).
#'
#' @return A \code{\link{data.frame}} with columns:
#' \describe{
#' \item{source}{Source node}
#' \item{target}{Target node}
#' \item{weight}{Weight of edge}
#' }
#'
#' @importFrom reshape2 melt
#' @export
#'
#' @examples
#' ontology.id <- sample_data$GOBP$ID[1:10]
#' createOntologyNetwork(ontology.id, method = "jaccard")

createOntologyNetwork <- function(ontology.id, weighted = FALSE, method = "jaccard", organism = "hsa"){
  ##------------------------------------------------------------------------
  ## Read ontology >> gene data
  ##------------------------------------------------------------------------
  geneData <- onto2gene(ontology.id, organism = organism)
  geneData <- geneData[!is.na(names(geneData))]

  ##------------------------------------------------------------------------
  ## Create network
  ##------------------------------------------------------------------------
  if(method == "jaccard"){
    ids <- matrix(1:length(geneData)^2, ncol = length(geneData))
    ids <- reshape2::melt(upper.tri(ids))
    ids <- ids[ids$value == TRUE, ]
    #ids <- expand.grid(seq_along(geneData), seq_along(geneData))

    network <- apply(ids, MARGIN = 1, function(x){
      networkWeight <- jaccardIndex(geneData[[x[1]]], geneData[[x[2]]])
      out <- data.frame(source = names(geneData)[x[1]],
                        target = names(geneData)[x[2]],
                        weight = networkWeight)
      return(out)
    }
    )
    network <- do.call(rbind, network)

  }else if(method == "intersect"){
    ids <- matrix(1:length(geneData)^2, ncol = length(geneData))
    ids <- reshape2::melt(upper.tri(ids))
    ids <- ids[ids$value == TRUE, ]

    network <- apply(ids, MARGIN = 1, function(x){
      networkWeight <- length(intersect(geneData[[x[1]]], geneData[[x[2]]]))
      out <- data.frame(source = names(geneData)[x[1]],
                        target = names(geneData)[x[2]],
                        weight = networkWeight)
      return(out)
    }
    )

    network <- do.call(rbind, network)
  }

  ##------------------------------------------------------------------------
  ## Remove 0 weights
  ##------------------------------------------------------------------------
  network <- network[network$weight > 0,]

  ##------------------------------------------------------------------------
  ## Keep weights
  ##------------------------------------------------------------------------
  if(!weighted){
    network <- network[, -3]
  }

  return(network)
}
