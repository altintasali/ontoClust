#' Create a heatmap of ontology clusters
#'
#' @param oc Output from \code{\link{getOntoClust}}
#' @param clusterColors A character \code{\link{vector}} with length of clusters (rows). If NULL (default), \code{\link{rainbow}} colors will be used.
#' @param clusterLC Logical to cluster link communities (columns).
#' @param rowNames Display text for ontologies. It can be either
#' \describe{
#' \item{id}{Ontology ID}
#' \item{description}{Description of the ontology ID (default)}
#' }
#' @param filename Default NULL. If provided with .pdf or .png extensions, function generates a PDF or PNG output file.
#' @param silent Default FALSE. If TRUE, the function does not plot the heatmap to the graphics device.
#' @param ... Additional parameters for \code{\link{pheatmap}}
#'
#' @return \code{\link{pheatmap}} object
#'
#' @export
#' @importFrom pheatmap pheatmap
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRampPalette rainbow
#' @importFrom stats dist hclust quantile
#'
#' @examples
#' ontology.id <- sample_data$GOBP$ID[1:50]
#' network <- createOntologyNetwork(ontology.id, method = "jaccard", weighted = FALSE)
#' lc <- getLC(network)
#' oc <- getOntoClust(lc)
#' heatmapOntoClust(oc)
heatmapOntoClust <- function(oc,
                          clusterColors = NULL,
                          clusterLC = FALSE,
                          rowNames = c("id", "description")[2],
                          filename = NA,
                          silent = FALSE,
                          ...
){
  ##------------------------------------------------------------------------
  ## Input matrix
  ##------------------------------------------------------------------------
  mat <- oc$mat

  ##------------------------------------------------------------------------
  ## Heatmap colors
  ##------------------------------------------------------------------------
  lout <- 3
  quantile.range <- unlist(quantile(mat[,1:2], probs = seq(0, 1, 0.01), na.rm=T))
  hmBreaks <- seq(-max(abs(range(mat))), max(abs(range(mat))), length.out = lout)
  hmBreaks <- unique(hmBreaks)
  hmCols <- colorRampPalette(colors=c("white", "black"))(length(hmBreaks)-1)

  ##------------------------------------------------------------------------
  ## Annotation
  ##------------------------------------------------------------------------
  rowAnnot <- data.frame(Cluster = oc$ontology_clusters)
  rowAnnot$Cluster <- as.factor(rowAnnot$Cluster)

  ##------------------------------------------------------------------------
  ## Colors
  ##------------------------------------------------------------------------
  if(is.null(clusterColors)){
    rowCols <- rainbow(length(unique(oc$ontology_clusters))) #brewer.pal(n = length(unique(oc$ontology_clusters)), name = "Set1")
    names(rowCols) <- seq_along((unique(oc$ontology_clusters)))
  }else{
    rowCols <- clusterColors
  }

  annotCols <- list(Cluster = rowCols)

  ##------------------------------------------------------------------------
  ## Row/Column names
  ##------------------------------------------------------------------------
  desc <- go2description(rownames(mat))
  rownames(desc) <- desc[,1]
  desc <- desc[, -c(1)]

  ##------------------------------------------------------------------------
  ## pheatmap
  ##------------------------------------------------------------------------
  pheatmap(mat = mat,
                  color = hmCols,
                  clustering_method = "ward.D2",
                  clustering_distance_rows = "binary",
                  clustering_distance_cols = "binary",
                  annotation_row = rowAnnot,
                  annotation_colors = annotCols,
                  cluster_cols = clusterLC,
                  cellwidth = 8,
                  cellheight = 8,
                  treeheight_row = 50 * .6,
                  treeheight_col = 50 * .6,
                  legend = FALSE,
                  labels_row = desc$TERM,
                  silent = silent,
                  filename = filename
  )

}
