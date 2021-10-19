#' Create a heatmap of ontology clusters
#'
#' @param oc Output from \code{\link{getOntoClust}}
#' @param clusterColors A character \code{\link{vector}} with length of clusters (rows). If NULL (default), \code{\link{rainbow}} colors will be used.
#' @param cluster_cols Logical to cluster link communities (columns).
#' @param rowNames Display text for ontologies. It can be either
#' \describe{
#' \item{id}{Ontology ID}
#' \item{description}{Description of the ontology ID (default)}
#' }
#' @param filename Default NULL. If provided with .pdf or .png extensions, function generates a PDF or PNG output file.
#' @param silent Default FALSE. If TRUE, the function does not plot the heatmap to the graphics device.
#' @param cellwidth Width each cell. Default is 8.
#' @param cellheight Height each cell. Default is 8.
#' @param treeheight_row Height of the tree on rows (ontology clusters). Default is 30.
#' @param treeheight_col Height of the tree on columns (link communities). Default is 30.
#' @param legend Legend for black/white colors. Default is FALSE.
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
){
  ##------------------------------------------------------------------------
  ## Input matrix
  ##------------------------------------------------------------------------
  mat <- oc$LCmatrix

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
  desc <- oc$result
  rownames(desc) <- desc[,1]
  desc <- desc[, -c(1)]

  if(rowNames == "description"){
    # mID <- match(rownames(mat), oc$result$ontology)
    # id2print <- oc$result$description[mID]
    id2print <- onto2description(rownames(mat))$description
  }else if(rowNames == "id"){
    id2print <- NULL
  }else{
    stop("'rowNames' can either be 'description' or 'id'")
  }
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
           cluster_cols = cluster_cols,
           cellwidth = cellwidth,
           cellheight = cellheight,
           treeheight_row = treeheight_row,
           treeheight_col = treeheight_row,
           legend = legend,
           labels_row = id2print,
           silent = silent,
           filename = filename,
           ...
  )

}
