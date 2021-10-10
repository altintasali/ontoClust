#' Calculate Jaccard Index
#'
#' @description Calculates Jaccard Index (length of intersection divided by length of union) for 2 given sets.
#' @param x Set 1 \code{\link{vector}}
#' @param y Set 1 \code{\link{vector}}
#'
#' @return Jaccard Index
#' @export
#'
#' @examples
#' id.x <- sample_data$GOBP$ID[1]
#' id.y <- sample_data$GOBP$ID[2]
#'
#' x <- go2gene(id.x)
#' y <- go2gene(id.y)
#'
#' jaccardIndex(x, y)
jaccardIndex <- function(x, y){
  jc <- length(intersect(x,y))/length(union(x,y))
  return(jc)
}
