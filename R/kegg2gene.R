#' KEGG to Gene ID
#' @description Engine for \code{\link{kegg2gene}}
#' @param kegg.id KEGG ID
#'
#' @return A list object containing Gene IDs for every KEGG IDs
#'
#' @importFrom KEGGREST keggGet
#'
#' @examples
#' \dontrun{
#' kegg.ids <- sample_data$KEGG$ID[1]
#' ontoClust:::.kegg2gene(kegg.ids)
#' }
#'
.kegg2gene <- function(kegg.id){
  pw <- keggGet(kegg.id)
  if(is.null(pw[[1]]$GENE)) return(NA)
  pw2 <- pw[[1]]$GENE[c(TRUE,FALSE)] # may need to modify this to c(FALSE, TRUE) for other organisms
  pw2 <- unlist(lapply(strsplit(pw2, split = ";", fixed = T), function(x)x[1]))
  return(pw2)
}

#' KEGG to Gene ID
#'
#' @param kegg.id A vector of KEGG IDs
#'
#' @return A list object containing Gene IDs for every KEGG IDs
#' @export
#'
#' @examples
#' \dontrun{
#' kegg.ids <- sample_data$KEGG$ID[1:3]
#' kegg2gene(kegg.ids)
#' }
#'
kegg2gene <- function(kegg.id){
  out <- sapply(kegg.id, .kegg2gene)
}


