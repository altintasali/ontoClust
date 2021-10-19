#' KEGG ID to Description
#' @description Engine for \code{\link{kegg2description}}
#'
#' @param kegg.id KEGG ID
#'
#' @return A \code{\link{data.frame}} with KEGG IDs and their descriptions.
#'
#' @importFrom KEGGREST keggGet
#'
#' @examples
#' \dontrun{
#' kegg.ids <- sample_data$KEGG$ID[1:3]
#' ontoClust:::.kegg2description(kegg.ids)
#' }
.kegg2description <- function(kegg.id){
  pw <- keggGet(kegg.id)
  if(is.null(pw[[1]]$PATHWAY_MAP)) return(NA)
  pw2 <- pw[[1]]$PATHWAY_MAP
  return(pw2)
}

#' KEGG ID to Description
#'
#' @param kegg.id A vector of KEGG IDs
#'
#' @return A \code{\link{data.frame}} with KEGG IDs and their descriptions.
#' @export
#'
#' @examples
#' \dontrun{
#' kegg.ids <- sample_data$KEGG$ID[1:3]
#' kegg2description(kegg.ids)
#' }
kegg2description <- function(kegg.id){
  id_names <- sapply(kegg.id, .kegg2description)
  out <- data.frame(id = kegg.id, description = id_names)
  rownames(out) <- NULL
  out$db <- "KEGG"
  return(out)
}

