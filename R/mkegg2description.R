#' MKEGG ID to Description
#' @description Engine for \code{\link{mkegg2description}}
#'
#' @param mkegg.id MKEGG ID
#'
#' @return A \code{\link{data.frame}} with MKEGG IDs and their descriptions.
#'
#' @importFrom KEGGREST keggGet
#'
#' @examples
#' \dontrun{
#' mkegg.ids <- sample_data$MKEGG$ID[1:3]
#' ontoClust:::.kegg2description(mkegg.ids)
#' }
.mkegg2description <- function(mkegg.id){
  pw <- keggGet(mkegg.id)
  if(is.null(pw[[1]]$ENTRY)) return(NA)
  pw2 <- pw[[1]]$NAME
  return(pw2)
}

#' MKEGG ID to Description
#'
#' @param mkegg.id A vector of MKEGG IDs
#'
#' @return A \code{\link{data.frame}} with MKEGG IDs and its descriptions.
#' @export
#'
#' @examples
#' \dontrun{
#' mkegg.ids <- sample_data$MKEGG$ID[1:3]
#' mkegg2description(mkegg.ids)
#' }
mkegg2description <- function(mkegg.id){
  id_names <- sapply(mkegg.id, .mkegg2description)
  out <- data.frame(id = mkegg.id, description = id_names)
  rownames(out) <- NULL
  return(out)
}

