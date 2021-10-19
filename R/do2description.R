#' Disease Ontology (DO) to Description
#'
#' @param do.id  A vector of DO IDs
#'
#' @return A \code{\link{data.frame}} with DO IDs and their descriptions.
#' @importFrom DO.db DOTERM
#'
#' @export
#'
#' @examples
#' do.ids <- sample_data$DO$ID[1:3]
#' do2description(do.ids)
do2description <- function(do.id){
  DOtable <- unique(as.data.frame(DOTERM)[,c(2,3)])
  colnames(DOtable) <- c("id", "description")
  #id <- grep(pattern = paste0( paste0("^",do.id,"$"), collapse = "|"), x = DOtable$id, )
  rownames(DOtable) <- DOtable[,1]
  out <- DOtable[do.id, ]
  rownames(out) <- NULL
  out$db <- "DO"
  return(out)
}


