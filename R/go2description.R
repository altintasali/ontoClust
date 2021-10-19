#' Gene Ontology (GO) to Description
#'
#' @param go.id  A vector of GO IDs
#'
#' @return A \code{\link{data.frame}} with GO IDs and their descriptions.
#'
#' @importFrom AnnotationDbi select
#' @import GO.db
#' @export
#'
#' @examples
#' go.ids <- sample_data$GOBP$ID[1:3]
#' go2description(go.ids)
#'
go2description <- function(go.id){
  out <- suppressMessages(
    select(x = GO.db, keys = go.id, columns =c("GOID", "TERM", "ONTOLOGY"), keytype = "GOID")
  )
  colnames(out) <- c("id", "description", "db")
  out$db <- paste0("GO:", out$db)
  return(out)
}
