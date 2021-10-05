#' Reactome ID to Description
#'
#' @param reactome.id A vector of Reactome IDs
#'
#' @return A \code{\link{data.frame}} with Reactome IDs and their descriptions.
#'
#' @importFrom stringr str_split_fixed
#' @importFrom AnnotationDbi subset
#' @import reactome.db
#' @import magrittr
#' @export
#'
#' @examples
#' reactome.ids <- sample_data$REACTOME$ID[1:3]
#' reactome2description(reactome.ids)
reactome2description <- function(reactome.id){
  descs <- subset(reactomePATHID2NAME, Lkeys = reactome.id) %>% as.data.frame #%>% toTable
  colnames(descs) <- c("id", "description")
  descs$description <- str_split_fixed(string = descs$description, pattern = ": ", n = 2)[,2]
  return(descs)
}


