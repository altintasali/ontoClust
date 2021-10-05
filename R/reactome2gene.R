#' Reactome to Gene ID
#'
#' @param reactome.id REACTOME ID (e.g. R-HSA-611105)
#'
#' @return A list object containing Gene IDs for every GO ID
#'
#' @importFrom reactome.db reactomePATHID2EXTID
#' @import magrittr
#' @export
#'
#' @examples
#' reactome.ids <- sample_data$REACTOME$ID[1:3]
#' reactome2gene(reactome.ids)
reactome2gene <- function(reactome.id){
  genes <- subset(reactomePATHID2EXTID, Lkeys = reactome.id) %>% as.list
  return(genes)
}

