#' Title
#'
#' @param id ID of any ontology term
#'
#' @return A \code{\link{list}} object containing gene IDs for each ontology ID
#' @export
#' @importFrom Biobase testBioCConnection
#' @import magrittr
#'
#' @examples
#' \dontrun{
#' ids <- sample_data$KEGG$ID[1:5]
#' onto2gene(ids)
#' }
#TODO: add organism support
onto2gene <- function(id){
  if(!is.character(id)){
    stop("No valid ID provided. 'id' should be character vector.")
  }

  dat <- data.frame(db = detectOntoDB(id), id)
  dat <- split(dat$id, f = dat$db)

  if(length(dat) == 0){
    stop("No valid ontology ID provided. Check your input!")
  }

  if(names(dat) == "DO"){
    genes <- do2gene(dat$DO)
    descs <- do2description(dat$DO)
  }else if(names(dat) == "GO"){
    genes <- go2gene(dat$GO, organism = "hsa")
    descs <- go2description(dat$GO)
  }else if(names(dat) == "KEGG"){
    if(testBioCConnection()){
      genes <- kegg2gene(dat$KEGG)
      descs <- kegg2description(dat$KEGG)
    }else{
      stop("No internet connection detected. KEGGREST API requires internet connection to retrieve gene IDs.")
    }
  }else if(names(dat) == "MKEGG"){
    message("MKEGG is not supported yet.")
    #TODO: Support MKEGG
    genes <- NA
    descs <- mkegg2description(dat$MKEGG)
  }else if(names(dat) == "REACTOME"){
    genes <- reactome2gene(dat$REACTOME)
    descs <- reactome2description(dat$REACTOME)
  }
}
