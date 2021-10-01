# ontology.ids <- lapply(sample_data, function(x){head(x$ID, 3)})
# ontology.ids <- do.call(c, ontology.ids)
# id <- ontology.ids

#' Title
#'
#' @param id ID of any ontology term
#'
#' @return A \code{\link{list}} object containing gene IDs for each ontology ID
#' @export
#' @importFrom Biobase testBioCConnection
#'
#' @examples
#' \dontrun{
#' ids <- sample_data$KEGG$ID[1:5]
#' onto2gene(ids)
#' }
onto2gene <- function(id){
  dat <- data.frame(db = detectOntoDB(id), id)
  dat <- split(dat$id, f = dat$db)
  if(names(dat) == "DO"){
    message("DO is not supported yet.")
    #TODO: Support DO
  }else if(names(dat) == "GO"){

  }else if(names(dat) == "KEGG"){
    if(testBioCConnection()){
      genes <- kegg2gene(dat$KEGG)
      descs <- kegg2description(dat$KEGG)
    }else{
      stop("No internet connection detected. KEGGREST API requires internet connection to retrieve gene IDs.")
    }
  }else if(names(dat) == "MKEGG"){

  }else if(names(dat) == "REACTOME"){

  }
}
