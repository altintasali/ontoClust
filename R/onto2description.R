#' Ontology ID to Ontology Descriptions
#'
#' @param id ID of any ontology term
#'
#' @return A \code{\link{data.frame}} with ontology IDs and their descriptions.
#' @export
#' @importFrom Biobase testBioCConnection
#' @import magrittr
#'
#' @examples
#' id <- lapply(sample_data, function(x){head(x$ID)})
#' id <- do.call(c, id)
#' onto2description(id)
onto2description <- function(id){
  if(!is.character(id)){
    stop("No valid ID provided. 'id' should be character vector.")
  }

  dat <- data.frame(db = detectOntoDB(id), id)
  dat <- split(dat$id, f = dat$db)

  descs <- list()

  if(length(dat) == 0){
    stop("No valid ontology ID provided. Check your input!")
  }

  if(any(names(dat) %in% "DO")){
    descs[["DO"]] <- do2description(dat$DO)
  }

  if(any(names(dat) %in% "GO")){
    descs[["GO"]] <- go2description(dat$GO)
  }

  if(any(names(dat) %in% "KEGG")){
    if(Biobase::testBioCConnection()){
      descs[["KEGG"]] <- kegg2description(dat$KEGG)
    }else{
      stop("No internet connection detected. KEGGREST API requires internet connection to retrieve gene IDs.")
    }
  }

  if(any(names(dat) %in% "MKEGG")){
    descs[["MKEGG"]] <- mkegg2description(dat$MKEGG)
  }

  if(any(names(dat) %in% "REACTOME")){
    descs[["REACTOME"]] <- reactome2description(dat$REACTOME)
  }

  out <- do.call(rbind, descs)
  rownames(out) <- out$id
  out <- out[id,]

  return(out)
}
