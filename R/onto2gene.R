#' Ontology ID to gene IDs
#'
#' @param id ID of any ontology term
#' @param organism Organism identifier (required for GO annotations). Default is "hsa". Currently human (\code{hsa}), mouse (\code{mmu}) and rat (\code{rno}) are supported. Full list of organism IDs can be found [here](https://www.genome.jp/kegg/catalog/org_list.html).
#'
#' @return A \code{\link{list}} object containing gene IDs for each ontology ID
#' @export
#' @importFrom Biobase testBioCConnection
#' @import magrittr
#'
#' @examples
#' id <- lapply(sample_data, function(x){head(x$ID)})
#' id <- do.call(c, id)
#' onto2gene(id)
onto2gene <- function(id, organism = "hsa"){
  if(!is.character(id)){
    stop("No valid ID provided. 'id' should be character vector.")
  }

  dat <- data.frame(db = detectOntoDB(id), id)
  dat <- split(dat$id, f = dat$db)

  genes <- list()

  if(length(dat) == 0){
    stop("No valid ontology ID provided. Check your input!")
  }

  if(any(names(dat) %in% "DO")){
    genes[["DO"]] <- do2gene(dat$DO)
  }

  if(any(names(dat) %in% "GO")){
    genes[["GO"]] <- go2gene(dat$GO, organism = organism)
  }

  if(any(names(dat) %in% "KEGG")){
    if(Biobase::testBioCConnection()){
      genes[["KEGG"]] <- kegg2gene(dat$KEGG)
    }else{
      stop("No internet connection detected. KEGGREST API requires internet connection to retrieve gene IDs.")
    }
  }

  if(any(names(dat) %in% "MKEGG")){
    message("MKEGG is not supported yet.")
    #TODO: Support MKEGG
    genes[["MKEGG"]] <- list()
    genes[["MKEGG"]] <- data.frame(id = dat$MKEGG, gene = NA)
    genes[["MKEGG"]] <- split(genes[["MKEGG"]]$gene, f = genes[["MKEGG"]]$id)
  }

  if(any(names(dat) %in% "REACTOME")){
    genes[["REACTOME"]] <- reactome2gene(dat$REACTOME)
  }

  out <- do.call(c, genes)
  names(out) <- gsub(pattern = paste("^", names(dat),".",  collapse = "|", sep = ""),
                     replacement = "",
                     x = names(out))

  out <- out[id]

  return(out)
}
