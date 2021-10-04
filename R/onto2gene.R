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
#' @importFrom AnnotationDbi keys select
#' @importFrom stringr str_split_fixed
#' @import magrittr
#' @import GO.db
#' @import reactome.db
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
    genes <- do2gene(dat$DO)
    descs <- do2description(dat$DO)
  }else if(names(dat) == "GO"){
    genes <- I("x")

    descs <- select(x = GO.db, keys = dat$GO, columns =c("GOID", "TERM", "ONTOLOGY"), keytype = "GOID")
    colnames(descs) <- c("id", "description", "ontology")

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

  }else if(names(dat) == "REACTOME"){
    genes <- subset(reactomePATHID2EXTID, Lkeys = dat$REACTOME) %>% as.list
    descs <- subset(reactomePATHID2NAME, Lkeys = dat$REACTOME) %>% as.data.frame #%>% toTable
    colnames(descs) <- c("id", "description")
    descs$description <- str_split_fixed(string = descs$description, pattern = ": ", n = 2)[,2]
  }
}
