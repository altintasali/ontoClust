#' Detect ontology database from ontology ID.
#' @description Checks ontology ID across "DO", "GO", "KEGG", "MKEGG" and "REACTOME" databases and returns the database name. This is the engine function for \code{\link{detectOntoDB}} and checks one entry at a time.
#'
#' @param ontology.id A character vector with ontology IDs
#' @param NA.if.no.match If an ID is not present across all databases, corresponding element of vector will be assigned as NA (default: TRUE). If FALSE, function will throw an error and stop.
#'
#' @return A character vector with the name of the databases.
.detectOntoDB <- function(ontology.id, NA.if.no.match = FALSE){
  if(grepl("DOID:\\d+", ontology.id)){
    db <- "DO"
  }else if(grepl("GO:\\d+", ontology.id)){
    db <- "GO"
  }else if(grepl("hsa\\d+", ontology.id)){
    db <- "KEGG"
  }else if(grepl("M\\d+", ontology.id)){
    db <- "MKEGG"
  }else if(grepl("R-[A-Z]{3}-\\d+", ontology.id)){
    db <- "REACTOME"
  }else{
    if(NA.if.no.match){
      db <- NA
      warning(paste0("No known ontology database detected for input: '", ontology.id, "'. Assigning NA as output."))
    }else{
      stop(paste0("No known ontology database detected for input: '", ontology.id, "'. Stopping!"))
    }
  }
  return(db)
}

#' Detect ontology database from ontology ID
#' @description Checks each ID across "DO", "GO", "KEGG", "MKEGG" and "REACTOME" databases and returns the database name.
#'
#' @param ontology.id A character vector with ontology IDs
#' @param NA.if.no.match If an ID is not present across all databases, corresponding element of vector will be assigned as NA (default: TRUE). If FALSE, function will throw an error and stop.
#'
#' @return A character vector with the name of the databases.
#' @export
#'
#' @examples
#' ontology.ids <- lapply(sample_data, function(x){head(x$ID, 3)})
#' ontology.ids <- do.call(c, ontology.ids)
#' ontology.ids
#' detectOntoDB(ontology.ids)
#'
#' ontology.ids2 <- c(ontology.ids[1:3], "random-id-here")
#' ontology.ids2
#' detectOntoDB(ontology.ids2)
detectOntoDB <- function(ontology.id, NA.if.no.match = TRUE){
  sapply(ontology.id, function(x){.detectOntoDB(x, NA.if.no.match)})
}
