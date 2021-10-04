#' Disease Ontology (DO) to Gene ID (ENTREZ)
#'
#' @param do.id A vector of DO IDs
#'
#' @return A list object containing Gene IDs for every DO ID
#' @import DOSE
#' @import magrittr
#' @importFrom data.table data.table rbindlist setkey
#' @export
#'
#' @examples
#' \dontrun{
#' do.ids <- sample_data$DO$ID[1:3]
#' do2gene(do.ids)
#' }
do2gene <- function(do.id){
  ## Create DO2GENE object
  .DOSEEnv <- get(".DOSEEnv", envir = .GlobalEnv)
  if (!exists("EG2ALLDO", envir = .DOSEEnv)) {
    tryCatch(utils::data(list = "EG2ALLDO", package = "DOSE"))
    EG2ALLDO <- get("EG2ALLDO")
    assign("EG2ALLDO", EG2ALLDO, envir = .DOSEEnv)
    rm(EG2ALLDO, envir = .GlobalEnv)
  }
  EG2ALLDO <- get("EG2ALLDO", envir = .DOSEEnv)

  ## Create output
  DO2GENEtable <- lapply(names(EG2ALLDO), function(x){
    data.table(id = EG2ALLDO[[x]], gene = x)
  }) %>% rbindlist
  setkey(x = DO2GENEtable, "id")

  DO2GENE <- split(DO2GENEtable$gene, f = DO2GENEtable$id)
  out <- DO2GENE[do.id]

  return(out)
}
