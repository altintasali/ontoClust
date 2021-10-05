#' Disease Ontology (DO) to Gene ID (ENTREZ)
#'
#' @param do.id A vector of DO IDs
#'
#' @return A list object containing Gene IDs for every DO ID
#' @import magrittr
#' @importFrom utils data
#' @importFrom data.table data.table rbindlist setkey
#' @export
#' @seealso \code{\link{onto2gene}} for all ontology to gene conversions as well as \code{\link{go2gene}}, \code{\link{kegg2gene}}, \code{\link{reactome2gene}}.
#'
#'
#' @examples
#' do.ids <- sample_data$DO$ID[1:3]
#' do2gene(do.ids)
do2gene <- function(do.id){
  ## Create DO2GENE object
  data(list = "EG2ALLDO", package = "DOSE", envir = environment())
  EG2ALLDO <- get("EG2ALLDO")

  ## Create output
  DO2GENEtable <- lapply(names(EG2ALLDO), function(x){
    data.table(id = EG2ALLDO[[x]], gene = x)
  }) %>% rbindlist
  setkey(x = DO2GENEtable, "id")

  DO2GENE <- split(DO2GENEtable$gene, f = DO2GENEtable$id)
  out <- DO2GENE[do.id]

  return(out)
}
