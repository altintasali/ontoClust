#' Gene Ontology (GO) to Gene ID
#'
#' @param go.id GO ID (e.g. GO:0045333)
#' @param organism Organism identifier. Currently human (\code{hsa}), mouse (\code{mmu}) and rat (\code{rnp}) are supported. Full list of organism IDs can be found [here](https://www.genome.jp/kegg/catalog/org_list.html).
#'
#' @return A list object containing Gene IDs for every GO ID
#' @export
#'
#' @examples
#' go.ids <- sample_data$GOBP$ID[1:3]
#' go2gene(go.ids)

go2gene <- function(go.id, organism = "hsa"){
  orgDb <- list(hsa = "org.Hs.eg.db",
                mmu = "org.Mm.eg.db",
                rno = "org.Rn.eg.db")
  lib2load <- orgDb[[organism]]
  library(lib2load, character.only = TRUE)

  GOtable <- paste0(gsub(".db$", "", x = lib2load), "GO2ALLEGS")
  GOtable <- eval(parse(text = GOtable))

  out <- as.list(GOtable[go.id])
  out <- lapply(out, unique)

  return(out)
}

