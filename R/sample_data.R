#' @title Sample ontology data
#'
#' @description Sample \code{\link{data.table}} containing enrichment results (over-representation, ORA) from different databases, which are:
#' \describe{
#' \item{DO}{Disease Ontology}
#' \item{GOBP}{Gene Ontology (GO), Biological Process (BP)}
#' \item{GOMF}{Gene Ontology (GO), Molecular Function (MF)}
#' \item{GOCC}{Gene Ontology (GO), Cellular Component(CC)}
#' \item{KEGG}{Kyoto Encyclopedia of Genes and Genomes}
#' \item{MKEGG}{KEGG Modules}
#' \item{REACTOME}{Reactome}
#' }
#'
#' @format \code{\link{data.table}} columns for each item in the \code{\link{list}} contain:
#' \describe{
#' \item{Condition}{Comparison made by differential expression analysis (e.g. AvsB: Group A vs. Group B)}
#' \item{Direction}{Enriched term with up-regulated (up), down-regulated (down) or both up/down-regulated (all) genes}
#' \item{Method}{Enrichment methods. ORA: Over-representation analysis}
#' \item{DB}{Ontology database}
#' \item{ID}{ID of the enrichment term in the ontology database}
#' \item{Description}{Description of the enriched term}
#' \item{GeneRatio}{Gene Ratio. See \code{clusterProfiler} package for details}
#' \item{BgRatio}{Background Ratio. See \code{clusterProfiler} package for details}
#' \item{pvalue}{p-value}
#' \item{p.adjust}{p-values adjusted for multiple testing using "BH" method. See \code{\link{p.adjust}} for details.}
#' \item{qvalue}{p-values adjusted for multiple testing using "qvalue" method. See \code{qvalue} package for details.}
#' }
#'
"sample_data"
