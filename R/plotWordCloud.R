#' Plot Word Cloud
#'
#' @param wc \code{\link{data.frame}} for word cloud with frequencies and group. The columns are
#' \describe{
#' \item{word}{Word (character)}
#' \item{freq}{Frequency of the word (numeric)}
#' \item{norm.freq}{Normalized frequency of the word (numeric). Normalization is done by division by the sum of frequencies}
#' \item{group}{ID of Ontology Cluster or Link Community (numeric)}
#' }
#' @param n Top "n" words to plot (numeric). Default is Inf (infinite), which uses all available words.
#' @param freq Use frequency of the words as size. Default TRUE.
#' @param norm.freq Use normalized frequency of the words as size. Default TRUE.
#'
#' @return A \code{\link{ggwordcloud}} object
#'
#' @import ggwordcloud
#' @import ggplot2
#' @importFrom data.table setDT
#' @importFrom utils head
#' @export
#'
#' @examples
#' ontology.id <- sample_data$GOBP$ID[1:50]
#' network <- createOntologyNetwork(ontology.id, method = "jaccard", weighted = FALSE)
#' lc <- getLC(network)
#' oc <- getOntoClust(lc)
#' wc <- createWordCloud(oc)
#' plotWordCloud(wc, n = 10, freq = TRUE, norm.freq = TRUE)
plotWordCloud <- function(wc, n = Inf, freq = TRUE, norm.freq = TRUE){
  word <- .SD <- NULL #to avoid NOTES from devtools::check()

  if(!is.infinite(n)){
    data.table::setDT(wc)
    wc <- wc[, head(.SD, eval(n)), by = "group"]
  }

  if(freq){
    if(norm.freq){
      p <- ggplot(wc, aes(label = word, size = norm.freq)) +
        geom_text_wordcloud() +
        theme_minimal() +
        facet_wrap(~group)
    }else{
      p <- ggplot(wc, aes(label = word, size = freq)) +
        geom_text_wordcloud() +
        theme_minimal() +
        facet_wrap(~group)
    }
  }else{
    p <- ggplot(wc, aes(label = word)) +
      geom_text_wordcloud() +
      scale_size_area(max_size = 20) +
      theme_minimal() +
      facet_wrap(~group)
  }

  return(p)
}
