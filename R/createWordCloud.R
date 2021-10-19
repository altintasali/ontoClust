#' Create Word Cloud
#'
#' @param oc  \code{ontoClust} object. The output of \code{\link{getOntoClust}}
#' @param by Create the word cloud from either "ontology cluster" or "link community". Default is "ontology_cluster"
#' @param excludeWord Words to exclude from word-cloud (e.g. regulation)
#'
#' @return A \code{\link{data.frame}} with columns "word", "freq" and "group".
#' @export
#'
#' @examples
#' ontology.id <- sample_data$GOBP$ID[1:50]
#' network <- createOntologyNetwork(ontology.id, method = "jaccard", weighted = FALSE)
#' lc <- getLC(network)
#' oc <- getOntoClust(lc)
#' createWordCloud(oc)
createWordCloud <- function(oc,
                            by = c("ontology_cluster", "link_community")[1],
                            excludeWord = NULL){

  ontoDescs <- oc$result$description
  if(by == "ontology_cluster"){
    dat <- split(oc$result$description, f = oc$result$cluster)
  }else if(by == "link_community"){
    dat <- merge(x = oc$link_communities,
                 y = oc$result[, c("ontology", "description")],
                 by.y = "ontology",
                 by.x = "node")
    dat <- dat[order(dat$cluster),]
    dat <- split(dat$description, f = dat$cluster)
  }else{
    stop("'by' can be either 'ontology_cluster' or 'link_community'")
  }

  wcDat <- lapply(dat, .createWordCloud)
  for(i in names(wcDat)){
    wcDat[[i]]$group <- i
  }
  wcDat <- do.call(rbind, wcDat)

  return(wcDat)
}

#' Engine for Create Word Cloud
#'
#' @param ontologyDescriptions Description of ontology terms
#' @param excludeWord Words to exclude from word cloud
#'
#' @return A \code{\link{data.frame}} with columns "word", "freq"
#'
#' @importFrom tm Corpus VectorSource tm_map removeWords TermDocumentMatrix removePunctuation stripWhitespace
#'
#' @examples
#' \dontrun{
#' ontology.id <- sample_data$GOBP$ID[1:50]
#' x <- onto2description(ontology.id)
#' y <- x$description
#' .createWordCloud(y)
#' }
.createWordCloud <- function(ontologyDescriptions, excludeWord = NULL){
  docs <- Corpus(VectorSource(ontologyDescriptions))
  # toSpace <- content_transformer(function (x, pattern ) gsub(pattern, "", x))
  # docs <- tm_map(docs, toSpace, ":")

  docs <- tm_map(docs, removeWords, c("the", "at", "of", "on",
                                      "and", "vs", "an"))

  docs <- tm_map(docs, removeWords, c("regulation", "process", "activity"))

  if(!is.null(excludeWord)){
    docs <- tm_map(docs, removeWords, excludeWord)
  }

  docs <- tm_map(docs, removePunctuation)
  docs <- tm_map(docs, stripWhitespace)
  dtm <- TermDocumentMatrix(docs)

  m <- as.matrix(dtm)
  v <- sort(rowSums(m), decreasing=TRUE)
  d <- data.frame(word = names(v), freq=v, norm.freq = v/sum(v))
  return(d)
}
