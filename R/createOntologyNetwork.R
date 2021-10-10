
ontology.id <- sample_data$GOBP$ID[1:10]

jaccardIndex <- function(x, y){
  jc <- length(intersect(x,y))/length(union(x,y))
  return(jc)
}
createOntologyNetwork <- function(ontology.id, weighted = FALSE, method = "Jaccard"){
  geneData <- go2gene(ontology.id, organism = "hsa")

  if(methods == "Jaccard"){
    ids <- expand.grid(seq_along(geneData), seq_along(geneData))
    ids <- split(ids, f = "rownames")
    network <- apply(ids, MARGIN = 1, function(x){
      jc <- jaccardIndex(geneData[[x[1]]], geneData[[x[2]]])
      out <- data.frame(source = names(geneData)[x[1]],
                        target = names(geneData)[x[2]],
                        weight = jc)
      return(out)
    }

    )
  }
}
