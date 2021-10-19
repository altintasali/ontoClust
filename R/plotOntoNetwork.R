#' Plot Network of Ontology Clusters
#'
#' @param oc \code{ontoClust} object. The output of \code{\link{getOntoClust}}
#' @param layout Network layout. See \code{\link{ggraph}} for details. Default is "kk". Some options are 'kk', 'dh', 'circle', 'gem', 'graphopt', 'grid', 'mds', 'randomly', 'fr', 'drl', 'lgl', 'nicely'
#' @param size_ontology Size of ontology (node) terms. Default is 3.
#' @param size_label_ontology Size of ontology (node) term labels. Default is 2.
#' @param alpha_edge Transparency parameter for edges (link communities). Default is 0.2.
#' @param alpha_node Transparency parameter for nodes (ontology clusters). Default is 1 (fully opaque).
#' @param facet_by Facets to draw. Default is NULL, thus no facets are drawn. It can be 'ontology_cluster' or 'link_community'
#' @param background Keep the nodes/edges in the background when facetted. Default is TRUE. If FALSE, only edges/nodes that connects each ontology-cluster/link-community will be drawn.
#'
#' @return \code{\link{ggraph}} plot
#' @export
#'
#' @importFrom igraph graph_from_data_frame
#' @import ggplot2
#' @import ggraph
#'
#' @examples
#' \dontrun{
#' ontology.id <- sample_data$GOBP$ID[1:50]
#' network <- createOntologyNetwork(ontology.id, method = "jaccard", weighted = FALSE)
#' lc <- getLC(network)
#' oc <- getOntoClust(lc)
#' plotOntoNetwork(oc)
#' plotOntoNetwork(oc, facet_by = c("ontology_cluster", "link_community")[1])
#' plotOntoNetwork(oc, facet_by = c("ontology_cluster", "link_community")[1], background = TRUE)
#' }
plotOntoNetwork <- function(oc,
                            layout = "kk",
                            size_ontology = 3,
                            size_label_ontology = 2,
                            alpha_edge = 0.2,
                            alpha_node = 1,
                            facet_by = NULL, #c("ontology_cluster", "link_community")[1]
                            background = TRUE
){
  `Link Community` <- `Ontology Cluster` <- description <- NULL
  ##------------------------------------------------------------------------
  ## Build network
  ##------------------------------------------------------------------------
  network <- oc$lc$edges
  colnames(network) <- c("source", "target", "Link Community")
  network$`Link Community` <- factor(network$`Link Community`,
                                     levels = as.character(seq_along(unique(network$`Link Community`))),
                                     labels = as.character(seq_along(unique(network$`Link Community`))))
  nodes <- oc$result
  nodes$`Ontology Cluster` <- factor(nodes$cluster,
                                     levels = as.character(seq_along(unique(nodes$cluster))),
                                     labels = as.character(seq_along(unique(nodes$cluster))))

  g <- igraph::graph_from_data_frame(network, vertices = nodes)

  p <- ggraph(g, layout = "kk") +
    geom_edge_link(aes(color = `Link Community`),
                   alpha = alpha_edge) +
    geom_node_point(aes(color = `Ontology Cluster`), size = 3) +
    geom_node_text(aes(label = description), size = 2, repel = TRUE) +
    theme_void()

  if(!is.null(facet_by)){
    if(facet_by == "ontology_cluster"){
      if(background){
        p <- p + facet_wrap(~`Ontology Cluster`)
      }else{
        p <- p + facet_nodes(~`Ontology Cluster`)
      }
    }else if(facet_by == "link_community"){
      if(background){
        p <- p + facet_wrap(~`Link Community`) #facet_edges have the same behaviour. "If structure" for only consistency
      }else{
        p <- p + facet_edges(~`Link Community`)
      }
    }else{
      stop("'facet_by' can be either 'ontology_cluster' or 'link_community'")
    }
  }

  return(p)
}
