
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ontoClust

<!-- badges: start -->
<!-- badges: end -->

This packages clusters gene ontology terms. Currently supported gene
ontology databases are Gene Ontology (GO): Biological Process (BP),
Molecular Function (MF), Cellular Component (CC); Kyoto Encyclopedia of
Genes and Genomes (KEGG): KEGG and KEGG Modules; REACTOME; Disease
Ontology (DO).

## Installation

Please install `devtools` if you haven’t yet.

``` r
install.packages("devtools")
```

Then, you basically install the `ontoClust` package by using:

``` r
library(devtools)
install_github("altintasali/ontoClust", build_vignettes = TRUE)
```

## Quick Start

For a quick start, I recommend checking the example code provided in the
vignette:

``` r
library(ontoClust)
vignette(topic = "quickstart", package = "ontoClust")
```

# News and TO-DOs

This package is still under development. Due to alpha testing stage, it
currently only supports **GO terms** for **human**.

**TO-DOs:**

-   Network plot
-   Support for other ontology terms (KEGG, REACTOME )
-   Support for other species (mouse, rat etc.)
-   Plots will show view for multiple contrasts/groups
-   Analysis with using terms from multiple ontology database
