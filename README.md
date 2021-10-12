
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
install_github("altintasali/ontoClust")
```

## Quick Start

For a quick start, I recommend checking the example code provided in the
vignette:

``` r
library(ontoClust)
#> 
vignette("quickstart", package = "ontoClust")
#> starting httpd help server ...
#>  done
```
