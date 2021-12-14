
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ontoClust

<!-- badges: start -->

[![R-CMD-check](https://github.com/altintasali/ontoClust/workflows/R-CMD-check/badge.svg)](https://github.com/altintasali/ontoClust/actions)
<!-- badges: end -->

This packages clusters gene ontology terms. Currently supported gene
ontology databases are

-   Gene Ontology (GO): Biological Process (BP)
-   Gene Ontology (GO): Molecular Function (MF)
-   Gene Ontology (GO): Cellular Component (CC)
-   Kyoto Encyclopedia of Genes and Genomes (KEGG): KEGG and KEGG
    Modules
-   REACTOME
-   Disease Ontology (DO)

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

This package is still under development.

**News:**

-   Network plot with facets
-   Support for other ontology terms (GO, KEGG, REACTOME, DO)
-   Support for human, mouse and rat

**TO-DOs:**

-   Same color scheme across plot functions
-   Support for KEGG Modules
-   Support for other species (yeast etc)
-   Plots will show view for multiple contrasts/groups
-   Analysis with using terms from multiple ontology database
-   Implement Word Clouds (with dendrograms)
