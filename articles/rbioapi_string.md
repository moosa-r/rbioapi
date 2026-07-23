# 2.F: STRING & rbioapi

## Introduction

STRING is a comprehensive database of protein-protein interactions (PPI)
that in version 11.0, covers 24,584,628 proteins from 5,090 organisms.
Directly quoting from their paper:

> The STRING database aims to collect, score and integrate all publicly
> available sources of protein–protein interaction information, and to
> complement these with computational predictions. Its goal is to
> achieve a comprehensive and objective global network, including direct
> (physical) as well as indirect (functional) interactions.
>
> (source: Szklarczyk, Damian, et al. “STRING v11: protein–protein
> association networks with increased coverage, supporting functional
> discovery in genome-wide experimental datasets.” *Nucleic acids
> research* 47.D1 (2019): D607-D613. )

------------------------------------------------------------------------

## Note about species argument

You can find an argument named “species” in every rbioapi STRING
function. Providing the species argument is not mandatory, but it has
been recommended in STRING API’s documentation to always specify the
species. An exception is when your input proteins’ vector length is more
than 100; In such cases, the species argument is required. Otherwise,
calling the function without providing the species will produce an
ERROR.

------------------------------------------------------------------------

## Map your IDs to STRING IDs

Although STRING API resources will handle and recognize a variety of
identifiers, it is recommended that you first map your IDs to STRING IDs
before using them in other rbioapi STRING functions.

``` r

## 1 We create a variable with our genes' NCBI IDs
proteins <- c(
  "p53", "BRCA1", "cdk2", "Q99835", "CDC42","CDK1","KIF23",
  "PLK1","RAC2","RACGAP1","RHOA","RHOB", "PHF14", "RBM3"
)

## 2 Now we map our protein IDs
proteins_mapped_df <- rba_string_map_ids(ids = proteins, species = 9606)

## 3 What we need and will use for the rest of this vignette is the `stringId` column
```

``` r


## 3 What we need and will use for the rest of this vignette is the `stringId` column
proteins_mapped <- proteins_mapped_df$stringId
```

------------------------------------------------------------------------

## Get interaction network of a protein set

You can retrieve a list of interactions that the proteins in your set
have with each other along with the STRING annotations of each
interaction. You may filter the results by using required_score and
network_type arguments.

See the ‘values’ section `rba_string_interactions_network` function’s
manual for information on the returned columns.

``` r

int_net <- rba_string_interactions_network(
  ids = proteins_mapped,
  species = 9606,
  required_score = 500
)
```

------------------------------------------------------------------------

## Get interaction partners of a protein set

In the last example, we only obtained the interaction which our proteins
have among themselves, what if we wanted to get a list of every protein
which interact with our protein(s)?

To do that, we can use `rba_string_interaction_partners`:

``` r

## Although we supply only one protein ID here (CD40 protein), you can provide a vector of proteins as the input
int_partners <- rba_string_interaction_partners(
  ids = "9606.ENSP00000361359",
  species = 9606,
  required_score = 900
)
```

------------------------------------------------------------------------

## Get network image of a protein set

Let’s go back to the interaction network. As you must have seen in the
STRING webpages, STRING plots the interaction network of your proteins
with many customizations available. You can also do that with STRING API
services. `rba_string_network_image` function is very flexible and you
have a variety of options; see the function’s manual.

``` r

## Example 1:
graph_ppi1 <- rba_string_network_image(
  ids = proteins_mapped,
  image_format = "image",
  species = 9606,
  save_image = FALSE,
  required_score = 500,
  network_flavor = "confidence"
)
```

![Network images - Example
1](rbioapi_string_files/figure-html/rba_string_network_image_ex1_image-1.png)

Network images - Example 1

``` r

## Example 2:
graph_ppi2 <- rba_string_network_image(
  ids = proteins_mapped,
  image_format = "image",
  species = 9606,
  save_image = FALSE,
  required_score = 500,
  add_color_nodes = 5,
  add_white_nodes = 5,
  network_flavor = "actions"
)
```

![Network images - Example
2](rbioapi_string_files/figure-html/rba_string_network_image_ex2_image-1.png)

Network images - Example 2

------------------------------------------------------------------------

## Enrichment analysis using STRING

STRING let you perform two types of enrichment analysis. See [STRING’s
paper](https://doi.org/10.1093/nar/gks1094 "STRING v9.1: protein-protein interaction networks, with increased coverage and integration")
for more information.

### Functional enrichment

The first type is the conventional type, which statistically tests your
supplied gene sets against some sets of annotation. Currently, STRING
supports Gene Ontology, KEGG pathways, UniProt Keywords, PubMed
publications, Pfam domains, InterPro domains, and SMART domains.
([source](https://version11.string-db.org/help/api/#getting-functional-enrichment "STRING API - Getting functional enrichment")).

``` r

enriched <- rba_string_enrichment(
  ids = proteins_mapped,
  species = 9606
)
```

As usual, we inspect the output using the
[`str()`](https://rdrr.io/r/utils/str.html) function. As you can see
below, the enrichment results of each category can be found as the
returned list’s elements.

``` r

str(enriched, max.level = 1)
#> List of 14
#>  $ COMPARTMENTS     :'data.frame':   25 obs. of  10 variables:
#>  $ Component        :'data.frame':   17 obs. of  10 variables:
#>  $ DISEASES         :'data.frame':   11 obs. of  10 variables:
#>  $ Function         :'data.frame':   12 obs. of  10 variables:
#>  $ InterPro         :'data.frame':   3 obs. of  10 variables:
#>  $ KEGG             :'data.frame':   45 obs. of  10 variables:
#>  $ Keyword          :'data.frame':   14 obs. of  10 variables:
#>  $ NetworkNeighborAL:'data.frame':   5 obs. of  10 variables:
#>  $ PMID             :'data.frame':   100 obs. of  10 variables:
#>  $ Process          :'data.frame':   148 obs. of  10 variables:
#>  $ RCTM             :'data.frame':   59 obs. of  10 variables:
#>  $ SMART            :'data.frame':   1 obs. of  10 variables:
#>  $ TISSUES          :'data.frame':   12 obs. of  10 variables:
#>  $ WikiPathways     :'data.frame':   46 obs. of  10 variables:
```

Let us see the “KEGG” results as an example. Below, we can see which
terms of the [KEGG pathways
database](https://www.genome.jp/kegg/pathway.html) were
over-represented:

**Please Note:** Other services supported by rbioapi also provide
Over-representation analysis tools. Please see the vignette article [Do
with rbioapi: Over-Representation (Enrichment) Analysis in
R](https://rbioapi.moosa-r.com/articles/rbioapi_do_enrich.md) ([link to
the documentation
site](https://rbioapi.moosa-r.com/articles/rbioapi_do_enrich.html)) for
an in-depth review.

### Functional enrichment Plot

In addition to a data frame, you can also get a plot summarizing the
enrichment results. This API endpoint supports extensive customization
of the plot; please refer to the
[`rba_string_enrichment_image()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_image.md)
function’s manual for detailed instructions. Here we perform the exact
enrichment analysis done above, and retrieve a plot of the results.

``` r

graph_enrich <- rba_string_enrichment_image(
  ids = proteins_mapped,
  species = 9606,
  category = "KEGG",
  image_format = "image",
  save_image = FALSE,
  group_by_similarity = 0.6
)
```

![Visualization of enrichment analysis
results](rbioapi_string_files/figure-html/rba_string_enrichment_plot_image-1.png)

Visualization of enrichment analysis results

### Protein-protein interaction enrichment

Even without incorporating annotation data, STRING can calculate if your
proteins are functionally related. Briefly, STRING accomplishes this by
comparing the interactions’ distribution in your protein-set to the
interactions’ distribution in the proteome. Read [STRING’s
paper](https://doi.org/10.1093/nar/gks1094 "STRING v9.1: protein-protein interaction networks, with increased coverage and integration")
for more information.

``` r

rba_string_enrichment_ppi(
  ids = proteins_mapped,
  species = 9606
)
#> $number_of_nodes
#> [1] 14
#> 
#> $number_of_edges
#> [1] 40
#> 
#> $average_node_degree
#> [1] 5.71
#> 
#> $local_clustering_coefficient
#> [1] 0.694
#> 
#> $expected_number_of_edges
#> [1] 19
#> 
#> $p_value
#> [1] 1.35e-05
```

------------------------------------------------------------------------

## Get functional annotations

As you have seen above, STRING maps the proteins to multiple annotation
sources. You can obtain any annotation associated with your proteins
without performing enrichment analysis and retrieving just the
significant portion.

``` r

annotations <- rba_string_annotations(
  ids = "9606.ENSP00000269305",
  species = 9606
)

## This function returns large results, so the results are not shown in this vignette.
```

------------------------------------------------------------------------

## See also in Functions’ manuals

Some rbioapi STRING functions were not covered in this vignette, please
check their manuals:

- [`rba_string_homology_intra()`](https://rbioapi.moosa-r.com/reference/rba_string_homology_intra.md)

- [`rba_string_homology_inter()`](https://rbioapi.moosa-r.com/reference/rba_string_homology_inter.md)

- [`rba_string_version()`](https://rbioapi.moosa-r.com/reference/rba_string_version.md)

------------------------------------------------------------------------

## How to Cite?

To cite STRING (Please see
<https://string-db.org/cgi/about?footer_active_subpage=references>):

- Damian Szklarczyk, Rebecca Kirsch, Mikaela Koutrouli, Katerina Nastou,
  Farrokh Mehryary, Radja Hachilif, Annika L Gable, Tao Fang, Nadezhda T
  Doncheva, Sampo Pyysalo, Peer Bork, Lars J Jensen, Christian von
  Mering, The STRING database in 2023: protein–protein association
  networks and functional enrichment analyses for any sequenced genome
  of interest, Nucleic Acids Research, Volume 51, Issue D1, 6 January
  2023, Pages D638–D646, <https://doi.org/10.1093/nar/gkac1000>

To cite rbioapi:

- Moosa Rezwani, Ali Akbar Pourfathollah, Farshid Noorbakhsh, rbioapi:
  user-friendly R interface to biologic web services’ API,
  Bioinformatics, Volume 38, Issue 10, 15 May 2022, Pages 2952–2953,
  <https://doi.org/10.1093/bioinformatics/btac172>

------------------------------------------------------------------------

## Links

- [This article in rbioapi
  documentation site](https://rbioapi.moosa-r.com/articles/rbioapi_string.html "2.E: STRING & rbioapi")

- [Functions references in rbioapi
  documentation site](https://rbioapi.moosa-r.com/reference/index.html#section-string-rba-string- "rbioapi reference")

- [rbioapi vignette
  index](https://rbioapi.moosa-r.com/articles/rbioapi.md "rbioapi: User-Friendly R Interface to Biologic Web Services' API")

------------------------------------------------------------------------

## Session info

    #> R version 4.6.1 (2026-06-24)
    #> Platform: x86_64-pc-linux-gnu
    #> Running under: Ubuntu 24.04.4 LTS
    #> 
    #> Matrix products: default
    #> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
    #> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
    #> 
    #> locale:
    #>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
    #>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
    #>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
    #> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
    #> 
    #> time zone: UTC
    #> tzcode source: system (glibc)
    #> 
    #> attached base packages:
    #> [1] stats     graphics  grDevices utils     datasets  methods   base     
    #> 
    #> other attached packages:
    #> [1] rbioapi_0.8.3
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] httr_1.4.8        cli_3.6.6         knitr_1.51        rlang_1.3.0      
    #>  [5] xfun_0.60         otel_0.2.0        png_0.1-9         textshaping_1.0.5
    #>  [9] jsonlite_2.0.0    DT_0.34.0         htmltools_0.5.9   ragg_1.5.2       
    #> [13] sass_0.4.10       rmarkdown_2.31    grid_4.6.1        crosstalk_1.2.2  
    #> [17] evaluate_1.0.5    jquerylib_0.1.4   fastmap_1.2.0     yaml_2.3.12      
    #> [21] lifecycle_1.0.5   compiler_4.6.1    fs_2.1.0          htmlwidgets_1.6.4
    #> [25] systemfonts_1.3.2 digest_0.6.39     R6_2.6.1          curl_7.1.0       
    #> [29] magrittr_2.0.5    bslib_0.11.0      tools_4.6.1       pkgdown_2.2.1    
    #> [33] cachem_1.1.0      desc_1.4.3
