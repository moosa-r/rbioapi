# 2.C: miEAA & rbioapi

## Introduction

The miRNA Enrichment Analysis and Annotation Tool
([miEAA](https://ccb-compute2.cs.uni-saarland.de/mieaa/ "miRNA Enrichment Analysis and Annotation Tool"))
is a multi-species microRNA enrichment-analysis service provided by the
[Chair for Clinical Bioinformatics at Saarland
University](https://www.ccb.uni-saarland.de/). For more information, see
the miEAA [website](https://ccb-compute2.cs.uni-saarland.de/mieaa/) or
its [latest
publication](https://doi.org/10.1093/nar/gkad392 "miEAA 2023: updates, new functional microRNA sets and improved enrichment visualizations").

------------------------------------------------------------------------

## First, find **enrichment categories**

Before performing enrichment analysis on a miRNA set, note that the
supported **enrichment categories** depend on both the **miRNA type**
(mature or precursor, without mixing the two) and the **species**. See
the [miEAA integrated data
sets](https://ccb-compute2.cs.uni-saarland.de/mieaa/downloads/) for
details.

Thus, it is recommended to retrieve a list of possible enrichment
categories that you may use:

``` r

## A list of available enrichment categories for:

## mature human miRNA:
rba_mieaa_cats(mirna_type = "mature", species = 9606)

## precursor human miRNA
rba_mieaa_cats(mirna_type = "precursor", species = 9606)

## precursor zebrafish miRNA
rba_mieaa_cats(mirna_type = "precursor", species = "Danio rerio")
```

------------------------------------------------------------------------

## Submit an enrichment-analysis request to miEAA

There are two approaches to do this, we will start with the simpler one.

### Approach 1: Using the wrapper function

Supply the arguments of
[`rba_mieaa_enrich()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich.md)
as described in its manual. You must provide `test_set`, `mirna_type`,
`test_type`, and `species`:

``` r

## 1 We create a variable with our miRNAs' mature IDs
mirs <- c(
  "hsa-miR-20b-5p", "hsa-miR-144-5p", "hsa-miR-17-5p", "hsa-miR-20a-5p",
  "hsa-miR-222-3p", "hsa-miR-106a-5p", "hsa-miR-93-5p", "hsa-miR-126-3p",
  "hsa-miR-363-3p", "hsa-miR-302c-3p", "hsa-miR-374b-5p", "hsa-miR-18a-5p",
  "hsa-miR-548d-3p", "hsa-miR-135a-3p", "hsa-miR-558", "hsa-miR-130b-5p",
  "hsa-miR-148a-3p"
)

## 2a Perform enrichment analysis without limiting it to selected categories
mieaa_all <- rba_mieaa_enrich(
  test_set = mirs,
  mirna_type = "mature",
  test_type = "ORA",
  species = 9606
)
#>  -- Step 1/3: Submitting Enrichment analysis request:

## 2b Limit the enrichment to selected data sets (enrichment categories)
mieaa_kegg <- rba_mieaa_enrich(
  test_set = mirs,
  mirna_type = "mature",
  test_type = "ORA",
  species = 9606,
  categories = "KEGG_mature"
)
#>  -- Step 1/3: Submitting Enrichment analysis request:
```

    #> [1] "Vignette building failed. It is probably because the web service was down during the building."

### Approach 2: Going step-by-step

As stated before,
[`rba_mieaa_enrich()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich.md)
is a wrapper function, meaning that it executes the following sequence
of functions:

``` r

## 1 Submit enrichment request to miEAA
request <- rba_mieaa_enrich_submit(
  test_set = mirs,
  mirna_type = "mature",
  test_type = "ORA",
  species = 9606,
  categories = c("miRWalk_Diseases_mature", "miRWalk_Organs_mature")
)

## 2 check for job's running status
rba_mieaa_enrich_status(job_id = request$job_id)

## 3 If the job has completed, retrieve the results
results <- rba_mieaa_enrich_results(job_id = request$job_id)
```

**Please Note:** Other services supported by rbioapi also provide
Over-representation analysis tools. Please see the vignette article [Do
with rbioapi: Over-Representation (Enrichment) Analysis in
R](https://rbioapi.moosa-r.com/articles/rbioapi_do_enrich.md) ([link to
the documentation
site](https://rbioapi.moosa-r.com/articles/rbioapi_do_enrich.html)) for
an in-depth review.

------------------------------------------------------------------------

## Convert miRNA identifiers

miEAA uses miRBase version 22 identifiers. Use
[`rba_mieaa_convert_version()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_convert_version.md)
to convert miRNA identifiers between supported miRBase versions. miEAA
also distinguishes mature and precursor miRNA identifiers; use
[`rba_mieaa_convert_type()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_convert_type.md)
to convert between these identifier types.

------------------------------------------------------------------------

## How to Cite?

To cite miEAA (see <https://ccb-compute2.cs.uni-saarland.de/mieaa/>):

- Ernesto Aparicio-Puerta, Pascal Hirsch, Georges P. Schmartz, Fabian
  Kern, Tobias Fehlmann, Andreas Keller, miEAA 2023: updates, new
  functional microRNA sets and improved enrichment visualizations,
  Nucleic Acids Research, Volume 51, Issue W1, 5 July 2023, Pages
  W319–W325, <https://doi.org/10.1093/nar/gkad392>

To cite rbioapi:

- Moosa Rezwani, Ali Akbar Pourfathollah, Farshid Noorbakhsh, rbioapi:
  user-friendly R interface to biologic web services’ API,
  Bioinformatics, Volume 38, Issue 10, 15 May 2022, Pages 2952–2953,
  <https://doi.org/10.1093/bioinformatics/btac172>

------------------------------------------------------------------------

## Links

- [This article on the rbioapi documentation
  site](https://rbioapi.moosa-r.com/articles/rbioapi_mieaa.html "2.C: miEAA & rbioapi")

- [Function references on the rbioapi documentation
  site](https://rbioapi.moosa-r.com/reference/index.html#section-mieaa-rba-mieaa- "rbioapi reference")

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
    #> [1] rbioapi_0.8.3.9000
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] digest_0.6.39     desc_1.4.3        R6_2.6.1          fastmap_1.2.0    
    #>  [5] xfun_0.60         cachem_1.1.0      knitr_1.51        htmltools_0.5.9  
    #>  [9] rmarkdown_2.31    lifecycle_1.0.5   cli_3.6.6         sass_0.4.10      
    #> [13] pkgdown_2.2.1     textshaping_1.0.5 jquerylib_0.1.4   systemfonts_1.3.2
    #> [17] compiler_4.6.1    httr_1.4.8        tools_4.6.1       ragg_1.5.2       
    #> [21] curl_8.0.0        bslib_0.12.0      evaluate_1.0.5    yaml_2.3.12      
    #> [25] otel_0.2.0        jsonlite_2.0.0    rlang_1.3.0       fs_2.1.0         
    #> [29] htmlwidgets_1.6.4
