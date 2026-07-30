# 2.A: Enrichr & rbioapi

## Introduction

[Enrichr](https://maayanlab.cloud/Enrichr/) is a popular gene-set
enrichment analysis tool developed in the Ma’ayan Lab.

------------------------------------------------------------------------

## Gene set library concept in Enrichr

Directly quoting from Enrichr’s help page:

> A *gene set library* is a set of related gene sets or enrichment terms
> \[…\] These libraries have been constructed from many sources such as
> published studies and major biological and biomedical online
> databases. Others have been created for and only available through
> Enrichr.
>
> (source: <https://maayanlab.cloud/Enrichr/help#background>)

To get a list of the available libraries in Enrichr, use:

``` r

enrichr_libs <- rba_enrichr_libs()
```

In the returned data frame, you can find the names of available Enrichr
libraries in “libraryName” column. As you will see in the following
sections, you can use these names to request an enrichment analysis
based on the selected library or libraries.

### Retrieve gene sets from an Enrichr library

To retrieve the gene sets contained in an Enrichr library, use
[`rba_enrichr_gene_sets()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_gene_sets.md).
You should supply one of the library names returned above in the
`gene_set_library` argument. For example:

``` r

reactome_gene_sets <- rba_enrichr_gene_sets(
  gene_set_library = "Reactome_Pathways_2024"
)
```

If you only need a particular gene set, you can supply its exact term
name in the `term` argument:

``` r

notch_gene_set <- rba_enrichr_gene_sets(
  gene_set_library = "Reactome_Pathways_2024",
  term = "Signaling by NOTCH"
)
```

You can also save the raw server’s response as a GMT file by supplying
the `save_file` rbioapi option through “ellipsis”. For example:

``` r

rba_enrichr_gene_sets(
  gene_set_library = "Reactome_Pathways_2024",
  save_file = "Reactome_Pathways_2024.gmt"
)
```

Alternatively, set `save_file = TRUE` to let rbioapi automatically
generate a proper file path. The organism will be included in the
automatically-generated file name. If you have supplied a `term`, its
name will also be included. See the manual of
[`rba_options()`](https://rbioapi.moosa-r.com/reference/rba_options.md)
function for more information on saving the server’s raw response files.

------------------------------------------------------------------------

## Enrichment analysis using Enrichr

To perform enrichment analysis on your gene-set with Enrichr using
rbioapi, you can take two approaches. We will begin with the simple one.
But first, we create a vector of genes’ NCBI IDs to use as the input
example in this article.

``` r

# Create a vector with our genes' NCBI IDs
genes <- c(
  "p53", "BRCA1", "cdk2", "Q99835", "CDC42","CDK1","KIF23","PLK1",
  "RAC2","RACGAP1","RHOA","RHOB", "PHF14", "RBM3", "MSL1"
)
```

### Approach 1: Using the one-step Wrapper function

The only required input for this function is to simply supply your
gene-set as a character vector. Optionally you can also select one or
more libraries. Please see
[`rba_enrichr()`](https://rbioapi.moosa-r.com/reference/rba_enrichr.md)
function’s manual for more details on the arguments.

``` r

# Request the enrichment analysis
results_all <- rba_enrichr(gene_list = genes)
```

Note that the default value for the argument `gene_set_library` in the
rba_enrichr function is “all”. This means that if you call the function
as above, all of the Enrichr libraries will be used for the enrichment
analysis of your uploaded gene list. In this case, you will have a named
list, where each of its elements is a dataframe containing your genes’
analysis results using that Enrichr library.

Alternatively, you can use the `gene_set_library` argument to specify
the library (or libraries) to use. Here we demonstrate using
“MSigDB_Hallmark_2020” library:

``` r

# Request the enrichment analysis by a specific library
results_msig_hallmark <- rba_enrichr(
  gene_list = genes,
  gene_set_library = "MSigDB_Hallmark_2020",
  progress_bar = FALSE # to avoid printing issues in the vignette
)
```

By default, rbioapi matches the supplied `gene_set_library` exactly. Set
`regex_library_name` to `TRUE` to use a regex pattern instead. This is
useful if you need, for example, partial matches in library names.
Suppose you want to perform the enrichment analysis on every Enrichr
library containing “MSig”. You can do the following:

``` r

# Request the enrichment analysis
results_msig <- rba_enrichr(
  gene_list = genes,
  gene_set_library = "msig",
  regex_library_name = TRUE,
  progress_bar = FALSE # to avoid printing issues in the vignette
)

# Regex matching must be enabled explicitly.
```

Note that when only one Enrichr library is selected, a data frame with
enrichment analysis result will be returned.

``` r

str(results_msig_hallmark)
#> 'data.frame':    18 obs. of  9 variables:
#>  $ Term                : chr  "Mitotic Spindle" "G2-M Checkpoint" "E2F Targets" "Apoptosis" ...
#>  $ Overlap             : chr  "5/199" "4/200" "4/200" "3/161" ...
#>  $ P.value             : num  2.57e-07 1.22e-05 1.22e-05 2.17e-04 2.74e-03 ...
#>  $ Adjusted.P.value    : num  4.62e-06 7.29e-05 7.29e-05 9.76e-04 9.87e-03 ...
#>  $ Old.P.value         : int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ Old.Adjusted.P.value: int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ Odds.Ratio          : num  51 36.7 36.7 31.4 29.7 ...
#>  $ Combined.Score      : num  774 416 416 265 175 ...
#>  $ Genes               : chr  "CDC42;RACGAP1;PLK1;CDK1;KIF23" "RACGAP1;PLK1;CDK1;KIF23" "RACGAP1;PLK1;CDK1;BRCA1" "CDK2;BRCA1;RHOB" ...
```

But when multiple libraries have been selected, the function’s output
will be a list where each element is a data frame corresponding to one
of the selected libraries.

``` r

str(results_msig, 1)
#> List of 3
#>  $ MSigDB_Computational       :'data.frame': 195 obs. of  9 variables:
#>  $ MSigDB_Oncogenic_Signatures:'data.frame': 26 obs. of  9 variables:
#>  $ MSigDB_Hallmark_2020       :'data.frame': 18 obs. of  9 variables:
```

### Approach 2: Going step-by-step

[`rba_enrichr()`](https://rbioapi.moosa-r.com/reference/rba_enrichr.md)
is a wrapper function. It internally executes a sequence of functions
necessary to run your analysis. Alternatively, you could go step by
step. We demonstrate these steps in this section.

First, you need to retrieve the list of available Enrichr libraries.
This step is optional. You can skip it if you already know the name of
your desired libraries or if you want to run the analysis over every
available library.

``` r

# Get a list of available Enrichr libraries
libs <- rba_enrichr_libs(store_in_options = TRUE)
```

Now, you need to upload your genes list to Enrichr. By this, an
identifier will be assigned to your submitted list, which is needed for
the next step.

``` r

# Submit your gene-set to enrichr
list_id <- rba_enrichr_add_list(gene_list = genes)
```

From the returned response, we need the numeric ID in the “userListId”
element.

``` r

str(list_id)
#> List of 2
#>  $ shortId   : chr "e5bc3b7639d029135c7dd88a645044b4"
#>  $ userListId: int 134160995
```

Finally, we are ready to submit the enrichment analysis request to
Enrichr. Same as explained above for the wrapper function
[`rba_enrichr()`](https://rbioapi.moosa-r.com/reference/rba_enrichr.md),
we can supply the “gene_set_library” argument in different ways. Here we
will only select the “Table_Mining_of_CRISPR_Studies” library:

``` r

# Request the analysis
results_crispr <- rba_enrichr_enrich(
  user_list_id = list_id$userListId,
  gene_set_library = "Table_Mining_of_CRISPR_Studies"
)
```

------------------------------------------------------------------------

## Working with Other Species

Enrichr also provides libraries for model organisms. The following
functions have an `organism` argument that allows you to perform the
analysis on species other than humans:

1.  [`rba_enrichr()`](https://rbioapi.moosa-r.com/reference/rba_enrichr.md)

2.  [`rba_enrichr_enrich()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_enrich.md)

3.  [`rba_enrichr_gene_map()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_gene_map.md)

4.  [`rba_enrichr_gene_sets()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_gene_sets.md)

5.  [`rba_enrichr_libs()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_libs.md)

The available options for the organism argument are “human” (H. sapiens
& M. musculus), “fly” (D. melanogaster), “yeast” (S. cerevisiae), “worm”
(C. elegans), and “fish” (D. rerio).

------------------------------------------------------------------------

## Providing background gene list

For human and mouse, Enrichr also support the use of background gene
list. The background gene list will be used to compute the results
statistics.

If using the one-step wrapper
[`rba_enrichr()`](https://rbioapi.moosa-r.com/reference/rba_enrichr.md)
([Approach 1](#approach-1-using-the-wrapper-function)), Everything is
handled under the hood; simply supply your background gene with the
`background_genes` parameter.

``` r

# Assume we have the background genes in the variable my_background_genes
results_msig <- rba_enrichr(
  gene_list = genes,
  background_genes = my_background_genes,
  gene_set_library = "MSigDB_Hallmark_2020"
)
```

If you choose to follow the step-by-step approach, please note that
Enrichr relies on an API back-end called speedrichr to handle analysis
with a background gene list. Therefore, when performing an analysis with
background gene list, you must explicitly upload the target gene list to
speedrichr. Later steps will automatically interact with speedrichr as
long as the relevant parameters for the background genes are specified.

``` r


# Assume we have the background genes in the variable my_background_genes

# Create a vector with our genes' NCBI IDs
genes <- c(
  "p53", "BRCA1", "cdk2", "Q99835", "CDC42","CDK1","KIF23","PLK1",
  "RAC2","RACGAP1","RHOA","RHOB", "PHF14", "RBM3", "MSL1"
)

# Step 1: Upload the target gene list to speedrichr
# Note: Ensure `speedrichr = TRUE` is specified. Otherwise, it will not be 
#       possible to use the background gene list later.
list_id_spdr <- rba_enrichr_add_list(
  gene_list = genes,
  speedrichr = TRUE
)

# Step 2: Upload the background gene list
# Assume we have the background genes in the variable my_background_genes
background_id <- rba_enrichr_add_background(background_genes = my_background_genes)

# Step 3: Submit the enrichment analysis and retrieve the results
go_results <- rba_enrichr_enrich(
  user_list_id = list_id_spdr$userListId,
  background_id = background_id$backgroundid,
  gene_set_library = "GO_Biological_Process_2025"
)
```

------------------------------------------------------------------------

## See also in Functions’ manuals

Some rbioapi Enrichr functions were not covered in this vignette, be
sure to check their manuals:

- [`rba_enrichr_gene_map()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_gene_map.md)

- [`rba_enrichr_view_list()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_view_list.md)

------------------------------------------------------------------------

## How to Cite?

To cite Enrichr (Please see
<https://maayanlab.cloud/Enrichr/help#terms>):

- Chen, E.Y., Tan, C.M., Kou, Y. *et al.* Enrichr: interactive and
  collaborative HTML5 gene list enrichment analysis tool.
  *Bioinformatics* **14,** 128 (2013).
  <https://doi.org/10.1186/1471-2105-14-128>

- Maxim V. Kuleshov, Matthew R. Jones, Andrew D. Rouillard, Nicolas F.
  Fernandez, Qiaonan Duan, Zichen Wang, Simon Koplev, Sherry L. Jenkins,
  Kathleen M. Jagodnik, Alexander Lachmann, Michael G. McDermott,
  Caroline D. Monteiro, Gregory W. Gundersen, Avi Ma’ayan, Enrichr: a
  comprehensive gene set enrichment analysis web server 2016 update,
  *Nucleic Acids Research*, Volume 44, Issue W1, 8 July 2016, Pages
  W90–W97, <https://doi.org/10.1093/nar/gkw377>

- Xie, Z., Bailey, A., Kuleshov, M. V., Clarke, D. J. B.,
  Evangelista, J. E., Jenkins, S. L., Lachmann, A., Wojciechowicz, M.
  L., Kropiwnicki, E., Jagodnik, K. M., Jeon, M., & Ma’ayan, A. (2021).
  Gene set knowledge discovery with Enrichr. *Current Protocols*, 1,
  e90. doi: 10.1002/cpz1.90

To cite rbioapi:

- Moosa Rezwani, Ali Akbar Pourfathollah, Farshid Noorbakhsh, rbioapi:
  user-friendly R interface to biologic web services’ API,
  Bioinformatics, Volume 38, Issue 10, 15 May 2022, Pages 2952–2953,
  <https://doi.org/10.1093/bioinformatics/btac172>

------------------------------------------------------------------------

## Over-representation analysis Using Other Services

Other services supported by rbioapi also provide Over-representation
analysis tools. Please see the vignette article [Do with rbioapi:
Over-Representation (Enrichment) Analysis in
R](https://rbioapi.moosa-r.com/articles/rbioapi_do_enrich.md) ([link to
the documentation
site](https://rbioapi.moosa-r.com/articles/rbioapi_do_enrich.html)) for
an in-depth review.

------------------------------------------------------------------------

## Links

- [This article in rbioapi
  documentation site](https://rbioapi.moosa-r.com/articles/rbioapi_enrichr.html "https://rbioapi.moosa-r.com/articles/rbioapi_enrichr.html")

- [Functions references in rbioapi
  documentation site](https://rbioapi.moosa-r.com/reference/index.html#section-enrichr-rba-enrichr- "rbioapi reference")

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
    #>  [5] xfun_0.60         otel_0.2.0        textshaping_1.0.5 jsonlite_2.0.0   
    #>  [9] DT_0.34.0         htmltools_0.5.9   ragg_1.5.2        sass_0.4.10      
    #> [13] rmarkdown_2.31    crosstalk_1.2.2   evaluate_1.0.5    jquerylib_0.1.4  
    #> [17] fastmap_1.2.0     yaml_2.3.12       lifecycle_1.0.5   compiler_4.6.1   
    #> [21] fs_2.1.0          htmlwidgets_1.6.4 systemfonts_1.3.2 digest_0.6.39    
    #> [25] R6_2.6.1          curl_7.1.0        magrittr_2.0.5    bslib_0.11.0     
    #> [29] tools_4.6.1       pkgdown_2.2.1     cachem_1.1.0      desc_1.4.3
