# 2.D: PANTHER & rbioapi

## Introduction

Directly quoting the the paper published by
[PANTHER](https://www.pantherdb.org "Protein Analysis THrough Evolutionary Relationships (PANTHER)")
(Protein Analysis THrough Evolutionary Relationships) authors:

> The PANTHER classification system (<https://www.pantherdb.org>) a
> comprehensive system that combines genomes, gene function , pathways
> and statistical analysis tools to enable to analyze large-scale
> genome-wide experimental data. The system (PANTHER v.14.0) covers 131
> complete genomes organized gene families and subfamilies; evolutionary
> relationships between are represented in phylogenetic trees, multiple
> sequence and statistical models (hidden Markov models (HMMs)). The
> families and subfamilies are annotated with Gene Ontology (GO) terms,
> sequences are assigned to PANTHER pathways. A suite of tools has built
> to allow users to browse and query gene functions and analyze-scale
> experimental data with a number of statistical tests. is widely used
> by bench scientists, bioinformaticians, computer and systems
> biologists.
>
> (source: Mi, Huaiyu, et al. “Protocol Update for large-scale genome
> and gene function analysis with the PANTHER classification system (v.
> 14.0).” *Nature protocols* 14.3 (2019): 703-721)

The available tools in PANTHER’s **RESTful API services** can be divided
into 3 broad categories: Mapping genes, retrieving information, and
research tools. Herein, we provide a very short introduction; you can
always check functions’ manuals for detailed guides and examples.

------------------------------------------------------------------------

## Map genes

- [`rba_panther_mapping()`](https://rbioapi.moosa-r.com/reference/rba_panther_mapping.md):
  map your gene-set to PANTHER database and retrieve attributes and
  annotations associated with your genes

- [`rba_panther_ortholog()`](https://rbioapi.moosa-r.com/reference/rba_panther_ortholog.md):
  Retrieve Orthologs of your genes

- [`rba_panther_homolog()`](https://rbioapi.moosa-r.com/reference/rba_panther_homolog.md):
  Retrieve Homologs of your genes

------------------------------------------------------------------------

## Get information

- `rba_panther_info`(): Retrieve a list of PANTHER’s supported
  organisms, datasets, families, or pathways

- `rba_panther_family`(): Retrieve Orthologs, MSA, or Tree topology of a
  given PANTHER family.

------------------------------------------------------------------------

## Gene List Analysis

[`rba_panther_enrich()`](https://rbioapi.moosa-r.com/reference/rba_panther_enrich.md)
is equivalent to [Gene List analysis tool’s
webpage](https://www.pantherdb.org/index.jsp "PANTHER Gene List Analysis").
Depending on the provided input’s class, PANTHER will perform either
over-representation analysis or statistical enrichment analysis. Below
we demonstrate how to perform such analyses.

### Get the available annotation datasets

First, we need to select an annotation dataset to conduct the analysis
based on it. Each annotation dataset contains a collection of terms,
where each term is associated with a group of genes.

To retrieve the list of available annotation datasets in PANTHER, use
the following command:

``` r
annots <- rba_panther_info(what = "datasets")
```

Please note that you should use the ID of the desired annotation
dataset, not its label. For example, using `"biological_process"` is
incorrect; you should rather use `"GO:0008150"`.

### Submit the analysis request

Depending on the provided input, PANTHER will conduct two types of
analysis:

1.  If a character vector is supplied, over-representation analysis will
    be performed using either Fisher’s exact or binomial test.

2.  If a data frame with gene identifiers and their corresponding
    expression values is supplied, statistical enrichment test is
    performed using Mann-Whitney U (Wilcoxon Rank-Sum) test.

rbioapi determines the proper analysis based on the class of the `genes`
parameter. Please refer to the details section of
[`rba_panther_enrich()`](https://rbioapi.moosa-r.com/reference/rba_panther_enrich.md)
function manual for more information.

#### Over-representation analysis

Now, suppose we want to perform an over-representation analysis against
the ‘GO biological process’ annotation dataset. In this example, we only
provide the gene names, thus over-representation analysis will be
conducted:

``` r
# Create a variable to store the genes vector
my_genes_vec <- c(
  "p53", "BRCA1", "cdk2", "Q99835", "CDC42", "CDK1","KIF23","PLK1",
  "RAC2","RACGAP1","RHOA", "RHOB", "PHF14", "RBM3", "MSL1"
)

# Submit the analysis request.
enriched <- rba_panther_enrich(
  genes = my_genes_vec,
  organism = 9606,
  annot_dataset = "GO:0008150",
  cutoff = 0.05
)
#> Performing PANTHER over-representation analysis (Fisher's exact test) on 15 genes from `organism 9606` against `GO:0008150` datasets.

# Note that we didn't supply the `test_type` parameter.
# In this case, the function will default to using Fisher's exact test # (i.e. `test_type = "FISHER"`).
# You may also use binomial test for the over-representation analysis # (i.e. `test_type = "BINOMIAL"`).
```

#### Statistical enrichment analysis

As you can see in the above example, only a vector of gene names was
used. We can also use the corresponding expression values of the genes.
In this case, PANTHER will perform a statistical enrichment analysis.

To do so, the only change will be to supply a data frame to the `genes`
parameter. Note that in this case, Mann-Whitney U Test will be
performed. The data frame should have two columns: the first column
should contain the gene identifiers as a character vector; the second
column should contain the corresponding expression values as a numeric
vector.

``` r
# Create a variable to store the data frame
my_genes_df <- data.frame(
  genes = c(
    "p53", "BRCA1", "cdk2", "Q99835", "CDC42", "CDK1","KIF23","PLK1",
    "RAC2","RACGAP1","RHOA", "RHOB", "PHF14", "RBM3", "MSL1"
  ),
  ## generate random expression values
  expression = runif(15, 0, 10) 
)

# Submit the analysis request.
enriched <- rba_panther_enrich(
  genes = my_genes_df,
  organism = 9606,
  annot_dataset = "GO:0008150",
  cutoff = 0.05
)

# Note that we didn't supply the `test_type` parameter.
# In this case, the function will default to Mann-Whitney U Test
# (i.e. `test_type = "Mann-Whitney"`).
# This is the only valid value for the statistical enrichment analysis test,
# thus ommiting or supplying it will not make a difference.
```

**Please Note:** Other services supported by rbioapi also provide
Over-representation analysis tools. Please see the vignette article [Do
with rbioapi: Over-Representation (Enrichment) Analysis in
R](https://rbioapi.moosa-r.com/articles/rbioapi_do_enrich.md) ([link to
the documentation
site](https://rbioapi.moosa-r.com/articles/rbioapi_do_enrich.html)) for
an in-depth review.

------------------------------------------------------------------------

## Tree grafter

[`rba_panther_tree_grafter()`](https://rbioapi.moosa-r.com/reference/rba_panther_tree_grafter.md)
is an equivalent to the “[Graft sequence into PANTHER library of
trees](https://www.pantherdb.org/tools/sequenceSearchForm.jsp)” tool.

------------------------------------------------------------------------

## How to Cite?

To cite PANTHER (Please see
<https://www.pantherdb.org/publications.jsp#HowToCitePANTHER>):

- Huaiyu Mi, Dustin Ebert, Anushya Muruganujan, Caitlin Mills,
  Laurent-Philippe Albou, Tremayne Mushayamaha, Paul D Thomas, PANTHER
  version 16: a revised family classification, tree-based classification
  tool, enhancer regions and extensive API, *Nucleic Acids Research*,
  Volume 49, Issue D1, 8 January 2021, Pages D394–D403,
  <https://doi.org/10.1093/nar/gkaa1106>

To cite rbioapi:

- Moosa Rezwani, Ali Akbar Pourfathollah, Farshid Noorbakhsh, rbioapi:
  user-friendly R interface to biologic web services’ API,
  Bioinformatics, Volume 38, Issue 10, 15 May 2022, Pages 2952–2953,
  <https://doi.org/10.1093/bioinformatics/btac172>

------------------------------------------------------------------------

## Links

- [This article in rbioapi
  documentation site](https://rbioapi.moosa-r.com/articles/rbioapi_panther.html "2.C: PANTHER & rbioapi")

- [Functions references in rbioapi
  documentation site](https://rbioapi.moosa-r.com/reference/index.html#section-enrichr-rba-enrichr- "rbioapi reference")

- [rbioapi vignette
  index](https://rbioapi.moosa-r.com/articles/rbioapi.md "rbioapi: User-Friendly R Interface to Biologic Web Services' API")

------------------------------------------------------------------------

## Session info

    #> R version 4.5.2 (2025-10-31)
    #> Platform: x86_64-pc-linux-gnu
    #> Running under: Ubuntu 24.04.3 LTS
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
    #>  [1] httr_1.4.7        cli_3.6.5         knitr_1.51        rlang_1.1.7      
    #>  [5] xfun_0.56         otel_0.2.0        textshaping_1.0.4 jsonlite_2.0.0   
    #>  [9] DT_0.34.0         htmltools_0.5.9   ragg_1.5.0        sass_0.4.10      
    #> [13] rmarkdown_2.30    crosstalk_1.2.2   evaluate_1.0.5    jquerylib_0.1.4  
    #> [17] fastmap_1.2.0     yaml_2.3.12       lifecycle_1.0.5   compiler_4.5.2   
    #> [21] fs_1.6.6          htmlwidgets_1.6.4 systemfonts_1.3.1 digest_0.6.39    
    #> [25] R6_2.6.1          curl_7.0.0        magrittr_2.0.4    bslib_0.9.0      
    #> [29] tools_4.5.2       pkgdown_2.2.0     cachem_1.1.0      desc_1.4.3
