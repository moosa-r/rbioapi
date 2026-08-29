# 2.B: JASPAR & rbioapi

## Introduction

[JASPAR](https://jaspar.elixir.no) is an open-access database of curated
transcription factor binding profiles. JASPAR 2026 is the 11th release.
It expands the CORE and UNVALIDATED position frequency matrix
collections and introduces a deep-learning collection containing BPNet
models and their interpreted binding profiles.

> Baydar Ovek D, et al. JASPAR 2026: expansion of transcription factor
> binding profiles and integration of deep learning models. *Nucleic
> Acids Research*. 2026;54(D1):D184-D193; doi:
> [10.1093/nar/gkaf1209](https://doi.org/10.1093/nar/gkaf1209)

------------------------------------------------------------------------

## Data Organization in JASPAR

JASPAR is a database of transcription factor binding matrices with
annotations and metadata. These entities are organized in a hierarchical
fashion that we will explore next.

### Releases

In addition to the latest JASPAR database release (2026), other active
releases are also available. Most rbioapi JASPAR functions have a
`release` argument; they use release 2026 by default.

``` r

## Call the function without any arguments to get a list of releases
releases <- rba_jaspar_releases()

## Supply a release number for details:
release_11_info <- rba_jaspar_releases(11)
```

### Collections

Within a release, matrix profiles are organized into collections. You
can use
[`rba_jaspar_collections()`](https://rbioapi.moosa-r.com/reference/rba_jaspar_collections.md)
to get a list of available collections, or read the “JASPAR Collections”
section on the [JASPAR documentation
website](https://jaspar.elixir.no/docs/ "JASPAR Documentation") for a
thorough review.

``` r

## To get a list of available collections in release 2026:
rba_jaspar_collections(release = 2026)
#>          name                                                      url
#> 1        CORE        https://jaspar.elixir.no/api/v1/collections/CORE/
#> 2 UNVALIDATED https://jaspar.elixir.no/api/v1/collections/UNVALIDATED/


## You can list information on all matrices available in a collection:
mat_in_core_2026 <- rba_jaspar_collections_matrices(collection = "CORE")
```

### Taxonomic Groups

Within each collection, the matrix profiles are organized based on main
taxonomic groups:

``` r

## To get a list of taxonomic groups in release 2026:
rba_jaspar_taxons(release = 2026)
#>             name                                                  url
#> 1         plants        https://jaspar.elixir.no/api/v1/taxon/plants/
#> 2    vertebrates   https://jaspar.elixir.no/api/v1/taxon/vertebrates/
#> 3        insects       https://jaspar.elixir.no/api/v1/taxon/insects/
#> 4   urochordates  https://jaspar.elixir.no/api/v1/taxon/urochordates/
#> 5      nematodes     https://jaspar.elixir.no/api/v1/taxon/nematodes/
#> 6          fungi         https://jaspar.elixir.no/api/v1/taxon/fungi/
#> 7        diatoms       https://jaspar.elixir.no/api/v1/taxon/diatoms/
#> 8     trematodes    https://jaspar.elixir.no/api/v1/taxon/trematodes/
#> 9  dictyostelium https://jaspar.elixir.no/api/v1/taxon/dictyostelium/
#> 10      cnidaria      https://jaspar.elixir.no/api/v1/taxon/cnidaria/
#> 11      oomycota      https://jaspar.elixir.no/api/v1/taxon/oomycota/


## You can list information on all matrices available in a taxonomic group:
mat_in_insects <- rba_jaspar_taxons_matrices(tax_group = "insects")
```

### Species

As we go down in the data organization hierarchy, each taxonomic group
consists of species:

``` r

## To get a list of species in release 2026:
species <- rba_jaspar_species(release = 2026)
head(species)
#>   tax_id                          species
#> 1   4151                Antirrhinum majus
#> 2  81972 Arabidopsis lyrata subsp. lyrata
#> 3   3702             Arabidopsis thaliana
#> 4 162425             Aspergillus nidulans
#> 5   9913                       Bos taurus
#> 6   6238          Caenorhabditis briggsae
#>                                               url
#> 1   https://jaspar.elixir.no/api/v1/species/4151/
#> 2  https://jaspar.elixir.no/api/v1/species/81972/
#> 3   https://jaspar.elixir.no/api/v1/species/3702/
#> 4 https://jaspar.elixir.no/api/v1/species/162425/
#> 5   https://jaspar.elixir.no/api/v1/species/9913/
#> 6   https://jaspar.elixir.no/api/v1/species/6238/
#>                                        matrix_url
#> 1   https://jaspar.elixir.no/api/v1/species/4151/
#> 2  https://jaspar.elixir.no/api/v1/species/81972/
#> 3   https://jaspar.elixir.no/api/v1/species/3702/
#> 4 https://jaspar.elixir.no/api/v1/species/162425/
#> 5   https://jaspar.elixir.no/api/v1/species/9913/
#> 6   https://jaspar.elixir.no/api/v1/species/6238/

## You can list information on all matrices available for a species:
mat_in_human <- rba_jaspar_species_matrices(tax_id = 9606)
```

------------------------------------------------------------------------

## Matrix Profiles

### Search Matrix Profiles

Retrieving a list of every matrix available in a given category is not
the only option. You can also build a search query using
`rba_jaspar_matrix_search`. Note that this is a search function, you are
not required to fill every argument. You may use any combination of
arguments you see fit to build your query. You can even call the
function without any argument to get a list of all the matrix profiles.
For instance:

``` r

## Get a list of all the available matrix profile:
all_matrices <- rba_jaspar_matrix_search()

## Search FOX:
FOX_matrices <- rba_jaspar_matrix_search(term = "FOX")

## Transcription factors named FOXP3
FOXP3_matrices <- rba_jaspar_matrix_search(term = "FOXP3")

## Transcription factors of Zipper-Type Class
zipper_matrices <- rba_jaspar_matrix_search(tf_class = "Zipper-Type")

## Transcription factors of Zipper-Type Class in PBM collection
zipper_pbm_matrices <- rba_jaspar_matrix_search(
  tf_class = "Zipper-Type",
  collection = "PBM"
)
```

### List Matrix Profiles Associated to a Base identifier

Since JASPAR release 2010, matrix profiles have been versioned. A matrix
profile identifier follows a “base_id.version” naming scheme; for
example, “MA0600.2” corresponds to the second version of a matrix with
base ID MA0600. You can use `rba_jaspar_matrix_versions` to get a list
of matrix profiles with a given base ID. Also note that functions used
to list available matrices generally have an argument called
`only_last_version`.

``` r

## Get matrix profiles versions associated to a base id
MA0600_versions <- rba_jaspar_matrix_versions("MA0600")
```

### Get a Matrix Profile

Now that you listed or searched for matrix profiles, you can use
`rba_jaspar_matrix` to retrieve matrix profiles. There are two ways in
which you can use this function:

#### Get Matrix and Annotations as an R Object

To do that, only fill in the `matrix_id` argument in `rba_jaspar_matrix`

``` r

pfm_matrix <- rba_jaspar_matrix(matrix_id = "MA0600.2")

## you can find the matrix in the pfm element along with
## other elements which correspond to annotations and details
str(pfm_matrix)
#> List of 24
#>  $ matrix_id    : chr "MA0600.2"
#>  $ name         : chr "RFX2"
#>  $ base_id      : chr "MA0600"
#>  $ version      : int 2
#>  $ collection   : chr "CORE"
#>  $ sequence_logo: chr "https://jaspar.elixir.no/static/logos/svg/MA0600.2.svg"
#>  $ versions_url : chr "https://jaspar.elixir.no/api/v1/matrix/MA0600/versions"
#>  $ sites_url    : NULL
#>  $ pfm          : num [1:4, 1:16] 1381 5653 4042 2336 270 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:4] "A" "C" "G" "T"
#>   .. ..$ : NULL
#>  $ class        : chr "Fork head/winged helix factors"
#>  $ family       : chr "RFX-related factors"
#>  $ tfe_id       : list()
#>  $ medline      : chr "8754849"
#>  $ pazar_tf_id  : list()
#>  $ remap_tf_name: chr "RFX2"
#>  $ source       : chr "23332764"
#>  $ tax_group    : chr "vertebrates"
#>  $ type         : chr "HT-SELEX"
#>  $ tfe_ids      : list()
#>  $ pubmed_ids   : chr "8754849"
#>  $ pazar_tf_ids : list()
#>  $ uniprot_ids  : chr "P48378"
#>  $ species      :'data.frame':   1 obs. of  2 variables:
#>   ..$ tax_id: int 9606
#>   ..$ name  : chr "Homo sapiens"
#>  $ tffm         :List of 7
#>   ..$ tffm_id        : chr "TFFM0576.1"
#>   ..$ base_id        : chr "TFFM0576"
#>   ..$ version        : int 1
#>   ..$ log_p_1st_order: num 6275
#>   ..$ log_p_detailed : num 6660
#>   ..$ experiment_name: chr "CistromeDB_58298"
#>   ..$ tffm_url       : chr "https://jaspar.elixir.no/api/v1/tffm/TFFM0576.1/"
```

#### Save a Matrix as a File in a Specific Format

JASPAR provides position frequency matrices (PFM) formatted as **Raw
PFM**, **JASPAR**, **TRANSFAC**, **YAML**, and **MEME**. You can
download a matrix profile as a file with any of these formats. To do
that, you should use the `file_format` and `save_to` arguments available
in `rba_jaspar_matrix`. There are two notes here:

1.  In this case, the function saves your matrix as a file and returns
    the unparsed content as a character string.

2.  The `save_to` argument in this and other rbioapi functions can be
    used in several ways:  
    2.1. `save_to = NA`: rbioapi automatically generates a file path
    under your working directory, saves the file there, and reports the
    path in a message.  
    2.2. `save_to = file_name` without a directory: rbioapi saves the
    file with the supplied name in your working directory.  
    2.3. `save_to = directory_path`: rbioapi saves the file with an
    appropriate name in that directory.  
    2.4. `save_to = file_path`: rbioapi saves the file to the exact
    path. Ensure that the path’s extension matches the requested file
    format; otherwise, rbioapi uses the supplied extension and issues a
    warning.

    In any of the aforementioned cases, the file path can be absolute or
    relative.

``` r

## Different ways in which you can save the matrix file:
meme_matrix1 <- rba_jaspar_matrix(
  matrix_id = "MA0600.2",
  file_format = "meme"
)

meme_matrix2 <- rba_jaspar_matrix(
  matrix_id = "MA0600.2",
  file_format = "meme",
  save_to = "my_matrix.meme"
)

meme_matrix3 <- rba_jaspar_matrix(
  matrix_id = "MA0600.2",
  file_format = "meme",
  save_to = "c:/rbioapi"
)

meme_matrix4 <- rba_jaspar_matrix(
  matrix_id = "MA0600.2",
  file_format = "meme",
  save_to = "c:/rbioapi/my_matrix.meme"
)
```

### Get Binding Sites for a Matrix Profile

If available, you can retrieve information on binding sites associated
with a matrix profile. The result includes a data frame of genomic
coordinates, URLs to FASTA and BED files, and other annotations.

``` r

## Get binding sites for a matrix profile:
binding_sites <- rba_jaspar_sites(matrix_id = "MA0600.2")
```

------------------------------------------------------------------------

## TF flexible models (TFFMs)

JASPAR also stores and assigns identifiers to TF flexible models
(TFFMs). As with position frequency matrices (PFMs), you can search
TFFMs or retrieve information and annotations using a TFFM identifier.
TFFM IDs are versioned and follow the `base_id.version` format.

``` r

## Search TFFMs. This is a search function. Thus, what has been presented
## in the `Search Matrix Profiles` section also applies here:

## Get a list of all available TFFM profiles:
all_tffms <- rba_jaspar_tffm_search()

## Search FOX:
FOX_tffms <- rba_jaspar_tffm_search(term = "FOX")

## Transcription factors named FOXP3
FOXP3_tffms <- rba_jaspar_tffm_search(term = "FOXP3")

## Transcription factors in the insects taxonomic group
insects_tffms <- rba_jaspar_tffm_search(tax_group = "insects")
```

``` r

## Now that you have a TFFM ID, you can retrieve it
TFFM0056 <- rba_jaspar_tffm("TFFM0056.3")

str(TFFM0056)
#>  chr "JASPAR returned an error response with HTTP Status '500' (Server Error: Internal Server Error).\nThe response d"| __truncated__
```

------------------------------------------------------------------------

## How to Cite?

To cite JASPAR (Please see <https://jaspar.elixir.no/faq/>):

- Baydar Ovek D, et al. *JASPAR 2026: expansion of transcription factor
  binding profiles and integration of deep learning models*. Nucleic
  Acids Res. 2026;54(D1):D184-D193;
  <https://doi.org/10.1093/nar/gkaf1209>
- Khan, A. and Mathelier, A. *JASPAR RESTful API: accessing JASPAR data
  from any programming language*. Bioinformatics, 2017,
  <https://doi.org/10.1093/bioinformatics/btx804>

To cite rbioapi:

- Moosa Rezwani, Ali Akbar Pourfathollah, Farshid Noorbakhsh, rbioapi:
  user-friendly R interface to biologic web services’ API,
  Bioinformatics, Volume 38, Issue 10, 15 May 2022, Pages 2952–2953,
  <https://doi.org/10.1093/bioinformatics/btac172>

------------------------------------------------------------------------

## Links

- [This article in rbioapi
  documentation site](https://rbioapi.moosa-r.com/articles/rbioapi_jaspar.html "2.G: JASPAR & rbioapi")

- [Functions references in rbioapi
  documentation site](https://rbioapi.moosa-r.com/reference/index.html#section-jaspar-rba-jaspar- "rbioapi reference")

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
