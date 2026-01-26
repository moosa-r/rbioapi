# 2.B: JASPAR & rbioapi

## Introduction

Directly quoting from Fornes O, Castro-Mondragon JA, Khan A, et al:

> JASPAR ([https://jaspar.elixir.no)](https://jaspar.elixir.no) is an
> open-access database of curated, non-redundant transcription factor
> (TF)-binding profiles stored as position frequency matrices (PFMs) for
> TFs across multiple species in six taxonomic groups. In this 8th
> release of JASPAR, the CORE collection has been expanded with 245 new
> PFMs (169 for vertebrates, 42 for plants, 17 for nematodes, 10 for
> insects, and 7 for fungi), and 156 PFMs were updated (125 for
> vertebrates, 28 for plants and 3 for insects). These new profiles
> represent an 18% expansion compared to the previous release.
>
> source:  
> Fornes O, Castro-Mondragon JA, Khan A, et al. JASPAR 2020: update of
> the open-access database of transcription factor binding profiles.
> *Nucleic Acids Res*. 2019; doi:
> [10.1093/nar/gkz1001](https://doi.org/10.1093/nar/gkz1001)

------------------------------------------------------------------------

## Data Organization in JASPAR

JASPAR is a database of transcription factor binding matrices with
annotations and metadata. These entities are organized in a hierarchical
fashion that we will explore next.

### Releases

In addition to the latest JASPAR database release (2020), other active
releases are also available. Most of the rbioapi JASPAR functions have a
`release` argument that allows you to use other database releases.

``` r
## Call the function without any arguments to get a list of releases
releases <- rba_jaspar_releases()

## Supply a release number for details:
release_7_info <- rba_jaspar_releases(7)
```

### Collections

Within a release, Matrix profiles are organized into collections, You
can use
[`rba_jaspar_collections()`](https://rbioapi.moosa-r.com/reference/rba_jaspar_collections.md)
to get a list of available collections, or read “JASPAR Collections”
section in [documentation page in JASPAR
web-site](https://jaspar.elixir.no/docs/ "JASPAR Documentation") for a
thorough review.

``` r
## To get a list of available collection in release 2020:
rba_jaspar_collections(release = 2020)
#>          name                                                      url
#> 1        CORE        https://jaspar.elixir.no/api/v1/collections/CORE/
#> 2 UNVALIDATED https://jaspar.elixir.no/api/v1/collections/UNVALIDATED/


## You can list information of all matrices available in a collection:
mat_in_core_2020 <- rba_jaspar_collections_matrices(collection = "CORE")
```

### Taxonomic Groups

Within each collection, the matrix profiles are organized based on main
taxonomic groups:

``` r
## To get a list of taxonomic groups in release 2020:
rba_jaspar_taxons(release = 2020)
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


## You can list information of all matrices available in a taxonomic group:
mat_in_insects <- rba_jaspar_taxons_matrices(tax_group = "insects")
```

### Species

As we go down in the data organization hierarchy, Each taxonomic group
consist of species:

``` r
## To get a list of species in release 2020:
species <- rba_jaspar_species(release = 2020)
head(species)
#> [1] "The server returned HTTP Status '500' (Redirection: Internal Server Error)."

## You can list information of all matrices available in a specie:
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

Since JASPAR release 2010, the matrix profiles are versioned. A matrix
profile Identifier has a “base_id.version” naming schema; for example
“MA0600.2” corresponds to the second version of a matrix with base ID
MA0600. You can Use `rba_jaspar_matrix_versions` to get a list of matrix
profiles with a given base ID. Also note that some functions, generally
those that are used to list available matrices, have an argument called
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

#### Save a Matrix a File in Specific Format

JASPAR provides position frequency matrices (PFM) formatted as **Raw
PFM**, **JASPAR**, **TRANSFAC**, **YAML**, and **MEME**. You can
download a matrix profile as a file with any of these formats. To do
that, You should use the `file_format` and `save_to` arguments available
in `rba_jaspar_matrix`. There are two notes here:

1.  In this case, the function will save your matrix as a file and
    returns the un-parsed content of the file as a character string.

2.  The `save_to` argument in this function, and in fact through any
    rbioapi function can be used in many ways:  
    2.1. save_to = NA: rbioapi will automatically generate a file path
    under your working directory, save the file in that path , and
    informs you with a message.  
    2.2 save_to = file_name without path: rbioapi will save the file
    with your supplied name in your working directory.  
    2.3. save_to = a directory path (without file): rbioapi will save
    the file with a proper name in that directory.  
    2.4. save_to = a file path (i.e. ending with .extension): rbioapi
    will save the file exactly to this path. Make sure that the file
    extension of the path matches your requested file format. If this
    was not the case, rbioapi will save the file with the extension
    supplied in the path, but issues a warning to inform you about that.

    In any of the aforementioned cases, the file path can be absolute or
    relative.

``` r
## Different wqays in which you can save the matrix file:
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

### Get Binding Sites of a Matrix Profiles

If available, you can retrieve information on binding sites associated
with a matrix profile. The information includes a data frame of genomic
coordination of the binding site, URL to FASTA and BED files, along with
other annotations.

``` r
## Get binding site of a matrix profile:
binding_sites <- rba_jaspar_sites(matrix_id = "MA0600.2")
```

------------------------------------------------------------------------

## TF flexible models (TFFMs)

JASPAR also stores and assigns identifiers to TF flexible models
(TFFMs). Just like PFM (position frequency matrices), you can search
TFFMs or retrieve information and annotations using a TFFM Identifier.
TFFM IDs are versioned, meaning that they are in base_id.version format.

``` r
## Search TFFMs. This is a search function. Thus, what has been presented
## in `Search Matrix Profiles` section also applies here:

## Get a list of all the available matrix profile:
all_tffms <- rba_jaspar_tffm_search()

## Search FOX:
FOX_tffms <- rba_jaspar_tffm_search(term = "FOX")

## Transcription factors named FOXP3
FOXP3_tffms <- rba_jaspar_tffm_search(term = "FOXP3")

## Transcription factors of insects taxonomic group
insects_tffms <- rba_jaspar_tffm_search(tax_group = "insects")
```

``` r
## Now that you have a TFFM ID, you can retrieve it
TFFM0056 <- rba_jaspar_tffm("TFFM0056.3")

str(TFFM0056)
#>  chr "The server returned HTTP Status '500' (Redirection: Internal Server Error)."
```

------------------------------------------------------------------------

## How to Cite?

To cite JASPAR (Please see <https://jaspar.elixir.no/faq/>):

- Rauluseviciute I, Riudavets-Puig R, Blanc-Mathieu R, Castro-Mondragon
  JA, Ferenc K, Kumar V, Lemma RB, Lucas J, Chèneby J, Baranasic D, Khan
  A, Fornes O, Gundersen S, Johansen M, Hovig E, Lenhard B, Sandelin A,
  Wasserman WW, Parcy F, Mathelier A *JASPAR 2024: 20th anniversary of
  the open-access database of transcription factor binding profiles*
  Nucleic Acids Res. in_press; <https://doi.org/10.1093/nar/gkad1059>
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
    #>  [1] digest_0.6.39     desc_1.4.3        R6_2.6.1          fastmap_1.2.0    
    #>  [5] xfun_0.56         cachem_1.1.0      knitr_1.51        htmltools_0.5.9  
    #>  [9] rmarkdown_2.30    lifecycle_1.0.5   cli_3.6.5         sass_0.4.10      
    #> [13] pkgdown_2.2.0     textshaping_1.0.4 jquerylib_0.1.4   systemfonts_1.3.1
    #> [17] compiler_4.5.2    httr_1.4.7        tools_4.5.2       ragg_1.5.0       
    #> [21] curl_7.0.0        bslib_0.9.0       evaluate_1.0.5    yaml_2.3.12      
    #> [25] otel_0.2.0        jsonlite_2.0.0    rlang_1.1.7       fs_1.6.6         
    #> [29] htmlwidgets_1.6.4
