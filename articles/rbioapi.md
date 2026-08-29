# 1: rbioapi: User-Friendly R Interface to Biologic Web Services' API

## What does rbioapi do?

Currently fully supports **Enrichr**, **JASPAR**, **miEAA**,
**PANTHER**, **Reactome**, **STRING**, and **UniProt**!

The goal of rbioapi is to provide a user-friendly and consistent
interface to biological databases and services: In a way that insulates
the user from technicalities of using web services API and creates a
unified and easy-to-use interface to biological and medical web
services.

With rbioapi, you do not need to have technical knowledge about web
services API or learn how to work with a new package for every biologic
service or database. This an ongoing project; New databases and services
will be added periodically. Feel free to
[suggest](https://github.com/moosa-r/rbioapi/issues "Issue section in rbioapi GitHub repository")
any databases or services you often use.

------------------------------------------------------------------------

## What is Supported by rbioapi?

rbioapi is dedicated to **Biological or Medical** databases and web
services. Currently, rbioapi supports and covers every API resources in
the following services: (in alphabetical order):

On CRAN (Stable) version: (<https://cran.r-project.org/package=rbioapi>)

1.  [Enrichr](https://maayanlab.cloud/Enrichr/ "Enrichr") ([rbioapi
    vignette
    article](https://rbioapi.moosa-r.com/articles/rbioapi_enrichr.md "rbioapi & Enrichr vignette article"))
    ^((new))
2.  [JASPAR](https://jaspar.elixir.no/ "JASPAR - A database of transcription factor binding profiles")
    ([rbioapi vignette
    article](https://rbioapi.moosa-r.com/articles/rbioapi_jaspar.md "rbioapi & Enrichr vignette article"))
    ^((new))
3.  [miEAA](https://ccb-compute2.cs.uni-saarland.de/mieaa/ "miRNA Enrichment Analysis and Annotation Tool (miEAA)")
    ([rbioapi vignette
    article](https://rbioapi.moosa-r.com/articles/rbioapi_mieaa.md "rbioapi & miEAA vignette article"))
4.  [PANTHER](https://www.pantherdb.org "Protein Analysis THrough Evolutionary Relationships (PANTHER)")
    ([rbioapi vignette
    article](https://rbioapi.moosa-r.com/articles/rbioapi_panther.md "rbioapi & PANTHER vignette article"))
5.  [Reactome](https://reactome.org/) ([rbioapi vignette
    article](https://rbioapi.moosa-r.com/articles/rbioapi_reactome.md "rbioapi & Reactome vignette article"))
6.  [STRING](https://string-db.org/ "STRING: Protein-Protein Interaction Networks Functional Enrichment Analysis")
    ([rbioapi vignette
    article](https://rbioapi.moosa-r.com/articles/rbioapi_string.md "rbioapi & STRING vignette article"))
7.  [UniProt](https://www.uniprot.org "Universal Protein Resource (UniProt)")
    ([rbioapi vignette
    article](https://rbioapi.moosa-r.com/articles/rbioapi_uniprot.md "rbioapi & UniProt vignette article"))

Only on Github (Developmental) version:
(<https://github.com/moosa-r/rbioapi/>):

1.  currently none

Each of the services has its dedicated vignette article. In this
article, I will write about the general framework of rbioapi. Make sure
to check the vignette article of each service to learn more about how to
use them.

**Note That:** rbioapi is an ongoing project. New databases and services
will be implemented periodically in order to gradually make the package
as comprehensive as possible. Do you see yourself often using a certain
database/service? Feel free to suggest any database/service by creating
an issue on our GitHub
[repository](https://github.com/moosa-r/ "rbioapi GitHub repositry"). I
will appreciate any suggestions.

------------------------------------------------------------------------

## How to install?

You can install the [stable release version of
rbioapi](https://cran.r-project.org/package=rbioapi "rbioapi: User-Friendly R Interface to Biologic Web Services' API")
from
[CRAN](https://cran.r-project.org/ "The Comprehensive R Archive Network")
with:

``` r

install.packages("rbioapi")
```

However, the CRAN version is released at longer intervals, You can
install the most recent -development- version from
[GitHub](https://github.com/moosa-r/rbioapi/ "rbioapi repository on GitHub")
with:

``` r

install.packages("remotes")
remotes::install_github("moosa-r/rbioapi")
```

Now, we can load the package:

``` r

library(rbioapi)
```

------------------------------------------------------------------------

## Naming conventions

To keep the namespace organized, functions names follow this pattern:

    rba_[service_name]_[resource_name]

For example,
[`rba_string_version()`](https://rbioapi.moosa-r.com/reference/rba_string_version.md)
will call
[STRING](https://string-db.org/ "STRING: Protein-Protein Interaction Networks Functional Enrichment Analysis")’s
version resource.

``` r

rba_string_version()
#> Retrieving the STRING database version and address used by rbioapi.
#> $string_version
#> [1] "12.0"
#> 
#> $stable_address
#> [1] "https://version-12-0.string-db.org"
```

Thus, to this version, rbioapi function will have one of the following
naming schema:

1.  rba_enrichr\_\*
2.  rba_jaspar\_\*
3.  rba_mieaa\_\*
4.  rba_panther\_\*
5.  rba_reactome\_\*
6.  rba_string\_\*
7.  rba_uniprot\_\*

There are four exceptions:
[`rba_options()`](https://rbioapi.moosa-r.com/reference/rba_options.md),
[`rba_connection_test()`](https://rbioapi.moosa-r.com/reference/rba_connection_test.md),
[`rba_pages()`](https://rbioapi.moosa-r.com/reference/rba_pages.md), and
[`rba_metadata()`](https://rbioapi.moosa-r.com/reference/rba_metadata.md);
these are helper functions. More on that later.

------------------------------------------------------------------------

## Changing the options

To give users greater control, rbioapi offers multiple configurable
options. See the manual of
[`rba_options()`](https://rbioapi.moosa-r.com/reference/rba_options.md)
function for a full description of available options. In short, some of
the options will govern rbioapi’s connection with servers (e.g. timeout,
retry) and some of the options will modify your experience with rbioapi
(e.g. verbose, diagnostics, save_file). There are two ways that you may
use to change any option. Also, you can get table of available rbioapi
options and their current values by calling
[`rba_options()`](https://rbioapi.moosa-r.com/reference/rba_options.md)without
any argument:

``` r

rba_options()
#>    rbioapi_option current_value            allowed_value
#> 1     diagnostics         FALSE     Logical (TRUE/FALSE)
#> 2        dir_name       rbioapi                Character
#> 3        progress         FALSE     Logical (TRUE/FALSE)
#> 4       retry_max             0   Numeric (0 or greater)
#> 5      retry_wait            10   Numeric (0 or greater)
#> 6       save_file         FALSE     Logical (TRUE/FALSE)
#> 7      skip_error          TRUE     Logical (TRUE/FALSE)
#> 8         timeout            30 Numeric (0.1 or greater)
#> 9         verbose          TRUE     Logical (TRUE/FALSE)
#> 10       metadata         FALSE     Logical (TRUE/FALSE)
```

Now, let us consider the ways in which we can alter the settings:

### Change the option globally

Changing an option globally means that for the rest of your R session,
any rbioapi function will respect the changed option. To do this, use
`rba_options().` Each argument in this function corresponds to a certain
option; Thus by running this function with your desired new values, you
could globally alter that rbioapi option. for example:

``` r

rba_options(save_file = TRUE)
## From now on, the raw file of server's response will be saved to your working directory.

rba_options(verbose = FALSE)
## From now on, the package will be quiet.
```

### Change the option only within a function call

You can pass additional arguments to any rbioapi function using
“ellipsis” (the familiar `…` or dot dot dot!). Meaning that you can call
any function with additional arguments where each is ‘option = value’
pair. This way, any changes in options will be confined within that
particular function call. For example:

``` r

## Save the server's raw response file:
x <- rba_reactome_species(
  only_main = TRUE,
  save_file = "reactome_species.json"
)

## Also, in the case of connection failure, retry up to 10 times:
x <- rba_reactome_species(
  only_main = TRUE,
  save_file = "reactome_species.json",
  retry_max = 10
)
```

``` r

## Run these codes in your own R session to see the difference:

## show internal diagnostics boring details
x <- rba_uniprot_proteins_crossref(
  db_id = "CD40",
  db_name = "HGNC",
  diagnostics = TRUE
)

## The next function you call, will still use the default rbioapi options
x <- rba_uniprot_proteins_crossref(
  db_id = "CD40",
  db_name = "HGNC"
)
```

## Connection test

The
[`rba_connection_test()`](https://rbioapi.moosa-r.com/reference/rba_connection_test.md)
helper checks your internet connection and whether supported services
and databases are available. If you encounter an error while using
rbioapi, run this function to check your internet connection and the
availability of supported services.

``` r

rba_connection_test(print_output = TRUE)
#> Checking Your connection to the Databases currently supported by rbioapi:
#> --->>> Internet :
#> +++ Connected to the Internet.
#> --->>> Enrichr :
#> +++ The server is responding.
#> --->>> Ensembl :
#> +++ The server is responding.
#> --->>> JASPAR :
#> +++ The server is responding.
#> --->>> miEAA :
#> !!! failed with error:
#>  Error in curl::curl_fetch_memory(url, handle = handle) : 
#>   Timeout was reached [ccb-compute2.cs.uni-saarland.de]:
#> SSL connection timeout
#> --->>> PANTHER :
#> +++ The server is responding.
#> --->>> Reactome Content Service :
#> +++ The server is responding.
#> --->>> Reactome Analysis Service :
#> +++ The server is responding.
#> --->>> STRING :
#> +++ The server is responding.
#> --->>> UniProt :
#> +++ The server is responding.
```

------------------------------------------------------------------------

## Iterating over paginated results

Some API resources return paginated responses, particularly when a query
can produce many records. Their rbioapi functions generally expose a
page argument, such as `page_number`, and may also expose a page-size
argument.
[`rba_pages()`](https://rbioapi.moosa-r.com/reference/rba_pages.md)
repeats one quoted rbioapi call for the pages you request.

Take rba_uniprot_taxonomy_name as an example. This function allows you
to search taxonomic nodes in
[UniProt](https://www.uniprot.org "Universal Protein Resource (UniProt)").
The response can potentially have a huge size, so
[UniProt](https://www.uniprot.org "Universal Protein Resource (UniProt)")
returns a paginated response. For example, if we search for nodes that
contain “adenovirus”, there is a large number of hits:

``` r

adeno <- rba_uniprot_taxonomy_name(
  name = "adenovirus",
  search_type = "contain",
  page_number = 1
)

str(adeno, max.level = 2)
#> List of 2
#>  $ taxonomies:'data.frame':  200 obs. of  8 variables:
#>   ..$ taxonomyId    : int [1:200] 10509 10510 10511 10512 10513 10514 10515 10519 10521 10522 ...
#>   ..$ mnemonic      : chr [1:200] "9ADEN" "ADEB3" "ADEB7" "ADEC1" ...
#>   ..$ scientificName: chr [1:200] "Mastadenovirus" "Bovine adenovirus B serotype 3" "Bovine adenovirus 7" "Canine adenovirus serotype 1" ...
#>   ..$ rank          : chr [1:200] "genus" "no rank" "no rank" "no rank" ...
#>   ..$ superregnum   : chr [1:200] "V" "V" "V" "V" ...
#>   ..$ hidden        : logi [1:200] FALSE TRUE TRUE TRUE TRUE TRUE ...
#>   ..$ commonName    : chr [1:200] NA "BAdV-3" "BAdV-7" "CAdV-1" ...
#>   ..$ synonym       : chr [1:200] NA "Mastadenovirus bos3" NA "Canine adenovirus 1" ...
#>  $ pageInfo  :List of 3
#>   ..$ resultsPerPage: int 200
#>   ..$ currentPage   : int 1
#>   ..$ totalRecords  : int 1259
```

As you can see, the server has returned the first page of the response.
To retrieve multiple pages, wrap the function call in
[`quote()`](https://rdrr.io/r/base/substitute.html) and replace its
named page argument with a specification of the form
`"pages:start:end"`. For example, the following call retrieves pages 1
to 3:

``` r

adeno_pages <- rba_pages(
  input_call = quote(
    rba_uniprot_taxonomy_name(
      name = "adenovirus",
      search_type = "contain",
      page_number = "pages:1:3"
    )
  )
)

## You can inspect the structure of the response:
str(adeno_pages, max.level = 2)
#> List of 3
#>  $ page_1:List of 2
#>   ..$ taxonomies:'data.frame':   200 obs. of  8 variables:
#>   ..$ pageInfo  :List of 3
#>  $ page_2:List of 2
#>   ..$ taxonomies:'data.frame':   200 obs. of  8 variables:
#>   ..$ pageInfo  :List of 3
#>  $ page_3:List of 2
#>   ..$ taxonomies:'data.frame':   200 obs. of  6 variables:
#>   ..$ pageInfo  :List of 3
```

There is another, functionally equivalent, way to call
[`rba_pages()`](https://rbioapi.moosa-r.com/reference/rba_pages.md):
omit the page argument from the quoted call, then supply its exact name
and the desired page numbers separately. This form is particularly
useful when you want to provide a vector of page numbers that does not
necessarily form a contiguous range:

``` r

adeno_pages <- rba_pages(
  input_call = quote(
    rba_uniprot_taxonomy_name(
      name = "adenovirus",
      search_type = "contain"
    )
  ),
  page_arg = "page_number",
  pages = c(1, 3, 5)
)
```

------------------------------------------------------------------------

## Saving API request metadata

rbioapi can store information about the API requests used to create a
result as an attribute of the returned object. Metadata collection is
off by default. Set `metadata = TRUE` for one call, then use
[`rba_metadata()`](https://rbioapi.moosa-r.com/reference/rba_metadata.md)
to get it:

``` r

## Save metadata with one result:
species <- rba_reactome_species(metadata = TRUE)

## Get and print it:
request_metadata <- rba_metadata(species)
request_metadata
```

The returned `rba_metadata` object prints a short summary and can be
used like a regular list:

``` r

## The rbioapi version used to create the result:
request_metadata$rbioapi_version

## Requests are listed in the order they were made. Functions that use several
## requests to create one result combine their entries. Each result returned by
## rba_pages() keeps its own metadata. Retry attempts that received an HTTP
## response are also included. Each entry contains its timestamp, API call,
## original httr response, and exact parser functions:
str(request_metadata$requests, max.level = 2)
```

To save metadata with all later rbioapi calls, use
`rba_options(metadata = TRUE)`. Turn it off again with
`rba_options(metadata = FALSE)`. For a result without metadata,
[`rba_metadata()`](https://rbioapi.moosa-r.com/reference/rba_metadata.md)
returns `NULL`.

Saving the complete `httr` responses and parser functions can make
results and saved files much larger.

------------------------------------------------------------------------

## How and what to cite?

rbioapi is an interface between you and other databases and services.
Thus, if you have used rbioapi in published research, **in addition to
kindly citing rbioapi, ensure to fully and properly cite the
databases/services you have used**. Suggested citations have been added
in the functions’ manuals, under the “references” section; Nevertheless,
it is the user’s responsibility to check for proper citations and to
properly cite the database/services that they have used.

### How to cite rbioapi

- Moosa Rezwani, Ali Akbar Pourfathollah, Farshid Noorbakhsh, rbioapi:
  user-friendly R interface to biologic web services’ API,
  Bioinformatics, Volume 38, Issue 10, 15 May 2022, Pages 2952–2953,
  <https://doi.org/10.1093/bioinformatics/btac172>

### How to cite the databases and web services

- [How to cite
  Enrichr](https://rbioapi.moosa-r.com/articles/rbioapi_enrichr.html#citations "How to cite Enrichr").
  (See on [Enrichr website](https://maayanlab.cloud/Enrichr/help#terms))

- [How to cite
  JASPAR](https://rbioapi.moosa-r.com/articles/rbioapi_jaspar.html#citations "How to cite JASPAR").
  (See on [JASPAR website](https://jaspar.elixir.no/faq/))

- [How to cite
  miEAA](https://rbioapi.moosa-r.com/articles/rbioapi_mieaa.html#citations "How to cite miEAA").
  (See the [miEAA
  website](https://ccb-compute2.cs.uni-saarland.de/mieaa/).)

- [How to cite
  PANTHER](https://rbioapi.moosa-r.com/articles/rbioapi_panther.html#citations "How to cite PANTHER").
  (See on [PANTHER
  website](https://www.pantherdb.org/publications.jsp#HowToCitePANTHER))

- [How to cite
  Reactome](https://rbioapi.moosa-r.com/articles/rbioapi_reactome.html#citations "How to cite Reactome").
  (See on [Reactome website](https://reactome.org/cite))

- [How to cite
  STRING](https://rbioapi.moosa-r.com/articles/rbioapi_string.html#citations "How to cite STRING").
  (See on [STRING
  website](https://string-db.org/cgi/about?footer_active_subpage=references))

- [How to cite
  UniProt](https://rbioapi.moosa-r.com/articles/rbioapi_uniprot.html#citations "How to cite UniProt").
  (See on [UniProt website](https://www.uniprot.org/help/publications))

------------------------------------------------------------------------

## Code of conduct

This package, rbioapi, is an unofficial interface implementation and is
not associated, endorsed, or officially connected in any way with the
original databases and web services. The creators and maintainers of
rbioapi are independent entities and have no official relationship with
those databases and web services.

When using rbioapi, remember that you are querying data from web
services; So please be considerate. Never flood a server with requests,
if you need to download *unreasonably* large volumes of data, directly
downloading the databases supplied in those services may be a better
alternative. If you see yourself being rate-limited from any server
(HTTP **429 Too Many Requests** response status code), know that you are
sending more requests than what the server interprets as normal
behavior, so please seek other methods or use
[`Sys.sleep()`](https://rdrr.io/r/base/Sys.sleep.html) between your
requests.

------------------------------------------------------------------------

## What next?

Each supported service has a dedicated vignette article. Make sure to
check those too.

1.  [Enrichr](https://rbioapi.moosa-r.com/articles/rbioapi_enrichr.md "rbioapi & Enrichr vignette")
    ^([Documentation site](https://rbioapi.moosa-r.com/articles/rbioapi_enrichr.html "rbioapi & Enrichr vignette"))^
2.  [JASPAR](https://rbioapi.moosa-r.com/articles/rbioapi_jaspar.md "rbioapi & JASPAR vignette article")
    ^([Documentation site](https://rbioapi.moosa-r.com/articles/rbioapi_jaspar.html "rbioapi & JASPAR vignette article"))^
3.  [miEAA](https://rbioapi.moosa-r.com/articles/rbioapi_mieaa.md "rbioapi & miEAA vignette article")
    ^([Documentation site](https://rbioapi.moosa-r.com/articles/rbioapi_mieaa.html "rbioapi & miEAA vignette article"))^
4.  [PANTHER](https://rbioapi.moosa-r.com/articles/rbioapi_panther.md "rbioapi & PANTHER vignette article")
    ^([Documentation site](https://rbioapi.moosa-r.com/articles/rbioapi_panther.html "rbioapi & PANTHER vignette article"))^
5.  [Reactome](https://rbioapi.moosa-r.com/articles/rbioapi_reactome.md "rbioapi & Reactome vignette article")
    ^([Documentation site](https://rbioapi.moosa-r.com/articles/rbioapi_reactome.html "rbioapi & Reactome vignette article"))^
6.  [STRING](https://rbioapi.moosa-r.com/articles/rbioapi_string.md "rbioapi & STRING vignette article")
    ^([Documentation site](https://rbioapi.moosa-r.com/articles/rbioapi_string.html "rbioapi & STRING vignette article"))^
7.  [UniProt](https://rbioapi.moosa-r.com/articles/rbioapi_uniprot.md "rbioapi & UniProt vignette article")
    ^([Documentation site](https://rbioapi.moosa-r.com/articles/rbioapi_uniprot.html "rbioapi & UniProt vignette article"))^

We are also adding vignette articles focusing on tasks and workflows:

1.  [Do with rbioapi: Enrichment (Over-Representation) Analysis in
    R](https://rbioapi.moosa-r.com/articles/rbioapi_do_enrich.md "Do with rbioapi: Enrichment (Over-Representation) Analysis in R")
    ^([documentation site](https://rbioapi.moosa-r.com/articles/rbioapi_do_enrich.html "rbioapi & UniProt vignette article"))^

------------------------------------------------------------------------

## Design of rbioapi

To learn more about the design and concepts behind developing rbioapi,
please read [our paper in
Bioinformatics](https://doi.org/10.1093/bioinformatics/btac172 "Rezwani, M., Pourfathollah, A. A., & Noorbakhsh, F. (2022). rbioapi: user-friendly R interface to biologic web services’ API. Bioinformatics, 38(10), 2952–2953. doi: 10.1093/bioinformatics/btac172").

------------------------------------------------------------------------

## Links

- [This article in rbioapi documentation
  site](https://rbioapi.moosa-r.com/articles/rbioapi.html "rbioapi: User-Friendly R Interface to Biologic Web Services' API")

- [Functions references in rbioapi documentation
  site](https://rbioapi.moosa-r.com/reference/index.html "rbioapi reference")

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
