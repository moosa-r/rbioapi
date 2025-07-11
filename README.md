rbioapi: User-Friendly R Interface to Biologic Web Services’ API
================
Moosa Rezwani
2025-06-29

# <img src="man/figures/logo.svg" align="right" width="200"/>

<!-- badges: start -->

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/rbioapi)](https://cran.r-project.org/package=rbioapi)
[![R-CMD-check](https://github.com/moosa-r/rbioapi/workflows/R-CMD-check/badge.svg)](https://github.com/moosa-r/rbioapi/actions)

<!-- badges: end -->

# What does rbioapi do?

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

# What is Supported by rbioapi?

rbioapi is dedicated to **Biological or Medical** databases and web
services. Currently, rbioapi supports and covers every API resources in
the following services: (in alphabetical order):

On CRAN (Stable) version: (<https://cran.r-project.org/package=rbioapi>)

1.  [Enrichr](https://maayanlab.cloud/Enrichr/ "Enrichr") ([rbioapi
    vignette
    article](https://rbioapi.moosa-r.com/articles/rbioapi_enrichr.html "rbioapi & Enrichr vignette article"))
    <sup>(new)</sup>
2.  [JASPAR](https://jaspar.elixir.no/ "JASPAR - A database of transcription factor binding profiles")
    ([rbioapi vignette
    article](https://rbioapi.moosa-r.com/articles/rbioapi_jaspar.html "rbioapi & Enrichr vignette article"))
    <sup>(new)</sup>
3.  [miEAA](https://ccb-compute2.cs.uni-saarland.de/mieaa2 "miRNA Enrichment Analysis and Annotation Tool (miEAA)")
    ([rbioapi vignette
    article](https://rbioapi.moosa-r.com/articles/rbioapi_mieaa.html "rbioapi & miEAA vignette article"))
4.  [PANTHER](https://www.pantherdb.org "Protein Analysis THrough Evolutionary Relationships (PANTHER)")
    ([rbioapi vignette
    article](https://rbioapi.moosa-r.com/articles/rbioapi_panther.html "rbioapi & PANTHER vignette article"))
5.  [Reactome](https://reactome.org/) ([rbioapi vignette
    article](https://rbioapi.moosa-r.com/articles/rbioapi_reactome.html "rbioapi & Reactome vignette article"))
6.  [STRING](https://string-db.org/ "STRING: Protein-Protein Interaction Networks Functional Enrichment Analysis")
    ([rbioapi vignette
    article](https://rbioapi.moosa-r.com/articles/rbioapi_string.html "rbioapi & STRING vignette article"))
7.  [UniProt](https://www.uniprot.org "Universal Protein Resource (UniProt)")
    ([rbioapi vignette
    article](https://rbioapi.moosa-r.com/articles/rbioapi_uniprot.html "rbioapi & UniProt vignette article"))

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

# How to install?

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

# Naming conventions

To keep the namespace organized, functions names follow this pattern:

    rba_[service_name]_[resource_name]

For example, `rba_string_version()` will call
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

There are three exceptions: `rba_options()`, `rba_connection_test()`,
and `rba_pages()`; these are helper functions. More on that later.

# Changing the options

To give users greater control, rbioapi offers multiple configurable
options. See the manual of `rba_options()` function for a full
description of available options. In short, some of the options will
govern rbioapi’s connection with servers (e.g. timeout, retry) and some
of the options will modify your experience with rbioapi (e.g. verbose,
diagnostics, save_file). There are two ways that you may use to change
any option. Also, you can get table of available rbioapi options and
their current values by calling `rba_options()`without any argument:

``` r
rba_options()
#>   rbioapi_option current_value            allowed_value
#> 1    diagnostics         FALSE     Logical (TRUE/FALSE)
#> 2       dir_name       rbioapi                Character
#> 3       progress         FALSE     Logical (TRUE/FALSE)
#> 4      retry_max             0   Numeric (0 or greater)
#> 5     retry_wait            10   Numeric (0 or greater)
#> 6      save_file         FALSE     Logical (TRUE/FALSE)
#> 7     skip_error          TRUE     Logical (TRUE/FALSE)
#> 8        timeout            30 Numeric (0.1 or greater)
#> 9        verbose          TRUE     Logical (TRUE/FALSE)
```

Now, let us consider the ways in which we can alter the settings:

## Change the option globally

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

## Change the option only within a function call

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

# Connection test

The second exception in functions’ naming schema is
`rba_connection_test()`. Run this simple function to check your
connection with the supported services/databases. If you encounter
errors when using rbioapi, kindly run this function to make sure that
your internet connection or the servers are fine.

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
#> +++ The server is responding.
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

# Iterating over paginated results

Some API resources will return paginated responses. This is particularly
common in API resources which return potentially very large responses.
In rbioapi, for these cases, there are arguments such as “page_number”
(with default value of 1) and -if the API resource allows- “page_size”.
To save your time, you may use `rba_pages()`. This function will iterate
over the pages you have specified.

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
#>   ..$ mnemonic      : chr [1:200] "9ADEN" "ADEB3" "ADEB7" "9ADEN" ...
#>   ..$ scientificName: chr [1:200] "Mastadenovirus" "Bovine adenovirus B serotype 3" "Bovine adenovirus 7" "Canine adenovirus 1" ...
#>   ..$ rank          : chr [1:200] "genus" "serotype" "serotype" "serotype" ...
#>   ..$ superregnum   : chr [1:200] "V" "V" "V" "V" ...
#>   ..$ hidden        : logi [1:200] FALSE TRUE TRUE TRUE TRUE TRUE ...
#>   ..$ commonName    : chr [1:200] NA "BAdV-3" "BAdV-7" NA ...
#>   ..$ synonym       : chr [1:200] NA "Mastadenovirus bos3" NA NA ...
#>  $ pageInfo  :List of 3
#>   ..$ resultsPerPage: int 200
#>   ..$ currentPage   : int 1
#>   ..$ totalRecords  : int 1121
```

As you can see, the server has returned the first page of the response,
to retrieve the other pages, you should make separate calls and change
the “page_number” argument within each call, or simply use `rba_pages()`
as demonstrated below:

``` r
adeno_pages = rba_pages(
  quote(
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

As you can see, what we have done was:

1.  Wrap the function call in `qoute()` and enter that as the input for
    `rba_pages()`.

2.  Replace the argument we want to iterate over it, with a string in
    this format: “pages:start:end”. For example, we supplied page_number
    = “pages:1:3” to get the responses of pages 1 to 3.

# How and what to cite?

rbioapi is an interface between you and other databases and services.
Thus, if you have used rbioapi in published research, **in addition to
kindly citing rbioapi, <u>*ensure to fully and properly cite the
databases/services you have used*</u>**. Suggested citations have been
added in the functions’ manuals, under the “references” section;
Nevertheless, it is the user’s responsibility to check for proper
citations and to properly cite the database/services that they have
used.

## How to cite rbioapi

- Moosa Rezwani, Ali Akbar Pourfathollah, Farshid Noorbakhsh, rbioapi:
  user-friendly R interface to biologic web services’ API,
  Bioinformatics, Volume 38, Issue 10, 15 May 2022, Pages 2952–2953,
  <https://doi.org/10.1093/bioinformatics/btac172>

## How to cite the databases and web services

- [How to cite
  Enrichr](https://rbioapi.moosa-r.com/articles/rbioapi_enrichr.html#citations "How to cite Enrichr").
  (See on [Enrichr website](https://maayanlab.cloud/Enrichr/help#terms))

- [How to cite
  JASPAR](https://rbioapi.moosa-r.com/articles/rbioapi_jaspar.html#citations "How to cite JASPAR").
  (See on [JASPAR website](https://jaspar.elixir.no/faq/))

- [How to cite
  miEAA](https://rbioapi.moosa-r.com/articles/rbioapi_mieaa.html#citations "How to cite miEAA").
  (See on [miEAA
  website](https://ccb-compute2.cs.uni-saarland.de/mieaa2))

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
behavior, so please seek other methods or use `Sys.sleep()` between your
requests.

# What next?

Each supported service has a dedicated vignette article. Make sure to
check those too.

1.  [Enrichr](https://rbioapi.moosa-r.com/articles/rbioapi_enrichr.html "rbioapi & Enrichr vignette")
2.  [JASPAR](https://rbioapi.moosa-r.com/articles/rbioapi_jaspar.html "rbioapi & JASPAR vignette article")
3.  [miEAA](https://rbioapi.moosa-r.com/articles/rbioapi_mieaa.html "rbioapi & miEAA vignette article")
4.  [PANTHER](https://rbioapi.moosa-r.com/articles/rbioapi_panther.html "rbioapi & PANTHER vignette article")
5.  [Reactome](https://rbioapi.moosa-r.com/articles/rbioapi_reactome.html "rbioapi & Reactome vignette article")
6.  [STRING](https://rbioapi.moosa-r.com/articles/rbioapi_string.html "rbioapi & STRING vignette article")
7.  [UniProt](https://rbioapi.moosa-r.com/articles/rbioapi_uniprot.html "rbioapi & UniProt vignette article")

We are also adding vignette articles focusing on tasks and workflows:

1.  [Do with rbioapi: Enrichment (Over-Representation) Analysis in
    R](https://rbioapi.moosa-r.com/articles/rbioapi_do_enrich.html "Do with rbioapi: Enrichment (Over-Representation) Analysis in R")

# Design of rbioapi

To learn more about the design and concepts behind developing rbioapi,
please read our paper in Bioinformatics:

[rbioapi: user-friendly R interface to biologic web services’
API](https://doi.org/10.1093/bioinformatics/btac172 "Rezwani, M., Pourfathollah, A. A., & Noorbakhsh, F. (2022). rbioapi: user-friendly R interface to biologic web services’ API. Bioinformatics, 38(10), 2952–2953. doi: 10.1093/bioinformatics/btac172")

# Links

- [This article in rbioapi documentation
  site](https://rbioapi.moosa-r.com/articles/rbioapi.html "rbioapi: User-Friendly R Interface to Biologic Web Services' API")

- [Functions references in rbioapi documentation
  site](https://rbioapi.moosa-r.com/reference/index.html "rbioapi reference")

# Session info

    #> R version 4.5.1 (2025-06-13 ucrt)
    #> Platform: x86_64-w64-mingw32/x64
    #> Running under: Windows 11 x64 (build 26100)
    #> 
    #> Matrix products: default
    #>   LAPACK version 3.12.1
    #> 
    #> locale:
    #> [1] LC_COLLATE=English_United States.utf8 
    #> [2] LC_CTYPE=English_United States.utf8   
    #> [3] LC_MONETARY=English_United States.utf8
    #> [4] LC_NUMERIC=C                          
    #> [5] LC_TIME=English_United States.utf8    
    #> 
    #> time zone: Europe/Brussels
    #> tzcode source: internal
    #> 
    #> attached base packages:
    #> [1] stats     graphics  grDevices utils     datasets  methods   base     
    #> 
    #> other attached packages:
    #> [1] rbioapi_0.8.3
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] digest_0.6.37     R6_2.6.1          fastmap_1.2.0     xfun_0.52        
    #>  [5] knitr_1.50        htmltools_0.5.8.1 rmarkdown_2.29    cli_3.6.5        
    #>  [9] compiler_4.5.1    httr_1.4.7        rstudioapi_0.17.1 tools_4.5.1      
    #> [13] curl_6.4.0        evaluate_1.0.4    yaml_2.3.10       rlang_1.1.6      
    #> [17] jsonlite_2.0.0
