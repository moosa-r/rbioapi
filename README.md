rbioapi: User-Friendly R Interface to Biologic Web Services’ API
================
Moosa Rezwani
2022-01-19

<!-- README.md is generated from README.Rmd. Please edit that file -->
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
2.  [JASPAR](http://jaspar.genereg.net/ "JASPAR - A database of transcription factor binding profiles")
    ([rbioapi vignette
    article](https://rbioapi.moosa-r.com/articles/rbioapi_jaspar.html "rbioapi & Enrichr vignette article"))
    <sup>(new)</sup>
3.  [miEAA](https://ccb-compute2.cs.uni-saarland.de/mieaa2 "miRNA Enrichment Analysis and Annotation Tool (miEAA)")
    ([rbioapi vignette
    article](https://rbioapi.moosa-r.com/articles/rbioapi_mieaa.html "rbioapi & miEAA vignette article"))
4.  [PANTHER](http://www.pantherdb.org "Protein Analysis THrough Evolutionary Relationships (PANTHER)")
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

Each of the services has its dedicated vignette article. However, In
this article, I will write about the general framework of rbioapi. Make
sure to check the vignette article of each service to learn more about
how to use them.

**Note That:** rbioapi is an ongoing project. New databases and services
will be implemented periodically in order to gradually make the package
as comprehensive as possible. Do you see yourself often using a certain
database/service? Feel free to suggest any database/service by creating
an issue on our GitHub
[repository](https://github.com/moosa-r/ "rbioapi GitHub repositry"). I
will appreciate any suggestions.

# How to install?

You can install the stable released version of rbioapi from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("rbioapi")
```

However, the CRAN version is released at most once every 1-2 months, You
can install the development version from [GitHub](https://github.com/)
with:

``` r
install.packages("remotes")
remotes::install_github("moosa-r/rbioapi")
```

# Design philosophy of rbioapi

-   The functions’ structure should be **consistent** across all
    databases and services. Meaning that there should not be any
    learning effort when using functions communicating with dif-ferent
    web services.

-   The package’s interface should be a simple **plugging of values** in
    a function’s arguments and running the code.

-   No function should explicitly demand the user to run another
    function beforehand.

-   The functions’ names and arguments should be as **faithful** as
    possible to the original API resources. This is to ensure that the
    users could conveniently establish links between rbioapi and the web
    service. rbioapi should be an interface to the API service, nothing
    more or less.

-   The package should be easy to **expand and contribute**. To this
    goal, exported functions should have a template-based structure and
    the internal functions should have a hierarchical organization with
    only a subset of them needed by the contributors.

-   **Beginner users** should conveniently use rbioapi and they should
    be completely insulated from any technicalities. To name a few
    examples: To prevent errors, user-input argu-ments should be
    vigorously checked. Informative messages, warnings, and errors
    should be produced. If a server returns an error in a particular
    format, the server’s error response should be parsed. The package
    should internally and grace-fully handle failure events such as
    connection loss. Saving files such as raw server’s response should
    be easy, yet moni-tored by the package. Altering the package options
    should be readily accessible.

# Naming conventions

To make the namespace more organized, functions has been named with the
following pattern:

    rba_[service_name]_[resource_name]

For example, `rba_string_version()` will call
[STRING](https://string-db.org/ "STRING: Protein-Protein Interaction Networks Functional Enrichment Analysis")’s
version resource.

``` r
rba_string_version()
#> Retrieving the STRING database version and address used by rbioapi.
#> $string_version
#> [1] "11.0"
#> 
#> $stable_address
#> [1] "https://version-11-0.string-db.org"
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

To provide more control, multiple options have been implemented. See the
manual of `rba_options()` function for a full description of available
options. In short, some of the options will govern rbioapi’s connection
with servers (e.g. timeout, retry) and some of the options will modify
your experience with rbioapi (e.g. verbose, diagnostics, save_file).
There are two ways that you may use to change any option. Also, you can
get table of available rbioapi options and their current values by
calling `rba_options()`without any argument:

``` r
rba_options()
#>   rbioapi_option current_value            allowed_value
#> 1    diagnostics         FALSE     Logical (TRUE/FALSE)
#> 2       dir_name       rbioapi                Character
#> 3       progress         FALSE     Logical (TRUE/FALSE)
#> 4      retry_max             1   Numeric (0 or greater)
#> 5     retry_wait            10   Numeric (0 or greater)
#> 6      save_file         FALSE     Logical (TRUE/FALSE)
#> 7     skip_error          TRUE     Logical (TRUE/FALSE)
#> 8        timeout           600 Numeric (0.1 or greater)
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
x <- rba_reactome_species(only_main = TRUE, save_file = "reactome_species.json")
## Also, in the case of connection failure, retry up to 10 times:
x <- rba_reactome_species(only_main = TRUE,
                         save_file = "reactome_species.json", retry_max = 10)
```

``` r
## Run these codes in your own R session to see the difference.
## show internal diagnostics boring details
x <- rba_uniprot_proteins_crossref(db_id = "CD40", db_name = "HGNC", diagnostics = TRUE)
## The next function you call, will still use the default rbioapi options
x <- rba_uniprot_proteins_crossref(db_id = "CD40", db_name = "HGNC")
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
adeno <- rba_uniprot_taxonomy_name(name = "adenovirus",
                                   search_type = "contain",
                                   page_number = 1)
str(adeno, max.level = 2)
#> List of 2
#>  $ taxonomies:'data.frame':  200 obs. of  8 variables:
#>   ..$ taxonomyId    : int [1:200] 10509 10510 10511 10512 10513 10514 10515 10519 10521 10522 ...
#>   ..$ mnemonic      : chr [1:200] "9ADEN" "ADEB3" "ADEB7" "9ADEN" ...
#>   ..$ scientificName: chr [1:200] "Mastadenovirus" "Bovine adenovirus B serotype 3" "Bovine adenovirus 7" "Canine adenovirus 1" ...
#>   ..$ rank          : chr [1:200] "genus" "no rank" "no rank" "no rank" ...
#>   ..$ superregnum   : chr [1:200] "V" "V" "V" "V" ...
#>   ..$ hidden        : logi [1:200] FALSE TRUE TRUE TRUE TRUE TRUE ...
#>   ..$ commonName    : chr [1:200] NA "BAdV-3" "BAdV-7" NA ...
#>   ..$ synonym       : chr [1:200] NA "Mastadenovirus bos3" NA NA ...
#>  $ pageInfo  :List of 3
#>   ..$ resultsPerPage: int 200
#>   ..$ currentPage   : int 1
#>   ..$ totalRecords  : int 935
```

As you can see, the server has returned the first page of the response,
to retrieve the other pages, you should make separate calls and change
the “page_number” argument within each call, or simply use `rba_pages()`
as demonstrated below:

``` r
adeno_pages = rba_pages(quote(rba_uniprot_taxonomy_name(name = "adenovirus",
                                   search_type = "contain",
                                   page_number = "pages:1:3")))
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
#>   ..$ taxonomies:'data.frame':   200 obs. of  8 variables:
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
Thus, if you have used rbioapi in published research, kindly **in
addition to citing rbioapi, <u>*make sure to fully and properly cite the
databases/services you have used*</u>**. Suggested citations have been
added in the functions’ manuals, under the “references” section;
Nevertheless, it is the user’s responsibility to check for proper
citations and to properly cite the database/services that they have
used.

# Code of conduct

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

1.  [Do with rbioapi: Over-Representation (Enrichment)
    Analysis](https://rbioapi.moosa-r.com/articles/rbioapi_do_enrich.html "Do with rbioapi: Over-Representation (Enrichment) Analysis")

# Links

-   [This article in rbioapi documentation
    site](https://rbioapi.moosa-r.com/articles/rbioapi.html "rbioapi: User-Friendly R Interface to Biologic Web Services' API")

-   [Functions references in rbioapi documentation
    site](https://rbioapi.moosa-r.com/reference/index.html "rbioapi reference")

## Session info

    #> R version 4.1.2 (2021-11-01)
    #> Platform: x86_64-w64-mingw32/x64 (64-bit)
    #> Running under: Windows 10 x64 (build 19044)
    #> 
    #> Matrix products: default
    #> 
    #> locale:
    #> [1] LC_COLLATE=English_United States.1252 
    #> [2] LC_CTYPE=English_United States.1252   
    #> [3] LC_MONETARY=English_United States.1252
    #> [4] LC_NUMERIC=C                          
    #> [5] LC_TIME=English_United States.1252    
    #> system code page: 1256
    #> 
    #> attached base packages:
    #> [1] stats     graphics  grDevices utils     datasets  methods   base     
    #> 
    #> other attached packages:
    #> [1] rbioapi_0.7.5
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] digest_0.6.29   R6_2.5.1        jsonlite_1.7.2  magrittr_2.0.1 
    #>  [5] evaluate_0.14   httr_1.4.2      rlang_0.4.12    stringi_1.7.6  
    #>  [9] curl_4.3.2      rmarkdown_2.11  tools_4.1.2     stringr_1.4.0  
    #> [13] xfun_0.29       yaml_2.2.1      fastmap_1.1.0   compiler_4.1.2 
    #> [17] htmltools_0.5.2 knitr_1.37
