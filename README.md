rbioapi: User-Friendly R Interface to Biologic Web Services’ API
================
Moosa Rezwani
2021-04-27

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->

------------------------------------------------------------------------

## What does rbioapi do?

Currently fully supports miEAA, PANTHER, Reactome, STRING, and UniProt!

rbioapi is an interface to Biological databases and web services. The
goal of rbioapi is to provide a user-friendly and consistent interface
to biological databases and services; It is designed in a way that
insulates the user from technicalities when it comes to using API
services and creates a unified and easy-to-implement tool to connect to
biological databases and services.

With rbioapi, You are not required to have any prior technical
knowledge. **Just fill in a function’s arguments and the rest is
internally handled for you**.

------------------------------------------------------------------------

## What is Supported by rbioapi?

rbioapi is dedicated to **Biological or Medical** databases and web
services. Currently, rbioapi supports and covers every API resources in
the following services: (in alphabetical order!):

1.  [miEAA](https://ccb-compute2.cs.uni-saarland.de/mieaa2 "miRNA Enrichment Analysis and Annotation Tool (miEAA)")
    ([rbioapi vignette
    article](mieaa.html "rbioapi & miEAA vignette article"))
2.  [PANTHER](http://www.pantherdb.org "Protein Analysis THrough Evolutionary Relationships (PANTHER)")
    ([rbioapi vignette
    article](panther.html "rbioapi & PANTHER vignette article"))
3.  [Reactome](https://reactome.org/) ([rbioapi vignette
    article](reactome.html "rbioapi & Reactome vignette article"))
4.  [STRING](https://string-db.org/ "STRING: Protein-Protein Interaction Networks Functional Enrichment Analysis")
    ([rbioapi vignette
    article](string.html "rbioapi & STRING vignette article"))
5.  [UniProt](https://www.uniprot.org "Universal Protein Resource (UniProt)")
    ([rbioapi vignette
    article](uniprot.html "rbioapi & UniProt vignette article"))

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

------------------------------------------------------------------------

## How to install?

You can install the stable released version of rbioapi from
[CRAN](https://CRAN.R-project.org) with:

``` r
# The package has been submitted to CRAN. I will update this part soon! :)
```

However, the CRAN version is released at most once every 1-2 months, You
can install the development version from [GitHub](https://github.com/)
with:

``` r
install.packages("devtools")
devtools::install_github("moosa-r/rbioapi")
```

------------------------------------------------------------------------

## Design philosophy of rbioapi

-   The functions’ structure should be **consistent across all the
    databases and services**. There should not be any learning effort
    when using functions from different services.

-   The interface should be **simple plugging values in a function’s
    arguments** and running the code. No function should explicitly
    demand the user to run another function beforehand.

-   The functions’ names and arguments should be as **faithful** as
    possible to the original API resources. This is to ensure that the
    users could conveniently connect the dots between rbioapi and the
    web service.

-   The user should be completely **insulated from any technicalities**.

-   **Beginner** end of users spectrum should conveniently use rbioapi.
    For example, to prevent errors, user input arguments will be
    vigorously checked; Or for example, if any service’s resource
    requires particular input conditions, those conditions will be
    checked during user’s input validation.

-   Produce **Informative messages**, warnings, and error. For example,
    if a server returns an error in a particular format, convert the
    server’s error response to R error.

------------------------------------------------------------------------

# Naming conventions

To make the namespace more organized, functions has been named with the
following pattern:

> rba\_\[service\_name\]\_\[resource\_name\]

For example, `rba_string_version()` will call
[STRING](https://string-db.org/ "STRING: Protein-Protein Interaction Networks Functional Enrichment Analysis")’s
version resource.

``` r
rba_string_version()
#> Retrieving the STRING database version and address used by rbioapi.
#> $string_version
#> [1] "11.0"
#> 
#> $string_stable_address
#> [1] "https://version-11-0.string-db.org"
```

Thus, to this version, rbioapi function will have one of the following
naming schema:

1.  rba\_mieaa\_\*
2.  rba\_panther\_\*
3.  rba\_reactome\_\*
4.  rba\_string\_\*
5.  rba\_uniprot\_\*

There are three exceptions: `rba_options()`, `rba_connection_test()`,
and `rba_pages()`; These are helper functions. More on that later.

------------------------------------------------------------------------

# Changing the options

To provide more control, multiple options have been implemented. Refer
to the manual of `rba_options()` function for a full description of
available options. In short, some of the options will govern rbioapi’s
connection with servers (e.g. timeout, retry) and some of the options
will modify your experience with rbioapi (e.g. verbose, diagnostics,
save\_file). There are two ways that you may use to change any option.
Also, you can get table of available rbioapi options and their current
values by calling `rba_options()`without any argument:

``` r
rba_options()
#>   rbioapi_option current_value value_class
#> 1        timeout           600     numeric
#> 2       dir_name       rbioapi   character
#> 3    diagnostics         FALSE     logical
#> 4      retry_max             1     numeric
#> 5       progress         FALSE     logical
#> 6      save_file         FALSE     logical
#> 7     skip_error         FALSE     logical
#> 8        verbose          TRUE     logical
#> 9     retry_wait            10     numeric
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
# from now on, the raw file of server's response will be saved to your working directory.
rba_options(verbose = FALSE)
# from now on, the package will be quiet.
```

## Change the option only within a function call

You can pass additional arguments to any rbioapi function using
“ellipsis” (the familiar `…` or dot dot dot!). Meaning that you can call
any function with additional arguments where each is ‘option = value’
pair. This way, any changes in options will be confined within that
particular function call. For example:

``` r
# save the server's raw response file:
x <- rba_reactome_species(only_main = TRUE, save_file = "reactome_species.json")
# also , in the case of connection failure, retry up to 10 times:
x <- rba_reactome_species(only_main = TRUE,
                         save_file = "reactome_species.json", retry_max = 10)
# in case of any failure, don't stop the code, just return
```

``` r
## run these codes in your own R session to see the difference.
# show internal diagnostics boring details
x <- rba_uniprot_proteins_crossref(db_id = "CD40", db_name = "HGNC", diangnostics = TRUE)
#> Retrieving UniProt entities that correspond to ID CD40 in database HGNC.
# the next function you call, will still use the default rbioapi options
x <- rba_uniprot_proteins_crossref(db_id = "CD40", db_name = "HGNC")
#> Retrieving UniProt entities that correspond to ID CD40 in database HGNC.
```

------------------------------------------------------------------------

# Connection test

The second exception in functions’ naming schema is
`rba_connection_test()`. Run this simple function to check your
connection with the supported services/databases. If you encounter
errors when using rbioapi, kindly run this function to make sure that
your internet connection or the servers are fine.

``` r
rba_connection_test()
#> Checking Your connection to the Databases currently supported by rbioapi:
#> --->>> Internet :
#> +++ Connected to the Internet.
#> --->>> Enrichr :
#> +++ The server is responding.
#> --->>> Ensembl :
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

------------------------------------------------------------------------

# Iterating over paginated results

Some API resources will return paginated responses. This is particularly
common in API resources which return potentially very large responses.
In rbioapi, for these cases, there are arguments such as “page\_number”
(with default value of 1) and -if the API resource allows- “page\_size”.
To save your time, you may use `rba_pages()`. This function will iterate
over the pages you have specified.

Take rba\_uniprot\_taxonomy\_name as an example. This function allows
you to search taxonomic nodes in
[UniProt](https://www.uniprot.org "Universal Protein Resource (UniProt)").
The response can potentially have a huge size, so
[UniProt](https://www.uniprot.org "Universal Protein Resource (UniProt)")
returns a paginated response. For example, if we search for nodes that
contain “adenovirus”, there is a large number of hits:

``` r
adeno <- rba_uniprot_taxonomy_name(name = "adenovirus",
                                   search_type = "contain",
                                   page_number = 1)
adeno$pageInfo
#> $resultsPerPage
#> [1] 200
#> 
#> $currentPage
#> [1] 1
#> 
#> $totalRecords
#> [1] 934
```

As you can see, the server has returned the first page of the response,
to retrieve the other pages, you should make separate calls and change
the “page\_number” argument within each call, or simply use
`rba_pages()` as demonstrated below:

``` r
adeno_pages = rba_pages(quote(rba_uniprot_taxonomy_name(name = "adenovirus",
                                   search_type = "contain",
                                   page_number = "pages:1:3")))
# You can inspect structure of the response:
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
    this format: “pages:start:end”. For example, we provided
    page\_number = “pages:1:3” to get the responses of pages 1 to 3.

------------------------------------------------------------------------

# How and what to cite?

rbioapi is an interface between you and other databases and services.
Thus, if you have used rbioapi in published research, kindly **in
addition to citing rbioapi, <u>*make sure to fully and properly cite the
databases/services you have used*</u>**. Suggested citations have been
added in the functions’ manuals, under the “references” section;
Nevertheless, it is the user’s responsibility to check for proper
citations and to properly cite the database/services that they have
used.

------------------------------------------------------------------------

# Code of conduct

When using rbioapi, remember that you are querying data from web
services; So please be considerate. Never flood a server with requests,
if you need to download *unreasonably* large volumes of data, directly
downloading the databases provided in those services may be a better
alternative. If you see yourself being rate-limited from any server
(HTTP **429 Too Many Requests** response status code), know that you are
sending more requests than what the server interprets as normal
behavior, so please seek other methods or use `Sys.sleep()` between your
requests.

------------------------------------------------------------------------

# What next?

Each supported service has a dedicated vignette article. Make sure to
check those too.

1.  [miEAA](mieaa.html "rbioapi & miEAA vignette article")
2.  [PANTHER](panther.html "rbioapi & PANTHER vignette article")
3.  [Reactome](reactome.html "rbioapi & Reactome vignette article")
4.  [STRING](string.html "rbioapi & STRING vignette article")
5.  [UniProt](uniprot.html "rbioapi & UniProt vignette article")

------------------------------------------------------------------------

# Session info

    #> R version 4.0.5 (2021-03-31)
    #> Platform: x86_64-w64-mingw32/x64 (64-bit)
    #> Running under: Windows 10 x64 (build 19043)
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
    #> [1] rbioapi_0.6.5
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] digest_0.6.27     R6_2.5.0          jsonlite_1.7.2    magrittr_2.0.1   
    #>  [5] evaluate_0.14     httr_1.4.2        rlang_0.4.10      stringi_1.5.3    
    #>  [9] curl_4.3          rmarkdown_2.7     tools_4.0.5       stringr_1.4.0    
    #> [13] xfun_0.22         yaml_2.2.1        compiler_4.0.5    htmltools_0.5.1.1
    #> [17] knitr_1.33
