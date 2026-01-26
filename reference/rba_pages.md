# Get Multiple Pages of a Paginated Resource

Some resources return paginated results, meaning that you have to make
separate calls for each page. Using this function, you can iterate over
up to 100 pages. Just supply your rbioapi function and change to page
argument to "pages:start_page:end_page", for example "pages:1:5".

## Usage

``` r
rba_pages(input_call, ...)
```

## Arguments

- input_call:

  A quoted call. supply a regular rbioapi function call, but with two
  differences:

  1.  : Wrap a quote() around it. meaning: quote(rba_example())

  2.  : Set the argument that corresponds to the page number to
      "pages:start_page:end_page", for example "pages:1:5".

  See the "examples" section to learn more.

- ...:

  Experimental internal options.

## Value

A named list where each element corresponds to a request's page.

## Details

To prevent flooding the server, there will be a 1 second delay between
calls, also the user cannot iterate on more than 100 pages. The function
will also override skip_error option and will always set it to TRUE.
This means that in case of server response error (e.g. requesting pages
that do not exist) an error message be returned to you instead of
halting function's execution.

## See also

Other "Helper functions":
[`rba_connection_test()`](https://rbioapi.moosa-r.com/reference/rba_connection_test.md),
[`rba_options()`](https://rbioapi.moosa-r.com/reference/rba_options.md)

## Examples

``` r
# \donttest{
rba_pages(input_call = quote(rba_uniprot_taxonomy(ids = 189831,
    hierarchy = "siblings",
    page_size = 50,
    page_number = "pages:1:5")))
# }
# \donttest{
rba_pages(input_call = quote(rba_uniprot_taxonomy_name(name = "adenovirus",
    field = "scientific",
    search_type = "contain",
    page_size = 200,
    page_number = "pages:1:5",
    verbose = FALSE)))
# }
# \donttest{
rba_pages(input_call = quote(rba_panther_info(what = "families",
    families_page = "pages:9:11")))
# }
```
