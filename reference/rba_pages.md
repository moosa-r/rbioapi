# Retrieve Multiple Pages of a Paginated Resource

Evaluate a quoted call to an exported rbioapi function for multiple page
numbers. Calls are made sequentially, and their results are returned in
the requested order.

## Usage

``` r
rba_pages(
  input_call,
  page_arg = NULL,
  pages = NULL,
  sleep_time = 2,
  skip_error = TRUE,
  progress = FALSE,
  verbose = getOption("rba_verbose")
)
```

## Arguments

- input_call:

  Call: A quoted call to an exported API-endpoint-facing
  rbioapifunction. To request an inclusive range, set the called
  function's named page argument to a character string of the form
  `"pages:start:end"`. Alternatively, omit the page argument from
  `input_call`, supply its exact name through `page_arg`, and supply the
  desired page numbers through `pages`. These two forms cannot be
  combined.

- page_arg:

  Character: (optional) Exact name of the formal argument of the called
  rbioapi function that accepts the page number. Supply together with
  `pages` when the page argument is omitted from `input_call`.

- pages:

  Numeric vector: (optional) Unique positive whole page numbers in the
  order in which they should be requested. A maximum of 100 values can
  be supplied. Must be supplied together with `page_arg`.

- sleep_time:

  Numeric: (default = `2`) Number of seconds to wait between successive
  calls. Must be at least 2.

- skip_error:

  Logical: (default = `TRUE`) Continue the operation after an
  unsuccessful page call and return its error message as that page's
  result? This value is passed to every page call. If `input_call`
  already supplies `skip_error`, its value is overridden and a warning
  is issued.

- progress:

  Logical: (default = `FALSE`) Display one progress bar for the complete
  operation? When `TRUE`, verbose messages and progress bars from
  individual page calls are suppressed.

- verbose:

  Logical: (default = current `rba_verbose` option) Generate an
  informative message describing the complete operation?

## Value

A named list containing one element per requested page. Element names
have the form `page_<number>`.

## Details

Pagination can be specified in either of two ways. To request an
inclusive range, set the named page argument in `input_call` to a
character string of the form `"pages:start:end"`. Alternatively, omit
the page argument from `input_call`, supply its exact name through
`page_arg`, and supply the desired page numbers through `pages`. The
range may run in either direction, and the two forms cannot be combined.
The page argument must exactly match a formal function argument; partial
and positional matching are not used for pagination.

Page numbers must be unique positive whole numbers, and no more than 100
pages can be requested in one call. `sleep_time` seconds are inserted
between successive calls.

The value of `skip_error` is passed to every page call, allowing
rbioapi's standard error-handling mechanism to determine whether a
failed request stops the operation. If `progress = TRUE`, one progress
bar is displayed and `verbose = FALSE` and `progress = FALSE` are passed
to the individual rbioapi calls.

## See also

Other "Helper functions":
[`rba_connection_test()`](https://rbioapi.moosa-r.com/reference/rba_connection_test.md),
[`rba_options()`](https://rbioapi.moosa-r.com/reference/rba_options.md)

## Examples

``` r
# \donttest{
rba_pages(
  input_call = quote(
    rba_uniprot_taxonomy_name(
      name = "adenovirus",
      search_type = "contain",
      page_size = 20,
      page_number = "pages:1:3"
    )
  )
)
# }
# \donttest{
rba_pages(
  input_call = quote(
    rba_uniprot_taxonomy_name(
      name = "adenovirus",
      search_type = "contain",
      page_size = 20
    )
  ),
  page_arg = "page_number",
  pages = c(1, 3, 5)
)
# }
```
