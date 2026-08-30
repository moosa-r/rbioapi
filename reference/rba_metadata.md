# Retrieve API Request Metadata

Retrieve API request metadata saved with an rbioapi result. Metadata is
off by default. Set `metadata = TRUE` on one call to save metadata for
that result, or use `rba_options(metadata = TRUE)` to save it for all
later calls.

## Usage

``` r
rba_metadata(result)
```

## Arguments

- result:

  Any: An object returned by an rbioapi function.

## Value

An object of class `rba_metadata` containing saved API request metadata,
or `NULL` if `result` has no metadata.

## Details

Saving metadata does not change the result's class. The returned
`rba_metadata` object is a list. Printing it shows a short summary; use
`$` or `[[` to access its elements. Using `rba_metadata(result)` is
equivalent to retrieving `attributes(result)$rbioapi_metadata`.

The list contains:

- `rbioapi_version`: the rbioapi version used to create the result.

- `requests`: request entries in the order they were made. Each entry
  contains:

  - `timestamp`: the `date` value from the original `httr` response.

  - `call`: the API call used for the request.

  - `response`: the original `httr` response object.

  - `parsers`: the exact parser functions used, in the order they ran.

Functions that use several requests to create one result save their
entries in the order the requests were made. Each result returned by
[`rba_pages()`](https://rbioapi.moosa-r.com/reference/rba_pages.md)
keeps its own metadata. Retry attempts are included when they receive an
HTTP response. If a response was not parsed, its `parsers` list is
empty.

Saving the complete `httr` responses and parser functions can make
results and saved files much larger.

## See also

Other "Helper functions":
[`rba_connection_test()`](https://rbioapi.moosa-r.com/reference/rba_connection_test.md),
[`rba_options()`](https://rbioapi.moosa-r.com/reference/rba_options.md),
[`rba_pages()`](https://rbioapi.moosa-r.com/reference/rba_pages.md)

## Examples

``` r
if (FALSE) { # \dontrun{
## Save metadata with one result:
result <- rba_reactome_species(metadata = TRUE)
request_metadata <- rba_metadata(result)

## Print a short summary:
request_metadata

## Check the rbioapi version saved with the result:
request_metadata$rbioapi_version

## View the requests without printing full functions and responses:
str(request_metadata$requests, max.level = 2)

## View one original httr response in more detail:
str(request_metadata$requests[[1]]$response, max.level = 1)
} # }
```
