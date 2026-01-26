# Search TF flexible models (TFFMs) available in JASPAR

You can use this function to list the JASPAR TF flexible models (TFFMs)
that match your search query, or run the function without any arguments
to return a list of every matrix profile available in the latest
release.

## Usage

``` r
rba_jaspar_tffm_search(
  term = NULL,
  release = 2024,
  tax_group = NULL,
  search = NULL,
  order = NULL,
  page_size = 1000,
  page = 1,
  ...
)
```

## Arguments

- term:

  Character: A search term.

- release:

  Numeric: (default = 2024) Which JASPAR database release to use?
  Available options are: 2024, 2022, 2020, 2018, 2016, and 2014.

- tax_group:

  Character: Taxonomic group. Use
  [`rba_jaspar_taxons`](https://rbioapi.moosa-r.com/reference/rba_jaspar_taxons.md)
  to get a list of supported Taxonomic groups.

- search:

  Character: A search term.

- order:

  Character: A character string or a vector of character strings of
  field names that will be used to order the results.  
  Providing multiple field names is supported. You can also use prefix
  "-" before a field name to indicate reverse ordering.

- page_size:

  Numeric: (default = 1000) This resource returns paginated results.
  What is the maximum numbers of results that you want to retrieve per a
  page? Accepted values are between 1 and 1000.

- page:

  Numeric: Which page of the results to retrieve? The accepted values
  depend on the page size and number of results.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list that contains a data frame with information of query hits' TFFMs.

## Details

Note that this is a search function. Thus, you are not required to fill
every argument; You may use whatever combinations of arguments you see
fit for your query.  
The results are paginated. You can control the page's size number with
the function's arguments. Also, you can use
[`rba_pages`](https://rbioapi.moosa-r.com/reference/rba_pages.md) to
automatically iterate over multiple pages.

## Corresponding API Resources

"GET https://jaspar.elixir.no/api/v1/api/v1/tffm/"

## References

- Rauluseviciute I, Riudavets-Puig R, Blanc-Mathieu R, Castro-Mondragon
  JA, Ferenc K, Kumar V, Lemma RB, Lucas J, Chèneby J, Baranasic D, Khan
  A, Fornes O, Gundersen S, Johansen M, Hovig E, Lenhard B, Sandelin A,
  Wasserman WW, Parcy F, Mathelier A JASPAR 2024: 20th anniversary of
  the open-access database of transcription factor binding profiles
  Nucleic Acids Res. in_press; doi: 10.1093/nar/gkad1059

- Khan, A. and Mathelier, A. JASPAR RESTful API: accessing JASPAR data
  from any programming language. Bioinformatics, 2017, doi:
  10.1093/bioinformatics/btx804

- [JASPAR API Documentation](https://jaspar.elixir.no/api/v1/docs/)

- [Citations note on JASPAR website](https://jaspar.elixir.no/faq/)

## See also

Other "JASPAR":
[`rba_jaspar_collections()`](https://rbioapi.moosa-r.com/reference/rba_jaspar_collections.md),
[`rba_jaspar_collections_matrices()`](https://rbioapi.moosa-r.com/reference/rba_jaspar_collections_matrices.md),
[`rba_jaspar_matrix()`](https://rbioapi.moosa-r.com/reference/rba_jaspar_matrix.md),
[`rba_jaspar_matrix_search()`](https://rbioapi.moosa-r.com/reference/rba_jaspar_matrix_search.md),
[`rba_jaspar_matrix_versions()`](https://rbioapi.moosa-r.com/reference/rba_jaspar_matrix_versions.md),
[`rba_jaspar_releases()`](https://rbioapi.moosa-r.com/reference/rba_jaspar_releases.md),
[`rba_jaspar_sites()`](https://rbioapi.moosa-r.com/reference/rba_jaspar_sites.md),
[`rba_jaspar_species()`](https://rbioapi.moosa-r.com/reference/rba_jaspar_species.md),
[`rba_jaspar_species_matrices()`](https://rbioapi.moosa-r.com/reference/rba_jaspar_species_matrices.md),
[`rba_jaspar_taxons()`](https://rbioapi.moosa-r.com/reference/rba_jaspar_taxons.md),
[`rba_jaspar_taxons_matrices()`](https://rbioapi.moosa-r.com/reference/rba_jaspar_taxons_matrices.md),
[`rba_jaspar_tffm()`](https://rbioapi.moosa-r.com/reference/rba_jaspar_tffm.md)

## Examples

``` r
# \donttest{
rba_jaspar_tffm_search(term = "FOX")
rba_jaspar_tffm_search(tax_group = "insects")
rba_jaspar_tffm_search(page_size = 100)
# }
```
