# List matrix profile versions associated with a base ID

Since JASPAR release 2010, matrix profiles have been versioned. A matrix
profile identifier follows a "base_id.version" naming scheme. Using this
function, you can retrieve a list of matrix profiles associated with a
base (stable) ID.

## Usage

``` r
rba_jaspar_matrix_versions(base_id, order = NULL, ...)
```

## Arguments

- base_id:

  Character: A base (stable) identifier. A matrix profile identifier
  follows a "base_id.version" naming scheme.

- order:

  Character: (optional) A field name or a vector of field names that
  will be used to order the results.  
  Providing multiple field names is supported. You can also use the
  prefix "-" before a field name to indicate reverse ordering.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A data frame with information on the matrix profile versions.

## Corresponding API Resources

"GET https://jaspar.elixir.no/api/v1/matrix/{base_id}/versions/"

## References

- Baydar Ovek D, et al. JASPAR 2026: expansion of transcription factor
  binding profiles and integration of deep learning models. Nucleic
  Acids Res. 2026;54(D1):D184-D193; doi: 10.1093/nar/gkaf1209

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
[`rba_jaspar_releases()`](https://rbioapi.moosa-r.com/reference/rba_jaspar_releases.md),
[`rba_jaspar_sites()`](https://rbioapi.moosa-r.com/reference/rba_jaspar_sites.md),
[`rba_jaspar_species()`](https://rbioapi.moosa-r.com/reference/rba_jaspar_species.md),
[`rba_jaspar_species_matrices()`](https://rbioapi.moosa-r.com/reference/rba_jaspar_species_matrices.md),
[`rba_jaspar_taxons()`](https://rbioapi.moosa-r.com/reference/rba_jaspar_taxons.md),
[`rba_jaspar_taxons_matrices()`](https://rbioapi.moosa-r.com/reference/rba_jaspar_taxons_matrices.md),
[`rba_jaspar_tffm()`](https://rbioapi.moosa-r.com/reference/rba_jaspar_tffm.md),
[`rba_jaspar_tffm_search()`](https://rbioapi.moosa-r.com/reference/rba_jaspar_tffm_search.md)

## Examples

``` r
# \donttest{
rba_jaspar_matrix_versions("MA0600")
# }
```
