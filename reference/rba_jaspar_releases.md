# Get information about JASPAR database releases

If a release number is supplied, this function returns the details of
that release. Otherwise, when called without the `release_number`
argument, it returns a list of all JASPAR database releases.

## Usage

``` r
rba_jaspar_releases(release_number = NULL, ...)
```

## Arguments

- release_number:

  Numeric: Which JASPAR database release number should be retrieved? If
  left NULL (the default), a list of all JASPAR database releases will
  be returned.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list that contains all JASPAR database releases' information or
details of a particular release.

## Corresponding API Resources

"GET https://jaspar.elixir.no/api/v1/releases/"  
"GET https://jaspar.elixir.no/api/v1/releases/{release_number}/"

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
[`rba_jaspar_matrix_versions()`](https://rbioapi.moosa-r.com/reference/rba_jaspar_matrix_versions.md),
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
rba_jaspar_releases()
rba_jaspar_releases(11)
# }
```
