# Get a TF flexible models (TFFMs) information

Using this function you can retrieve details and annotations of
Transcription Factor flexible models (TFFMs) associated with a TFFM ID.
If a base ID (i.e. without version suffix) was supplied, the latest
version will be returned.

## Usage

``` r
rba_jaspar_tffm(tffm_id, ...)
```

## Arguments

- tffm_id:

  Character: A TF flexible model (TFFM) Identifier.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list that contains the TFFM's information and annotations.

## Corresponding API Resources

"GET https://jaspar.elixir.no/api/v1/fttm/{tffm_id}/"

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
[`rba_jaspar_tffm_search()`](https://rbioapi.moosa-r.com/reference/rba_jaspar_tffm_search.md)

## Examples

``` r
# \donttest{
rba_jaspar_tffm("TFFM0056.3")
# }
```
