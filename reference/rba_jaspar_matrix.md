# Get a position frequency matrix (PFM) with annotations

Using this function, you can retrieve the position frequency matrix
(PFM) associated with a matrix profile identifier, along with its
details and annotations. If a base ID (i.e., without a version suffix)
is supplied, the latest version will be returned.

## Usage

``` r
rba_jaspar_matrix(matrix_id, file_format = NULL, save_to = NULL, ...)
```

## Arguments

- matrix_id:

  Character: A matrix profile identifier. It follows a "base_id.version"
  naming scheme.

- file_format:

  Character: Instead of returning an R object, you can directly download
  the profile matrix as a file in this format. Supported formats are
  "yaml", "jaspar", "transfac", "meme", and "pfm".

- save_to:

  NULL or Character:

  - NULL: (only if file_format was supplied) Save the file to an
    automatically-generated path.

  - Character string: A valid file or directory path to save the file
    to.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list containing the PFM, details, and annotations. If `file_format` is
supplied, the function returns the unparsed file content as a character
string.

## Corresponding API Resources

"GET https://jaspar.elixir.no/api/v1/matrix/{matrix_id}/"

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
[`rba_jaspar_matrix_search()`](https://rbioapi.moosa-r.com/reference/rba_jaspar_matrix_search.md),
[`rba_jaspar_matrix_versions()`](https://rbioapi.moosa-r.com/reference/rba_jaspar_matrix_versions.md),
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
rba_jaspar_matrix("MA0600.2")
# }
if (FALSE) { # \dontrun{
rba_jaspar_matrix(matrix_id = "MA0600.2",
                  file_format = "meme",
                  save_to = "my_matrix.meme")
} # }
```
