# Convert Between Mature and Precursor miRNA Identifiers

miRBase identifiers can refer to either mature or precursor miRNAs.
(see: [A uniform system for microRNA
annotation](https://rnajournal.cshlp.org/content/9/3/277)). Use this
function to convert mature miRNA identifiers to precursor identifiers or
vice versa.

## Usage

``` r
rba_mieaa_convert_type(
  mirna,
  input_type,
  only_unique = FALSE,
  simple_output = FALSE,
  ...
)
```

## Arguments

- mirna:

  Character vector: miRNA identifiers to convert.

- input_type:

  Character: Type of the supplied miRNA identifiers; either "mature" or
  "precursor".

- only_unique:

  Logical: (default = `FALSE`) Mature and precursor miRNA identifiers do
  not always map uniquely. If `TRUE`, do not return mappings for inputs
  with multiple matches. In tabular output, these inputs remain as rows
  with `"-"` in the output column.

- simple_output:

  Logical: (default = `FALSE`) If `FALSE`, return a two-column data
  frame containing the input and output identifier mappings; multiple
  output identifiers are separated by semicolons. If `TRUE`, expand
  one-to-many mappings into a flat character vector of converted
  identifiers without their association with the supplied identifiers.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

Depending on `simple_output`, a data frame or character vector
containing the mappings returned by miEAA. Unrecognized or unmapped
supplied identifiers can be omitted from the output.

## Corresponding API Resources

"POST
https://ccb-compute2.cs.uni-saarland.de/mieaa/api/v1/mirna_precursor_converter/"

## References

- Ernesto Aparicio-Puerta, Pascal Hirsch, Georges P. Schmartz, Fabian
  Kern, Tobias Fehlmann, Andreas Keller, miEAA 2023: updates, new
  functional microRNA sets and improved enrichment visualizations,
  Nucleic Acids Research, Volume 51, Issue W1, 5 July 2023, Pages
  W319–W325, https://doi.org/10.1093/nar/gkad392

- [miEAA browsable API
  tutorial](https://ccb-compute2.cs.uni-saarland.de/mieaa/tutorial/api/)

- [Citation note on miEAA
  website](https://ccb-compute2.cs.uni-saarland.de/mieaa/)

## See also

Other "miEAA":
[`rba_mieaa_cats()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_cats.md),
[`rba_mieaa_convert_version()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_convert_version.md),
[`rba_mieaa_enrich()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich.md),
[`rba_mieaa_enrich_results()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich_results.md),
[`rba_mieaa_enrich_status()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich_status.md),
[`rba_mieaa_enrich_submit()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich_submit.md)

## Examples

``` r
# \donttest{
Sys.sleep(1) # to prevent 429 error during R CMD check
rba_mieaa_convert_type(mirna = c("hsa-miR-20b-5p", "hsa-miR-144-5p"),
    input_type = "mature")
# }
```
