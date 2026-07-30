# Convert miRNA Identifiers Between Different miRBase Versions

miEAA uses miRBase v22 identifiers. This function converts a set of
mature or precursor miRNA identifiers between two supported miRBase
versions.

## Usage

``` r
rba_mieaa_convert_version(
  mirna,
  mirna_type,
  input_version,
  output_version,
  simple_output = FALSE,
  ...
)
```

## Arguments

- mirna:

  Character vector: miRNA identifiers to convert.

- mirna_type:

  Character: Type of the supplied miRNA identifiers; either "mature" or
  "precursor".

- input_version:

  Numeric: miRBase version of the supplied identifiers.

- output_version:

  Numeric: miRBase version to which the identifiers should be converted.

- simple_output:

  Logical: (default = `FALSE`) If `FALSE`, return a two-column data
  frame containing the input and output identifier mappings. If `TRUE`,
  return only the converted identifiers without their association with
  the supplied identifiers.

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
https://ccb-compute2.cs.uni-saarland.de/mieaa/api/v1/mirbase_converter/"

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
[`rba_mieaa_convert_type()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_convert_type.md),
[`rba_mieaa_enrich()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich.md),
[`rba_mieaa_enrich_results()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich_results.md),
[`rba_mieaa_enrich_status()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich_status.md),
[`rba_mieaa_enrich_submit()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich_submit.md)

## Examples

``` r
# \donttest{
Sys.sleep(1) # to prevent 429 error during R CMD check
rba_mieaa_convert_version(mirna = c("hsa-miR-20b-5p", "hsa-miR-144-5p"),
    mirna_type = "mature", input_version = 22, output_version =  16)
# }
```
