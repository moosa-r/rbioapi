# Retrieve the Results of a Finished miEAA Enrichment Analysis

After your submitted enrichment analysis request has finished (check
using
[`rba_mieaa_enrich_status`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich_status.md)),
you can retrieve the results using this function.

## Usage

``` r
rba_mieaa_enrich_results(job_id, sort_by = "p_adjusted", sort_asc = TRUE, ...)
```

## Arguments

- job_id:

  Character: Job ID of a submitted enrichment analysis.

- sort_by:

  Character: (default = `"p_adjusted"`) Result column to sort by. One
  of: "category", "subcategory", "enrichment", "p_value", "p_adjusted",
  "q_value", or "observed".

- sort_asc:

  Logical: (default = `TRUE`) If `TRUE`, sort the results in ascending
  order. If `FALSE`, sort them in descending order.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A data frame with your enrichment analysis results.

## Details

Note that using
[`rba_mieaa_enrich`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich.md)
is a more convenient way to automatically perform this and other
required function calls to perform enrichment analysis on your input
miRNA-set using miEAA.

## Corresponding API Resources

"GET
https://ccb-compute2.cs.uni-saarland.de/mieaa/api/v1/enrichment_analysis/results/{job_id}/"

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
[`rba_mieaa_convert_version()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_convert_version.md),
[`rba_mieaa_enrich()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich.md),
[`rba_mieaa_enrich_status()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich_status.md),
[`rba_mieaa_enrich_submit()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich_submit.md)

## Examples

``` r
if (FALSE) { # \dontrun{
rba_mieaa_enrich_results("f52d1aef-6d3d-4d51-9020-82e68fe99012")
} # }
```
