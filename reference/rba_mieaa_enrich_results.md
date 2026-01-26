# Retrieve Results of a finished Enrichment Analysis from miEAA

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

  The job-id (a character string) of a submitted enrichment analysis.

- sort_by:

  A column name to the result's table based on that. one of: "category",
  "subcategory", "enrichment", "p_value", "p_adjusted" (default),
  "q_value" or "observed" .

- sort_asc:

  (logical) If TRUE, the results will be sorted in ascending order. If
  FALSE, the results will be sorted in descending order.

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
https://ccb-compute2.cs.uni-saarland.de/mieaa2/api/v1/results/{job_id}"

## References

- Fabian Kern, Tobias Fehlmann, Jeffrey Solomon, Louisa Schwed, Nadja
  Grammes, Christina Backes, Kendall Van Keuren-Jensen, David Wesley
  Craig,Eckart Meese, Andreas Keller, miEAA 2.0: integrating
  multi-species microRNA enrichment analysis and workflow management
  systems, Nucleic Acids Research, Volume 48, Issue W1, 02 July 2020,
  Pages W521–W528, https://doi.org/10.1093/nar/gkaa309

- [miEAA browsable API
  tutorial](https://ccb-compute2.cs.uni-saarland.de/mieaa2/tutorial/api/)

- [Citations note on miEAA
  website](https://ccb-compute2.cs.uni-saarland.de/mieaa2/)

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
