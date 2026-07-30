# Check the Status of a Submitted miEAA Enrichment Analysis

After you have submitted your enrichment analysis (using
[`rba_mieaa_enrich_submit`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich_submit.md))
and retrieved a job-id, you can use this function to check the status of
the job. The status is either a numeric completion percentage or
`"FAILED"`. A status value equal to 100 means that the requested
analysis has finished and you may retrieve the results using
[`rba_mieaa_enrich_results`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich_results.md).

## Usage

``` r
rba_mieaa_enrich_status(job_id, ...)
```

## Arguments

- job_id:

  Character: Job ID of a submitted enrichment analysis.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list containing a `status` element with either the numeric completion
percentage or `"FAILED"` for the supplied job ID. A completed job also
includes its results URL.

## Details

Note that using
[`rba_mieaa_enrich`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich.md)
is a more convenient way to automatically perform this and other
required function calls to perform enrichment analysis on your input
miRNA-set using miEAA.

## Corresponding API Resources

"GET
https://ccb-compute2.cs.uni-saarland.de/mieaa/api/v1/job_status/{job_id}/"

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
[`rba_mieaa_enrich_results()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich_results.md),
[`rba_mieaa_enrich_submit()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich_submit.md)

## Examples

``` r
if (FALSE) { # \dontrun{
Sys.sleep(1) # to prevent 429 error during R CMD check
rba_mieaa_enrich_status("f52d1aef-6d3d-4d51-9020-82e68fe99012")
} # }
```
