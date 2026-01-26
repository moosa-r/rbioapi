# Check Status of a Submitted Enrichment Analysis in miEAA

After you have submitted your enrichment analysis (using
[`rba_mieaa_enrich_submit`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich_submit.md))
and retrieved a job-id, you can use this function to check the status of
your job. Status value equal to 100 means that your requested analysis
has finished and you may retrieve the results using
[`rba_mieaa_enrich_results`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich_results.md).

## Usage

``` r
rba_mieaa_enrich_status(job_id, ...)
```

## Arguments

- job_id:

  The job-id (a character string) of a submitted enrichment analysis.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list containing the status value for a analysis that corresponds to
your supplied job-id.

## Details

Note that using
[`rba_mieaa_enrich`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich.md)
is a more convenient way to automatically perform this and other
required function calls to perform enrichment analysis on your input
miRNA-set using miEAA.

## Corresponding API Resources

"GET
https://ccb-compute2.cs.uni-saarland.de/mieaa2/api/v1/job_status/{job_id}"

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
[`rba_mieaa_enrich_results()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich_results.md),
[`rba_mieaa_enrich_submit()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich_submit.md)

## Examples

``` r
if (FALSE) { # \dontrun{
Sys.sleep(1) # to prevent 429 error during R CMD check
rba_mieaa_enrich_status("f52d1aef-6d3d-4d51-9020-82e68fe99012")
} # }
```
