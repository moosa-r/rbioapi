# A One-step Wrapper for miRNA Enrichment Using miEAA

This function is a wrapper for the multiple function calls necessary to
perform enrichment analysis on a given miRNA list using miEAA. See
Details section for more information.

## Usage

``` r
rba_mieaa_enrich(
  test_set,
  mirna_type,
  test_type,
  species,
  categories = NULL,
  p_adj_method = "fdr",
  independent_p_adj = TRUE,
  sig_level = 0.05,
  min_hits = 2,
  ref_set = NULL,
  sort_by = "p_adjusted",
  sort_asc = TRUE,
  poll_interval = 5,
  poll_timeout = 300,
  ...
)
```

## Arguments

- test_set:

  Character vector: Mature or precursor miRBase miRNA identifiers. Note
  that

  1.  Only miRBase v22 identifiers are accepted. You can use
      [`rba_mieaa_convert_version`](https://rbioapi.moosa-r.com/reference/rba_mieaa_convert_version.md)
      to convert older identifiers to miRBase v22.

  2.  The list must contain either mature or precursor miRNA
      identifiers, not a mixture of both.

- mirna_type:

  Character: Type of the supplied miRNA identifiers; either "mature" or
  "precursor".

- test_type:

  Character: Analysis to perform; either "ORA" for over-representation
  analysis or "GSEA" for miRNA gene set enrichment analysis. For GSEA,
  the input list must already be ranked by an appropriate criterion.

- species:

  Character or Numeric: Scientific name, abbreviation, or NCBI taxon ID
  of one of the following species:

  1.  "Homo sapiens", "hsa" or 9606

  2.  "Mus musculus", "mmu" or 10090

  3.  "Rattus norvegicus", "rno" or 10116

  4.  "Arabidopsis thaliana", "ath" or 3702

  5.  "Bos taurus", "bta" or 9913

  6.  "Caenorhabditis elegans", "cel" or 6239

  7.  "Drosophila melanogaster", "dme" or 7227

  8.  "Danio rerio", "dre" or 7955

  9.  "Gallus gallus", "gga" or 9031

  10. "Sus scrofa", "ssc" or 9823

- categories:

  Character vector: (default = `NULL`) One or more category identifiers
  to use for miRNA set enrichment analysis. Note that

  - Available categories vary with the selected species and whether the
    supplied miRNAs are mature or precursor. Use
    [`rba_mieaa_cats`](https://rbioapi.moosa-r.com/reference/rba_mieaa_cats.md)
    to retrieve a list of available category identifiers for a given
    species and miRNA type.

  - If `NULL`, the analysis is performed using all available categories.

- p_adj_method:

  Character: (default = `"fdr"`) P-value adjustment method to use. One
  of: "none", "fdr", "bonferroni", "BY", "hochberg", "holm", or
  "hommel".

- independent_p_adj:

  Logical: (default = `TRUE`) The scope of p-value adjustment. If
  `TRUE`, p-values are adjusted separately within each category. If
  `FALSE`, p-values are adjusted collectively over all categories.

- sig_level:

  Numeric: (default = `0.05`) Significance threshold for adjusted
  p-values. Values equal to or greater than this threshold are omitted
  from the results. Must be greater than 0 and at most 1.

- min_hits:

  Numeric: (default = `2`) Minimum number of miRNAs from the test set
  that a subcategory must contain to be included in the results. Must be
  a positive integer.

- ref_set:

  Character vector: (default = `NULL`) Only applicable when
  `test_type = "ORA"`. Used as the reference (background or universe)
  set for p-value calculations.

- sort_by:

  Character: (default = `"p_adjusted"`) Result column to sort by. One
  of: "category", "subcategory", "enrichment", "p_value", "p_adjusted",
  "q_value", or "observed".

- sort_asc:

  Logical: (default = `TRUE`) If `TRUE`, sort the results in ascending
  order. If `FALSE`, sort them in descending order.

- poll_interval:

  Numeric: (default = `5`) Number of seconds to wait between job-status
  requests; must be between 5 and 300 seconds.

- poll_timeout:

  Numeric: (default = `300`) Maximum number of seconds to wait for the
  enrichment analysis to finish. Use `Inf` to wait without a time limit.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A data frame with your enrichment analysis results.

## Details

This function will call other rba_mieaa\_\*\*\* functions with the
following order:

1.  Call
    [`rba_mieaa_enrich_submit`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich_submit.md)
    to submit an enrichment analysis request to the miEAA server using
    the supplied miRNA list and other arguments.

2.  Once the job is successfully submitted, call
    [`rba_mieaa_enrich_status`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich_status.md)
    at the selected polling interval to check whether the server-side
    analysis has finished.

3.  Call
    [`rba_mieaa_enrich_results`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich_results.md)
    to retrieve the results of your enrichment analysis.

See each function's manual for more details. The `save_file` rbioapi
option applies only to the final enrichment results and not to the
intermediate submission or status responses.

## Corresponding API Resources

"https://ccb-compute2.cs.uni-saarland.de/mieaa/api/"

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
[`rba_mieaa_enrich_results()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich_results.md),
[`rba_mieaa_enrich_status()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich_status.md),
[`rba_mieaa_enrich_submit()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich_submit.md)

Other "Enrichment/Over-representation":
[`rba_enrichr()`](https://rbioapi.moosa-r.com/reference/rba_enrichr.md),
[`rba_panther_enrich()`](https://rbioapi.moosa-r.com/reference/rba_panther_enrich.md),
[`rba_reactome_analysis()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis.md),
[`rba_string_enrichment()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment.md),
[`rba_string_enrichment_image()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_image.md)

## Examples

``` r
if (FALSE) { # \dontrun{
rba_mieaa_enrich(test_set = c("hsa-miR-20b-5p", "hsa-miR-144-5p",
 "hsa-miR-17-5p", "hsa-miR-20a-5p"),
     mirna_type = "mature",
     test_type = "ORA",
     species = 9606,
     categories = "miRPathDB_GO_Biological_process_mature")
} # }
```
