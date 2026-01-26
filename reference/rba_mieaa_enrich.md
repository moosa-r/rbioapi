# A One-step Wrapper for miRNA Enrichment Using miEAA

This function is a wrapper for the multiple function calls necessary to
perform enrichment analysis on a given miRNA list using miEAA. see
details section for more information.

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
  ...
)
```

## Arguments

- test_set:

  a character vector with your mature or precursor miRBase miRNA
  accessions. Note that

  1.  Only miRBase v22 miRNA accession are accepted. You can use
      [`rba_mieaa_convert_version`](https://rbioapi.moosa-r.com/reference/rba_mieaa_convert_version.md)
      to convert your accessions to miRBase v22.

  2.  Your list should be entirely consisted of either mature or
      precursor miRNA accession. A mixture of both is not accepted.

- mirna_type:

  Type of your supplied miRNA accession. either "mature" or "precursor".

- test_type:

  The analysis to perform. can be either "ORA" for 'Over Representation
  Analysis' or "GSEA" for miRNA (Gene) 'Set Enrichment Analysis'. Note
  that in GSEA, your list should be sorted beforehand based on some
  criterion.

- species:

  Fully or partially matching Scientific name, abbreviation or NCBI
  taxon ID of one of the following species:

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

  one or multiple Category names to be used for miRNA set enrichment
  analysis. Note that

  - Available categories varies based on your chosen specie and if your
    supplied miRNA type is mature or precursor. Use
    [`rba_mieaa_cats`](https://rbioapi.moosa-r.com/reference/rba_mieaa_cats.md)
    to retrieve a list of available category names for a given specie
    and miRNA type.

  - If you supply NULL, the analysis will be performed on all of the
    available categories.

- p_adj_method:

  P-value adjustment method to be used. Should be one of: "none", "fdr"
  (default), "bonferroni", "BY", "hochberg", "holm" or "hommel"

- independent_p_adj:

  (logical) The scope and level of p-value adjustment; if TRUE
  (default), the categories will be considered independent from each
  other and the p-value will be adjusted separately for each category.
  if FALSE, the p-value will be adjusted collectively over all
  categories.

- sig_level:

  (numeric) The significance threshold of adjusted P-value. values equal
  to or greater than this threshold will be dropped from the results.

- min_hits:

  (numeric) How many miRNA should a sub-category have from your supplied
  test-list to be included in the results? (default is 2)

- ref_set:

  (Optional) Only applicable when test_type is "ORA". This character
  vector will be used as your reference (background or universe) set for
  p-value calculations.

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

This function will call other rba_mieaa\_\*\*\* functions with the
following order:

1.  Call
    [`rba_mieaa_enrich_submit`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich_submit.md)
    to Submit an enrichment analysis request to miEAA servers, using
    your supplied miRNA lists and other arguments.

2.  Once your job was successfully submitted, it will call
    [`rba_mieaa_enrich_status`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich_status.md)
    every 5 seconds, to check the status of your running server-side job
    and whether your analysis job is finished and the results are
    available.

3.  Call
    [`rba_mieaa_enrich_results`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich_results.md)
    to retrieve the results of your enrichment analysis.

See each function's manual for more details.

## Corresponding API Resources

"GET https://ccb-compute2.cs.uni-saarland.de/mieaa2/api/"

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
