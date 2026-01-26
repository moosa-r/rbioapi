# Submit miEAA miRNA Enrichment Analysis Request

Using This function you can submit a request in miEAA servers to perform
Over-representation or GSEA Analysis for a given set of miRNA
identifiers. see "arguments" section for more information.

## Usage

``` r
rba_mieaa_enrich_submit(
  test_set,
  mirna_type,
  test_type,
  species = "hsa",
  categories = NULL,
  p_adj_method = "fdr",
  independent_p_adj = TRUE,
  sig_level = 0.05,
  min_hits = 2,
  ref_set = NULL,
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

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list that contains your submitted job's ID and a URL to manually check
for your job status.

## Details

Note that using
[`rba_mieaa_enrich`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich.md)
is a more convenient way to automatically perform this and other
required function calls to perform enrichment analysis on your input
miRNA-set using miEAA.

## Corresponding API Resources

"POST
https://ccb-compute2.cs.uni-saarland.de/mieaa2/api/v1/enrichment_analysis/{species}/{type}/{test}/"

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
[`rba_mieaa_enrich_status()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich_status.md)

## Examples

``` r
# \donttest{
Sys.sleep(1) # to prevent 429 error during R CMD check
rba_mieaa_enrich_submit(test_set = c("hsa-miR-20b-5p", "hsa-miR-144-5p"),
    mirna_type = "mature",
    test_type = "GSEA",
    species = 9606,
    categories = NULL)
# }
```
