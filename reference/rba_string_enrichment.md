# Get Functional Enrichment

STRING cross-references proteins with several annotation resources. (See
'Details' section). Provide an input protein set and, optionally, a
background protein set to perform an enrichment test and retrieve
enriched terms with their associated statistics. Use
[`rba_string_enrichment_image`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_image.md)
to retrieve the analysis results as a plot.

## Usage

``` r
rba_string_enrichment(
  ids,
  species = NULL,
  background = NULL,
  split_df = TRUE,
  add_scores = FALSE,
  max_fdr = NULL,
  min_gene_count = NULL,
  min_strength = NULL,
  min_signal = NULL,
  merge_similarity = NULL,
  ...
)
```

## Arguments

- ids:

  Character or Numeric: Your protein ID(s). It is strongly recommended
  to supply STRING IDs. See
  [`rba_string_map_ids`](https://rbioapi.moosa-r.com/reference/rba_string_map_ids.md)
  for more information. Note that if only one ID is supplied, STRING
  expands the network by 10 proteins.

- species:

  Numeric: (optional) [NCBI Taxonomy
  identifier](https://www.ncbi.nlm.nih.gov/taxonomy/); Human Taxonomy ID
  is 9606. (Recommended, but optional.)

- background:

  Character: (optional) A set of STRING protein IDs to be used as the
  statistical background (or universe) when computing term p-values.
  Only STRING IDs are accepted. See
  [`rba_string_map_ids`](https://rbioapi.moosa-r.com/reference/rba_string_map_ids.md)
  to map your IDs.

- split_df:

  Logical: (default = `TRUE`) Split results into a list of data frames
  by `category`; otherwise, return one data frame.

- add_scores:

  Logical: (default = `FALSE`) Include `strength` (log10 of the
  observed-to-expected gene-count ratio) and `signal` (a size-aware
  score combining that ratio and enrichment significance).

- max_fdr:

  Numeric: (optional) Maximum FDR, greater than 0 and no greater than 1.
  Only terms meeting this threshold are returned.

- min_gene_count:

  Numeric: (optional) Minimum positive whole number of input genes
  assigned to a returned term.

- min_strength:

  Numeric: (optional) Minimum enrichment strength.

- min_signal:

  Numeric: (optional) Minimum enrichment signal.

- merge_similarity:

  Numeric: (optional) Overlap threshold for merging terms based on their
  assigned input-gene sets, greater than 0 and no greater than 1. The
  most significant term is retained. A value of 1 merges only identical
  gene sets. Omit to retain unmerged terms.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A data frame in which each row is an enriched term and the columns
contain the term category, description, gene counts, p-value, FDR, and
other pertinent information. If `split_df = TRUE`, a list of data frames
split by category is returned. `add_scores = TRUE` adds numeric
`strength` and `signal` columns. If no terms are returned, the result is
a zero-row data frame.

## Details

STRING currently returns enrichment results from Gene Ontology (GO),
KEGG pathways, UniProt Keywords, PubMed publications, Pfam domains,
InterPro domains, SMART domains, and other resources including GWAS
Catalog and Hallmark gene sets. Category availability depends on
species.  
STRING returns only terms with a raw p-value below 0.1. To retrieve
annotations without filtering by enrichment p-value, use
[`rba_string_annotations`](https://rbioapi.moosa-r.com/reference/rba_string_annotations.md).

## Corresponding API Resources

"POST https://string-db.org/api/{output-format}/enrichment?identifiers=
{your_identifiers}&{optional_parameters}"

## References

- Damian Szklarczyk, Katerina Nastou, Mikaela Koutrouli, Rebecca Kirsch,
  Farrokh Mehryary, Radja Hachilif, Dewei Hu, Matteo E Peluso, Qingyao
  Huang, Tao Fang, Nadezhda T Doncheva, Sampo Pyysalo, Peer Bork, Lars J
  Jensen, Christian von Mering, The STRING database in 2025: protein
  networks with directionality of regulation, Nucleic Acids Research,
  Volume 53, Issue D1, 6 January 2025, Pages D730–D737,
  https://doi.org/10.1093/nar/gkae1113

- [STRING API Documentation](https://string-db.org/help/api/)

- [Citations note on STRING
  website](https://string-db.org/cgi/about?footer_active_subpage=references)

## See also

` `[`rba_string_map_ids`](https://rbioapi.moosa-r.com/reference/rba_string_map_ids.md)`, `[`rba_string_annotations`](https://rbioapi.moosa-r.com/reference/rba_string_annotations.md)`, `[`rba_string_enrichment_image`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_image.md)`, `[`rba_string_functional_terms`](https://rbioapi.moosa-r.com/reference/rba_string_functional_terms.md)` `

Other "STRING":
[`rba_string_annotations()`](https://rbioapi.moosa-r.com/reference/rba_string_annotations.md),
[`rba_string_enrichment_image()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_image.md),
[`rba_string_enrichment_ppi()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_ppi.md),
[`rba_string_functional_terms()`](https://rbioapi.moosa-r.com/reference/rba_string_functional_terms.md),
[`rba_string_homology_inter()`](https://rbioapi.moosa-r.com/reference/rba_string_homology_inter.md),
[`rba_string_homology_intra()`](https://rbioapi.moosa-r.com/reference/rba_string_homology_intra.md),
[`rba_string_interaction_partners()`](https://rbioapi.moosa-r.com/reference/rba_string_interaction_partners.md),
[`rba_string_interactions_network()`](https://rbioapi.moosa-r.com/reference/rba_string_interactions_network.md),
[`rba_string_map_ids()`](https://rbioapi.moosa-r.com/reference/rba_string_map_ids.md),
[`rba_string_network_image()`](https://rbioapi.moosa-r.com/reference/rba_string_network_image.md),
[`rba_string_version()`](https://rbioapi.moosa-r.com/reference/rba_string_version.md)

Other "Enrichment/Over-representation":
[`rba_enrichr()`](https://rbioapi.moosa-r.com/reference/rba_enrichr.md),
[`rba_mieaa_enrich()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich.md),
[`rba_panther_enrich()`](https://rbioapi.moosa-r.com/reference/rba_panther_enrich.md),
[`rba_reactome_analysis()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis.md),
[`rba_string_enrichment_image()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_image.md)

## Examples

``` r
# \donttest{
rba_string_enrichment(ids = c("TP53", "TNF", "EGFR"), species = 9606)
# }
# \donttest{
## Include enrichment scores, filter results, and merge similar terms
rba_string_enrichment(
    ids = c("TP53", "MDM2", "CDKN1A", "BRCA1", "CDK1", "CDK2"),
    species = 9606,
    add_scores = TRUE,
    max_fdr = 0.05,
    min_gene_count = 2,
    merge_similarity = 0.8,
    split_df = FALSE
)
# }
```
