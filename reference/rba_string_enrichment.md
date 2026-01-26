# Getting Functional Enrichment

STRING cross-reference the proteins with several databases (see
"Details" section). By providing your input set o proteins (and
optionally background or universe protein set), you can use this
function to perform enrichment test and retrieve a list of enriched
terms in each database, among with pertinent information for each term.
Use
[`rba_string_enrichment_image`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_image.md)
to retrieve the analysis results as a plot.

## Usage

``` r
rba_string_enrichment(
  ids,
  species = NULL,
  background = NULL,
  split_df = TRUE,
  ...
)
```

## Arguments

- ids:

  Your protein ID(s). It is strongly recommended to supply STRING IDs.
  See
  [`rba_string_map_ids`](https://rbioapi.moosa-r.com/reference/rba_string_map_ids.md)
  for more information. Note that if only one id is supplied, STRING
  expands the network by 10 proteins.

- species:

  Numeric: NCBI Taxonomy identifier; Human Taxonomy ID is 9606.
  (Recommended, but optional if your input is less than 100 IDs.)

- background:

  character vector: A set of STRING protein IDs to be used as the
  statistical background (or universe) when computing P-value for the
  terms. Only STRING IDs are acceptable. (See
  [`rba_string_map_ids`](https://rbioapi.moosa-r.com/reference/rba_string_map_ids.md)
  to map your IDs.)

- split_df:

  (logical, default = TRUE), If TRUE, instead of one data frame, results
  from different categories will be split into multiple data frames
  based on their 'category'.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list of data frames which every row is an enriched terms with p-value
smaller than 0.1 and the columns are the terms category, description,
number of genes, p-value, fdr and other pertinent information.

## Details

STRING currently maps to and retrieve enrichment results based on Gene
Ontology (GO), KEGG pathways, UniProt Keywords, PubMed publications,
Pfam domains, InterPro domains, and SMART domains.  
Note that this function will only return the enriched terms pertinent to
your proteins that have a p-value lesser than 0.1. To retrieve a full
list of the terms -unfiltered by enrichment p-values-, use
[`rba_string_annotations`](https://rbioapi.moosa-r.com/reference/rba_string_annotations.md).

## Corresponding API Resources

"POST https://string-db.org/api/{output_format}/enrichment?identifiers=
{your_identifiers}&{optional_parameters}"

## References

- Damian Szklarczyk, Rebecca Kirsch, Mikaela Koutrouli, Katerina Nastou,
  Farrokh Mehryary, Radja Hachilif, Annika L Gable, Tao Fang, Nadezhda T
  Doncheva, Sampo Pyysalo, Peer Bork, Lars J Jensen, Christian von
  Mering, The STRING database in 2023: protein–protein association
  networks and functional enrichment analyses for any sequenced genome
  of interest, Nucleic Acids Research, Volume 51, Issue D1, 6 January
  2023, Pages D638–D646, https://doi.org/10.1093/nar/gkac1000

- [STRING API Documentation](https://string-db.org/help/api/)

- [Citations note on STRING
  website](https://string-db.org/cgi/about?footer_active_subpage=references)

## See also

[`rba_string_map_ids`](https://rbioapi.moosa-r.com/reference/rba_string_map_ids.md)`, `[`rba_string_annotations`](https://rbioapi.moosa-r.com/reference/rba_string_annotations.md)`, `[`rba_string_enrichment_image`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_image.md)` `

Other "STRING":
[`rba_string_annotations()`](https://rbioapi.moosa-r.com/reference/rba_string_annotations.md),
[`rba_string_enrichment_image()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_image.md),
[`rba_string_enrichment_ppi()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_ppi.md),
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
```
