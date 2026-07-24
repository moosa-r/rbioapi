# Get Functional Annotations

STRING cross-references proteins with several annotation resources. This
function retrieves the complete set of annotations assigned to the input
proteins, together with information about each term.

## Usage

``` r
rba_string_annotations(
  ids,
  species = NULL,
  allow_pubmed = FALSE,
  split_df = TRUE,
  only_pubmed = FALSE,
  ...
)
```

## Arguments

- ids:

  Your protein ID(s). It is strongly recommended to supply STRING IDs.
  See
  [`rba_string_map_ids`](https://rbioapi.moosa-r.com/reference/rba_string_map_ids.md)
  for more information.

- species:

  Numeric: [NCBI Taxonomy
  identifier](https://www.ncbi.nlm.nih.gov/taxonomy/); Human Taxonomy ID
  is 9606. (Recommended, but optional.)

- allow_pubmed:

  Logical (default = `FALSE`): Include PubMed annotations. These
  annotations are excluded by default because many publications may be
  assigned to each protein. This argument is ignored when
  `only_pubmed = TRUE`.

- split_df:

  Logical: (default = `TRUE`) Split results into a list of data frames
  by `category`; otherwise, return one data frame.

- only_pubmed:

  Logical (default = `FALSE`): Return only PubMed annotations. This
  takes precedence over `allow_pubmed`.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A data frame in which every row is an assigned term and the columns
contain the term category, description, number of genes, and other
pertinent information. If `split_df = TRUE`, a list of data frames split
by category is returned. With `only_pubmed = TRUE`, a one-element list
named `PMID` is returned when PubMed annotations are available.

## Details

STRING currently retrieves annotations based on Gene Ontology (GO),
UniProt Keywords, PubMed publications, Pfam domains, InterPro domains,
and SMART domains. KEGG annotations are unavailable from this endpoint
because of KEGG licensing restrictions.  
This function returns annotations without enrichment filtering. To
perform enrichment and retrieve only enriched terms, use
[`rba_string_enrichment`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment.md).

## Corresponding API Resources

"POST https://string-db.org/api/{output-format}/functional_annotation?
identifiers={your_identifiers}&{optional_parameters}"

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

` `[`rba_string_map_ids`](https://rbioapi.moosa-r.com/reference/rba_string_map_ids.md)`, `[`rba_string_enrichment`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment.md)`, `[`rba_string_enrichment_image`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_image.md)`, `[`rba_string_functional_terms`](https://rbioapi.moosa-r.com/reference/rba_string_functional_terms.md)` `

Other "STRING":
[`rba_string_enrichment()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment.md),
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

## Examples

``` r
# \donttest{
rba_string_annotations(ids = "TP53", species = 9606)
# }
if (FALSE) { # \dontrun{
rba_string_annotations(
    ids = "TP53",
    species = 9606,
    only_pubmed = TRUE
)
} # }
```
