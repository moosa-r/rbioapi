# Retrieving Functional Annotation

STRING cross-reference the proteins with several databases (see
"Details" section). By providing your input set o proteins (and
optionally background or universe protein set), you can use this
function to retrieve full set of terms (annotations) pertinent to your
input proteins in each database, among with information for each term.

## Usage

``` r
rba_string_annotations(
  ids,
  species = NULL,
  allow_pubmed = FALSE,
  split_df = TRUE,
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

  Numeric: NCBI Taxonomy identifier; Human Taxonomy ID is 9606.
  (Recommended, but optional if your input is less than 100 IDs.)

- allow_pubmed:

  logical: (default = FALSE) PubMed usually assigns a large number of
  reference publications to each protein. In order to reduce the output
  size, PubMed's results will be excluded from the results, unless
  stated otherwise by setting this argument to TRUE.

- split_df:

  (logical, default = TRUE), If TRUE, instead of one data frame, results
  from different categories will be split into multiple data frames
  based on their 'category'.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A data frame which every row is an assigned terms and the columns are
the terms category, description, number of genes, and other pertinent
information.

## Details

STRING currently maps to and retrieve enrichment results based on Gene
Ontology (GO), KEGG pathways, UniProt Keywords, PubMed publications,
Pfam domains, InterPro domains, and SMART domains.  
Note that this function will return a full list of the terms containing
your supplied proteins. To perform enrichment and only retrieve a
enriched subset of the terms, use
[`rba_string_enrichment`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment.md).

## Corresponding API Resources

"POST https://string-db.org/api/{output_format}/functional_annotation?
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

[`rba_string_map_ids`](https://rbioapi.moosa-r.com/reference/rba_string_map_ids.md)`, `[`rba_string_enrichment`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment.md)`, `[`rba_string_enrichment_image`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_image.md)` `

Other "STRING":
[`rba_string_enrichment()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment.md),
[`rba_string_enrichment_image()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_image.md),
[`rba_string_enrichment_ppi()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_ppi.md),
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
```
