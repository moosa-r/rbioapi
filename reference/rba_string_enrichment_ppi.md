# Get Protein-Protein Interaction Enrichment

STRING compares the interaction pattern of your input proteins with the
proteome-wide background interaction distribution to determine whether
the protein set contains more interactions than expected.

## Usage

``` r
rba_string_enrichment_ppi(
  ids,
  species = NULL,
  required_score = NULL,
  background = NULL,
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
  is 9606. (Recommended, but required if your input contains more than
  10 unique IDs.)

- required_score:

  Numeric (between 0 and 1000): Minimum interaction score used when
  calculating PPI enrichment. If omitted, STRING applies a
  network-dependent threshold. Common confidence thresholds are 150
  (low), 400 (medium), 700 (high), and 900 (highest).

- background:

  Character vector: A set of STRING protein IDs to be used as the
  background proteome. Only STRING IDs are acceptable. See
  [`rba_string_map_ids`](https://rbioapi.moosa-r.com/reference/rba_string_map_ids.md)
  to map your IDs.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list with protein-protein interaction enrichment results.

## Corresponding API Resources

"POST
https://string-db.org/api/{output-format}/ppi_enrichment?identifiers=
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

` `[`rba_string_map_ids`](https://rbioapi.moosa-r.com/reference/rba_string_map_ids.md)`, `[`rba_string_interactions_network`](https://rbioapi.moosa-r.com/reference/rba_string_interactions_network.md)`, `[`rba_string_interaction_partners`](https://rbioapi.moosa-r.com/reference/rba_string_interaction_partners.md)`, `[`rba_string_network_image`](https://rbioapi.moosa-r.com/reference/rba_string_network_image.md)` `

Other "STRING":
[`rba_string_annotations()`](https://rbioapi.moosa-r.com/reference/rba_string_annotations.md),
[`rba_string_enrichment()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment.md),
[`rba_string_enrichment_image()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_image.md),
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
rba_string_enrichment_ppi(ids = c("p53", "BRCA1", "cdk2", "Q99835",
       "CDC42", "CDK1", "KIF23", "PLK1", "RAC2", "RACGAP1"),
    species = 9606)
# }
```
