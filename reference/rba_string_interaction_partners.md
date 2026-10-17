# Get All STRING Interaction Partners

This function retrieves STRING interactions involving any of your input
proteins as one party of the interaction, including interactions with
proteins outside the input set.  
Given the size of the STRING database, this function can return many
interactions. Use the filtering arguments to limit the results.

## Usage

``` r
rba_string_interaction_partners(
  ids,
  species = NULL,
  required_score = NULL,
  network_type = "functional",
  limit = NULL,
  network_flavor = NULL,
  typed_physical_edges = TRUE,
  typed_regulatory_edges = TRUE,
  ...
)
```

## Arguments

- ids:

  Character or Numeric: Your protein ID(s). It is strongly recommended
  to supply STRING IDs. See
  [`rba_string_map_ids`](https://rbioapi.moosa-r.com/reference/rba_string_map_ids.md)
  for more information.

- species:

  Numeric: (optional) [NCBI Taxonomy
  identifier](https://www.ncbi.nlm.nih.gov/taxonomy/); Human Taxonomy ID
  is 9606. (Recommended, but required if your input contains more than
  10 unique IDs.)

- required_score:

  Numeric: (optional) (between 0 and 1000): Minimum interaction score
  required for an interaction to be included in the returned
  interactions. If omitted, STRING applies a network-dependent
  threshold. Common confidence thresholds are 150 (low), 400 (medium),
  700 (high), and 900 (highest).

- network_type:

  Character: (default = `"functional"`) One of:

  - "functional": (default) Edges indicate both physical and functional
    associations.

  - "physical": Edges indicate that two proteins have a physical
    interaction or are parts of a complex.

  - "regulatory": Directed edges indicate regulation from a source
    protein to a target protein.

- limit:

  Numeric: (optional) Maximum number of interaction partners returned
  for each input protein, ordered by confidence. This limits partners
  rather than response rows; directed or typed results can contain more
  rows.

- network_flavor:

  Character: (optional) Set to `"typed"` to retrieve functional,
  physical, and directional regulatory scores in one response. Requires
  `network_type = "functional"`. Omit for the ordinary schema.

- typed_physical_edges:

  Logical: (default = `TRUE`) Include physical scores in a typed
  response. If `FALSE`, their columns contain missing values.

- typed_regulatory_edges:

  Logical: (default = `TRUE`) Include regulatory scores in a typed
  response. If `FALSE`, their columns contain missing values.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A data frame in which each row is a network interaction and the columns
contain interactor information and interaction scores. Regulatory and
typed requests use the corresponding schemas described in
[`rba_string_interactions_network`](https://rbioapi.moosa-r.com/reference/rba_string_interactions_network.md).
Missing typed evidence scores are numeric `NA`.

## Details

To retrieve only interactions among the input proteins, see
[`rba_string_interactions_network`](https://rbioapi.moosa-r.com/reference/rba_string_interactions_network.md).

## Corresponding API Resources

"POST https://string-db.org/api/{output-format}/interaction_partners?
identifiers={your_identifiers}&{optional_parameters}"

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

` `[`rba_string_map_ids`](https://rbioapi.moosa-r.com/reference/rba_string_map_ids.md)`, `[`rba_string_interactions_network`](https://rbioapi.moosa-r.com/reference/rba_string_interactions_network.md)`, `[`rba_string_network_image`](https://rbioapi.moosa-r.com/reference/rba_string_network_image.md)`, `[`rba_string_enrichment_ppi`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_ppi.md)` `

Other "STRING":
[`rba_string_annotations()`](https://rbioapi.moosa-r.com/reference/rba_string_annotations.md),
[`rba_string_enrichment()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment.md),
[`rba_string_enrichment_image()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_image.md),
[`rba_string_enrichment_ppi()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_ppi.md),
[`rba_string_functional_terms()`](https://rbioapi.moosa-r.com/reference/rba_string_functional_terms.md),
[`rba_string_homology_inter()`](https://rbioapi.moosa-r.com/reference/rba_string_homology_inter.md),
[`rba_string_homology_intra()`](https://rbioapi.moosa-r.com/reference/rba_string_homology_intra.md),
[`rba_string_interactions_network()`](https://rbioapi.moosa-r.com/reference/rba_string_interactions_network.md),
[`rba_string_map_ids()`](https://rbioapi.moosa-r.com/reference/rba_string_map_ids.md),
[`rba_string_network_image()`](https://rbioapi.moosa-r.com/reference/rba_string_network_image.md),
[`rba_string_version()`](https://rbioapi.moosa-r.com/reference/rba_string_version.md)

## Examples

``` r
# \donttest{
rba_string_interaction_partners(ids = c("9606.ENSP00000269305",
    "9606.ENSP00000398698",
    "9606.ENSP00000275493"),
    network_type = "functional")
# }
# \donttest{
    rba_string_interaction_partners(ids = "9606.ENSP00000269305",
    species = 9606,
    required_score = 700)
# }
# \donttest{
## Limit partners per input protein; directed results can contain more rows
rba_string_interaction_partners(
    ids = "TP53",
    species = 9606,
    network_type = "regulatory",
    limit = 5
)
# }
```
