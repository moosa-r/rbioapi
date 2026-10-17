# Get STRING Network Interactions

This function retrieves STRING interaction pairs among the input
proteins and any added neighbors, including the combined score and
separate scores for each evidence channel.

## Usage

``` r
rba_string_interactions_network(
  ids,
  species = NULL,
  required_score = NULL,
  add_nodes = NULL,
  network_type = "functional",
  use_query_labels = FALSE,
  network_term_id = NULL,
  network_flavor = NULL,
  typed_physical_edges = TRUE,
  typed_regulatory_edges = TRUE,
  ...
)
```

## Arguments

- ids:

  Character or Numeric: Your protein IDs. It is strongly recommended to
  supply STRING IDs. See
  [`rba_string_map_ids`](https://rbioapi.moosa-r.com/reference/rba_string_map_ids.md)
  for more information.  
  Alternatively, you can retrieve interactions among proteins annotated
  with a STRING functional term by setting `ids = NULL` and supplying
  `network_term_id`.

- species:

  Numeric: (optional) [NCBI Taxonomy
  identifier](https://www.ncbi.nlm.nih.gov/taxonomy/); Human Taxonomy ID
  is 9606. Required when using `network_term_id`; otherwise recommended,
  but required if your input contains more than 10 unique IDs.

- required_score:

  Numeric: (optional) (between 0 and 1000): Minimum interaction score
  required for an interaction to be included in the returned network. If
  omitted, STRING applies a network-dependent threshold. Common
  confidence thresholds are 150 (low), 400 (medium), 700 (high), and 900
  (highest).

- add_nodes:

  Numeric: (optional) Number of neighboring proteins to add to the
  network. For identifier-based requests, if omitted, STRING determines
  the value from the number of input IDs:

  1.  One ID: STRING adds 10 proteins to retrieve its interaction
      neighborhood.

  2.  Multiple IDs: STRING adds no proteins, so only interactions among
      the input proteins are returned.

- network_type:

  Character: (default = `"functional"`) One of:

  - "functional": (default) Edges indicate both physical and functional
    associations.

  - "physical": Edges indicate that two proteins have a physical
    interaction or are parts of a complex.

  - "regulatory": Directed edges indicate regulation from a source
    protein to a target protein.

- use_query_labels:

  Logical: (default = `FALSE`) Use the names supplied in `ids` as node
  labels instead of STRING's default labels.

- network_term_id:

  Character: (optional) A functional term identifier (e.g. a Gene
  Ontology, KEGG, or Reactome identifier). Instead of using proteins
  supplied through `ids`, STRING constructs the network from proteins
  annotated with the specified term. Set `ids = NULL` and supply
  `species`.

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
contain interactor information and interaction scores:

- stringId_A: STRING identifier (protein A)

- stringId_B: STRING identifier (protein B)

- preferredName_A: common protein name (protein A)

- preferredName_B: common protein name (protein B)

- ncbiTaxonId: NCBI taxon identifier

- score: combined score

- nscore: gene neighborhood score

- fscore: gene fusion score

- pscore: phylogenetic profile score

- ascore: co-expression score

- escore: experimental score

- dscore: database score

- tscore: textmining score

  
A `network_type = "regulatory"` response instead contains
`source_string_id`, `target_string_id`, `source_preferred_name`,
`target_preferred_name`, `ncbiTaxonId`, `experimental_score`,
`database_score`, `textmining_score`, `sign`, and `combined_score`. Each
row reports one direction from source to target; scores apply to that
direction.  
A `network_flavor = "typed"` response contains the ordinary identifier
columns, evidence scores prefixed with `functional_`, `physical_`, and
`regulatory_`, and `sign`. The combined-score columns are
`functional_combined_score`, `physical_combined_score`, and
`regulatory_combined_score`. Each functional pair is returned in both
orientations. Physical scores are symmetric; regulatory scores and signs
describe the direction from `stringId_A` to `stringId_B`. Missing or
below-threshold relationships have numeric `NA` scores. Disabled
overlays have `NA` score columns.  
Scores range from 0 to 1; the request's `required_score` uses the 0 to
1000 scale. Regulatory signs are `"pos"`, `"neg"`, or `"n/a"`.

## Details

To retrieve interactions between your input proteins and all their
STRING interaction partners, see
[`rba_string_interaction_partners`](https://rbioapi.moosa-r.com/reference/rba_string_interaction_partners.md).

## Corresponding API Resources

"POST https://string-db.org/api/{output-format}/network?identifiers=
{your_identifiers}&{optional_parameters}"  
"POST https://string-db.org/api/{output-format}/network?network_term_id=
{your_term}&{optional_parameters}"

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

` `[`rba_string_map_ids`](https://rbioapi.moosa-r.com/reference/rba_string_map_ids.md)`, `[`rba_string_interaction_partners`](https://rbioapi.moosa-r.com/reference/rba_string_interaction_partners.md)`, `[`rba_string_network_image`](https://rbioapi.moosa-r.com/reference/rba_string_network_image.md)`, `[`rba_string_enrichment_ppi`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_ppi.md)` `

Other "STRING":
[`rba_string_annotations()`](https://rbioapi.moosa-r.com/reference/rba_string_annotations.md),
[`rba_string_enrichment()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment.md),
[`rba_string_enrichment_image()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_image.md),
[`rba_string_enrichment_ppi()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_ppi.md),
[`rba_string_functional_terms()`](https://rbioapi.moosa-r.com/reference/rba_string_functional_terms.md),
[`rba_string_homology_inter()`](https://rbioapi.moosa-r.com/reference/rba_string_homology_inter.md),
[`rba_string_homology_intra()`](https://rbioapi.moosa-r.com/reference/rba_string_homology_intra.md),
[`rba_string_interaction_partners()`](https://rbioapi.moosa-r.com/reference/rba_string_interaction_partners.md),
[`rba_string_map_ids()`](https://rbioapi.moosa-r.com/reference/rba_string_map_ids.md),
[`rba_string_network_image()`](https://rbioapi.moosa-r.com/reference/rba_string_network_image.md),
[`rba_string_version()`](https://rbioapi.moosa-r.com/reference/rba_string_version.md)

## Examples

``` r
# \donttest{
rba_string_interactions_network(ids = c("9606.ENSP00000269305",
    "9606.ENSP00000398698",
    "9606.ENSP00000275493"),
    network_type = "functional")
# }
# \donttest{
rba_string_interactions_network(ids = c("9606.ENSP00000269305",
    "9606.ENSP00000398698",
    "9606.ENSP00000275493"),
    species = 9606,
    add_nodes = 10)
# }
# \donttest{
rba_string_interactions_network(
    ids = NULL,
    network_term_id = "GO:0050852",
    species = 9606,
    required_score = 900
)
# }
# \donttest{
## Retrieve directed regulatory interactions
rba_string_interactions_network(
    ids = c("TP53", "MDM2", "CDKN1A"),
    species = 9606,
    network_type = "regulatory"
)
# }
# \donttest{
## Retrieve typed scores without the physical overlay
rba_string_interactions_network(
    ids = c("TP53", "MDM2", "CDKN1A"),
    species = 9606,
    network_flavor = "typed",
    typed_physical_edges = FALSE
)
# }
```
