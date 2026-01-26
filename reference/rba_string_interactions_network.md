# Get STRING Network Interactions

This function will retrieve Sting interaction pairs among your input
protein ids, with the combined score and separate score for each STRING
score channels. You can further expand your network to a defined size by
providing "add_node" parameter.

## Usage

``` r
rba_string_interactions_network(
  ids,
  species = NULL,
  required_score = NULL,
  add_nodes = NULL,
  network_type = "functional",
  use_query_labels = FALSE,
  ...
)
```

## Arguments

- ids:

  Your protein IDs. It is strongly recommended to supply STRING IDs. See
  [`rba_string_map_ids`](https://rbioapi.moosa-r.com/reference/rba_string_map_ids.md)
  for more information.

- species:

  Numeric: NCBI Taxonomy identifier; Human Taxonomy ID is 9606.
  (Recommended, but optional if your input is less than 100 IDs.)

- required_score:

  Numeric: A minimum of interaction score for an interaction to be
  included in the image. if not supplied, the threshold will be applied
  by STRING Based in the network. (low Confidence = 150, Medium
  Confidence = 400, High Confidence = 700, Highest confidence = 900)

- add_nodes:

  Numeric: Number of neighboring proteins to be added to the network. If
  none supplied by the user, this argument value will depend on the
  number of supplied "ids" argument:

  1.  Single id: add_node will be set to 10 to retrieve the interaction
      neighborhood of you input protein.

  2.  Multiple ids: add_node will be set to 0, thus the output will be
      the interactions between your input proteins.

- network_type:

  should be one of:

  - "functional": (default) The edge's indicate both physical and
    functional associations.

  - "physical": The edges indicate that two proteins have a physical
    interaction or are parts of a complex.

- use_query_labels:

  Logical: (Default = FALSE) Use the names supplied with the 'ids'
  argument as the nodes labels instead of STRING's default ones.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A data frame which each row is a network interaction and the columns
contains interactor information and interaction scores:

- stringId_A: STRING identifier (protein A)

- stringId_B:STRING identifier (protein B)

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

## Details

Note that this function will return interactions between your set of
supplied proteins, or at most, expand the interaction network by the
given parameters. TO retrieve a list of all possible interacting
proteins with your given input, see
[`rba_string_interaction_partners`](https://rbioapi.moosa-r.com/reference/rba_string_interaction_partners.md).

## Corresponding API Resources

"POST https://string-db.org/api/{output-format}/network?identifiers=
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

[`rba_string_map_ids`](https://rbioapi.moosa-r.com/reference/rba_string_map_ids.md)`, `[`rba_string_interaction_partners`](https://rbioapi.moosa-r.com/reference/rba_string_interaction_partners.md)

Other "STRING":
[`rba_string_annotations()`](https://rbioapi.moosa-r.com/reference/rba_string_annotations.md),
[`rba_string_enrichment()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment.md),
[`rba_string_enrichment_image()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_image.md),
[`rba_string_enrichment_ppi()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_ppi.md),
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
```
