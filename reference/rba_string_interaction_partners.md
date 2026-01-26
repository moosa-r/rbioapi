# Get All STRING Interaction Partners

This function will retrieve all the STRING interactions which include
your proteins as one party of the interaction. (e.g. interaction between
your proteins and every other STRING proteins.)  
Given the size of STRING database, this function could return a very
long results. See "Arguments" section for information on how to filter
the interactions.

## Usage

``` r
rba_string_interaction_partners(
  ids,
  species = NULL,
  required_score = NULL,
  network_type = "functional",
  limit = NULL,
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

- required_score:

  Numeric: A minimum of interaction score for an interaction to be
  included in the image. if not supplied, the threshold will be applied
  by STRING Based in the network. (low Confidence = 150, Medium
  Confidence = 400, High Confidence = 700, Highest confidence = 900)

- network_type:

  should be one of:

  - "functional": (default) The edge's indicate both physical and
    functional associations.

- limit:

  Limit the number returned interaction partners per each of your input
  proteins. (e.g. Number of the most confident interaction partner to
  return per each input protein.)

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A data frame which each row is a network interaction and the columns
contains interactor information and interaction scores.

## Details

Note that this function will retrieve the interactions between your
input proteins and every other STRING proteins. To retrieve the
interaction among your input protein-set, see
[`rba_string_interactions_network`](https://rbioapi.moosa-r.com/reference/rba_string_interactions_network.md).

## Corresponding API Resources

"POST https://string-db.org/api/{output-format}/
interaction_partners?identifiers={your_identifiers}&{optional_parameters}"

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

[`rba_string_map_ids`](https://rbioapi.moosa-r.com/reference/rba_string_map_ids.md)`, `[`rba_string_interactions_network`](https://rbioapi.moosa-r.com/reference/rba_string_interactions_network.md)

Other "STRING":
[`rba_string_annotations()`](https://rbioapi.moosa-r.com/reference/rba_string_annotations.md),
[`rba_string_enrichment()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment.md),
[`rba_string_enrichment_image()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_image.md),
[`rba_string_enrichment_ppi()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_ppi.md),
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
```
