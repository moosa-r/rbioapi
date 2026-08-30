# Get STRING Network Image

This function retrieves a static image of the interaction network among
your input proteins and, where applicable, additional interactors. The
available arguments control the network contents and appearance.

## Usage

``` r
rba_string_network_image(
  ids,
  image_format = "image",
  save_image = TRUE,
  species = NULL,
  add_color_nodes = NULL,
  add_white_nodes = NULL,
  required_score = NULL,
  network_flavor = "evidence",
  network_type = "functional",
  hide_node_labels = FALSE,
  use_query_labels = FALSE,
  hide_disconnected_nodes = FALSE,
  hide_structure_pics = FALSE,
  flat_nodes = FALSE,
  node_labels_center = FALSE,
  node_labels_font_size = 12,
  network_term_id = NULL,
  ...
)
```

## Arguments

- ids:

  Character or Numeric: Your protein ID(s). It is strongly recommended
  to supply STRING IDs. See
  [`rba_string_map_ids`](https://rbioapi.moosa-r.com/reference/rba_string_map_ids.md)
  for more information.  
  Alternatively, you can retrieve the network of proteins annotated with
  a STRING functional term by setting `ids = NULL` and supplying
  `network_term_id`.

- image_format:

  Character: (default = `"image"`) One of:

  - "image": PNG image with normal resolution.

  - "highres_image": High-resolution PNG image.

  - "svg": Scalable Vector Graphics image.

- save_image:

  Logical or Character: (default = `TRUE`)

  - TRUE: Save the image to an automatically-generated path.

  - FALSE: Do not save the image, just return it as an R object.

  - Character string: A valid file path to save the image to.

- species:

  Numeric: (optional) [NCBI Taxonomy
  identifier](https://www.ncbi.nlm.nih.gov/taxonomy/); Human Taxonomy ID
  is 9606. Required when using `network_term_id`; otherwise recommended,
  but required if your input contains more than 10 unique IDs.

- add_color_nodes:

  Numeric: (optional) The number of colored nodes (queried proteins and
  first shell of interactors) to be added.

- add_white_nodes:

  Numeric: (optional) The number of white nodes (second shell of
  interactors) to be added after colored nodes.

- required_score:

  Numeric: (optional) (between 0 and 1000): Minimum interaction score
  required for an interaction to be included in the image. If omitted,
  STRING applies a network-dependent threshold. Common confidence
  thresholds are 150 (low), 400 (medium), 700 (high), and 900 (highest).

- network_flavor:

  Character: (default = `"evidence"`) The network-edge style. One of:

  - "evidence": (default) Edge colors indicate the types of evidence
    supporting each interaction.

  - "confidence": Edge thickness indicates the interaction confidence
    score.

  - "actions": Edge shape indicates the predicted mode of action.

- network_type:

  Character: (default = `"functional"`) One of:

  - "functional": (default) Edges indicate both physical and functional
    associations.

  - "physical": Edges indicate that two proteins have a physical
    interaction or are parts of a complex.

- hide_node_labels:

  Logical: (default = `FALSE`) Hide protein names from the image.

- use_query_labels:

  Logical: (default = `FALSE`) Use the names supplied in `ids` as node
  labels instead of STRING's default labels.

- hide_disconnected_nodes:

  Logical: (default = `FALSE`) Hide proteins that are not connected to
  any other protein.

- hide_structure_pics:

  Logical: (default = `FALSE`) Hide protein structure images inside the
  nodes.

- flat_nodes:

  Logical: (default = `FALSE`) Use a flat node design instead of the
  default 3D design.

- node_labels_center:

  Logical: (default = `FALSE`) Center protein labels on the nodes.

- node_labels_font_size:

  Numeric: (default = `12`) (between 5 and 50; default = 12): Font size
  of the protein node labels.

- network_term_id:

  Character: (optional) A functional term identifier (e.g. a Gene
  Ontology, KEGG, or Reactome identifier). Instead of using proteins
  supplied through `ids`, STRING constructs the network from proteins
  annotated with the specified term. Set `ids = NULL` and supply
  `species`.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A PNG image array or raw SVG content, depending on `image_format`.

## Corresponding API Resources

"POST https://string-db.org/api/{output-format}/network?identifiers=
{your_identifiers}&{optional_parameters}"  
"POST https://string-db.org/api/{output-format}/network?network_term_id=
{your_term}&{optional_parameters}"

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

` `[`rba_string_map_ids`](https://rbioapi.moosa-r.com/reference/rba_string_map_ids.md)`, `[`rba_string_interactions_network`](https://rbioapi.moosa-r.com/reference/rba_string_interactions_network.md)`, `[`rba_string_interaction_partners`](https://rbioapi.moosa-r.com/reference/rba_string_interaction_partners.md)`, `[`rba_string_enrichment_ppi`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_ppi.md)` `

Other "STRING":
[`rba_string_annotations()`](https://rbioapi.moosa-r.com/reference/rba_string_annotations.md),
[`rba_string_enrichment()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment.md),
[`rba_string_enrichment_image()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_image.md),
[`rba_string_enrichment_ppi()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_ppi.md),
[`rba_string_functional_terms()`](https://rbioapi.moosa-r.com/reference/rba_string_functional_terms.md),
[`rba_string_homology_inter()`](https://rbioapi.moosa-r.com/reference/rba_string_homology_inter.md),
[`rba_string_homology_intra()`](https://rbioapi.moosa-r.com/reference/rba_string_homology_intra.md),
[`rba_string_interaction_partners()`](https://rbioapi.moosa-r.com/reference/rba_string_interaction_partners.md),
[`rba_string_interactions_network()`](https://rbioapi.moosa-r.com/reference/rba_string_interactions_network.md),
[`rba_string_map_ids()`](https://rbioapi.moosa-r.com/reference/rba_string_map_ids.md),
[`rba_string_version()`](https://rbioapi.moosa-r.com/reference/rba_string_version.md)

## Examples

``` r
if (FALSE) { # \dontrun{
rba_string_network_image(ids = c("9606.ENSP00000269305",
    "9606.ENSP00000398698",
    "9606.ENSP00000275493"),
    network_type = "functional",
    save_image = FALSE)
} # }
if (FALSE) { # \dontrun{
rba_string_network_image(ids = c("TP53", "TNF", "EGFR"),
    species = 9606,
    save_image = TRUE)
} # }
if (FALSE) { # \dontrun{
rba_string_network_image(ids = "9606.ENSP00000269305",
    image_format = "highres_image",
    save_image = file.path(getwd(), "TP53_network.png"))
} # }
if (FALSE) { # \dontrun{
rba_string_network_image(
    ids = NULL,
    network_term_id = "GO:0050852",
    species = 9606,
    save_image = FALSE
)
} # }
```
