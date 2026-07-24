# Get STRING Enrichment Plot

Retrieve a plot that summarizes STRING functional-enrichment results.
Use
[`rba_string_enrichment`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment.md)
to retrieve the results as a data frame.

## Usage

``` r
rba_string_enrichment_image(
  ids,
  species = NULL,
  category = "Process",
  image_format = "image",
  save_image = TRUE,
  group_by_similarity = NULL,
  color_palette = "mint_blue",
  number_of_term_shown = 10,
  x_axis = "signal",
  ...
)
```

## Arguments

- ids:

  Your protein ID(s). It is strongly recommended to supply STRING IDs.
  See
  [`rba_string_map_ids`](https://rbioapi.moosa-r.com/reference/rba_string_map_ids.md)
  for more information. If only one ID is supplied, STRING expands the
  network by 10 proteins.

- species:

  Numeric: [NCBI Taxonomy
  identifier](https://www.ncbi.nlm.nih.gov/taxonomy/); Human Taxonomy ID
  is 9606. (Recommended, but required if your input contains more than
  10 unique IDs.)

- category:

  Character: The term set to use for enrichment analysis. Valid values
  are: "Process" (default), "Function", "Component", "Keyword", "KEGG",
  "RCTM", "HPO", "MPO", "DPO", "WPO", "ZPO", "FYPO", "Pfam", "SMART",
  "InterPro", "PMID", "NetworkNeighborAL", "COMPARTMENTS", "TISSUES",
  "DISEASES", or "WikiPathways". See Details for descriptions.

- image_format:

  Character: One of:

  - "image": PNG image with normal resolution.

  - "highres_image": High-resolution PNG image.

  - "svg": Scalable Vector Graphics image.

- save_image:

  Logical or Character:

  - TRUE: Save the image to an automatically-generated path.

  - FALSE: Do not save the image, just return it as an R object.

  - Character string: A valid file path to save the image to.

- group_by_similarity:

  Numeric: Jaccard-index threshold used to group related terms visually.
  Valid values range from 0.1 to 1 in increments of 0.1. The default is
  `NULL`, which disables grouping.

- color_palette:

  Character: Color palette used to represent FDR values. Valid values
  are "mint_blue" (default), "lime_emerald", "green_blue",
  "peach_purple", "straw_navy", or "yellow_pink".

- number_of_term_shown:

  Numeric: (default = 10) Maximum number of terms to include in the
  plot.

- x_axis:

  Character: Variable displayed on the x-axis and used to rank the
  results. Valid values are "signal" (default), "strength", "FDR", and
  "gene_count".

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A PNG image array or raw SVG content, depending on `image_format`.

## Details

Available `category` values are listed below. The default is "Process".

- Process: Biological Process (Gene Ontology)

- Function: Molecular Function (Gene Ontology)

- Component: Cellular Component (Gene Ontology)

- Keyword: Annotated Keywords (UniProt)

- KEGG: KEGG Pathways

- RCTM: Reactome Pathways

- HPO: Human Phenotype (Monarch)

- MPO: The Mammalian Phenotype Ontology (Monarch)

- DPO: Drosophila Phenotype (Monarch)

- WPO: C. elegans Phenotype Ontology (Monarch)

- ZPO: Zebrafish Phenotype Ontology (Monarch)

- FYPO: Fission Yeast Phenotype Ontology (Monarch)

- Pfam: Protein Domains (Pfam)

- SMART: Protein Domains (SMART)

- InterPro: Protein Domains and Features (InterPro)

- PMID: Reference Publications (PubMed)

- NetworkNeighborAL: Local Network Cluster (STRING)

- COMPARTMENTS: Subcellular Localization (COMPARTMENTS)

- TISSUES: Tissue Expression (TISSUES)

- DISEASES: Disease-gene Associations (DISEASES)

- WikiPathways: WikiPathways

## Corresponding API Resources

"POST https://string-db.org/api/{output-format}/enrichmentfigure?
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

` `[`rba_string_map_ids`](https://rbioapi.moosa-r.com/reference/rba_string_map_ids.md)`, `[`rba_string_enrichment`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment.md)`, `[`rba_string_annotations`](https://rbioapi.moosa-r.com/reference/rba_string_annotations.md)`, `[`rba_string_functional_terms`](https://rbioapi.moosa-r.com/reference/rba_string_functional_terms.md)` `

Other "STRING":
[`rba_string_annotations()`](https://rbioapi.moosa-r.com/reference/rba_string_annotations.md),
[`rba_string_enrichment()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment.md),
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
[`rba_string_enrichment()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  rba_string_enrichment_image(
  ids = c("TP53", "TNF", "EGFR"),
  species = 9606,
  category = "KEGG"
  )
} # }
if (FALSE) { # \dontrun{
  rba_string_enrichment_image(
  ids = c("TP53", "TNF", "EGFR"),
  species = 9606,
  x_axis = "strength",
  number_of_term_shown = 20
  )
} # }
if (FALSE) { # \dontrun{
  rba_string_enrichment_image(
  ids = c("TP53", "TNF", "EGFR"),
  species = 9606,
  color_palette = "straw_navy"
  )
} # }
```
