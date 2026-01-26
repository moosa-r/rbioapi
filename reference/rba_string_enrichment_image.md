# Get STRING Enrichment Plot

In addition to performing enrichment analysis, STRING allows you to also
visualize the analysis results. Use
[`rba_string_enrichment`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment.md)
to retrieve the analysis results as a data frame.

## Usage

``` r
rba_string_enrichment_image(
  ids,
  species,
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
  for more information. Note that if only one id is supplied, STRING
  expands the network by 10 proteins.

- species:

  Numeric: NCBI Taxonomy identifier; Human Taxonomy ID is 9606.

- category:

  The terms set to use to perform enrichment analysis. valid values are
  (See details for more info): "Process" (default), "Function",
  "Component", "Keyword", "KEGG", "RCTM", "HPO", "MPO", "DPO", "WPO",
  "ZPO", "FYPO", "Pfam", "SMART", "InterPro", "PMID",
  "NetworkNeighborAL", "COMPARTMENTS", "TISSUES", "DISEASES", or
  "WikiPathways"

- image_format:

  one of:

  - "image": PNG image with normal resolution.

  - "highres_image": High-resolution PNG image.

  - "svg": Scalable Vector Graphics image.

- save_image:

  Logical or Character:

  - TRUE: Save the image to an automatically-generated path.

  - FALSE: Do not save the image, just return it as an R object.

  - Character string: A valid file path to save the image to.

- group_by_similarity:

  Jackard index treshold to visually group the related terms. Valid
  values are between 0.1 to 1 with increment of 0.1. Default value is
  NULL (i.e. no grouping).

- color_palette:

  Color pallet to code FDR values. Valid values are: "mint_blue"
  (default), "lime_emerald", "green_blue", "peach_purple", "straw_navy",
  or "yellow_pink"

- number_of_term_shown:

  (default: 10) Maximum number of results to include in the plot.

- x_axis:

  The variable to show on the x axis and rank the results based on it.
  Valid values are: "signal" (default), "strength", "FDR", or
  "gene_count"

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A plot summarizing the enrichment results, which can be PNG or SVG
depending on the inputs.

## Details

Available values for category are as follow. Default value is "Process".

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

"POST https://string-db.org/api/{output_format}/enrichmentfigure"

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

[`rba_string_map_ids`](https://rbioapi.moosa-r.com/reference/rba_string_map_ids.md)`, `[`rba_string_enrichment`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment.md)`, `[`rba_string_annotations`](https://rbioapi.moosa-r.com/reference/rba_string_annotations.md)` `

Other "STRING":
[`rba_string_annotations()`](https://rbioapi.moosa-r.com/reference/rba_string_annotations.md),
[`rba_string_enrichment()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment.md),
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
