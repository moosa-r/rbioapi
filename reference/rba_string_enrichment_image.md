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
  sort_by = NULL,
  graph_type = "dotplot",
  max_fdr = NULL,
  min_gene_count = NULL,
  min_strength = NULL,
  min_signal = NULL,
  merge_similarity = NULL,
  ...
)
```

## Arguments

- ids:

  Character or Numeric: Your protein ID(s). It is strongly recommended
  to supply STRING IDs. See
  [`rba_string_map_ids`](https://rbioapi.moosa-r.com/reference/rba_string_map_ids.md)
  for more information. If only one ID is supplied, STRING expands the
  network by 10 proteins.

- species:

  Numeric: (optional) [NCBI Taxonomy
  identifier](https://www.ncbi.nlm.nih.gov/taxonomy/); Human Taxonomy ID
  is 9606. (Recommended, but required if your input contains more than
  10 unique IDs.)

- category:

  Character: (default = `"Process"`) The term set to use for enrichment
  analysis. Valid values are:

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

  - GWAS: GWAS Catalog

  - Hallmark: Hallmark Gene Set (Molecular Signatures DB)

  - Pfam: Protein Domains (Pfam)

  - SMART: Protein Domains (SMART)

  - InterPro: Protein Domains and Features (InterPro)

  - PMID: Reference Publications (PubMed)

  - NetworkNeighborAL: Local Network Cluster (STRING)

  - COMPARTMENTS: Subcellular Localization (COMPARTMENTS)

  - TISSUES: Tissue Expression (TISSUES)

  - DISEASES: Disease-gene Associations (DISEASES)

  - WikiPathways: WikiPathways

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

- group_by_similarity:

  Numeric: (optional) Jaccard-index threshold used to group related
  terms visually. Valid values range from 0.1 to 1 in increments of 0.1.
  Omit to use STRING's default threshold of 0.8. This controls visual
  grouping; `merge_similarity` controls which terms are retained.

- color_palette:

  Character: (default = `"mint_blue"`) Color palette used to represent
  FDR values. Valid values are "mint_blue" (default), "lime_emerald",
  "green_blue", "peach_purple", "straw_navy", "yellow_pink", or
  "red_blue".

- number_of_term_shown:

  Numeric: (default = `10`) Maximum number of terms to include in the
  plot, from 1 to 100.

- x_axis:

  Character: (default = `"signal"`) Variable displayed on the x-axis.
  Valid values are `"signal"`, `"strength"`, `"fdr"`, `"count"`, or
  `"gene_count"`. `"FDR"` is also accepted for `"fdr"`.

- sort_by:

  Character: (optional) Variable used to select and order terms. Valid
  values are `"signal"`, `"strength"`, `"fdr"`, `"count"`, or
  `"gene_count"`. `"FDR"` is also accepted for `"fdr"`. Omit to sort by
  the selected axis.

- graph_type:

  Character: (default = `"dotplot"`) Either `"dotplot"` or `"barplot"`.

- max_fdr:

  Numeric: (optional) Maximum FDR, greater than 0 and no greater than 1.
  Only terms meeting this threshold are shown.

- min_gene_count:

  Numeric: (optional) Minimum positive whole number of input genes
  assigned to a displayed term.

- min_strength:

  Numeric: (optional) Minimum enrichment strength.

- min_signal:

  Numeric: (optional) Minimum enrichment signal.

- merge_similarity:

  Numeric: (optional) Overlap threshold for merging terms based on their
  assigned input-gene sets, greater than 0 and no greater than 1. The
  most significant term is retained. A value of 1 merges only identical
  gene sets. Omit to retain unmerged terms.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A PNG image array or raw SVG content, depending on `image_format`.
STRING can return empty content if no terms can be plotted. An empty PNG
response is returned as a numeric array with dimensions `c(0L, 0L, 4L)`;
an empty SVG response is returned as `raw(0)`. Both have length zero.

## Corresponding API Resources

"POST https://string-db.org/api/{output-format}/enrichmentfigure?
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
if (FALSE) { # \dontrun{
  ## Display strength on the axis and select/order terms by FDR
  rba_string_enrichment_image(
  ids = c("TP53", "MDM2", "CDKN1A", "BRCA1", "CDK1", "CDK2"),
  species = 9606,
  category = "Hallmark",
  x_axis = "strength",
  sort_by = "fdr",
  graph_type = "barplot",
  color_palette = "red_blue",
  save_image = FALSE
  )
} # }
```
