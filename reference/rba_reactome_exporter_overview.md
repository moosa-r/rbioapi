# Get a Reactome Pathway Overview

This function will save a pathway overview of the supplied species as an
image file.

## Usage

``` r
rba_reactome_exporter_overview(
  species,
  output_format = "png",
  save_to = NULL,
  image_quality = 5,
  flag_element = NULL,
  flg_interactors = TRUE,
  sel = NULL,
  title = TRUE,
  margin = 15,
  diagram_profile = "Copper",
  token = NULL,
  resource = "TOTAL",
  exp_column = NULL,
  coverage = FALSE,
  ...
)
```

## Arguments

- species:

  Character or Numeric: NCBI Taxonomy identifier (Human Taxonomy ID is
  9606.) or species name (e.g. "Homo sapiens"). See
  [`rba_reactome_species`](https://rbioapi.moosa-r.com/reference/rba_reactome_species.md)
  or [Reactome Data Schema: Entries:
  Species](https://reactome.org/content/schema/objects/Species/).

- output_format:

  Character: (default = `"png"`) Image format, can be one of: png, jpg,
  jpeg, svg or gif.

- save_to:

  Character: (optional)

  - NULL: Save the file to an automatically-generated path.

  - Character string: A valid file path to save the file to.

- image_quality:

  Numeric: (default = `5`) A number ranging from 1 to 10. 1 is the
  lowest quality and 10 is the highest.

- flag_element:

  Character: (optional) Gene name, protein ID, chemical ID or Reactome
  ID of a diagram's element to be flagged.

- flg_interactors:

  Logical: (default = `TRUE`) Should the interactor be considered when
  flagging a diagram element?

- sel:

  Character: (optional) CSV line for highlighting element(s) selection
  in the diagram.

- title:

  Logical: (default = `TRUE`) Should the pathway name be displayed below
  the image?

- margin:

  Numeric: (default = `15`) A number ranging from 0 to 20 to set as the
  image's margin.

- diagram_profile:

  Character: (default = `"Copper"`) Color profile of the overview,
  should be one of "Copper", "Copper plus", "Barium Lithium" or "Calcium
  Salts".

- token:

  Character: (optional) The analysis Token for which the results will be
  overlaid on top of the given pathways overview. see:
  [`rba_reactome_analysis`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis.md).

- resource:

  Character: (default = `"TOTAL"`) The analysis resource for which the
  results will be overlaid on top of the given pathways overview.

- exp_column:

  Numeric: (optional) (only if token is supplied) Specify the expression
  column for the overlay.

- coverage:

  Logical: (default = `FALSE`) Should the analysis coverage values be
  overlaid?

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

NULL, Based to the inputs, an image file will be saved to disk.

## Corresponding API Resources

"GET
https://reactome.org/ContentService/exporter/fireworks/{species}.{ext}"

## References

- Ragueneau, E., Gong, C., Sinquin, P., Sevilla, C., Beavers, D.,
  Grentner, A., ... D’Eustachio, P. (2026). The Reactome
  Knowledgebase 2026. Nucleic Acids Res., 54(D1), D673–D681. doi:
  10.1093/nar/gkaf1223

- Griss, J., Viteri, G., Sidiropoulos, K., Nguyen, V., Fabregat, A., &
  Hermjakob, H. (2020). ReactomeGSA—Efficient Multi-Omics Comparative
  Pathway Analysis. Molecular & Cellular Proteomics, 19(12), 2115–2125.
  doi: 10.1074/mcp.TIR120.002155

- [Reactome Content Services API
  Documentation](https://reactome.org/ContentService/)

- [Citations note on Reactome website](https://reactome.org/cite/)

## See also

[`rba_reactome_analysis`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis.md)

Other "Reactome Content Service - Format Exporter":
[`rba_reactome_exporter_diagram()`](https://rbioapi.moosa-r.com/reference/rba_reactome_exporter_diagram.md),
[`rba_reactome_exporter_event()`](https://rbioapi.moosa-r.com/reference/rba_reactome_exporter_event.md),
[`rba_reactome_exporter_reaction()`](https://rbioapi.moosa-r.com/reference/rba_reactome_exporter_reaction.md)

## Examples

``` r
if (FALSE) { # \dontrun{
rba_reactome_exporter_overview(species = 9606,
    output_format = "svg",
    save_to = "human_pathways.svg")
} # }
if (FALSE) { # \dontrun{
rba_reactome_exporter_overview(species = 9606,
    token = 123456789)
} # }
```
