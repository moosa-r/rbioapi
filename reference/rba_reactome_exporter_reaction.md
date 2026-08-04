# Get a Reactome Reaction Event

This function will Save a Reactome event of class "ReactionLikeEvent" as
an image file.

## Usage

``` r
rba_reactome_exporter_reaction(
  event_id,
  save_to = NULL,
  output_format = "png",
  resource = "TOTAL",
  diagram_profile = "Modern",
  analysis_profile = "Standard",
  token = NULL,
  exp_column = NULL,
  image_quality = 5,
  flag_element = NULL,
  flg_interactors = TRUE,
  sel = NULL,
  title = TRUE,
  margin = 15,
  ...
)
```

## Arguments

- event_id:

  Character: Reactome [Reaction-like
  event](https://reactome.org/content/schema/ReactionLikeEvent/)'s
  identifier.

- save_to:

  NULL or Character: (default = `NULL`)

  - NULL: Save the file to an automatically-generated path.

  - Character string: A valid file path to save the file to.

- output_format:

  Character: (default = `"png"`) Image format, can be one of: png, jpg,
  jpeg, svg or gif.

- resource:

  Character: (default = `"TOTAL"`) The analysis resource for which the
  results will be overlaid on top of the given pathways overview.

- diagram_profile:

  Character: (default = `"Modern"`) Color profile of diagrams, should be
  either "Modern" or "Standard".

- analysis_profile:

  Character: (default = `"Standard"`) Color profile of analysis, should
  be one of: "Standard", "Strosobar" or "Copper Plus".

- token:

  Character: (optional) The analysis Token for which the results will be
  overlaid on top of the given pathways overview. see:
  [`rba_reactome_analysis`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis.md).

- exp_column:

  Numeric: (optional) (only if token is supplied) Specify the expression
  column for the overlay.

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

  Character vector: (optional) CSV line for highlighting element(s)
  selection in the diagram.

- title:

  Logical: (default = `TRUE`) Should the pathway name be displayed below
  the image?

- margin:

  Numeric: (default = `15`) A number ranging from 0 to 20 to set as the
  image's margin.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

NULL, Based to the inputs, an image file will be saved to disk.

## Details

Note that this function will save Reaction-like event separately and out
of it's parent pathway context. To overlay a Reaction on it's parent
pathway, use
[`rba_reactome_exporter_diagram`](https://rbioapi.moosa-r.com/reference/rba_reactome_exporter_diagram.md).

## Corresponding API Resources

"GET https://reactome.org/ContentService/exporter/reaction/
{identifier}.{ext}"

## References

- Ragueneau, E., Gong, C., Sinquin, P., Sevilla, C., Beavers, D.,
  Grentner, A., ... D’Eustachio, P. (2026). The Reactome
  Knowledgebase 2026. Nucleic Acids Res., 54(D1), D673–D681. doi:
  10.1093/nar/gkaf1223

- Griss J, Viteri G, Sidiropoulos K, Nguyen V, Fabregat A, Hermjakob H.
  ReactomeGSA - Efficient Multi-Omics Comparative Pathway Analysis. Mol
  Cell Proteomics. 2020 Sep 9. doi: 10.1074/mcp. PubMed PMID: 32907876.

- [Reactome Content Services API
  Documentation](https://reactome.org/ContentService/)

- [Citations note on Reactome website](https://reactome.org/cite/)

## See also

[`rba_reactome_exporter_diagram`](https://rbioapi.moosa-r.com/reference/rba_reactome_exporter_diagram.md)
[`rba_reactome_analysis`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis.md)

Other "Reactome Content Service - Format Exporter":
[`rba_reactome_exporter_diagram()`](https://rbioapi.moosa-r.com/reference/rba_reactome_exporter_diagram.md),
[`rba_reactome_exporter_event()`](https://rbioapi.moosa-r.com/reference/rba_reactome_exporter_event.md),
[`rba_reactome_exporter_overview()`](https://rbioapi.moosa-r.com/reference/rba_reactome_exporter_overview.md)

## Examples

``` r
if (FALSE) { # \dontrun{
rba_reactome_exporter_diagram(event_id = "R-HSA-6787403",
    create_document = FALSE)
} # }
if (FALSE) { # \dontrun{
rba_reactome_exporter_diagram(event_id = "R-HSA-6787403",
     output_format = "svg",
     save_to = "reactome_reacion_image.svg")
} # }
```
