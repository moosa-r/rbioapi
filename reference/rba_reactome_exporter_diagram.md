# Get a Reactome Event Diagram

This function could be called in two scenarios:

1.  With create_document = FALSE: To retrieve an image of that event's
    Diagram.

2.  With create_document = TRUE: To retrieve a PDF document with the
    event's diagram image and additional information.

see "Details section" for more information

## Usage

``` r
rba_reactome_exporter_diagram(
  event_id,
  save_to = NULL,
  create_document = FALSE,
  resource = "TOTAL",
  diagram_profile = "Modern",
  analysis_profile = "Standard",
  token = NULL,
  exp_column = NULL,
  document_level = 1,
  output_format = "png",
  image_quality = 5,
  flag_element = NULL,
  flg_interactors = TRUE,
  sel = NULL,
  title = TRUE,
  margin = 15,
  ehld = TRUE,
  ...
)
```

## Arguments

- event_id:

  Character: Reactome event's identifier.

- save_to:

  NULL or Character: (default = `NULL`)

  - NULL: Save the file to an automatically-generated path.

  - Character string: A valid file path to save the file to.

- create_document:

  Logical: (default = `FALSE`) Create PDF document instead of image?

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

- document_level:

  Numeric: (default = `1`) (Only if "create_document" is TRUE) use 0 to
  exclude the event's children or 1 to include them.

- output_format:

  Character: (default = `"png"`) (Only if "create_document" is FALSE)
  Image format of the saved diagram. Can be one of: png, jpg, jpeg, svg
  or gif.

- image_quality:

  Numeric: (default = `5`) (Only if "create_document" is FALSE), a
  number ranging from 1 to 10. 1 is the lowest quality and 10 is the
  highest.

- flag_element:

  Character: (optional) (Only if "create_document" is FALSE) gene name,
  protein ID, chemical ID or Reactome ID of a diagram's element to be
  flagged.

- flg_interactors:

  Logical: (default = `TRUE`) (Only if "create_document" is FALSE)
  Should the interactor be considered when flagging a diagram element?

- sel:

  Character vector: (optional) (Only if "create_document" is FALSE) CSV
  line for highlighting element(s) selection in the diagram.

- title:

  Logical: (default = `TRUE`) (Only if "create_document" is FALSE)
  Should the pathway name be displayed below the image?

- margin:

  Numeric: (default = `15`) (Only if "create_document" is FALSE) A
  number ranging from 0 to 20 to set as the image's margin.

- ehld:

  Logical: (default = `TRUE`) (Only if "create_document" is FALSE)
  Should Enhanced High Level Diagrams be considered?

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

NULL, Based to the inputs, an image or PDF file will be saved to disk.

## Details

If the function is called with create_document = FALSE:  
The result will be an image with the format supplied in "output_format"
argument. If the supplied event ID refers to a pathway, the image's
content will be the that pathways diagram. If the supplied event ID
refers to a sub-pathway or reaction event, the parent pathway's diagram
will be exported, with that reaction or sub-pathway's events
highlighted.  
Note that to export an image of reaction-like event separately, you
should use
[`rba_reactome_exporter_reaction`](https://rbioapi.moosa-r.com/reference/rba_reactome_exporter_reaction.md).  
If the function is called with create_document = TRUE:  
A PDF document will contain an image of the event's diagram and the
following information of that events: Summation, Literature references,
Edit history type, location, compartments and diseases. note that if you
call the function with "document level = 1", information of your
supplied event's children will also be included.

## Corresponding API Resources

"GET https://reactome.org/ContentService/exporter/diagram/{identifier}
.{ext}"  
"GET https://reactome.org/ContentService/exporter/document/event/
{identifier}.pdf"

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

[`rba_reactome_exporter_reaction`](https://rbioapi.moosa-r.com/reference/rba_reactome_exporter_reaction.md)
[`rba_reactome_analysis`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis.md)

Other "Reactome Content Service - Format Exporter":
[`rba_reactome_exporter_event()`](https://rbioapi.moosa-r.com/reference/rba_reactome_exporter_event.md),
[`rba_reactome_exporter_overview()`](https://rbioapi.moosa-r.com/reference/rba_reactome_exporter_overview.md),
[`rba_reactome_exporter_reaction()`](https://rbioapi.moosa-r.com/reference/rba_reactome_exporter_reaction.md)

## Examples

``` r
if (FALSE) { # \dontrun{
rba_reactome_exporter_diagram(event_id = "R-HSA-177929",
  create_document = FALSE)
} # }
if (FALSE) { # \dontrun{
rba_reactome_exporter_diagram(event_id = "R-HSA-6787403",
    create_document = FALSE)
} # }
if (FALSE) { # \dontrun{
rba_reactome_exporter_diagram(event_id = "R-HSA-177929",
    create_document = TRUE)
} # }
if (FALSE) { # \dontrun{
rba_reactome_exporter_diagram(event_id = "R-HSA-177929",
    output_format = "svg",
    save_to = "reactome_event_diagram.svg")
} # }
```
