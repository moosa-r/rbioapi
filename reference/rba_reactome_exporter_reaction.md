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

  Reactome [Reaction-like
  event](https://reactome.org/content/schema/ReactionLikeEvent/)'s
  identifier.

- save_to:

  NULL or Character:

  - NULL: Save the file to an automatically-generated path.

  - Character string: A valid file path to save the file to.

- output_format:

  Images format, Can be one of: png (default), jpeg, svg or gif.

- resource:

  The analysis resource for which the results will be overlaid on top of
  the given pathways overview.

- diagram_profile:

  Color profile of diagrams, should be one of "Copper" (default),
  "Copper Plus", "Barium Lithium" or "calcium salts".

- analysis_profile:

  Color profile of analysis, should be one of: "Standard" (default),
  "Strosobar" or "Copper Plus".

- token:

  The analysis Token for which the results will be overlaid on top of
  the given pathways overview. see:
  [`rba_reactome_analysis`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis.md).

- exp_column:

  numeric: (only if token is supplied) Specify the expression column for
  the overlay.

- image_quality:

  Numeric: A number ranging from 1 to 10. 1 is the lowest quality and 10
  is the highest (default = 5).

- flag_element:

  Gene name, protein ID, chemical ID or Reactome ID of a diagram's
  element to be flagged.

- flg_interactors:

  Logical: Should the interactor be considered when flagging a diagram
  element? (default = TRUE)

- sel:

  CSV line for highlighting element(s) selection in the diagram.

- title:

  Logical: Should the pathway name be displayed below the image?
  (default = TRUE)

- margin:

  Numeric: A number ranging from 0 to 20 to set as the image's margin.
  (default = 15)

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

- Marc Gillespie, Bijay Jassal, Ralf Stephan, Marija Milacic, Karen
  Rothfels, Andrea Senff-Ribeiro, Johannes Griss, Cristoffer Sevilla,
  Lisa Matthews, Chuqiao Gong, Chuan Deng, Thawfeek Varusai, Eliot
  Ragueneau, Yusra Haider, Bruce May, Veronica Shamovsky, Joel Weiser,
  Timothy Brunson, Nasim Sanati, Liam Beckman, Xiang Shao, Antonio
  Fabregat, Konstantinos Sidiropoulos, Julieth Murillo, Guilherme
  Viteri, Justin Cook, Solomon Shorser, Gary Bader, Emek Demir, Chris
  Sander, Robin Haw, Guanming Wu, Lincoln Stein, Henning Hermjakob,
  Peter D’Eustachio, The reactome pathway knowledgebase 2022, Nucleic
  Acids Research, 2021;, kab1028, https://doi.org/10.1093/nar/gkab1028

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
