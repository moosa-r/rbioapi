# Exports A Reactome Event to SBGN or SBML

This function will export a supplied Reactome Event (Pathway or
Reaction) to a SBGN (Systems Biology Graphical Notation) or SBML
(Systems Biology Markup Language)

## Usage

``` r
rba_reactome_exporter_event(event_id, output_format, save_to = NULL, ...)
```

## Arguments

- event_id:

  Character: Reactome event's database IDs (DbId) or Stable IDs (StId).

- output_format:

  Character: Either "sbgn" or "sbml".

- save_to:

  Character: (optional)

  - NULL: Save the file to an automatically-generated path.

  - Character string: A valid file path to save the file to.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

NULL, According to the inputs, a SBGN or SBML file will be saved to
disk.

## Corresponding API Resources

"GET https://reactome.org/ContentService/exporter/event/
{identifier}.sbgn"  
"GET https://reactome.org/ContentService/exporter/event/
{identifier}.sbml"

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

Other "Reactome Content Service - Format Exporter":
[`rba_reactome_exporter_diagram()`](https://rbioapi.moosa-r.com/reference/rba_reactome_exporter_diagram.md),
[`rba_reactome_exporter_overview()`](https://rbioapi.moosa-r.com/reference/rba_reactome_exporter_overview.md),
[`rba_reactome_exporter_reaction()`](https://rbioapi.moosa-r.com/reference/rba_reactome_exporter_reaction.md)

## Examples

``` r
if (FALSE) { # \dontrun{
rba_reactome_exporter_event(event_id = "R-HSA-177929",
    output_format = "sbgn",
    save_to = "R-HSA-177929.sbgn")
} # }
if (FALSE) { # \dontrun{
rba_reactome_exporter_event(event_id = "R-HSA-177929",
    output_format = "sbgn")
} # }
```
