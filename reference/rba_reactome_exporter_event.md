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

  Reactome event's database IDs (DbId) or Stable IDs (StId).

- output_format:

  Either "sbgn" or "sbml".

- save_to:

  NULL or Character:

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
