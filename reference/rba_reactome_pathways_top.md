# Get Top Level Pathways in a Species

This function will Return a list of all pathways with the class
"TopLevelPathway" which are annotated in your supplied species.

## Usage

``` r
rba_reactome_pathways_top(species, ...)
```

## Arguments

- species:

  Numeric or Character: NCBI Taxonomy identifier (Human Taxonomy ID is
  9606.) or species name (e.g. "Homo sapiens"). See
  [`rba_reactome_species`](https://rbioapi.moosa-r.com/reference/rba_reactome_species.md)
  or [Reactome Data Schema: Entries:
  Species](https://reactome.org/content/schema/objects/Species/).

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

Data frame where each row is a Top Level Pathway and columns are
pertinent information.

## Details

Reactome's Events hierarchy for any specie will begin with pathways with
class "TopLevelPathway" (e.g. "Immune System", "Metabolism of
proteins"). further down in the event's hierarchy tree, each
TopLevelPathway has has other events itself (e.g. "Adaptive immune
system", "Innate immune system"). Based on the chosen pathway, the
hierarchy tree would typically goes further down.

## Corresponding API Resources

"GET https://reactome.org/ContentService/data/pathways/top/{species}"

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

Other "Reactome Content Service - Pathway Related Queries":
[`rba_reactome_pathways_events()`](https://rbioapi.moosa-r.com/reference/rba_reactome_pathways_events.md),
[`rba_reactome_pathways_low()`](https://rbioapi.moosa-r.com/reference/rba_reactome_pathways_low.md)

## Examples

``` r
# \donttest{
rba_reactome_pathways_top(species = 9606)
# }
# \donttest{
rba_reactome_pathways_top(species = "Saccharomyces cerevisiae")
# }
```
