# Get Full Event Hierarchy of a Species

This function will retrieve the full Events hierarchy of your supplied
species. Directly under each species, each child element is a "top Level
Pathway". You can traverse the events tree down by following the
"children" element.

## Usage

``` r
rba_reactome_event_hierarchy(species, ...)
```

## Arguments

- species:

  Character or Numeric: NCBI Taxonomy identifier (Human Taxonomy ID is
  9606.) or species name (e.g. "Homo sapiens"). See
  [`rba_reactome_species`](https://rbioapi.moosa-r.com/reference/rba_reactome_species.md)
  or [Reactome Data Schema: Entries:
  Species](https://reactome.org/content/schema/objects/Species/).

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

List which is a representation of the species's events hierarchy
described in the "Details section".

## Details

By Reactome's definition, Events are the building blocks of biological
processes and could be of two main classes: "Pathway" or "Reaction-like
events". The events are organized in a hierarchical structure; and each
event could be child or parent to another event; The hierarchy will
always begin with a "Top level pathway" event. Also note that a given
event could be part of more that one hierarchies.

## Corresponding API Resources

"GET https://reactome.org/ContentService/data/eventsHierarchy/{species}"

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

Other "Reactome Content Service - Queries Related to Events":
[`rba_reactome_event_ancestors()`](https://rbioapi.moosa-r.com/reference/rba_reactome_event_ancestors.md)

## Examples

``` r
if (FALSE) { # \dontrun{
#very large response!
rba_reactome_event_hierarchy("Homo sapiens")
} # }
if (FALSE) { # \dontrun{
#very large response!
rba_reactome_event_hierarchy(9606)
} # }
```
