# Get Events Contained in an Upstream Events

A Reactome Event could be comprised of other events (meaning, a pathway
that include other pathways itself). Use this function to recursively
return all the events which reside downstream of your supplied event ID
(or an attribute of that events).

## Usage

``` r
rba_reactome_pathways_events(event_id, attribute_name = NULL, ...)
```

## Arguments

- event_id:

  Character or Numeric: Reactome event's database ID (DbId) or Stable ID
  (StId).

- attribute_name:

  Character: (optional) An attribute of the events to be returned
  instead of the whole events. see [Reactome Data Schema:
  Event](https://reactome.org/content/schema/Event) for available
  options.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

Data frame where each row is a contained event and columns are event's
attributes. If an "attribute_name" argument was supplied, a character
vector will be returned.

## Details

By Reactome's definition, Events are the building blocks of biological
processes and could be of two main classes: "Pathway" or "Reaction-like
events". The events are organized in a hierarchical structure; and each
event could be child or parent to another event; The hierarchy will
always begin with a "Top level pathway" event. Also note that a given
event could be part of more that one hierarchies.

## Corresponding API Resources

"GET https://reactome.org/ContentService/data/pathway/{id}/
containedEvents"  
"GET https://reactome.org/ContentService/data/pathway/{id}/
containedEvents/{attributeName}"

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

Other "Reactome Content Service - Pathway Related Queries":
[`rba_reactome_pathways_low()`](https://rbioapi.moosa-r.com/reference/rba_reactome_pathways_low.md),
[`rba_reactome_pathways_top()`](https://rbioapi.moosa-r.com/reference/rba_reactome_pathways_top.md)

## Examples

``` r
# \donttest{
rba_reactome_pathways_events(event_id = "R-HSA-5673001")
# }
# \donttest{
rba_reactome_pathways_events(event_id = "R-HSA-5673001",
    attribute_name = "displayName")
# }
```
