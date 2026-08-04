# Get Reactome Events Ancestors

Along with Reactome's events hierarchy, This function will retrieve all
the events beginning from your supplied event up to the "Top level
Pathway". see "Details section" for more information.

## Usage

``` r
rba_reactome_event_ancestors(event_id, ...)
```

## Arguments

- event_id:

  Character: Reactome event's identifier.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

List which every element is a Data frame listing your supplied event
along with it's ancestor events. Because any given event can be part of
more than one pathway hierarchy, the list may contain multiple data
frames.

## Details

By Reactome's definition, Events are the building blocks of biological
processes and could be of two main classes: "Pathway" or "Reaction-like
events". The events are organized in a hierarchical structure; and each
event could be child or parent to another event; The hierarchy will
always begin with a "Top level pathway" event. Also note that a given
event could be part of more that one hierarchies.

## Corresponding API Resources

"GET https://reactome.org/ContentService/data/event/{id}/ancestors"

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
[`rba_reactome_event_hierarchy()`](https://rbioapi.moosa-r.com/reference/rba_reactome_event_hierarchy.md)

## Examples

``` r
# \donttest{
rba_reactome_event_ancestors("R-HSA-5673001")
# }
```
