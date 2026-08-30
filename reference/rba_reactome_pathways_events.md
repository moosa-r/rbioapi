# Get Events Contained in an Upstream Event

Reactome events can contain other events; for example, a pathway can
contain smaller pathways and reactions. This function recursively
retrieves all events downstream of the supplied event, or one attribute
of those events.

## Usage

``` r
rba_reactome_pathways_events(event_id, attribute_name = NULL, ...)
```

## Arguments

- event_id:

  Character or Numeric: Reactome event's database ID (DbId) or Stable ID
  (StId).

- attribute_name:

  Character: (optional) Optional event attribute to return instead of
  complete event records. See [Reactome Data Schema:
  Event](https://reactome.org/content/schema/Event) for available
  options.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list with information about the contained events. If `attribute_name`
is supplied, one value for each contained event is returned. If the
individual values cannot be identified reliably, the complete result is
returned as a single value.

## Details

Reactome defines events as the building blocks of biological processes.
Events can be pathways or reaction-like events and are organized
hierarchically. An event can be a child or parent of another event, each
hierarchy begins with a top-level pathway, and an event can belong to
more than one hierarchy.

When `attribute_name` is supplied, the function returns one value for
each contained event whenever the individual values can be identified
reliably. Empty values and line breaks within a value are preserved.
Otherwise, the complete result is returned unchanged with a warning.
When `save_file` is used, the saved file always contains the result
exactly as supplied by Reactome.

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

- Griss, J., Viteri, G., Sidiropoulos, K., Nguyen, V., Fabregat, A., &
  Hermjakob, H. (2020). ReactomeGSA—Efficient Multi-Omics Comparative
  Pathway Analysis. Molecular & Cellular Proteomics, 19(12), 2115–2125.
  doi: 10.1074/mcp.TIR120.002155

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
    attribute_name = "stId")
# }
```
