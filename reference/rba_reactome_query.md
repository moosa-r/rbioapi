# Retrieve Reactome Knowledgebase Objects

Retrieve one or more Reactome objects by database or stable identifier.
A single object can be returned with additional related information or
reduced to one of its attributes. Multiple identifiers can optionally be
mapped to the corresponding current Reactome objects.

## Usage

``` r
rba_reactome_query(
  ids,
  enhanced = FALSE,
  map = FALSE,
  attribute_name = NULL,
  fetch_incoming_relationships = TRUE,
  summarize_reference_entity = FALSE,
  include_disease = TRUE,
  ...
)
```

## Arguments

- ids:

  Character or Numeric vector: One or more database identifiers (DbIds),
  stable identifiers (StIds), or a mixture of both. At most 20
  identifiers can be supplied.

- enhanced:

  Logical: (default = `FALSE`) Should additional related information be
  retrieved? This can only be used with one identifier.

- map:

  Logical: (default = `FALSE`) When multiple identifiers are supplied,
  should each input identifier be mapped to its current Reactome object?
  This is useful for previous versions of stable identifiers.

- attribute_name:

  Character: (optional) Return only this attribute of a single Reactome
  object. This cannot be combined with `enhanced = TRUE`.

- fetch_incoming_relationships:

  Logical: (default = `TRUE`) When `enhanced = TRUE`, should incoming
  relationships be included where they are relevant to the queried
  object?

- summarize_reference_entity:

  Logical: (default = `FALSE`) When `enhanced = TRUE` and the queried
  object is a ReferenceEntity, should its physical forms be represented
  by a summary?

- include_disease:

  Logical: (default = `TRUE`) When `enhanced = TRUE`, should
  disease-specific information be included?

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

An R object containing the requested Reactome object or objects. The
returned fields depend on the type of each object. Mapped multiple-ID
queries return a named list, and attribute queries return character
values.

## Details

With `enhanced = TRUE`, Reactome also retrieves second-level
relationships involving regulations and catalysts. The enhanced query
can include incoming relationships and disease-specific information.
When the queried object is a ReferenceEntity, its physical forms can
instead be represented by a summary.

## Corresponding API Resources

"POST https://reactome.org/ContentService/data/query/ids"  
"POST https://reactome.org/ContentService/data/query/ids/map"  
"GET https://reactome.org/ContentService/data/query/{id}"  
"GET https://reactome.org/ContentService/data/query/enhanced/v2/{id}"  
"GET
https://reactome.org/ContentService/data/query/{id}/{attributeName}"

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

## Examples

``` r
# \donttest{
rba_reactome_query(ids = c("8953958", "11982506", "R-ALL-9649879"))
# }
# \donttest{
rba_reactome_query(ids = "R-HSA-9656256", enhanced = TRUE)
# }
# \donttest{
rba_reactome_query(
  ids = 66247,
  enhanced = TRUE,
  summarize_reference_entity = TRUE
)
# }
# \donttest{
rba_reactome_query(ids = "8863054", attribute_name = "displayName")
# }
```
