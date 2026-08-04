# Query and Retrieve any Reactome knowledge-base Object

Using this Comprehensive function, You can Retrieve any object from
[Reactome
knowledge-base](https://reactome.org/content/schema/DatabaseObject/)

## Usage

``` r
rba_reactome_query(
  ids,
  enhanced = FALSE,
  map = FALSE,
  attribute_name = NULL,
  ...
)
```

## Arguments

- ids:

  Character or Numeric vector: A single or Multiple database IDs (DbId),
  Stable IDs (StId) or a mixture of both.

- enhanced:

  Logical: (default = `FALSE`) If 'TRUE' more information on the
  supplied entry will be returned. (You can set this argument to 'TRUE'
  Only when you supply a single ID).

- map:

  Logical: (default = `FALSE`) Should the supplied IDs be mapped? This
  argument will only be considered when you supply multiple IDs. (e.g.
  when you supply previous version of stable identifiers.)

- attribute_name:

  Character: (optional) Only Return an Attribute of the supplied
  Database Object. (You can use this argument Only when you supply a
  single ID)

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

List containing your query outputs.

## Corresponding API Resources

"POST https://reactome.org/ContentService/data/query/ids"  
"POST https://reactome.org/ContentService/data/query/ids/map"  
"GET https://reactome.org/ContentService/data/query/{id}"  
"GET https://reactome.org/ContentService/data/query/enhanced/{id}"  
"GET
https://reactome.org/ContentService/data/query/{id}/{attributeName}"

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

## Examples

``` r
# \donttest{
rba_reactome_query(ids = c("8953958", "11982506", "R-ALL-9649879"))
# }
# \donttest{
rba_reactome_query(ids = "R-HSA-9656256", enhanced = TRUE)
# }
# \donttest{
rba_reactome_query(ids = "8863054", attribute_name = "displayName")
# }
```
