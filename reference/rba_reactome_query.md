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

  A single or Multiple database IDs (DbId), Stable IDs (StId) or a
  mixture of both.

- enhanced:

  Logical: (Default = FALSE) If 'TRUE' more information on the supplied
  entry will be returned. (You can set this argument to 'TRUE' Only when
  you supply a single ID).

- map:

  (Default = FALSE) Should the supplied IDs be mapped? This argument
  will only be considered when you supply multiple IDs. (e.g. when you
  supply previous version of stable identifiers.)

- attribute_name:

  (Optional) Only Return an Attribute of the supplied Database Object.
  (You can use this argument Only when you supply a single ID)

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
