# Get a Person by Identifier

Get a Person by Identifier

## Usage

``` r
rba_reactome_people_id(
  person_id,
  authored_pathways = FALSE,
  publications = FALSE,
  attribute_name = NULL,
  ...
)
```

## Arguments

- person_id:

  Character: Reactome database ID (DbId) or ORCID identifier.

- authored_pathways:

  Logical: (default = `FALSE`) Only return Pathway list authored by the
  person?

- publications:

  Logical: (default = `FALSE`) Only return publications list authored by
  the person?

- attribute_name:

  Character: (optional) A Reactome person attribute to return only. see
  [Reactome Data Schema:
  person](https://reactome.org/content/schema/Person/) for available
  options.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

List containing the requested informations of your supplied person.

## Corresponding API Resources

"GET https://reactome.org/ContentService/data/person/{id}"  
"GET https://reactome.org/ContentService/data/person/{id}/
authoredPathways"  
"GET
https://reactome.org/ContentService/data/person/{id}/publications"  
"GET https://reactome.org/ContentService/data/person/{id}/
{attributeName}"

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

Other "Reactome Content Service - Person Queries":
[`rba_reactome_people_name()`](https://rbioapi.moosa-r.com/reference/rba_reactome_people_name.md)

## Examples

``` r
# \donttest{
rba_reactome_people_id("391309")
# }
# \donttest{
rba_reactome_people_id(person_id = "391309", authored_pathways = TRUE)
# }
```
