# Get Persons Information by Name

Using this function you can query people by partially matching or exact
name and retrieve a list of matching people in Reactome.

## Usage

``` r
rba_reactome_people_name(person_name, exact_match = FALSE, ...)
```

## Arguments

- person_name:

  Character: first and last name of the person

- exact_match:

  Logical: (default = `FALSE`) should the supplied name be considered as
  an exact match?

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

List where each element is a search hit contains the person's
information.

## Corresponding API Resources

"GET https://reactome.org/ContentService/data/people/name/{name}"  
"GET https://reactome.org/ContentService/data/people/name/{name}/exact"

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
[`rba_reactome_people_id()`](https://rbioapi.moosa-r.com/reference/rba_reactome_people_id.md)

## Examples

``` r
# \donttest{
rba_reactome_people_name("Jupe")
# }
# \donttest{
rba_reactome_people_name("Steve Jupe", exact_match = TRUE)
# }
```
