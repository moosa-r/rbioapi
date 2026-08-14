# Map Cross-Reference Identifiers to Reactome

Retrieve the Reactome reference entities associated with an identifier
from an external database. With `expanded = TRUE`, also retrieve the
other external identifiers associated with each reference entity and the
stable identifiers of its physical forms in Reactome.

## Usage

``` r
rba_reactome_xref(
  xref_id,
  expanded = FALSE,
  db_filter = NULL,
  page_size = 100,
  page = 1,
  ...
)
```

## Arguments

- xref_id:

  Character or Numeric vector: One or more cross-reference identifiers
  from external databases. Multiple identifiers can only be supplied
  when `expanded = TRUE`.

- expanded:

  Logical: (default = `FALSE`) Should other external identifiers and
  associated Reactome physical forms also be retrieved?

- db_filter:

  Character: (optional) When `expanded = TRUE`, restrict the returned
  cross-references to this external database name as used by Reactome,
  e.g. `"ENSEMBL"`.

- page_size:

  Numeric: (default = `100`) For an expanded query with multiple
  identifiers, the number of supplied identifiers to process per page.

- page:

  Numeric: (default = `1`) For an expanded query with multiple
  identifiers, the one-based page to retrieve.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

An R object containing the corresponding Reactome reference entities.
Expanded results also contain associated cross-references and stable
identifiers of physical entities. Expanded queries with multiple
identifiers additionally contain pagination information.

## Details

Multiple identifiers can be supplied for an expanded query. Reactome
processes these identifiers in pages, while a standard query accepts one
identifier. See
[`rba_reactome_participants`](https://rbioapi.moosa-r.com/reference/rba_reactome_participants.md)
for more information about how Reactome represents molecules.

## Corresponding API Resources

"GET
https://reactome.org/ContentService/references/mapping/{identifier}"  
"POST https://reactome.org/ContentService/references/mapping/xrefs"

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

[`rba_pages`](https://rbioapi.moosa-r.com/reference/rba_pages.md)

## Examples

``` r
# \donttest{
rba_reactome_xref("CD40")
# }
# \donttest{
rba_reactome_xref("ENSP00000361350")
# }
# \donttest{
rba_reactome_xref("P36897", expanded = TRUE, db_filter = "ENSEMBL")
# }
# \donttest{
rba_reactome_xref(
  c("P36897", "Q5S007"),
  expanded = TRUE,
  db_filter = "ENSEMBL",
  page_size = 2,
  page = 1
)
# }
```
