# Search the Reactome Knowledgebase

Search Reactome for entries that match a text query. The search can be
limited by species, result type, cellular compartment, and keyword.

## Usage

``` r
rba_reactome_search(
  query,
  species = NULL,
  types = NULL,
  compartments = NULL,
  keywords = NULL,
  cluster = TRUE,
  page_size = 10,
  page = 1,
  scope = "PHYSICAL_ENTITY",
  force_filters = FALSE,
  ...
)
```

## Arguments

- query:

  Character: Text to search for in the Reactome knowledgebase.

- species:

  Character vector: (optional) Scientific species name(s) used to filter
  the results, e.g. `"Homo sapiens"`. See
  [`rba_reactome_species`](https://rbioapi.moosa-r.com/reference/rba_reactome_species.md)
  for species available in Reactome.

- types:

  Character vector: (optional) Result type(s) used to filter the search,
  e.g. `"Protein"`, `"Pathway"`, or `"Reaction"`.

- compartments:

  Character vector: (optional) Cellular compartment name(s) used to
  filter the results.

- keywords:

  Character vector: (optional) Reactome search keyword(s) used to filter
  the results.

- cluster:

  Logical: (default = `TRUE`) Should matches be separated into groups
  according to their result type? If FALSE, matches are returned in one
  ranked group.

- page_size:

  Numeric: (default = `10`) Maximum number of matches to return from
  each result group on a page. If `cluster = FALSE`, this is the maximum
  number returned from the single combined group.

- page:

  Numeric: (default = `1`) One-based results page to retrieve.

- scope:

  Character: (default = `"PHYSICAL_ENTITY"`) Which form of matching
  entities should be returned? Can be one of:

  - "PHYSICAL_ENTITY": Return specific physical forms annotated in
    Reactome.

  - "REFERENCE_ENTITY": Group applicable physical forms by their
    underlying reference molecule; entries without a reference molecule
    are retained as physical entities.

  - "BOTH": Return both representations.

- force_filters:

  Logical: (default = `FALSE`) Should Reactome keep all supplied filters
  when they produce no matches? If FALSE, Reactome may remove the
  filters and return results from a broader search.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list with the following elements:

- results:

  A data frame with one row per returned result group. The `entries`
  column contains data frames of matching Reactome entries; their fields
  vary according to result type.

- rowCount:

  Number of matching entries returned on the requested page.

- numberOfGroups:

  Number of matching result groups reported by Reactome.

- numberOfMatches:

  Total number of matches reported by Reactome.

## Details

By default, matches are separated into groups such as proteins,
pathways, and reactions. In this case, `page_size` is applied separately
to each group. If `cluster = FALSE`, matches are returned in one group
and `page_size` applies to that group.

Reactome normally removes supplied filters when they produce no matches.
Set `force_filters = TRUE` to require all supplied filters, so that a
search with no filtered matches is reported instead of being broadened.
Reactome also marks matching text in some returned names and
descriptions; rbioapi leaves these highlighting markers unchanged.

## Corresponding API Resources

"GET https://reactome.org/ContentService/search/query"

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

[`rba_reactome_query`](https://rbioapi.moosa-r.com/reference/rba_reactome_query.md)
[`rba_reactome_species`](https://rbioapi.moosa-r.com/reference/rba_reactome_species.md)
[`rba_pages`](https://rbioapi.moosa-r.com/reference/rba_pages.md)

## Examples

``` r
# \donttest{
rba_reactome_search(
  query = "TP53",
  species = "Homo sapiens",
  types = c("Protein", "Pathway")
)
# }
# \donttest{
rba_reactome_search(
  query = "apoptosis",
  cluster = FALSE,
  page_size = 20
)
# }
```
