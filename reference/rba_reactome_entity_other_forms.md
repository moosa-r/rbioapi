# Get Other forms of a Reactome Entity

This function retrieve a list containing all other forms of your
supplied Physical Entity ID.

## Usage

``` r
rba_reactome_entity_other_forms(entity_id, ...)
```

## Arguments

- entity_id:

  Character: Reactome's entity ID.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

Data frame where each row is other forms of your supplied Entity ID and
columns are pertinent information.

## Details

According to Reactome API documentation, "These other forms are Physical
Entities that share the same Reference Entity identifier, e.g. PTEN H93R
R-HSA-2318524 and PTEN C124R R-HSA-2317439 are two forms of PTEN."

## Corresponding API Resources

"GET https://reactome.org/ContentService/data/entity/{id}/otherForms"

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

Other "Reactome Content Service - Physical Entity Queries":
[`rba_reactome_complex_list()`](https://rbioapi.moosa-r.com/reference/rba_reactome_complex_list.md),
[`rba_reactome_complex_subunits()`](https://rbioapi.moosa-r.com/reference/rba_reactome_complex_subunits.md),
[`rba_reactome_participant_of()`](https://rbioapi.moosa-r.com/reference/rba_reactome_participant_of.md)

## Examples

``` r
# \donttest{
rba_reactome_entity_other_forms("R-HSA-199420")
# }
```
