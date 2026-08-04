# Get Larger Reactome Structures Which Include an Entity

This function will retrieve a list of complexes and sets that Your
supplied entity ID participates in (e.g. as a complex component,
reaction output).

## Usage

``` r
rba_reactome_participant_of(entity_id, ...)
```

## Arguments

- entity_id:

  Character: Reactome's entity ID.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

List of Reactome database Entities which Your supplied ID is a
participant in them.

## Corresponding API Resources

"GET https://reactome.org/ContentService/data/entity/{id}/componentOf"

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

[`rba_reactome_participants`](https://rbioapi.moosa-r.com/reference/rba_reactome_participants.md)

Other "Reactome Content Service - Physical Entity Queries":
[`rba_reactome_complex_list()`](https://rbioapi.moosa-r.com/reference/rba_reactome_complex_list.md),
[`rba_reactome_complex_subunits()`](https://rbioapi.moosa-r.com/reference/rba_reactome_complex_subunits.md),
[`rba_reactome_entity_other_forms()`](https://rbioapi.moosa-r.com/reference/rba_reactome_entity_other_forms.md)

## Examples

``` r
# \donttest{
rba_reactome_participant_of(entity_id = "R-HSA-199420")
# }
```
