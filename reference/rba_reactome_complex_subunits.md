# Get a Complex's Subunits

This function will return a list of subunits which are participants of
your supplied complex.

## Usage

``` r
rba_reactome_complex_subunits(complex_id, exclude_structures = FALSE, ...)
```

## Arguments

- complex_id:

  Character: Reactome stable Identifier of the complex.

- exclude_structures:

  Logical: (default = `FALSE`) Should the contained complexes and entity
  sets be excluded from the results?

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

Data frame which each row is a subunit of your supplied complex and the
columns are pertinent information of that subunit.

## Details

Subunits will be returned recursively; Which means that if a subunit was
itself a complex, subunit of that complex will be also returned in the
results.

## Corresponding API Resources

"GET https://reactome.org/ContentService/data/complex/{id}/subunits"

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

Other "Reactome Content Service - Physical Entity Queries":
[`rba_reactome_complex_list()`](https://rbioapi.moosa-r.com/reference/rba_reactome_complex_list.md),
[`rba_reactome_entity_other_forms()`](https://rbioapi.moosa-r.com/reference/rba_reactome_entity_other_forms.md),
[`rba_reactome_participant_of()`](https://rbioapi.moosa-r.com/reference/rba_reactome_participant_of.md)

## Examples

``` r
# \donttest{
rba_reactome_complex_subunits(complex_id = "R-HSA-5674003",
    exclude_structures = FALSE)
# }
```
