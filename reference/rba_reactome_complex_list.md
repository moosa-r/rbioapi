# Get Complexes That Include a Molecule

This function will retrieve a list of complexes that include your
supplied molecule as a component.

## Usage

``` r
rba_reactome_complex_list(id, resource, ...)
```

## Arguments

- id:

  Character: Molecule's external Identifier

- resource:

  Character: What is the resource of your supplied ID? see: [Reactome
  External
  Identifiers](https://reactome.org/content/schema/objects/ReferenceDatabase/)

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

Data frame where each row is a complex containing your supplied molecule
and columns are pertinent information.

## Corresponding API Resources

"GET https://reactome.org/ContentService/data/complexes/{resource}/
{identifier}"

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
[`rba_reactome_complex_subunits()`](https://rbioapi.moosa-r.com/reference/rba_reactome_complex_subunits.md),
[`rba_reactome_entity_other_forms()`](https://rbioapi.moosa-r.com/reference/rba_reactome_entity_other_forms.md),
[`rba_reactome_participant_of()`](https://rbioapi.moosa-r.com/reference/rba_reactome_participant_of.md)

## Examples

``` r
# \donttest{
rba_reactome_complex_list(id = "3845", resource = "NCBI Gene")
# }
# \donttest{
rba_reactome_complex_list(id = "P00533", resource = "UniProt")
# }
```
