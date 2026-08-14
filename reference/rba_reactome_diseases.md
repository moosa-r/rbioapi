# Reactome Diseases

This function Retrieve a list of all diseases or disease DOIDs annotated
in Reactome.

## Usage

``` r
rba_reactome_diseases(doid = FALSE, ...)
```

## Arguments

- doid:

  Logical: (default = `FALSE`) Return disease DOIDs instead of diseases?

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

Data frame containing all the disease annotation available at Reactome.
If doid was set to TRUE, DOID info will be returned instead.

## Corresponding API Resources

"GET https://reactome.org/ContentService/data/diseases"  
"GET https://reactome.org/ContentService/data/diseases/doid"

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

## Examples

``` r
# \donttest{
rba_reactome_diseases()
# }
# \donttest{
rba_reactome_diseases(doid = TRUE)
# }
```
