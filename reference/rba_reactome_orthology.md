# Get Orthologous (Computationally Inferred) Events

Reactome incorporate manually curated human reactions and PANTHER's
protein homology data to Computationally infer events in other
eukaryotic species.

## Usage

``` r
rba_reactome_orthology(event_ids, species_dbid, ...)
```

## Arguments

- event_ids:

  Character: Up to 20 human Reactome event IDs to retrieve their
  orthologous events.

- species_dbid:

  Numeric: Reactome database ID (DbId) of the target species. (e.g Mus
  musculus is 48892). See
  [`rba_reactome_species`](https://rbioapi.moosa-r.com/reference/rba_reactome_species.md)
  or [Reactome Data Schema: Entries:
  Species](https://reactome.org/content/schema/objects/Species/).

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

List containing found Orthologous event(s) in your supplied species and
their pertinent information.

## Details

Reactome uses an orthology-based approach to project curated human
events to supported non-human species. See [Reactome Computationally
Inferred Events](https://reactome.org/documentation/inferred-events/)
for more information.

## Corresponding API Resources

"POST https://reactome.org/ContentService/data/orthologies/ids/
species/{speciesId}"

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

[`rba_reactome_analysis_species`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_species.md)

## Examples

``` r
# \donttest{
rba_reactome_orthology(event_ids = c("R-HSA-6799198", " R-HSA-72764"),
    species_dbid = 49633)
# }
```
