# Map External ID to Reactome Pathways/Reactions

By providing an external identifier from a given resource, you can
retrieve a list of pathways/reactions that include your supplied ID.

## Usage

``` r
rba_reactome_mapping(id, resource, map_to, species = "Homo sapiens", ...)
```

## Arguments

- id:

  Character or Numeric: Molecule's external Identifier

- resource:

  Character: What is the resource of your supplied ID? see: [Reactome
  External
  Identifiers](https://reactome.org/content/schema/objects/ReferenceDatabase/)

- map_to:

  Character: Either "pathways" or "reactions".

- species:

  Character or Numeric: (default = `"Homo sapiens"`) NCBI Taxonomy
  identifier (Human is 9606), species name (e.g. "Homo sapiens") or
  Reactome DbId (e.g Homo sapiens is 48887). See
  [`rba_reactome_species`](https://rbioapi.moosa-r.com/reference/rba_reactome_species.md)
  or [Reactome Data Schema: Entries:
  Species](https://reactome.org/content/schema/objects/Species/).

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

Data frame where each row is a pathway/reaction and columns are
pertinent information.

## Corresponding API Resources

"GET https://reactome.org/ContentService/data/mapping/{resource}/
{identifier}/pathways"  
"GET https://reactome.org/ContentService/data/mapping/{resource}/
{identifier}/reactions"

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
rba_reactome_mapping(id = "PTEN", resource =  "UniProt",
    map_to = "reactions", species = 9606)
# }
```
