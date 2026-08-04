# Get lower level pathways Containing a 'Physical Entity' or Event

Use this function to search the event hierarchy and retrieve a list of
all lower level pathways (non TopLevelPathway class) that contain a
given 'Physical Entity' or Event. See "Arguments section" on how to
modify your search.

## Usage

``` r
rba_reactome_pathways_low(
  entity_id,
  with_diagram = FALSE,
  all_forms = FALSE,
  species = NULL,
  ...
)
```

## Arguments

- entity_id:

  Character: The entity that should exist in the pathways.

- with_diagram:

  Logical: (default = `FALSE`) only include pathways with diagram?

- all_forms:

  Logical: (default = `FALSE`) should other variants of your supplied
  entity_id be considered? (e.g. same molecule but in different
  compartment, secretory form etc.) see
  [`rba_reactome_participants`](https://rbioapi.moosa-r.com/reference/rba_reactome_participants.md)'s
  "Details section" to learn more about how Reactome classifies
  molecules.

- species:

  Character or Numeric: (optional) confine your search to a specific
  species by providing it's NCBI Taxonomy identifier (Human Taxonomy ID
  is 9606) or species name (e.g. "Homo sapiens"). See
  [`rba_reactome_species`](https://rbioapi.moosa-r.com/reference/rba_reactome_species.md)
  or [Reactome Data Schema: Entries:
  Species](https://reactome.org/content/schema/objects/Species/).

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

Data frame where each row is a pathway that contains your supplied
entity and columns are pertinent information.

## Corresponding API Resources

"GET
https://reactome.org/ContentService/data/pathways/low/entity/{id}"  
"GET https://reactome.org/ContentService/data/pathways/low/diagram/
entity/{id}"  
"GET https://reactome.org/ContentService/data/pathways/low/diagram/
entity/{id}/allForms"

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

Other "Reactome Content Service - Pathway Related Queries":
[`rba_reactome_pathways_events()`](https://rbioapi.moosa-r.com/reference/rba_reactome_pathways_events.md),
[`rba_reactome_pathways_top()`](https://rbioapi.moosa-r.com/reference/rba_reactome_pathways_top.md)

## Examples

``` r
# \donttest{
rba_reactome_pathways_low(entity_id = "R-HSA-199420")
# }
# \donttest{
rba_reactome_pathways_low(entity_id = "R-HSA-199420", with_diagram = TRUE)
# }
# \donttest{
rba_reactome_pathways_low(entity_id = "R-HSA-199420", with_diagram = TRUE,
    all_forms = TRUE)
# }
```
