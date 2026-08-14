# Retrieve the Reactome Event Hierarchy of a Species

Retrieve the full hierarchy of pathways and reaction-like events for a
species, or restrict the hierarchy to pathways. The hierarchy begins
with top-level pathways and can be traversed through the `children`
field. Because an event can participate in more than one biological
process, it may occur in more than one place in the hierarchy.

## Usage

``` r
rba_reactome_event_hierarchy(
  species,
  pathways_only = FALSE,
  token = NULL,
  resource = "TOTAL",
  interactors = FALSE,
  importable_only = FALSE,
  ...
)
```

## Arguments

- species:

  Character or Numeric: NCBI Taxonomy identifier (Human Taxonomy ID is
  9606.) or species name (e.g. "Homo sapiens"). See
  [`rba_reactome_species`](https://rbioapi.moosa-r.com/reference/rba_reactome_species.md)
  or [Reactome Data Schema: Entries:
  Species](https://reactome.org/content/schema/objects/Species/).

- pathways_only:

  Logical: (default = `FALSE`) Should reaction-like events be omitted
  from the hierarchy?

- token:

  Character: (optional) Reactome analysis token whose results should be
  added to the hierarchy. See
  [`rba_reactome_analysis`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis.md).

- resource:

  Character: (default = `"TOTAL"`) Analysis resource to add when a
  `token` is supplied. Available choices are: "TOTAL", "UNIPROT",
  "ENSEMBL", "CHEBI", "IUPHAR", "MIRBASE", "NCBI_PROTEIN", "EMBL",
  "COMPOUND", or "PUBCHEM_COMPOUND".

- interactors:

  Logical: (default = `FALSE`) When a `token` is supplied, should
  interactor results be included?

- importable_only:

  Logical: (default = `FALSE`) When a `token` is supplied, should the
  analysis results be restricted to importable resources?

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A nested list representing the species's event hierarchy. When an
analysis token is supplied, the corresponding analysis information is
included with the pathway entries.

## Details

An analysis token can be supplied to add the corresponding analysis
results to the hierarchy. The analysis resource can then be selected,
and interactors or only importable resources can optionally be included.

## Corresponding API Resources

"GET https://reactome.org/ContentService/data/eventsHierarchy/{species}"

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

Other "Reactome Content Service - Queries Related to Events":
[`rba_reactome_event_ancestors()`](https://rbioapi.moosa-r.com/reference/rba_reactome_event_ancestors.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Very large response
rba_reactome_event_hierarchy("Homo sapiens")
} # }
if (FALSE) { # \dontrun{
# Restrict the hierarchy to pathways
rba_reactome_event_hierarchy(9606, pathways_only = TRUE)
} # }
```
