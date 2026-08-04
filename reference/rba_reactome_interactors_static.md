# Get Static(IntAct) Interaction Information of a Protein

Reactome maintains a locally hosted snapshot of the IntAct interactions
database. Using this function, you can retrieve IntAct information for
one or more proteins in two scenarios:

1.  If `endpoint = "details"` or `endpoint = "summary"`, retrieve
    detailed or summary information for the supplied accessions.

2.  If `endpoint = "pathways"`, retrieve Reactome pathways which include
    your supplied protein accession. Pathways with the class
    "TopLevelPathway" will be excluded.

Results depend on Reactome's current static interaction snapshot; a
valid accession can therefore have no mapped pathways.

## Usage

``` r
rba_reactome_interactors_static(
  proteins,
  endpoint = "details",
  only_diagrammed = FALSE,
  species = NULL,
  ...
)
```

## Arguments

- proteins:

  Character or Numeric vector: UniProt protein accession(s). If
  `endpoint = "pathways"`, only a single protein accession can be
  supplied.

- endpoint:

  Character: (default = `"details"`) Can be one of:

  1.  "details": Return detailed information for the supplied
      accessions.

  2.  "summary": Return summary information for the supplied accessions.

  3.  "pathways": Return pathways containing the interacting molecules
      (excluding the TopLevelPathway class).

- only_diagrammed:

  Logical: (default = `FALSE`) (only when `endpoint = "pathways"`) If
  TRUE, pathways without diagram will be excluded.

- species:

  Character: (optional) (only when `endpoint = "pathways"`) The
  scientific name of the species to search for pathways. See
  [`rba_reactome_species`](https://rbioapi.moosa-r.com/reference/rba_reactome_species.md)
  or [Reactome Data Schema: Entries:
  Species](https://reactome.org/content/schema/objects/Species/).

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

List which it's content varies based on the supplied "endpoint"
argument.

## Corresponding API Resources

"POST https://reactome.org/ContentService/interactors/static/
molecules/details"  
"POST https://reactome.org/ContentService/interactors/static/
molecules/summary"  
"GET https://reactome.org/ContentService/interactors/static/molecule/
{identifier}/pathways"

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

Other "Reactome Content Service - Molecule Interactors":
[`rba_reactome_interactors_psicquic()`](https://rbioapi.moosa-r.com/reference/rba_reactome_interactors_psicquic.md)

## Examples

``` r
if (FALSE) { # \dontrun{
rba_reactome_interactors_static(proteins = "Q9BXM7-1",
    endpoint = "pathways", species = "Homo sapiens")
} # }
# \donttest{
rba_reactome_interactors_static(proteins = c("Q9BXM7-1", "Q13501"),
    endpoint = "details")
# }
# \donttest{
rba_reactome_interactors_static(proteins = c("Q9BXM7-1", "Q13501"),
    endpoint = "summary")
# }
```
