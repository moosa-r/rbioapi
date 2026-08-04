# Compare Human Pathways with with Other Species

Use This function to Compare human's manually-curated pathways and
computationally inferred pathways (orthologous) in other species.

## Usage

``` r
rba_reactome_analysis_species(
  species_dbid,
  sort_by = "ENTITIES_PVALUE",
  order = "ASC",
  resource = "TOTAL",
  p_value = 1,
  min = NULL,
  max = NULL,
  ...
)
```

## Arguments

- species_dbid:

  Numeric: Reactome DbId (e.g Mus musculus is 48892) of the species you
  want to compare with Homo sapiens. See
  [`rba_reactome_species`](https://rbioapi.moosa-r.com/reference/rba_reactome_species.md)
  or [Reactome Data Schema: Entries:
  Species](https://reactome.org/content/schema/objects/Species/).

- sort_by:

  Character: (default = `"ENTITIES_PVALUE"`) Sort the result based on
  what column? Available choices are: "NAME", "TOTAL_ENTITIES",
  "TOTAL_INTERACTORS", "TOTAL_REACTIONS", "FOUND_ENTITIES",
  "FOUND_INTERACTORS", "FOUND_REACTIONS", "ENTITIES_RATIO",
  "ENTITIES_PVALUE", "ENTITIES_FDR" or "REACTIONS_RATIO".

- order:

  Character: (default = `"ASC"`) Sort Order. Can be either "ASC" or
  "DESC".

- resource:

  Character: (default = `"TOTAL"`) Filter results based on the resource.
  Available choices are: "TOTAL", "UNIPROT", "ENSEMBL", "CHEBI",
  "IUPHAR", "MIRBASE", "NCBI_PROTEIN", "EMBL", "COMPOUND" or
  "PUBCHEM_COMPOUND".

- p_value:

  Numeric: (default = `1`) Set a P value threshold. Only results with P
  value equal to or less than your supplied threshold will be returned
  (1 means no P value filtering).

- min:

  Numeric: (optional) Minimum number of entities that a pathways should
  have to be included in the results.

- max:

  Numeric: (optional) Maximum number of entities that a pathways should
  have to be included in the results.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

List with the results of the comparison.

## Details

Reactome incorporate manually curated human reactions and PANTHER's
protein homology data to Computationally infer events in other
eukaryotic species.  
Reactome uses an orthology-based approach to project curated human
events to supported non-human species. See [Reactome Computationally
Inferred Events](https://reactome.org/documentation/inferred-events/)
for more information.

## Corresponding API Resources

"GET https://reactome.org/AnalysisService/species/homoSapiens/{species}"

## References

- Ragueneau, E., Gong, C., Sinquin, P., Sevilla, C., Beavers, D.,
  Grentner, A., ... D’Eustachio, P. (2026). The Reactome
  Knowledgebase 2026. Nucleic Acids Res., 54(D1), D673–D681. doi:
  10.1093/nar/gkaf1223

- Griss J, Viteri G, Sidiropoulos K, Nguyen V, Fabregat A, Hermjakob H.
  ReactomeGSA - Efficient Multi-Omics Comparative Pathway Analysis. Mol
  Cell Proteomics. 2020 Sep 9. doi: 10.1074/mcp. PubMed PMID: 32907876.

- [Reactome Analysis Services API
  Documentation](https://reactome.org/AnalysisService/)

- [Citations note on Reactome website](https://reactome.org/cite)

## See also

[`rba_reactome_orthology`](https://rbioapi.moosa-r.com/reference/rba_reactome_orthology.md)

Other "Reactome Analysis Service":
[`rba_reactome_analysis()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis.md),
[`rba_reactome_analysis_download()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_download.md),
[`rba_reactome_analysis_import()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_import.md),
[`rba_reactome_analysis_mapping()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_mapping.md),
[`rba_reactome_analysis_pdf()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_pdf.md),
[`rba_reactome_analysis_token()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_token.md)

## Examples

``` r
# \donttest{
rba_reactome_analysis_species(species_dbid = 48892)
# }
```
