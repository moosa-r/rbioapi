# Return the Results Associated with a Token

Use a token generated After a Reactome analysis (via
[`rba_reactome_analysis`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis.md))
to Retrieve the analysis results. The output format is identical to the
returned object of
[`rba_reactome_analysis`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis.md).

## Usage

``` r
rba_reactome_analysis_token(
  token,
  species = NULL,
  sort_by = "ENTITIES_PVALUE",
  order = "ASC",
  resource = "TOTAL",
  p_value = 1,
  include_disease = TRUE,
  min = NULL,
  max = NULL,
  ...
)
```

## Arguments

- token:

  Character: A token associated to your previous Reactome analysis.

- species:

  Character or Numeric: (optional) NCBI Taxonomy identifier (Human is
  9606), species name (e.g. "Homo sapiens") or Reactome DbId (e.g. Homo
  sapiens is 48887). See
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

- include_disease:

  Logical: (default = `TRUE`) Should the disease pathways be included in
  the results?

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

List containing the results and information of your analysis.

## Details

After any analysis, Reactome will associate a token with your analysis.
It can later be used in functions that require the token (e.g. to
retrieve the analysis results, download pdf).  
Note that Reactome will store your token for only 7 days. You can
download your full results with
[`rba_reactome_analysis_download`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_download.md),
and re-import it anytime to reactome (using
[`rba_reactome_analysis_import`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_import.md))
to generate a new token.

## Corresponding API Resources

"GET https://reactome.org/AnalysisService/token/{token}"

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

[`rba_reactome_analysis`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis.md)

Other "Reactome Analysis Service":
[`rba_reactome_analysis()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis.md),
[`rba_reactome_analysis_download()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_download.md),
[`rba_reactome_analysis_import()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_import.md),
[`rba_reactome_analysis_mapping()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_mapping.md),
[`rba_reactome_analysis_pdf()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_pdf.md),
[`rba_reactome_analysis_species()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_species.md)

## Examples

``` r
if (FALSE) { # \dontrun{
rba_reactome_analysis_token(token = "MjAyMDEwMTYwMTI3MTNfMjY1MjM",
    species = 9606)
} # }
```
