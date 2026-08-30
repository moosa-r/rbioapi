# Generate PDF file with Reactome Analysis Results

Use this function to save a detailed report of your previous analysis
(That you have done with
[`rba_reactome_analysis`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis.md)).
You need to supply a 'token' associated to your previous analysis.

## Usage

``` r
rba_reactome_analysis_pdf(
  token,
  species,
  save_to = NULL,
  number = 25,
  resource = "TOTAL",
  diagram_profile = "Modern",
  analysis_profile = "Standard",
  fireworks_profile = "Barium Lithium",
  ...
)
```

## Arguments

- token:

  Character: A token associated to your previous Reactome analysis.

- species:

  Character or Numeric: NCBI Taxonomy identifier (Human Taxonomy ID is
  9606.) or species name (e.g. "Homo sapiens"). See
  [`rba_reactome_species`](https://rbioapi.moosa-r.com/reference/rba_reactome_species.md)
  or [Reactome Data Schema: Entries:
  Species](https://reactome.org/content/schema/objects/Species/).

- save_to:

  Character: (optional)

  - NULL: Save the file to an automatically-generated path.

  - Character string: A valid file path to save the file to.

- number:

  Numeric: (default = `25`) Positive integer giving the maximum number
  of pathways to include in the report.

- resource:

  Character: (default = `"TOTAL"`) Filter results based on the resource.
  Available choices are: "TOTAL", "UNIPROT", "ENSEMBL", "CHEBI",
  "IUPHAR", "MIRBASE", "NCBI_PROTEIN", "EMBL", "COMPOUND" or
  "PUBCHEM_COMPOUND".

- diagram_profile:

  Character: (default = `"Modern"`) Color profile of diagrams, should be
  either "Modern" or "Standard".

- analysis_profile:

  Character: (default = `"Standard"`) Color profile of analysis, should
  be one of: "Standard", "Strosobar" or "Copper Plus".

- fireworks_profile:

  Character: (default = `"Barium Lithium"`) Color profile of the
  overview diagram, should be one of: "Cooper", "Cooper Plus", "Barium
  Lithium" or "Calcium Salts".

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

NULL, a PDF file will be saved to disk.

## Details

Token is associated to each Reactome analysis results and kept by
Reactome for at least 7 days. You can locate it in
[`rba_reactome_analysis`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis.md)'s
output, under a sub-list named "summary" (i.e.
results\$summary\$token).  
Note that Reactome will store your token for only 7 days. You can
download your full results with
[`rba_reactome_analysis_download`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_download.md),
and re-import it anytime to reactome (using
[`rba_reactome_analysis_import`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_import.md))
to generate a new token. Use
[`rba_reactome_analysis_download`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_download.md)
to save your results in other formats.

## Corresponding API Resources

"GET https://reactome.org/AnalysisService/report/{token}/{species}/
{filename}.pdf"

## References

- Ragueneau, E., Gong, C., Sinquin, P., Sevilla, C., Beavers, D.,
  Grentner, A., ... D’Eustachio, P. (2026). The Reactome
  Knowledgebase 2026. Nucleic Acids Res., 54(D1), D673–D681. doi:
  10.1093/nar/gkaf1223

- Griss, J., Viteri, G., Sidiropoulos, K., Nguyen, V., Fabregat, A., &
  Hermjakob, H. (2020). ReactomeGSA—Efficient Multi-Omics Comparative
  Pathway Analysis. Molecular & Cellular Proteomics, 19(12), 2115–2125.
  doi: 10.1074/mcp.TIR120.002155

- [Reactome Analysis Services API
  Documentation](https://reactome.org/AnalysisService/)

- [Citations note on Reactome website](https://reactome.org/cite)

## See also

[`rba_reactome_analysis_download`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_download.md)
[`rba_reactome_analysis`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis.md)

Other "Reactome Analysis Service":
[`rba_reactome_analysis()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis.md),
[`rba_reactome_analysis_download()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_download.md),
[`rba_reactome_analysis_import()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_import.md),
[`rba_reactome_analysis_mapping()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_mapping.md),
[`rba_reactome_analysis_species()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_species.md),
[`rba_reactome_analysis_token()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_token.md)

## Examples

``` r
if (FALSE) { # \dontrun{
rba_reactome_analysis_pdf(token = "MjAyMDEwMTYwMTI3MTNfMjY1MjM%3D",
    species = 9606, save_to = "my_analysis.pdf")
} # }
```
