# Download Different Reactome Analysis Results

Based on the "request" argument, you can download different analysis
results data associated with a given token.

## Usage

``` r
rba_reactome_analysis_download(
  token,
  request,
  save_to = NULL,
  resource = "TOTAL",
  ...
)
```

## Arguments

- token:

  Character: A token associated to your previous Reactome analysis.

- request:

  Character: What to download? Should be one of:

  - "found_ids": Download a CSV file containing the found user-supplied
    identifiers in the analysis associated with your supplied token and
    resource.

  - "not_found_ids"" Download a CSV file containing the user-supplied
    Identifiers which has not been found in the analysis associated with
    your supplied token.

  - "pathways": Download a CSV file containing Pathway analysis results
    of the analysis associated with your supplied token and resource.

  - "results": Download a JSON file containing the complete analysis
    results associated with your supplied token.

  - "results_gz" Same as "results", but the output will be compress
    (gzipped).

- save_to:

  Character: (optional)

  - NULL: Save the file to an automatically-generated path.

  - Character string: A valid file path to save the file to.

- resource:

  Character: (default = `"TOTAL"`) (Only when request is "found_ids" or
  "pathways") Filter results based on the resource. Available choices
  are:"TOTAL", "UNIPROT", "ENSEMBL", "CHEBI", "IUPHAR", "MIRBASE",
  "NCBI_PROTEIN", "EMBL", "COMPOUND" or "PUBCHEM_COMPOUND".

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

NULL, a CSV,JSON or Gzipped JSON file will be saved to disk based on
your input.

## Details

Token is associated to each Reactome analysis results and kept by
Reactome for at least 7 days. You can locate it in
[`rba_reactome_analysis`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis.md)'s
output, under a sub-list named "summary" (i.e.
results\$summary\$token).  
Use
[`rba_reactome_analysis_pdf`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_pdf.md)
to save a full report in PDF format.

## Corresponding API Resources

GET https://reactome.org/AnalysisService/download/{token}/entities/
found/{resource}/{filename}.csv"  
GET https://reactome.org/AnalysisService/download/{token}/entities/
notfound/{filename}.csv"  
GET https://reactome.org/AnalysisService/download/{token}/pathways/
{resource}/{filename}.csv"  
GET https://reactome.org/AnalysisService/download/{token}/result.json"  
GET
https://reactome.org/AnalysisService/download/{token}/result.json.gz"

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

[`rba_reactome_analysis_pdf`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_pdf.md)
[`rba_reactome_analysis`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis.md)

Other "Reactome Analysis Service":
[`rba_reactome_analysis()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis.md),
[`rba_reactome_analysis_import()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_import.md),
[`rba_reactome_analysis_mapping()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_mapping.md),
[`rba_reactome_analysis_pdf()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_pdf.md),
[`rba_reactome_analysis_species()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_species.md),
[`rba_reactome_analysis_token()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_token.md)

## Examples

``` r
if (FALSE) { # \dontrun{
rba_reactome_analysis_download(token = "MjAyMDEwMTYwMTI3MTNfMjY1MjM",
    request = "pathways", save_to = "found_ids.csv")
} # }
if (FALSE) { # \dontrun{
rba_reactome_analysis_download(token = "MjAyMDEwMTYwMTI3MTNfMjY1MjM",
    request = "found_ids", save_to = "found_ids.csv")
} # }
```
