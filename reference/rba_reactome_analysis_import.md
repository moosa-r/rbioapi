# Import Saved Analysis JSON to Reactome

If you have a JSON file of analysis results (only obtained via
[`rba_reactome_analysis_download`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_download.md)
with the result argument set to "results", or "results_gz"), you can
import the results back to Reactome and retrieve a token.  
This is useful when you want to use other Reactome services which
require a token but you do not have a token or your token has been
expired (i.e. more than 7 days passed from your analysis).

## Usage

``` r
rba_reactome_analysis_import(input, input_format = NULL, ...)
```

## Arguments

- input:

  Character: A local file path or URL that points to your -optionally
  gzipped- JSON file.

- input_format:

  Character: (optional) This function will automatically identify your
  supplied input's format. But in case of unexpected issues or if you
  want to be explicit, set this argument to one of:

  - "file": If you supplied a local file path pointing to the JSON file.

  - "url": If you supplied a URL pointing to the JSON file.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list containing the new token and other information of your imported
results.

## Corresponding API Resources

"POST https://reactome.org/AnalysisService/import/"  
"POST https://reactome.org/AnalysisService/import/url"

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

Other "Reactome Analysis Service":
[`rba_reactome_analysis()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis.md),
[`rba_reactome_analysis_download()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_download.md),
[`rba_reactome_analysis_mapping()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_mapping.md),
[`rba_reactome_analysis_pdf()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_pdf.md),
[`rba_reactome_analysis_species()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_species.md),
[`rba_reactome_analysis_token()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_token.md)

## Examples

``` r
if (FALSE) { # \dontrun{
rba_reactome_analysis_import("c:/rbioapi/res.json")
} # }
if (FALSE) { # \dontrun{
rba_reactome_analysis_import("https://qaz.com/res.json.gz")
} # }
```
