# Maps Molecule Identifiers

Use this function to map molecule identifiers of different species to
Reactome Identifiers.

## Usage

``` r
rba_reactome_analysis_mapping(
  input,
  input_format = NULL,
  projection = TRUE,
  interactors = FALSE,
  ...
)
```

## Arguments

- input:

  Character or Numeric vector: A vector, local file path or URL that
  points to your identifiers list.

- input_format:

  Character: (optional) This function will automatically identify your
  supplied input's format. But in case of unexpected issues or if you
  want to be explicit, set this argument to one of:

  - "vector": If you supplied a simple vector (numeric or character) as
    input.

  - "file": If you supplied a local file path pointing to a
    correctly-formatted text file.

  - "url": If you supplied a URL pointing to a correctly-formatted text
    file.

- projection:

  Logical: (default = `TRUE`) Should non-human identifiers be projected
  to their human equivalents? (using Reactome orthology data)

- interactors:

  Logical: (default = `FALSE`) Should IntAct interaction data be
  included?

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

List containing your identifiers and the IDS and resources they are
mapped to.

## Corresponding API Resources

"POST https://reactome.org/AnalysisService/mapping"  
"POST https://reactome.org/AnalysisService/mapping/projection"  
"POST https://reactome.org/AnalysisService/mapping/url"  
"POST https://reactome.org/AnalysisService/mapping/url/projection"

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
[`rba_reactome_analysis_import()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_import.md),
[`rba_reactome_analysis_pdf()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_pdf.md),
[`rba_reactome_analysis_species()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_species.md),
[`rba_reactome_analysis_token()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_token.md)

## Examples

``` r
if (FALSE) { # \dontrun{
rba_reactome_analysis_mapping(c("Q8SQ34", "cd40"))
} # }
```
