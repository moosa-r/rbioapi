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

  A vector, local file path or URL that points to your identifiers list.

- input_format:

  (Optional) This function will automatically identify your supplied
  input's format. But in case of unexpected issues or if you want to be
  explicit, set this argument to one of:

  - "vector": If you supplied a simple vector (numeric or character) as
    input.

  - "file": If you supplied a local file path pointing to a
    correctly-formatted text file.

  - "url": If you supplied a URL pointing to a correctly-formatted text
    file.

- projection:

  Logical (default = TRUE) Should non-human identifiers be projected to
  their human equivalents? (using Reactome orthology data)

- interactors:

  Logical (default = FALSE) Should IntAct interaction data be included?

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

List containing your identifiers and the IDS and resources they are
mapped to.

## Corresponding API Resources

"GET https://reactome.org/AnalysisService/mapping"  
"GET https://reactome.org/AnalysisService/mapping/form"  
"GET https://reactome.org/AnalysisService/mapping/form/projection"  
"GET https://reactome.org/AnalysisService/mapping"  
"GET https://reactome.org/AnalysisService/mapping/url"  
"GET https://reactome.org/AnalysisService/mapping/url/projection"

## References

- Marc Gillespie, Bijay Jassal, Ralf Stephan, Marija Milacic, Karen
  Rothfels, Andrea Senff-Ribeiro, Johannes Griss, Cristoffer Sevilla,
  Lisa Matthews, Chuqiao Gong, Chuan Deng, Thawfeek Varusai, Eliot
  Ragueneau, Yusra Haider, Bruce May, Veronica Shamovsky, Joel Weiser,
  Timothy Brunson, Nasim Sanati, Liam Beckman, Xiang Shao, Antonio
  Fabregat, Konstantinos Sidiropoulos, Julieth Murillo, Guilherme
  Viteri, Justin Cook, Solomon Shorser, Gary Bader, Emek Demir, Chris
  Sander, Robin Haw, Guanming Wu, Lincoln Stein, Henning Hermjakob,
  Peter D’Eustachio, The reactome pathway knowledgebase 2022, Nucleic
  Acids Research, 2021;, kab1028, https://doi.org/10.1093/nar/gkab1028

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
