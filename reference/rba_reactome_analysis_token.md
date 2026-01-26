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
  species,
  sort_by = "ENTITIES_PVALUE",
  order = "ASC",
  resource = "TOTAL",
  p_value = NULL,
  include_disease = TRUE,
  min = NULL,
  max = NULL,
  ...
)
```

## Arguments

- token:

  A token associated to your previous Reactome analysis.

- species:

  Numeric or Character: NCBI Taxonomy identifier (Human is 9606),
  species name (e.g. "Homo sapiens") or Reactome DbId (e.g Homo sapiens
  is 48887). See
  [`rba_reactome_species`](https://rbioapi.moosa-r.com/reference/rba_reactome_species.md)
  or [Reactome Data Schema: Entries:
  Species](https://reactome.org/content/schema/objects/Species/).

- sort_by:

  Sort the result based on what column? available choices are: "NAME",
  "TOTAL_ENTITIES", "TOTAL_INTERACTORS", "TOTAL_REACTIONS",
  "FOUND_ENTITIES", "FOUND_INTERACTORS", "FOUND_REACTIONS",
  "ENTITIES_RATIO", "ENTITIES_PVALUE", "ENTITIES_FDR" or
  "REACTIONS_RATIO"

- order:

  Sort Order. Can be either "ASC" (default) or "DESC".

- resource:

  Filter results based on the resource. Default is "TOTAL", available
  choices are: "TOTAL", "UNIPROT", "ENSEMBL", "CHEBI", "IUPHAR",
  "MIRBASE", "NCBI_PROTEIN", "EMBL", "COMPOUND", "ENTITIES_FDR" or
  "PUBCHEM_COMPOUND".

- p_value:

  Set a P value threshold. Only results with P value equal to or less
  than your supplied threshold will be returned. (default = 1, Meaning
  no P value filtering)

- include_disease:

  Logical (default = TRUE) Should the disease pathways be included in
  the results?

- min:

  (numeric) Minimum number of entities that a pathways should have to be
  included in the results.

- max:

  (numeric) Maximum number of entities that a pathways should have to be
  included in the results.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

List containing the results and information of your analysis.

## Details

After Any Analysis, Reactome will associate a token to your analysis. It
can be later used to in function that requires the token (e.g to
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
