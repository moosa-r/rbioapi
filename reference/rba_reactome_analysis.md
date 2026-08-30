# Reactome Over-Representation or Expression Analysis

Using this function, you can perform Reactome Analysis In a convenient
way. The Analysis Type will be chosen depending on your supplied input:

1.  If you supply a vector or a single-columned table,
    "Over-Representation" analysis will be performed.

2.  If you supply a multi-column table, with the first column being
    molecules identifiers and the rest being numeral expression values,
    "Expression" analysis will be performed.

See the details section for the accepted input types and format.

## Usage

``` r
rba_reactome_analysis(
  input,
  input_format = NULL,
  projection = TRUE,
  interactors = FALSE,
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

- input:

  Character, Numeric, Data frame or Matrix: A vector, data frame, matrix
  or a local file path or URL that points to your data. See "Details
  section" for more information of how to organize and supply your
  input.

- input_format:

  Character: (optional) This function will automatically identify your
  supplied input's format. But in case of unexpected issues or if you
  want to be explicit, set this argument to one of:

  - "table": If you supplied a data frame or matrix as input.

  - "vector": If you supplied a simple vector (numeric or character) as
    input.

  - "file": If you supplied a local file path pointing to a
    correctly-formatted text file.

  - "url": If you supplied an HTTP or HTTPS URL pointing to a
    correctly-formatted text file.

  An explicit value takes precedence. Otherwise, HTTP and HTTPS
  addresses are identified first, followed by existing local files,
  tables, and then other non-empty character or numeric inputs as
  identifier vectors.

- projection:

  Logical: (default = `TRUE`) Should non-human identifiers be projected
  to their human equivalents? (using Reactome orthology data)

- interactors:

  Logical: (default = `FALSE`) Should IntAct interaction data be used to
  increase the analysis background?

- species:

  Character or Numeric: (optional) NCBI Taxonomy identifier (Human is
  9606), species name (e.g. "Homo sapiens") or Reactome DbId (e.g Homo
  sapiens is 48887). See
  [`rba_reactome_species`](https://rbioapi.moosa-r.com/reference/rba_reactome_species.md)
  or [Reactome Data Schema: Entries:
  Species](https://reactome.org/content/schema/objects/Species/). Note
  that you cannot supply the species parameter when projection parameter
  is TRUE.

- sort_by:

  Character: (default = `"ENTITIES_PVALUE"`) Sort the result based on
  what column? Available choices are: "NAME", "TOTAL_ENTITIES",
  "TOTAL_INTERACTORS", "TOTAL_REACTIONS", "FOUND_ENTITIES",
  "FOUND_INTERACTORS", "FOUND_REACTIONS", "ENTITIES_RATIO",
  "ENTITIES_PVALUE", "ENTITIES_FDR" or "REACTIONS_RATIO"

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

A list containing the results and information about the analysis. The
`pathways` element is a data frame with information about each pathway
expanded into columns; it is an empty data frame when no pathways match.
The token in `results$summary$token` can be used to retrieve the results
later or in other Reactome analysis functions.

## Details

You can supply your table or vector input in numerous formats:

1.  An R object which can be a data frame, matrix, or simple vector.

2.  A path to a local text file in your device that contains the
    molecules data. (The file should be formatted correctly, see below.)

3.  An HTTP or HTTPS URL pointing to a text file on the web that
    contains the molecules data. (The file should be formatted
    correctly, see below.)

If you supply a text file (as a local file path or URL), it should be in
TSV (Tab-Separated Values) format; the first column name should start
with "#". Note that if you are providing the file for
"Over-Representation" analysis (i.e. Single columned-data) this header
line is optional and will be used as your 'Sample Name', otherwise it is
required.  
Also, form the "summary" element in the function's output, you can see
how Reactome Interpreted your input and subsequently the type of
analysis that has been performed.  
There is no strict criteria about the type of your molecules
Identifiers, Reactome will Map the IDs to it's internal database
entities. Nevertheless, You can check if all your identifiers has been
found in "identifiersNotFound" element in the function's output.  
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

"POST https://reactome.org/AnalysisService/identifiers/form"  
"POST
https://reactome.org/AnalysisService/identifiers/form/projection"  
"POST https://reactome.org/AnalysisService/identifiers/url"  
"POST https://reactome.org/AnalysisService/identifiers/url/projection"

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

Other "Reactome Analysis Service":
[`rba_reactome_analysis_download()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_download.md),
[`rba_reactome_analysis_import()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_import.md),
[`rba_reactome_analysis_mapping()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_mapping.md),
[`rba_reactome_analysis_pdf()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_pdf.md),
[`rba_reactome_analysis_species()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_species.md),
[`rba_reactome_analysis_token()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_token.md)

Other "Enrichment/Over-representation":
[`rba_enrichr()`](https://rbioapi.moosa-r.com/reference/rba_enrichr.md),
[`rba_mieaa_enrich()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich.md),
[`rba_panther_enrich()`](https://rbioapi.moosa-r.com/reference/rba_panther_enrich.md),
[`rba_string_enrichment()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment.md),
[`rba_string_enrichment_image()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_image.md)

## Examples

``` r
if (FALSE) { # \dontrun{
rba_reactome_analysis(input = c("p53", "BRCA1", "cdk2", "Q99835", "CDC42"))
} # }
if (FALSE) { # \dontrun{
rba_reactome_analysis(input = "c:/rbioapi/genes.txt")
} # }
if (FALSE) { # \dontrun{
rba_reactome_analysis(input = "https://example.com/genes.txt")
} # }
```
