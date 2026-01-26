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

List with the results of the comparison.

## Details

Reactome incorporate manually curated human reactions and PANTHER's
protein homology data to Computationally infer events in other
eukaryotic species.  
In version 73 (11 June 2020), using an orthology-based approach, Homo
sapiens events was projected to 18,654 orthologous pathways (with 81,835
orthologous proteins) in 15 non-human species. See [Reactome
Computationally Inferred
Events](https://reactome.org/documentation/inferred-events/) for more
information.

## Corresponding API Resources

"GET https://reactome.org/AnalysisService/species/homoSapiens/{species}"

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
