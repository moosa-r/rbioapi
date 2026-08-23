# A One-step Wrapper for Gene-list Enrichment Using Enrichr

This function provides a convenient one-step wrapper for performing
enrichment analysis on a given gene list using Enrichr. It simplifies
the process by internally calling the necessary functions in the correct
order. See the details section for more information.

## Usage

``` r
rba_enrichr(
  gene_list,
  description = NULL,
  gene_set_library = "all",
  regex_library_name = FALSE,
  organism = "human",
  background_genes = NULL,
  progress_bar = TRUE,
  ...
)
```

## Arguments

- gene_list:

  A character vector with Entrez gene symbols of test genes.

- description:

  (optional) A description to be associated with your uploaded gene-set
  to Enrichr servers.

- gene_set_library:

  One of the:

  1.  "all" to select all of the available Enrichr gene-set libraries.

  2.  A gene-set library name. You can retrieve the available options
      for a given species using
      [`rba_enrichr_libs`](https://rbioapi.moosa-r.com/reference/rba_enrichr_libs.md).

  3.  If regex_library_name = TRUE, A partially-matching name a regex
      pattern that correspond to one or more of Enrichr library names.

- regex_library_name:

  logical: (default = FALSE) if TRUE the supplied gene_set_library will
  be considered as a regex pattern. If FALSE, gene_set_library will be
  considered as an exact match.

- organism:

  (default = "human") Which model organism version of Enrichr to use?
  Available options are: "human", (H. sapiens & M. musculus), "fly" (D.
  melanogaster), "yeast" (S. cerevisiae), "worm" (C. elegans) and "fish"
  (D. rerio).

- background_genes:

  A character vector of Entrez gene symbols of the background genes.

- progress_bar:

  logical: (default = TRUE) if multiple Enrichr libraries are selected,
  should a progress bar be displayed?

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list containing data frames of the enrichment results of your supplied
gene-list against the selected Enrichr libraries.

## Details

This function will call other rba_enrichr\_\*\*\* functions with the
following order:

1.  (If necessary) Call
    [`rba_enrichr_libs`](https://rbioapi.moosa-r.com/reference/rba_enrichr_libs.md)
    to obtain a list of available libraries in Enrichr for the given
    organism.

2.  Call
    [`rba_enrichr_add_list`](https://rbioapi.moosa-r.com/reference/rba_enrichr_add_list.md)
    to upload your gene-list and obtain a 'user list ID'.

3.  (If necessary) Call
    [`rba_enrichr_add_background`](https://rbioapi.moosa-r.com/reference/rba_enrichr_add_background.md)
    to upload your background gene-list and obtain a 'background list
    ID'.

4.  Call
    [`rba_enrichr_enrich`](https://rbioapi.moosa-r.com/reference/rba_enrichr_enrich.md)
    to perform enrichment analysis on the gene-list against one or
    multiple Enrichr libraries

## Corresponding API Resources

"GET https://maayanlab.cloud/Enrichr/datasetStatistics"  
"POST https://maayanlab.cloud/Enrichr/addList"  
"POST https://maayanlab.cloud/speedrichr/api/addList"  
"POST https://maayanlab.cloud/speedrichr/api/addbackground"  
"GET https://maayanlab.cloud/Enrichr/export"  
"POST https://maayanlab.cloud/speedrichr/api/backgroundenrich"

## References

- Chen, E.Y., Tan, C.M., Kou, Y. et al. Enrichr: interactive and
  collaborative HTML5 gene list enrichment analysis tool. Bioinformatics
  14, 128 (2013). https://doi.org/10.1186/1471-2105-14-128

- Maxim V. Kuleshov, Matthew R. Jones, Andrew D. Rouillard, Nicolas F.
  Fernandez, Qiaonan Duan, Zichen Wang, Simon Koplev, Sherry L. Jenkins,
  Kathleen M. Jagodnik, Alexander Lachmann, Michael G. McDermott,
  Caroline D. Monteiro, Gregory W. Gundersen, Avi Ma’ayan, Enrichr: a
  comprehensive gene set enrichment analysis web server 2016 update,
  Nucleic Acids Research, Volume 44, Issue W1, 8 July 2016, Pages
  W90–W97, https://doi.org/10.1093/nar/gkw377

- Xie, Z., Bailey, A., Kuleshov, M. V., Clarke, D. J. B.,
  Evangelista, J. E., Jenkins, S. L., Lachmann, A., Wojciechowicz, M.
  L., Kropiwnicki, E., Jagodnik, K. M., Jeon, M., & Ma’ayan, A. (2021).
  Gene set knowledge discovery with Enrichr. Current Protocols, 1, e90.
  doi: 10.1002/cpz1.90

- [Enrichr API Documentation](https://maayanlab.cloud/Enrichr/help#api)

- [Citations note on Enrichr
  website](https://maayanlab.cloud/Enrichr/help#terms)

## See also

Other "Enrichr":
[`rba_enrichr_add_background()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_add_background.md),
[`rba_enrichr_add_list()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_add_list.md),
[`rba_enrichr_enrich()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_enrich.md),
[`rba_enrichr_gene_map()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_gene_map.md),
[`rba_enrichr_gene_sets()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_gene_sets.md),
[`rba_enrichr_libs()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_libs.md),
[`rba_enrichr_view_list()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_view_list.md)

Other "Enrichment/Over-representation":
[`rba_mieaa_enrich()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich.md),
[`rba_panther_enrich()`](https://rbioapi.moosa-r.com/reference/rba_panther_enrich.md),
[`rba_reactome_analysis()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis.md),
[`rba_string_enrichment()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment.md),
[`rba_string_enrichment_image()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_image.md)

## Examples

``` r
if (FALSE) { # \dontrun{
rba_enrichr(gene_list = c("TP53", "TNF", "EGFR"))
} # }
# \donttest{
rba_enrichr(gene_list = c("TP53", "TNF", "EGFR"),
    gene_set_library = "GO_Molecular_Function_2025",
    regex_library_name = FALSE)
# }
# \donttest{
rba_enrichr(gene_list = c("TP53", "TNF", "EGFR"),
    gene_set_library = "^GO_Molecular_Function_(2025|2026)$",
    regex_library_name = TRUE)
# }
```
