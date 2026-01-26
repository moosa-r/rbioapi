# Get Enrichr Enrichment Results

This function retrieves enrichment analysis results for your supplied
\`user_list_id\` against one or multiple Enrichr libraries.

## Usage

``` r
rba_enrichr_enrich(
  user_list_id,
  gene_set_library = "all",
  regex_library_name = FALSE,
  organism = "human",
  background_id = NULL,
  progress_bar = TRUE,
  ...
)
```

## Arguments

- user_list_id:

  An ID returned after uploading a gene list using
  [`rba_enrichr_add_list`](https://rbioapi.moosa-r.com/reference/rba_enrichr_add_list.md),
  with the \`speedrichr\` set to TRUE or FALSE depending on whether you
  intend to analyze this gene list with or without a background gene
  list, respectively.

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
  (D. rerio). If \`background_id\` is provided, the only available
  option is "human".

- background_id:

  An ID returned after uploading a background gene list using
  [`rba_enrichr_add_background`](https://rbioapi.moosa-r.com/reference/rba_enrichr_add_background.md)

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

If \`background_id\` is supplied, this function will interact with the
speedrichr API. In this case, \`user_list_id\` must have been obtained
from a
[`rba_enrichr_add_list`](https://rbioapi.moosa-r.com/reference/rba_enrichr_add_list.md)
call with the \`speedrichr\` parameter set to \`TRUE\`. Additionally,
this feature is only available for "human" organism.

Please note that
[`rba_enrichr`](https://rbioapi.moosa-r.com/reference/rba_enrichr.md)
provides a one-step and more convenient way to automatically handle this
and other required function calls needed to perform gene set enrichment
analysis with Enrichr.

## Corresponding API Resources

"GET https://maayanlab.cloud/Enrichr/enrich"  
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

[`rba_enrichr`](https://rbioapi.moosa-r.com/reference/rba_enrichr.md)

Other "Enrichr":
[`rba_enrichr()`](https://rbioapi.moosa-r.com/reference/rba_enrichr.md),
[`rba_enrichr_add_background()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_add_background.md),
[`rba_enrichr_add_list()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_add_list.md),
[`rba_enrichr_gene_map()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_gene_map.md),
[`rba_enrichr_libs()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_libs.md),
[`rba_enrichr_view_list()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_view_list.md)

## Examples

``` r
if (FALSE) { # \dontrun{
rba_enrichr_enrich(user_list_id = 11111)
} # }
if (FALSE) { # \dontrun{
rba_enrichr_enrich(user_list_id = 11111,
    gene_set_library = "GO_Molecular_Function_2017",
    regex_library_name = FALSE)
} # }
if (FALSE) { # \dontrun{
rba_enrichr_enrich(user_list_id = 11111,
    gene_set_library = "go",
    regex_library_name = TRUE)
} # }
```
