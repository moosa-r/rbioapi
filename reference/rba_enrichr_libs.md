# Retrieve a List of available libraries from Enrichr

This function retrieves a list of libraries available in Enrichr along
with their associated statistics. Each library represents a collection
of gene sets that can be used for enrichment analysis.

## Usage

``` r
rba_enrichr_libs(organism = "human", store_in_options = TRUE, ...)
```

## Arguments

- organism:

  Character: (default = `"human"`) Which model organism version of
  Enrichr to use? Available options are: "human", (H. sapiens & M.
  musculus), "fly" (D. melanogaster), "yeast" (S. cerevisiae), "worm"
  (C. elegans) and "fish" (D. rerio).

- store_in_options:

  Logical: (default = `TRUE`) Should a list of available Enrichr
  libraries be saved as a global option?

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A data frame with the names of available library in Enrichr and their
statistics.

## Details

By default, this function will save the library names as a global option
("rba_enrichr_libs") for other Enrichr functions that internally require
the names of Enrichr libraries. You should call this function once per R
session with the argument 'store_in_options = TRUE' before using
[`rba_enrichr_gene_sets`](https://rbioapi.moosa-r.com/reference/rba_enrichr_gene_sets.md),
[`rba_enrichr`](https://rbioapi.moosa-r.com/reference/rba_enrichr.md) or
[`rba_enrichr_enrich`](https://rbioapi.moosa-r.com/reference/rba_enrichr_enrich.md).
However, if you do not explicitly call it, rbioapi will automatically
execute this function in the background the when it is needed.

Please note that
[`rba_enrichr`](https://rbioapi.moosa-r.com/reference/rba_enrichr.md)
provides a one-step and more convenient way to automatically handle this
and other required function calls needed to perform gene set enrichment
analysis with Enrichr.

## Corresponding API Resources

"GET https://maayanlab.cloud/Enrichr/datasetStatistics"

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
[`rba_enrichr_enrich()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_enrich.md),
[`rba_enrichr_gene_map()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_gene_map.md),
[`rba_enrichr_gene_sets()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_gene_sets.md),
[`rba_enrichr_view_list()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_view_list.md)

## Examples

``` r
# \donttest{
rba_enrichr_libs()
# }
```
