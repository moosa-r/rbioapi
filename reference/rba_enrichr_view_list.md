# View an Uploaded Gene List

Retrieve the list of uploaded genes with a given 'user list ID'.

## Usage

``` r
rba_enrichr_view_list(
  user_list_id,
  organism = "human",
  speedrichr = FALSE,
  ...
)
```

## Arguments

- user_list_id:

  a user list ID returned after uploading a gene list using
  [`rba_enrichr_add_list`](https://rbioapi.moosa-r.com/reference/rba_enrichr_add_list.md)

- organism:

  (default = "human") Which model organism version of Enrichr to use?
  Available options are: "human", (H. sapiens & M. musculus), "fly" (D.
  melanogaster), "yeast" (S. cerevisiae), "worm" (C. elegans) and "fish"
  (D. rerio).

- speedrichr:

  logical (default = FALSE) Did you upload your gene list to speedrichr
  API? (i.e. did you intend to use this gene list along with a
  background gene list?)

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list containing the genes and description associated to the supplied
user_list_id.

## Corresponding API Resources

"GET https://maayanlab.cloud/Enrichr/view"  
"GET https://maayanlab.cloud/speedrichr/api/view"

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
[`rba_enrichr()`](https://rbioapi.moosa-r.com/reference/rba_enrichr.md),
[`rba_enrichr_add_background()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_add_background.md),
[`rba_enrichr_add_list()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_add_list.md),
[`rba_enrichr_enrich()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_enrich.md),
[`rba_enrichr_gene_map()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_gene_map.md),
[`rba_enrichr_gene_sets()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_gene_sets.md),
[`rba_enrichr_libs()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_libs.md)

## Examples

``` r
if (FALSE) { # \dontrun{
rba_enrichr_view_list(user_list_id = 11111)
} # }
```
