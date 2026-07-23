# Upload Background Gene-List to Enrichr

In addition to the main gene list, you can also submit a background gene
list to Enrichr. This gene list can be used later to compute the
statistics of the enrichment analysis.

## Usage

``` r
rba_enrichr_add_background(background_genes, ...)
```

## Arguments

- background_genes:

  A character vector of Entrez gene symbols of the background genes.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list with the unique IDs for your uploaded background gene list.

## Details

Please note that
[`rba_enrichr`](https://rbioapi.moosa-r.com/reference/rba_enrichr.md)
provides a one-step and more convenient way to automatically handle this
and other required function calls needed to perform gene set enrichment
analysis with Enrichr.

## Corresponding API Resources

"POST https://maayanlab.cloud/speedrichr/api/addbackground"

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
[`rba_enrichr_add_list()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_add_list.md),
[`rba_enrichr_enrich()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_enrich.md),
[`rba_enrichr_gene_map()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_gene_map.md),
[`rba_enrichr_gene_sets()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_gene_sets.md),
[`rba_enrichr_libs()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_libs.md),
[`rba_enrichr_view_list()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_view_list.md)

## Examples

``` r
# \donttest{
my_background_genes <- c(
"NSUN3", "POLRMT", "NLRX1", "SFXN5", "ZC3H12C", "SLC25A39", "ARSG",
"DEFB29", "PCMTD2", "ACAA1A", "LRRC1", "2810432D09RIK", "SEPHS2",
"SAC3D1", "TMLHE", "LOC623451", "TSR2", "PLEKHA7", "GYS2", "ARHGEF12",
"HIBCH", "LYRM2", "ZBTB44", "ENTPD5", "RAB11FIP2", "LIPT1",
"INTU", "ANXA13", "KLF12", "SAT2", "GAL3ST2", "VAMP8", "FKBPL",
"AQP11", "TRAP1", "PMPCB", "TM7SF3", "RBM39", "BRI3", "KDR", "ZFP748",
"NAP1L1", "DHRS1", "LRRC56", "WDR20A", "STXBP2", "KLF1", "UFC1",
"CCDC16", "9230114K14RIK", "RWDD3", "2610528K11RIK")

rba_enrichr_add_background(background_genes = my_background_genes)
# }
```
