# Retrieve Gene Sets From an Enrichr Library

This function retrieves all gene sets in a given Enrichr library. If a
term is supplied, only the gene set associated with that term will be
returned.

## Usage

``` r
rba_enrichr_gene_sets(gene_set_library, term = NULL, organism = "human", ...)
```

## Arguments

- gene_set_library:

  A valid gene-set library name which exists in the results retrieved
  via
  [`rba_enrichr_libs`](https://rbioapi.moosa-r.com/reference/rba_enrichr_libs.md).

- term:

  (optional) An exact Enrichr term name. If left NULL, all gene sets in
  the supplied library will be returned.

- organism:

  (default = "human") Which model organism version of Enrichr to use?
  Available options are: "human", (H. sapiens & M. musculus), "fly" (D.
  melanogaster), "yeast" (S. cerevisiae), "worm" (C. elegans) and "fish"
  (D. rerio).

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A named list of character vectors. Each element name is an Enrichr term
and the corresponding character vector contains the genes in that gene
set. If the `save_file` option was set to `TRUE` or a valid path, the
raw GMT file will also be saved to the requested path.

## Details

If the available Enrichr library names are not already stored in the
"rba_enrichr_libs" global option, this function will retrieve and store
them before validating the supplied library name.

## Corresponding API Resources

"GET https://maayanlab.cloud/Enrichr/geneSetLibrary"

## Saving Gene Sets as a GMT File

To save the raw server's response as a GMT file, supply the `save_file`
rbioapi option through `...`. Set it to `TRUE` to automatically generate
a proper file path, or supply a character string with a valid file or
directory path. See
[`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
arguments manual for more information on available options. The organism
will be included in the automatically-generated file name. If a term is
supplied, its name will also be included.

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
[`rba_enrichr_libs()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_libs.md),
[`rba_enrichr_view_list()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_view_list.md)

## Examples

``` r
# \donttest{
rba_enrichr_gene_sets(
    gene_set_library = "Reactome_Pathways_2024",
    term = "Signaling by NOTCH")
# }
# \donttest{
rba_enrichr_gene_sets(gene_set_library = "Reactome_Pathways_2024")
# }
if (FALSE) { # \dontrun{
rba_enrichr_gene_sets(
    gene_set_library = "Reactome_Pathways_2024",
    save_file = "Reactome_Pathways_2024.gmt")
} # }
```
