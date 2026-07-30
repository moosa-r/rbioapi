# Get Supported Enrichment Categories for a Species and miRNA Type

Each combination of species and miRNA type supports a predefined set of
enrichment categories. This function retrieves the categories available
for a given combination.

## Usage

``` r
rba_mieaa_cats(mirna_type, species, mode = "all", ...)
```

## Arguments

- mirna_type:

  Character: Type of the miRNA identifiers; either "mature" or
  "precursor".

- species:

  Character or Numeric: Scientific name, abbreviation, or NCBI taxon ID
  of one of the following species:

  1.  "Homo sapiens", "hsa" or 9606

  2.  "Mus musculus", "mmu" or 10090

  3.  "Rattus norvegicus", "rno" or 10116

  4.  "Arabidopsis thaliana", "ath" or 3702

  5.  "Bos taurus", "bta" or 9913

  6.  "Caenorhabditis elegans", "cel" or 6239

  7.  "Drosophila melanogaster", "dme" or 7227

  8.  "Danio rerio", "dre" or 7955

  9.  "Gallus gallus", "gga" or 9031

  10. "Sus scrofa", "ssc" or 9823

- mode:

  Character: (default = `"all"`) Category subset to retrieve. One of:
  "all" to include default and expert categories, "default" to include
  only default categories, or "expert" to include only expert
  categories.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A named character vector whose values are the supported category
identifiers and whose names are their descriptions. If the selected
subset has no categories, returns `character(0)`.

## Corresponding API Resources

"GET
https://ccb-compute2.cs.uni-saarland.de/mieaa/api/v1/enrichment_categories/{species}/{mirna_type}/{mode}"

## References

- Ernesto Aparicio-Puerta, Pascal Hirsch, Georges P. Schmartz, Fabian
  Kern, Tobias Fehlmann, Andreas Keller, miEAA 2023: updates, new
  functional microRNA sets and improved enrichment visualizations,
  Nucleic Acids Research, Volume 51, Issue W1, 5 July 2023, Pages
  W319–W325, https://doi.org/10.1093/nar/gkad392

- [miEAA browsable API
  tutorial](https://ccb-compute2.cs.uni-saarland.de/mieaa/tutorial/api/)

- [Citation note on miEAA
  website](https://ccb-compute2.cs.uni-saarland.de/mieaa/)

## See also

Other "miEAA":
[`rba_mieaa_convert_type()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_convert_type.md),
[`rba_mieaa_convert_version()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_convert_version.md),
[`rba_mieaa_enrich()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich.md),
[`rba_mieaa_enrich_results()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich_results.md),
[`rba_mieaa_enrich_status()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich_status.md),
[`rba_mieaa_enrich_submit()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich_submit.md)

## Examples

``` r
# \donttest{
rba_mieaa_cats("mature", "Homo sapiens")
# }
```
