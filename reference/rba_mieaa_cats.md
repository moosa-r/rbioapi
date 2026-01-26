# Get Supported Enrichment Categories for a Species and miRNA Type

For each Combination of species and miRNA type, Only a pre-defined
categories groups are supported. Use this function to retrieve a list of
supported categories for a given combination of Species and miRNA type.

## Usage

``` r
rba_mieaa_cats(mirna_type, species, ...)
```

## Arguments

- mirna_type:

  Type of your miRNA accession. either "mature" or "precursor".

- species:

  Fully or partially matching Scientific name, abbreviation or NCBI
  taxon ID of one of the following species:

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

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

a named character vector with the supported categories for your supplied
input combination.

## Corresponding API Resources

"GET
https://ccb-compute2.cs.uni-saarland.de/mieaa2/api/v1/enrichment_categories/{species}/{mirna_type}/"

## References

- Fabian Kern, Tobias Fehlmann, Jeffrey Solomon, Louisa Schwed, Nadja
  Grammes, Christina Backes, Kendall Van Keuren-Jensen, David Wesley
  Craig,Eckart Meese, Andreas Keller, miEAA 2.0: integrating
  multi-species microRNA enrichment analysis and workflow management
  systems, Nucleic Acids Research, Volume 48, Issue W1, 02 July 2020,
  Pages W521–W528, https://doi.org/10.1093/nar/gkaa309

- [miEAA browsable API
  tutorial](https://ccb-compute2.cs.uni-saarland.de/mieaa2/tutorial/api/)

- [Citations note on miEAA
  website](https://ccb-compute2.cs.uni-saarland.de/mieaa2/)

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
