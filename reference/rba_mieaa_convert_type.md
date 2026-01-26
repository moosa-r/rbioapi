# Convert Between Mature and precursor miRNA Accession

miRBase miRNA accession could refer to either mature or precursor
miRNAs. (see: [A uniform system for microRNA
annotation](https://rnajournal.cshlp.org/content/9/3/277)). Use this
function to mature miRNA accession to corresponding miRNA accessions or
vice versa.

## Usage

``` r
rba_mieaa_convert_type(
  mirna,
  input_type,
  only_unique = FALSE,
  simple_output = FALSE,
  ...
)
```

## Arguments

- mirna:

  A vector of miRNA accessions to be converted.

- input_type:

  Type of your supplied miRNA accession. either "mature" or "precursor".

- only_unique:

  (logical) miRBase precursor and mature miRNA accessions are not
  uniquely mapped. (i.e. you may get more than one results for a given
  accession). set this to TRUE to only retrieve the unique mappings.
  (default = FALSE)

- simple_output:

  (logical) If FALSE (default), the result will be a two-columned data
  frame with your input and output accessions. Otherwise, if TRUE, only
  the output miRNA accessions will be returned.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

Depending on the arguments, a data frame or a character vectors
containing the miRNA accessions in your output version.

## Corresponding API Resources

"POST
https://ccb-compute2.cs.uni-saarland.de/mieaa2/api/v1/mirna_precursor_converter/"

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
[`rba_mieaa_cats()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_cats.md),
[`rba_mieaa_convert_version()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_convert_version.md),
[`rba_mieaa_enrich()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich.md),
[`rba_mieaa_enrich_results()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich_results.md),
[`rba_mieaa_enrich_status()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich_status.md),
[`rba_mieaa_enrich_submit()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich_submit.md)

## Examples

``` r
# \donttest{
Sys.sleep(1) # to prevent 429 error during R CMD check
rba_mieaa_convert_type(mirna = c("hsa-miR-20b-5p", "hsa-miR-144-5p"),
    input_type = "mature")
# }
```
