# Map a Set of Identifiers to STRING Identifiers

This function calls STRING's API to map a set of common gene or protein
identifiers to STRING identifiers. Although STRING services accept a
variety of identifiers, the STRING API documentation recommends mapping
them to STRING identifiers before using other STRING functions.

## Usage

``` r
rba_string_map_ids(ids, species = NULL, echo_query = TRUE, limit = NULL, ...)
```

## Arguments

- ids:

  Character or Numeric: Your common gene/protein identifier(s) to be
  mapped.

- species:

  Numeric: (optional) [NCBI Taxonomy
  identifier](https://www.ncbi.nlm.nih.gov/taxonomy/); Human Taxonomy ID
  is 9606. (Recommended, but optional.)

- echo_query:

  Logical: (default = `TRUE`) Include your input IDs as a column of the
  results.

- limit:

  Numeric: (optional) Deprecated: Retained temporarily for backward
  compatibility. STRING v12 returns only the single best match per input
  ID, so this argument has no effect.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A data frame with at most one mapped STRING ID per input ID and other
pertinent information. The `queryIndex` column contains the zero-based
position of each resolved ID in the input vector. Unresolved inputs are
omitted; if none can be resolved, a zero-row data frame retaining the
response columns is returned.

## Corresponding API Resources

"POST https://string-db.org/api/tsv/get_string_ids?identifiers=
{your_identifiers}&{optional_parameters}"

## References

- Damian Szklarczyk, Rebecca Kirsch, Mikaela Koutrouli, Katerina Nastou,
  Farrokh Mehryary, Radja Hachilif, Annika L Gable, Tao Fang, Nadezhda T
  Doncheva, Sampo Pyysalo, Peer Bork, Lars J Jensen, Christian von
  Mering, The STRING database in 2023: protein–protein association
  networks and functional enrichment analyses for any sequenced genome
  of interest, Nucleic Acids Research, Volume 51, Issue D1, 6 January
  2023, Pages D638–D646, https://doi.org/10.1093/nar/gkac1000

- [STRING API Documentation](https://string-db.org/help/api/)

- [Citations note on STRING
  website](https://string-db.org/cgi/about?footer_active_subpage=references)

## See also

Other "STRING":
[`rba_string_annotations()`](https://rbioapi.moosa-r.com/reference/rba_string_annotations.md),
[`rba_string_enrichment()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment.md),
[`rba_string_enrichment_image()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_image.md),
[`rba_string_enrichment_ppi()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_ppi.md),
[`rba_string_functional_terms()`](https://rbioapi.moosa-r.com/reference/rba_string_functional_terms.md),
[`rba_string_homology_inter()`](https://rbioapi.moosa-r.com/reference/rba_string_homology_inter.md),
[`rba_string_homology_intra()`](https://rbioapi.moosa-r.com/reference/rba_string_homology_intra.md),
[`rba_string_interaction_partners()`](https://rbioapi.moosa-r.com/reference/rba_string_interaction_partners.md),
[`rba_string_interactions_network()`](https://rbioapi.moosa-r.com/reference/rba_string_interactions_network.md),
[`rba_string_network_image()`](https://rbioapi.moosa-r.com/reference/rba_string_network_image.md),
[`rba_string_version()`](https://rbioapi.moosa-r.com/reference/rba_string_version.md)

## Examples

``` r
# \donttest{
rba_string_map_ids(ids = c("TP53", "TNF", "EGFR"), species = 9606)
# }
```
