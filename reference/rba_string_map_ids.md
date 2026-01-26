# Map a Set of Identifiers to STRING Identifiers

This function Calls STRING's API to Convert a set of identifiers to
STRING Identifiers. Although You can call STRING services with a variety
of common identifiers, It is recommended by STRING's documentations that
you first map Your Protein/genes IDs to STRING IDs and then proceed with
other STRING's functions.

## Usage

``` r
rba_string_map_ids(ids, species = NULL, echo_query = FALSE, limit = NULL, ...)
```

## Arguments

- ids:

  Your Common gene/protein Identifier(s) to be mapped.

- species:

  Numeric: NCBI Taxonomy identifier; Human Taxonomy ID is 9606.
  (Recommended, but optional if your input is less than 100 IDs.)

- echo_query:

  (default = FALSE) Include your input IDs as a column of the results.

- limit:

  (Numeric, Optional) A limit on the number of matches per input ID. The
  output are sorted to have the best matches first.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A data frame with the mapped STRING IDs and other pertinent information.

## Corresponding API Resources

"POST
https://string-db.org/api/{output-format}/get_string_ids?identifiers=
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
