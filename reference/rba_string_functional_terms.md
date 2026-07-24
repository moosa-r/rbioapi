# Search STRING Functional Terms

STRING maps several functional annotation resources onto its proteins.
This function searches for functional terms using an identifier or
descriptive text and retrieves the matching terms and their annotated
proteins.

## Usage

``` r
rba_string_functional_terms(term_text, species, ...)
```

## Arguments

- term_text:

  Character: A functional term identifier or descriptive text used to
  match one or more functional terms.

- species:

  Numeric: [NCBI Taxonomy
  identifier](https://www.ncbi.nlm.nih.gov/taxonomy/); Human Taxonomy ID
  is 9606.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A data frame in which every row is a matching functional term and the
columns contain the term category, identifier, description, number of
annotated proteins, preferred protein names, and STRING protein IDs.
`preferredNames` and `stringIds` are returned as list-columns.

## Details

This endpoint supports only one species per query. If multiple
functional terms match `term_text`, STRING returns them in order of
relevance, with the best match first.  
The complete number of annotated proteins is reported in `proteinCount`.

## Corresponding API Resources

"POST
https://string-db.org/api/{output-format}/functional_terms?term_text=
{your_term}&{optional_parameters}"

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

` `[`rba_string_annotations`](https://rbioapi.moosa-r.com/reference/rba_string_annotations.md)`, `[`rba_string_enrichment`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment.md)`, `[`rba_string_enrichment_image`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_image.md)` `

Other "STRING":
[`rba_string_annotations()`](https://rbioapi.moosa-r.com/reference/rba_string_annotations.md),
[`rba_string_enrichment()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment.md),
[`rba_string_enrichment_image()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_image.md),
[`rba_string_enrichment_ppi()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_ppi.md),
[`rba_string_homology_inter()`](https://rbioapi.moosa-r.com/reference/rba_string_homology_inter.md),
[`rba_string_homology_intra()`](https://rbioapi.moosa-r.com/reference/rba_string_homology_intra.md),
[`rba_string_interaction_partners()`](https://rbioapi.moosa-r.com/reference/rba_string_interaction_partners.md),
[`rba_string_interactions_network()`](https://rbioapi.moosa-r.com/reference/rba_string_interactions_network.md),
[`rba_string_map_ids()`](https://rbioapi.moosa-r.com/reference/rba_string_map_ids.md),
[`rba_string_network_image()`](https://rbioapi.moosa-r.com/reference/rba_string_network_image.md),
[`rba_string_version()`](https://rbioapi.moosa-r.com/reference/rba_string_version.md)

## Examples

``` r
# \donttest{
rba_string_functional_terms(
    term_text = "T cell receptor signaling pathway",
    species = 9606
)
# }
```
