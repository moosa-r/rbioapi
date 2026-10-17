# Get Best Protein Similarity Hits Across Species

Retrieve the highest Smith-Waterman bit-score hit between each input
protein and proteins in every other STRING species. STRING uses these
sequence-similarity scores as a proxy for protein homology.

## Usage

``` r
rba_string_homology_inter(ids, species = NULL, species_b = NULL, ...)
```

## Arguments

- ids:

  Character or Numeric: Your protein ID(s). It is strongly recommended
  to supply STRING IDs. See
  [`rba_string_map_ids`](https://rbioapi.moosa-r.com/reference/rba_string_map_ids.md)
  for more information.

- species:

  Numeric: (optional) [NCBI Taxonomy
  identifier](https://www.ncbi.nlm.nih.gov/taxonomy/) of your input
  proteins; Human Taxonomy ID is 9606. (Recommended, but required if
  your input contains more than 10 unique IDs.)

- species_b:

  Numeric: (optional) One or more [NCBI Taxonomy
  identifiers](https://www.ncbi.nlm.nih.gov/taxonomy/) used to restrict
  the search for closest homologs. The default is `NULL`, which searches
  all STRING species.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A data frame containing each input protein and its closest homolog in
every other STRING species, or in the species selected by `species_b`.

## Details

To retrieve pairwise similarity scores among input proteins within one
species, see
[`rba_string_homology_intra`](https://rbioapi.moosa-r.com/reference/rba_string_homology_intra.md).

STRING imports the similarity matrix from the [Similarity Matrix of
Proteins (SIMAP)](https://doi.org/10.1093/nar/gkt970) project.

## Corresponding API Resources

"POST https://string-db.org/api/{output-format}/homology_best?
identifiers={your_identifiers}&{optional_parameters}"

## References

- Damian Szklarczyk, Katerina Nastou, Mikaela Koutrouli, Rebecca Kirsch,
  Farrokh Mehryary, Radja Hachilif, Dewei Hu, Matteo E Peluso, Qingyao
  Huang, Tao Fang, Nadezhda T Doncheva, Sampo Pyysalo, Peer Bork, Lars J
  Jensen, Christian von Mering, The STRING database in 2025: protein
  networks with directionality of regulation, Nucleic Acids Research,
  Volume 53, Issue D1, 6 January 2025, Pages D730–D737,
  https://doi.org/10.1093/nar/gkae1113

- [STRING API Documentation](https://string-db.org/help/api/)

- [Citations note on STRING
  website](https://string-db.org/cgi/about?footer_active_subpage=references)

## See also

` `[`rba_string_map_ids`](https://rbioapi.moosa-r.com/reference/rba_string_map_ids.md)`, `[`rba_string_homology_intra`](https://rbioapi.moosa-r.com/reference/rba_string_homology_intra.md)` `

Other "STRING":
[`rba_string_annotations()`](https://rbioapi.moosa-r.com/reference/rba_string_annotations.md),
[`rba_string_enrichment()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment.md),
[`rba_string_enrichment_image()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_image.md),
[`rba_string_enrichment_ppi()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_ppi.md),
[`rba_string_functional_terms()`](https://rbioapi.moosa-r.com/reference/rba_string_functional_terms.md),
[`rba_string_homology_intra()`](https://rbioapi.moosa-r.com/reference/rba_string_homology_intra.md),
[`rba_string_interaction_partners()`](https://rbioapi.moosa-r.com/reference/rba_string_interaction_partners.md),
[`rba_string_interactions_network()`](https://rbioapi.moosa-r.com/reference/rba_string_interactions_network.md),
[`rba_string_map_ids()`](https://rbioapi.moosa-r.com/reference/rba_string_map_ids.md),
[`rba_string_network_image()`](https://rbioapi.moosa-r.com/reference/rba_string_network_image.md),
[`rba_string_version()`](https://rbioapi.moosa-r.com/reference/rba_string_version.md)

## Examples

``` r
# \donttest{
## Find the best hit in mouse and fruit fly for the human input protein
rba_string_homology_inter(ids = "CDK1",
    species = 9606,
    species_b = c(10090, 7227))
# }
# \donttest{
rba_string_homology_inter(ids = "ENSP00000269305", species = 9606)
# }
```
