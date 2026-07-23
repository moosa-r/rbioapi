# Get Similarity Scores Hits of Proteins in a Species

Using this function, you can retrieve the Smith-Waterman bit scores
among proteins of the same species. Bit Scores serve as similarity
scores between protein sequence; And, according to STRING
documentations, as a proxy for protein homology.

## Usage

``` r
rba_string_homology_intra(ids, species = NULL, ...)
```

## Arguments

- ids:

  Your protein ID(s). It is strongly recommended to supply STRING IDs.
  See
  [`rba_string_map_ids`](https://rbioapi.moosa-r.com/reference/rba_string_map_ids.md)
  for more information.

- species:

  Numeric: NCBI Taxonomy identifier; Human Taxonomy ID is 9606.
  (Recommended, but optional if your input is less than 100 IDs.)

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A data frame with bit scores between your supplied proteins and their
self-hit. To Reduce the transferred data, STRING returns only one half
of the similarity matrix; This will not pose a problem because
similarity matrix is symmetrical.

## Details

Note that this function will retrieve similarity scores of different
proteins "within the same species". To Get a similarity scores of a
given protein and it's closets homologous proteins in other species, see
[`rba_string_homology_inter`](https://rbioapi.moosa-r.com/reference/rba_string_homology_inter.md).  
Similarity matrix is imported -by STRING- from: [Similarity Matrix of
Proteins (SIMAP)](https://pubmed.ncbi.nlm.nih.gov/24165881/)

## Corresponding API Resources

"POST https://string-db.org/api/{output-format}/homology?identifiers=
{your_identifiers}"

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

[`rba_string_map_ids`](https://rbioapi.moosa-r.com/reference/rba_string_map_ids.md)`, `[`rba_string_homology_inter`](https://rbioapi.moosa-r.com/reference/rba_string_homology_inter.md)

Other "STRING":
[`rba_string_annotations()`](https://rbioapi.moosa-r.com/reference/rba_string_annotations.md),
[`rba_string_enrichment()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment.md),
[`rba_string_enrichment_image()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_image.md),
[`rba_string_enrichment_ppi()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_ppi.md),
[`rba_string_homology_inter()`](https://rbioapi.moosa-r.com/reference/rba_string_homology_inter.md),
[`rba_string_interaction_partners()`](https://rbioapi.moosa-r.com/reference/rba_string_interaction_partners.md),
[`rba_string_interactions_network()`](https://rbioapi.moosa-r.com/reference/rba_string_interactions_network.md),
[`rba_string_map_ids()`](https://rbioapi.moosa-r.com/reference/rba_string_map_ids.md),
[`rba_string_network_image()`](https://rbioapi.moosa-r.com/reference/rba_string_network_image.md),
[`rba_string_version()`](https://rbioapi.moosa-r.com/reference/rba_string_version.md)

## Examples

``` r
# \donttest{
rba_string_homology_intra(ids = c("CDK1", "CDK2"), species = 9606)
# }
```
