# Search UniProt entries by taxonomy and genomic coordinates

Retrieve UniProt entries or mapped protein features for a taxon and
supplied [genomic
coordinates](https://www.uniprot.org/help/genomic_coordinates).

## Usage

``` r
rba_uniprot_coordinates_location(
  taxid,
  locations,
  in_range = TRUE,
  feature = FALSE,
  ...
)
```

## Arguments

- taxid:

  Numeric: NIH-NCBI [Taxon ID](https://www.uniprot.org/taxonomy/).

- locations:

  Character: Genomic location formatted as chromosome:start-end. (e.g.
  "Y:17100001-19600000"). If you omit chromosome, it will be interpreted
  as any chromosome (e.g. "1-10000").

- in_range:

  Logical: (default = `TRUE`) If `TRUE`, return only proteins that are
  fully contained in the supplied range.

- feature:

  Logical: (default = `FALSE`) If `TRUE`, return mapped protein features
  rather than protein coordinate records.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list containing UniProt proteins that match the supplied genomic
location and taxonomy ID.

## Corresponding API Resources

"GET
https://www.ebi.ac.uk/proteins/api/coordinates/{taxonomy}/{locations}/feature"  
"GET
https://www.ebi.ac.uk/proteins/api/coordinates/{taxonomy}/{locations}"

## References

- The UniProt Consortium. (2025). UniProt: the Universal Protein
  Knowledgebase in 2025. Nucleic Acids Research, 53(D1), D609–D617.
  https://doi.org/10.1093/nar/gkae1010

- Nightingale, A., Antunes, R., Alpi, E., Bursteinas, B., Gonzales, L.,
  Liu, W., Luo, J., Qi, G., Turner, E., & Martin, M. (2017). The
  Proteins API: Accessing key integrated protein and genome information.
  Nucleic Acids Research, 45(W1), W539–W544.
  https://doi.org/10.1093/nar/gkx237

- McGarvey, P. B., Nightingale, A., Luo, J., Huang, H., Martin, M. J.,
  Wu, C., & The UniProt Consortium. (2019). UniProt genomic mapping for
  deciphering functional effects of missense variants. Human Mutation,
  40(6), 694–705. https://doi.org/10.1002/humu.23738

- [Proteins API Documentation](https://www.ebi.ac.uk/proteins/api/doc/)

- [Citations note on UniProt
  website](https://www.uniprot.org/help/publications)

## See also

Other "UniProt - Coordinates":
[`rba_uniprot_coordinates()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_coordinates.md),
[`rba_uniprot_coordinates_location_genome()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_coordinates_location_genome.md),
[`rba_uniprot_coordinates_location_protein()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_coordinates_location_protein.md),
[`rba_uniprot_coordinates_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_coordinates_search.md)

## Examples

``` r
# \donttest{
rba_uniprot_coordinates_location(taxid = 9606,
    locations = "Y:17100001-19600000", in_range = TRUE)
# }
# \donttest{
rba_uniprot_coordinates_location(taxid = 9606,
    locations = "20:39000001", in_range = FALSE)
# }
```
