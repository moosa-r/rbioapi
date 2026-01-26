# Search UniProt entries by taxonomy and genomic coordinates

For more information about how UniProt imports and calculates genomic
coordinates data, see:  
McGarvey, P. B., Nightingale, A., Luo, J., Huang, H., Martin, M. J., Wu,
C., & UniProt Consortium (2019). UniProt genomic mapping for deciphering
functional effects of missense variants. Human mutation, 40(6), 694–705.
https://doi.org/10.1002/humu.23738

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

  NIH-NCBI [Taxon ID](https://www.uniprot.org/taxonomy/).

- locations:

  genomic location formatted as: chromosome:start-end. (e.g.
  "Y:17100001-19600000"). If you omit chromosome, it will be interpreted
  as any chromosome (e.g. "1-10000").

- in_range:

  Only return proteins that are in range.

- feature:

  (logical) Get features?

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

a list containing UniProt proteins which match the supplied genomic
location and taxonomy ID.

## Corresponding API Resources

"GET https://ebi.ac.uk/proteins/api/coordinates
/{taxonomy}/{locations}/feature"  
"GET https://ebi.ac.uk/proteins/api/coordinates /{taxonomy}/{locations}"

## References

- The UniProt Consortium , UniProt: the Universal Protein Knowledgebase
  in 2025, Nucleic Acids Research, 2024;, gkae1010,
  https://doi.org/10.1093/nar/gkae1010

- Andrew Nightingale, Ricardo Antunes, Emanuele Alpi, Borisas
  Bursteinas, Leonardo Gonzales, Wudong Liu, Jie Luo, Guoying Qi, Edd
  Turner, Maria Martin, The Proteins API: accessing key integrated
  protein and genome information, Nucleic Acids Research, Volume 45,
  Issue W1, 3 July 2017, Pages W539–W544,
  https://doi.org/10.1093/nar/gkx237

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
