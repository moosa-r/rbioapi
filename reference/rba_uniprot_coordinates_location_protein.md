# Map Protein Sequence Positions to Genomic Coordinates

Map an amino-acid position or range in a UniProt protein sequence to its
corresponding [genomic
coordinates](https://www.uniprot.org/help/genomic_coordinates). A
protein sequence location may have more than one genomic mapping. Supply
`p_position` alone, or supply `p_start` and `p_end` together.

## Usage

``` r
rba_uniprot_coordinates_location_protein(
  accession,
  p_position = NULL,
  p_start = NULL,
  p_end = NULL,
  ...
)
```

## Arguments

- accession:

  Character: [UniProtKB primary or secondary
  accession](https://www.uniprot.org/help/accession_numbers).

- p_position:

  Numeric: (optional) Protein sequence position. Supply this alone, or
  supply both `p_start` and `p_end`.

- p_start:

  Numeric: (optional) Protein sequence range start.

- p_end:

  Numeric: (optional) Protein sequence range end.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list with a `locations` element containing the mapped protein and
genomic boundaries. Records can include chromosome, strand, genome
assembly, nucleotide and Ensembl identifiers, amino acids, and mapped
sequence features.

## Corresponding API Resources

"GET
https://www.ebi.ac.uk/proteins/api/coordinates/location/{accession}:{pPosition}"  
"GET
https://www.ebi.ac.uk/proteins/api/coordinates/location/{accession}:{pStart}-{pEnd}"

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
[`rba_uniprot_coordinates_location()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_coordinates_location.md),
[`rba_uniprot_coordinates_location_genome()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_coordinates_location_genome.md),
[`rba_uniprot_coordinates_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_coordinates_search.md)

## Examples

``` r
# \donttest{
rba_uniprot_coordinates_location_protein(accession = "P25942", p_position = 1)
# }
# \donttest{
rba_uniprot_coordinates_location_protein(accession = "P25942",
    p_start = 1, p_end = 277)
# }
```
