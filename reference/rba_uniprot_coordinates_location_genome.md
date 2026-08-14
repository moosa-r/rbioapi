# Map Genomic Coordinates to Protein Sequence Positions

Map a genomic position or range within a chromosome and taxon to the
corresponding UniProt protein sequence locations. A genomic location may
match multiple proteins, isoforms, or transcript mappings. Supply
`g_position` alone, or supply `g_start` and `g_end` together.

## Usage

``` r
rba_uniprot_coordinates_location_genome(
  taxid,
  chromosome,
  g_position = NULL,
  g_start = NULL,
  g_end = NULL,
  ...
)
```

## Arguments

- taxid:

  Numeric: NIH-NCBI [Taxon ID](https://www.uniprot.org/taxonomy/).

- chromosome:

  Character or Numeric: Chromosome name, e.g. 1, 20, or X.

- g_position:

  Numeric: (optional) Genomic position. Supply this alone, or supply
  both `g_start` and `g_end`.

- g_start:

  Numeric: (optional) Genomic range start.

- g_end:

  Numeric: (optional) Genomic range end.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list with a `locations` element containing the matching UniProt
protein and genomic mappings. Records can include protein positions,
amino acids, transcript and translation identifiers, chromosome, strand,
genome assembly, and mapped sequence features.

## Corresponding API Resources

"GET
https://www.ebi.ac.uk/proteins/api/coordinates/glocation/{taxonomy}/{chromosome}:{gPosition}"  
"GET
https://www.ebi.ac.uk/proteins/api/coordinates/glocation/{taxonomy}/{chromosome}:{gStart}-{gEnd}"

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
[`rba_uniprot_coordinates_location_protein()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_coordinates_location_protein.md),
[`rba_uniprot_coordinates_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_coordinates_search.md)

## Examples

``` r
# \donttest{
 rba_uniprot_coordinates_location_genome(
 taxid = 9606, chromosome = 11, g_position = 36573305)
# }
```
