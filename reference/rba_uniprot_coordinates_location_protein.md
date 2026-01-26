# Get Genome coordinate by Protein Sequence position

Using this function you can retrieve genome coordinates of a given
UniProt protein by providing protein position or position range. You can
either supply 'p_position' alone or supply 'p_start' and 'p_end'
together.

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

  [UniProtKB primary or secondary
  accession](https://www.uniprot.org/help/accession_numbers).

- p_position:

  (numeric) Protein sequence position

- p_start:

  (numeric) Protein sequence position start

- p_end:

  (numeric) Protein sequence position end

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

Genome coordinates of your supplied proteins.

## Details

For more information about how UniProt imports and calculates genomic
coordinates data, see:  
McGarvey, P. B., Nightingale, A., Luo, J., Huang, H., Martin, M. J., Wu,
C., & UniProt Consortium (2019). UniProt genomic mapping for deciphering
functional effects of missense variants. Human mutation, 40(6), 694–705.
https://doi.org/10.1002/humu.23738

## Corresponding API Resources

"GET https://ebi.ac.uk/proteins/api/coordinates/location
/{accession}:{pPosition}"  
"GET https://ebi.ac.uk/proteins/api/coordinates/location
/{accession}:{pStart}-{pEnd}"

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
