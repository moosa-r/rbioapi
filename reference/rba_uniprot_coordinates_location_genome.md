# Get Genome coordinate by Gene Sequence position

Using this function you can retrieve genome coordinates of a given
UniProt protein by providing Genome location position or range. You can
either supply 'g_position' alone or supply 'g_start' and 'g_end'
together.

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

  NIH-NCBI [Taxon ID](https://www.uniprot.org/taxonomy/). You can supply
  up to 20 taxon IDs.

- chromosome:

  (Character or Numeric): Chromosome name, e.g. 1, 20, X.

- g_position:

  (numeric) Genome location position

- g_start:

  (numeric) Genome location position start

- g_end:

  (numeric) Genome location position end

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

"GET https://ebi.ac.uk/proteins/api/coordinates/glocation
/{accession}:{pPosition}"  
"GET https://ebi.ac.uk/proteins/api/coordinates/glocation
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
[`rba_uniprot_coordinates_location_protein()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_coordinates_location_protein.md),
[`rba_uniprot_coordinates_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_coordinates_search.md)

## Examples

``` r
# \donttest{
 rba_uniprot_coordinates_location_genome(
 taxid = 9606, chromosome = 11, g_position = 36573305)
# }
```
