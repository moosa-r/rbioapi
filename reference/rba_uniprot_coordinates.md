# Get Genomic Coordinates of a Protein

Using this function you can retrieve genomic Coordinates of a Protein by
either providing the protein's UniProt accession or it's ID in a
cross-reference database (Ensembl, CCDC, HGNC or RefSeq). You should
supply either 'accession' alone or 'db_type' and 'db_id' together.

## Usage

``` r
rba_uniprot_coordinates(accession = NULL, db_type = NULL, db_id = NULL, ...)
```

## Arguments

- accession:

  [UniProtKB primary or secondary
  accession](https://www.uniprot.org/help/accession_numbers).

- db_type:

  cross-reference database name, Should be one of: "Ensembl", "CCDC",
  "HGNC" or "RefSeq".

- db_id:

  Protein's ID in the cross-reference database

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list with genome coordinates of your supplied protein.

## Details

For more information about how UniProt imports and calculates genomic
coordinates data, see:  
McGarvey, P. B., Nightingale, A., Luo, J., Huang, H., Martin, M. J., Wu,
C., & UniProt Consortium (2019). UniProt genomic mapping for deciphering
functional effects of missense variants. Human mutation, 40(6), 694–705.
https://doi.org/10.1002/humu.23738

## Corresponding API Resources

"GET https://ebi.ac.uk/proteins/api/coordinates/{accession}"  
"GET https://ebi.ac.uk/proteins/api/coordinates/{dbtype}:{dbid}"

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
[`rba_uniprot_coordinates_location()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_coordinates_location.md),
[`rba_uniprot_coordinates_location_genome()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_coordinates_location_genome.md),
[`rba_uniprot_coordinates_location_protein()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_coordinates_location_protein.md),
[`rba_uniprot_coordinates_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_coordinates_search.md)

## Examples

``` r
# \donttest{
rba_uniprot_coordinates(accession = "P25942")
# }
# \donttest{
rba_uniprot_coordinates(db_type = "HGNC", db_id = "CD40")
# }
```
