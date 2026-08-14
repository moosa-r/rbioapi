# Get Genomic Coordinates of a Protein

Retrieve [genomic
coordinates](https://www.uniprot.org/help/genomic_coordinates) for a
protein using either its UniProt accession or its ID in a
cross-reference database (Ensembl, CCDS, HGNC, or RefSeq). You should
supply either `accession` alone or `db_type` and `db_id` together.

## Usage

``` r
rba_uniprot_coordinates(accession = NULL, db_type = NULL, db_id = NULL, ...)
```

## Arguments

- accession:

  Character: (optional) [UniProtKB primary or secondary
  accession](https://www.uniprot.org/help/accession_numbers).

- db_type:

  Character: (optional) Cross-reference database name. One of "Ensembl",
  "CCDS", "HGNC", or "RefSeq".

- db_id:

  Character: (optional) Protein identifier in the cross-reference
  database.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list containing the requested protein's genomic coordinates.

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/coordinates/{accession}"  
"GET https://www.ebi.ac.uk/proteins/api/coordinates/{dbtype}:{dbid}"

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
