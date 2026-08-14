# Search Genomic Coordinates of UniProt entries

Search [genomic
coordinates](https://www.uniprot.org/help/genomic_coordinates)
associated with UniProt entries by accession, chromosome, Ensembl
identifier, gene, protein, taxonomy, or genomic range.

## Usage

``` r
rba_uniprot_coordinates_search(
  accession = NULL,
  chromosome = NULL,
  ensembl_id = NULL,
  gene = NULL,
  protein = NULL,
  taxid = NULL,
  location = NULL,
  ...
)
```

## Arguments

- accession:

  Character: (optional) [UniProtKB primary or secondary
  accession](https://www.uniprot.org/help/accession_numbers)(s). You can
  supply up to 100 accession numbers.

- chromosome:

  Character or Numeric: (optional) Chromosome name, such as "X", "Y", 1,
  or 20. You can supply up to 20 values.

- ensembl_id:

  Character: (optional) Ensembl stable gene ID, transcript ID, or
  translation ID. You can supply up to 20 IDs.

- gene:

  Character: (optional) [UniProt gene
  name(s)](https://www.uniprot.org/help/gene_name). You can supply up to
  20 gene names.

- protein:

  Character: (optional) [UniProt protein
  name](https://www.uniprot.org/help/protein_names).

- taxid:

  Numeric: (optional) NIH-NCBI [Taxon
  ID](https://www.uniprot.org/taxonomy/). You can supply up to 20 taxon
  IDs.

- location:

  Character: (optional) Genome location range, such as
  "58205437-58219305".

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list named by accession. Each element contains one matching protein's
genomic-coordinate information.

## Details

At least one search criterion is required.

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/coordinates"

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
[`rba_uniprot_coordinates_location_protein()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_coordinates_location_protein.md)

## Examples

``` r
# \donttest{
rba_uniprot_coordinates_search(taxid = 9606, chromosome = "y")
# }
```
