# Search Genomic Coordinates of UniProt entries

Use this function to search genomic coordinates of UniProt entries. You
may also refine your search with modifiers such as chromosome, taxon id
etc. See "Arguments section" for more information.

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

  [UniProtKB primary or secondary
  accession](https://www.uniprot.org/help/accession_numbers)(s). You can
  supply up to 100 accession numbers.

- chromosome:

  chromosome name, such as "X", "Y", 1, 20, etc. You can supply up to 20
  values.

- ensembl_id:

  Ensembl Stable gene ID, transcript ID or translation ID. You can
  supply up to 20 IDs.

- gene:

  [UniProt gene name(s)](https://www.uniprot.org/help/gene_name). You
  can supply up to 20 gene names.

- protein:

  [UniProt protein name](https://www.uniprot.org/help/protein_names)

- taxid:

  NIH-NCBI [Taxon ID](https://www.uniprot.org/taxonomy/). You can supply
  up to 20 taxon IDs.

- location:

  Genome location range such as "58205437-58219305"

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

List where each element corresponds to one UniProt entity returned by
your search query. The element itself is a sub-list containing that
protein's coordinates information.

## Details

Note that this is a search function. Thus, you are not required to fill
every argument; You may use whatever combinations of arguments you see
fit for your query.  
For more information about how UniProt imports and calculates genomic
coordinates data, see:  
McGarvey, P. B., Nightingale, A., Luo, J., Huang, H., Martin, M. J., Wu,
C., & UniProt Consortium (2019). UniProt genomic mapping for deciphering
functional effects of missense variants. Human mutation, 40(6), 694–705.
https://doi.org/10.1002/humu.23738

## Corresponding API Resources

"GET https://ebi.ac.uk/proteins/api/coordinates"

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
[`rba_uniprot_coordinates_location_protein()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_coordinates_location_protein.md)

## Examples

``` r
# \donttest{
rba_uniprot_coordinates_search(taxid = 9606, chromosome = "y")
# }
```
