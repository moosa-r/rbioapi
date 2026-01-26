# Get UniParc Longest Sequence for Entries

This function returns the UniParc Entry with a cross-reference to the
longest active UniProtKB sequence (preferably from Swiss-Prot and if not
then TrEMBL). If it finds more than one longest active UniProtKB
sequence it returns 400 (Bad Request) error response with the list of
cross references found.

## Usage

``` r
rba_uniprot_uniparc_bestguess(
  upi = NULL,
  accession = NULL,
  db_id = NULL,
  gene = NULL,
  taxid = NULL,
  ...
)
```

## Arguments

- upi:

  unique UniParc Identifier.

- accession:

  [UniProtKB primary or secondary
  accession](https://www.uniprot.org/help/accession_numbers)(s). You can
  supply up to 100 accession numbers.

- db_id:

  Protein ID in the cross-reference (external) database. You can supply
  up to 100 IDs.

- gene:

  [UniProt gene name(s)](https://www.uniprot.org/help/gene_name). You
  can supply up to 20 gene names.

- taxid:

  NIH-NCBI [Taxon ID](https://www.uniprot.org/taxonomy/). You can supply
  up to 20 taxon IDs.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list where each element correspond to a UniParc entry.

## Corresponding API Resources

"GET https://ebi.ac.uk/proteins/api/uniparc/bestguess"

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

Other "UniProt - UniParc":
[`rba_uniprot_uniparc()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_uniparc.md),
[`rba_uniprot_uniparc_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_uniparc_search.md),
[`rba_uniprot_uniparc_sequence()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_uniparc_sequence.md)

## Examples

``` r
# \donttest{
rba_uniprot_uniparc_bestguess("UPI00000000C9")
# }
```
