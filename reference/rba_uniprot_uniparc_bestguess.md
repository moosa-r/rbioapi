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

  Character: (optional) Unique UniParc identifier(s). You can supply up
  to 100 IDs.

- accession:

  Character: (optional) [UniProtKB primary or secondary
  accession](https://www.uniprot.org/help/accession_numbers)(s). You can
  supply up to 100 accession numbers.

- db_id:

  Character: (optional) Protein ID in a cross-reference database. You
  can supply up to 100 IDs.

- gene:

  Character: (optional) [UniProt gene
  name(s)](https://www.uniprot.org/help/gene_name). You can supply up to
  20 gene names.

- taxid:

  Numeric: (optional) NIH-NCBI [Taxon
  ID](https://www.uniprot.org/taxonomy/) used to refine the search. You
  can supply up to 20 taxon IDs.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

The best matching UniParc entry.

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/uniparc/bestguess"

## References

- The UniProt Consortium. (2025). UniProt: the Universal Protein
  Knowledgebase in 2025. Nucleic Acids Research, 53(D1), D609–D617.
  https://doi.org/10.1093/nar/gkae1010

- Nightingale, A., Antunes, R., Alpi, E., Bursteinas, B., Gonzales, L.,
  Liu, W., Luo, J., Qi, G., Turner, E., & Martin, M. (2017). The
  Proteins API: Accessing key integrated protein and genome information.
  Nucleic Acids Research, 45(W1), W539–W544.
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
