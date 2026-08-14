# Search UniParc Entries

Use this function to search [UniProt Archive
(UniParc)](https://www.uniprot.org/help/uniparc) entries. Search by
identifier, annotation, organism, sequence properties, or other
supported criteria. The `rf_*` arguments filter the cross-references
returned within matching entries; they do not select entries by
themselves.

## Usage

``` r
rba_uniprot_uniparc_search(
  upi = NULL,
  accession = NULL,
  db_type = NULL,
  db_id = NULL,
  gene = NULL,
  protein = NULL,
  taxid = NULL,
  organism = NULL,
  sequence_checksum = NULL,
  ipr = NULL,
  signature_db = NULL,
  signature_id = NULL,
  upid = NULL,
  seq_length = NULL,
  rf_dd_type = NULL,
  rf_db_id = NULL,
  rf_active = NULL,
  rf_tax_id = NULL,
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

- db_type:

  Character: (optional) [Cross-reference
  database](https://www.uniprot.org/database/) name.

- db_id:

  Character: (optional) Protein ID in a cross-reference database. You
  can supply up to 100 IDs.

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

- organism:

  Character: (optional) [Organism
  name](https://www.uniprot.org/taxonomy/).

- sequence_checksum:

  Character: (optional) A 16-character hexadecimal sequence CRC64
  checksum.

- ipr:

  Character: (optional) [InterPro
  identifier(s)](https://www.ebi.ac.uk/interpro/about/interpro/). You
  can supply up to 20 IDs.

- signature_db:

  Character: (optional) InterPro [signature
  database](https://interpro-documentation.readthedocs.io/en/latest/databases.html).
  You can supply up to 20 values.

- signature_id:

  Character: (optional) Signature ID in an InterPro [signature
  database](https://interpro-documentation.readthedocs.io/en/latest/databases.html).
  You can supply up to 20 IDs.

- upid:

  Character: (optional) [UniProt Proteome identifier
  (UPID)](https://www.uniprot.org/help/proteome_id). You can supply up
  to 100 UPIDs.

- seq_length:

  Character or Numeric: (optional) An exact sequence length (e.g. 150)
  or a range of sequence lengths (e.g. "130-158").

- rf_dd_type:

  Character: (optional) Filter each UniParc entry's content by
  [cross-reference](https://www.uniprot.org/database/) names. You can
  supply multiple values.

- rf_db_id:

  Character: (optional) Filter each UniParc entry's content by protein
  identifiers in any cross-reference database. You can supply multiple
  values.

- rf_active:

  Logical: (optional) Filter each UniParc entry's content by active
  status in the source database: `TRUE` retains active database
  references, `FALSE` retains inactive references, and `NULL` applies no
  active-status filter.

- rf_tax_id:

  Numeric: (optional) Filter each UniParc entry's content by NIH-NCBI
  [Taxon ID](https://www.uniprot.org/taxonomy/). You can supply multiple
  values.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list named by UniParc accession. Each element contains sequence
information and cross-reference entries for one search hit.

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/uniparc"

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
[`rba_uniprot_uniparc_bestguess()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_uniparc_bestguess.md),
[`rba_uniprot_uniparc_sequence()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_uniparc_sequence.md)

## Examples

``` r
# \donttest{
rba_uniprot_uniparc_search(upi = "UPI00000000C9")
# }
# \donttest{
rba_uniprot_uniparc_search(accession = "P30914")
# }
# \donttest{
rba_uniprot_uniparc_search(accession = "P30914", rf_active = TRUE)
# }
# \donttest{
rba_uniprot_uniparc_search(taxid = 694009, protein = "Nucleoprotein")
# }
```
