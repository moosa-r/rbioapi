# Search UniParc Entries

Use this function to search [UniProt Archive
(UniParc)](https://www.uniprot.org/help/uniparc) entries.You may also
refine your search with modifiers such as sequence length, taxon id etc.
See "Arguments section" for more information.

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

  unique UniParc Identifier(s). You can supply up to 100 IDs.

- accession:

  [UniProtKB primary or secondary
  accession](https://www.uniprot.org/help/accession_numbers)(s). You can
  supply up to 100 accession numbers.

- db_type:

  [cross-reference](https://www.uniprot.org/database/) (external
  database) name.

- db_id:

  Protein ID in the cross-reference (external) database. You can supply
  up to 100 IDs.

- gene:

  [UniProt gene name(s)](https://www.uniprot.org/help/gene_name). You
  can supply up to 20 gene names.

- protein:

  [UniProt protein name](https://www.uniprot.org/help/protein_names).

- taxid:

  NIH-NCBI [Taxon ID](https://www.uniprot.org/taxonomy/). You can supply
  up to 20 taxon IDs.

- organism:

  [Organism name](https://www.uniprot.org/taxonomy/).

- sequence_checksum:

  Sequence CRC64 checksum.

- ipr:

  [InterPro
  identifier(s)](https://www.ebi.ac.uk/interpro/about/interpro/). You
  can supply up to 20 IDs.

- signature_db:

  InterPro's [signature
  database](https://interpro-documentation.readthedocs.io/en/latest/databases.html).
  You can supply up to 13 of the following values:  
  "CATH", "CDD", "HAMAP", "MobiDB Lite", "Panther", "Pfam", "PIRSF",
  "PRINTS", "Prosite", "SFLD", "SMART", "SUPERFAMILY" and/or "TIGRfams"

- signature_id:

  Signature ID in the InterPro's [signature
  database](https://interpro-documentation.readthedocs.io/en/latest/databases.html).
  You can supply up to 20 IDs.

- upid:

  [UniProt Proteome identifier
  (UPID)](https://www.uniprot.org/help/proteome_id). You can supply up
  to 100 UPIDs.

- seq_length:

  An exact sequence length (e.g. 150) or a range of sequence lengths
  (e.g. "130-158").

- rf_dd_type:

  Filter the content of the each UniParc entry by
  [cross-reference](https://www.uniprot.org/database/) names. You can
  supply multiple values.

- rf_db_id:

  Filter the content of the each UniParc entry by protein identifiers in
  any cross-reference database. You can supply multiple values.

- rf_active:

  (logical ) Filter the content of each UniParc entry based on active
  status on source database:

  - NULL: (default) don't filter contents based on active status.

  - TRUE: only return contents which are still active.

  - FALSE: Only return contents which are not active.

- rf_tax_id:

  (Numeric) Filter the content of each UniParc entry by NIH-NCBI [Taxon
  ID](https://www.uniprot.org/taxonomy/). You can supply multiple
  values.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A List where each element corresponds to one UniParc entry returned by
your search query. The element itself is a sub-list containing sequence
information and reference entries.

## Details

Note that this is a search function. Thus, you are not required to fill
every argument; You may use whatever combinations of arguments you see
fit for your query.

## Corresponding API Resources

"GET https://ebi.ac.uk/proteins/api/uniparc"

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
rba_uniprot_uniparc_search(taxid = "694009", protein = "Nucleoprotein")
# }
```
