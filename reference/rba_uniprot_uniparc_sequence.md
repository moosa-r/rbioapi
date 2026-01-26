# Get UniParc Entries by Sequence

Retrieve UniParc Entry by providing an exact sequence. Note that partial
matches will not be accepted. You can also filter the returned content
of the returned UniParc entry. see "Argument" section for more details.

## Usage

``` r
rba_uniprot_uniparc_sequence(
  sequence,
  rf_dd_type = NULL,
  rf_db_id = NULL,
  rf_active = NULL,
  rf_tax_id = NULL,
  ...
)
```

## Arguments

- sequence:

  Exact UniParc protein sequence. Partial matches will not be accepted.

- rf_dd_type:

  Filter the content of the UniParc entry by
  [cross-reference](https://www.uniprot.org/database/) names. You can
  supply multiple values.

- rf_db_id:

  Filter the content of the UniParc entry by protein identifiers in any
  cross-reference database. You can supply multiple values.

- rf_active:

  (logical ) Filter the content of UniParc entry based on active status
  on source database:

  - NULL: (default) don't filter contents based on active status.

  - TRUE: only return contents which are still active.

  - FALSE: Only return contents which are not active.

- rf_tax_id:

  (Numeric) Filter the content of the UniParc entry by NIH-NCBI [Taxon
  ID](https://www.uniprot.org/taxonomy/). You can supply multiple
  values.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list which correspond to a UniParc entry.

## Corresponding API Resources

"POST https://ebi.ac.uk/proteins/api/uniparc/sequence"

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
[`rba_uniprot_uniparc_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_uniparc_search.md)

## Examples

``` r
# \donttest{
rba_uniprot_uniparc_sequence("GMRSCPRGCSQRGRCENGRCVCNPGYTGEDC")
# }
```
