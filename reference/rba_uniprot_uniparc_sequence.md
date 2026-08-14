# Get UniParc Entries by Sequence

Retrieve a UniParc entry using an exact protein sequence. Partial
matches are not accepted. The `rf_*` arguments filter cross-references
within the returned entry.

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

  Character: Exact protein sequence. Partial matches are not accepted.

- rf_dd_type:

  Character: (optional) Filter the UniParc entry's content by
  [cross-reference](https://www.uniprot.org/database/) names. You can
  supply multiple values.

- rf_db_id:

  Character: (optional) Filter the UniParc entry's content by protein
  identifiers in any cross-reference database. You can supply multiple
  values.

- rf_active:

  Logical: (optional) Filter the UniParc entry's content by active
  status in the source database: `TRUE` retains active database
  references, `FALSE` retains inactive references, and `NULL` applies no
  active-status filter.

- rf_tax_id:

  Numeric: (optional) Filter the UniParc entry's content by NIH-NCBI
  [Taxon ID](https://www.uniprot.org/taxonomy/). You can supply multiple
  values.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

The matching UniParc entry.

## Corresponding API Resources

"POST https://www.ebi.ac.uk/proteins/api/uniparc/sequence"

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
[`rba_uniprot_uniparc_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_uniparc_search.md)

## Examples

``` r
# \donttest{
rba_uniprot_uniparc_sequence("GMRSCPRGCSQRGRCENGRCVCNPGYTGEDC")
# }
```
