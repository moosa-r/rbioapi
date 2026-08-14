# Get UniProt Entry by UniProt Cross-Reference Database and ID

UniProt cross-references connect protein entries with identifiers in
[external databases](https://www.uniprot.org/database/). Retrieve
UniProtKB entries associated with an identifier from one of these
databases.

## Usage

``` r
rba_uniprot_proteins_crossref(
  db_id,
  db_name,
  reviewed = NULL,
  isoform = NULL,
  ...
)
```

## Arguments

- db_id:

  Character: Protein identifier in the cross-reference database.

- db_name:

  Character: [Cross-reference database
  name](https://www.uniprot.org/database/).

- reviewed:

  Logical: (optional) If `TRUE`, return only reviewed Swiss-Prot
  entries. If `FALSE`, return only unreviewed TrEMBL entries.

- isoform:

  Numeric: (optional) One of:

  - 0: Exclude isoforms.

  - 1: Return isoforms only.

  See [alternative
  products](https://www.uniprot.org/help/alternative_products).

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list named by UniProt accession. Each element is a UniProtKB entry
corresponding to the supplied cross-reference identifier.

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/proteins/{dbtype}:{dbid}"

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

Other "UniProt - Proteins":
[`rba_uniprot_proteins()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteins.md),
[`rba_uniprot_proteins_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteins_search.md)

## Examples

``` r
# \donttest{
rba_uniprot_proteins_crossref("cd40", "hgnc")
# }
# \donttest{
rba_uniprot_proteins_crossref("cd40", "hgnc", reviewed = TRUE)
# }
# \donttest{
rba_uniprot_proteins_crossref("mica", "hgnc", isoform = 0)
# }
```
