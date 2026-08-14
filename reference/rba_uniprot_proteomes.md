# Get a Proteome by UPID

UniProt collects and annotates proteomes (protein sets expressed in an
organism). Retrieve a proteome's metadata by UPID, optionally including
its proteins. When proteins are requested, they can be filtered by
UniProtKB review status. See [What are
proteomes?](https://www.uniprot.org/help/proteome) for more information.

## Usage

``` r
rba_uniprot_proteomes(upid, get_proteins = FALSE, reviewed = NULL, ...)
```

## Arguments

- upid:

  Character: [UniProt Proteome identifier
  (UPID)](https://www.uniprot.org/help/proteome_id).

- get_proteins:

  Logical: (default = `FALSE`) If `TRUE`, embed the proteins belonging
  to the supplied proteome in its genome components.

- reviewed:

  Logical: (optional) Used only when `get_proteins` is `TRUE`. If
  `TRUE`, return only reviewed UniProtKB/Swiss-Prot proteins; if
  `FALSE`, return only unreviewed UniProtKB/TrEMBL entries; if `NULL`,
  do not filter by review status.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list containing the requested proteome. With `get_proteins = TRUE`,
protein entries are included under each element of `component`.

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/proteomes/proteins/{upid}"  
"GET https://www.ebi.ac.uk/proteins/api/proteomes/{upid}"

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

Other "UniProt - Proteomes":
[`rba_uniprot_genecentric()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_genecentric.md),
[`rba_uniprot_genecentric_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_genecentric_search.md),
[`rba_uniprot_proteomes_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomes_search.md)

## Examples

``` r
# \donttest{
rba_uniprot_proteomes(upid = "UP000000354")
# }
# \donttest{
rba_uniprot_proteomes(upid = "UP000000354", get_proteins = TRUE)
# }
```
