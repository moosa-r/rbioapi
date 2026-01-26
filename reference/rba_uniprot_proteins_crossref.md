# Get UniProt Entry by UniProt Cross-Reference Database and ID

[UniProt Cross-Reference](https://www.uniprot.org/database/) links
protein Entities with cross-reference (external) databases. Using this
function, you can retrieve a UniProt entity using external database name
and protein ID in that database.

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

  The protein ID in the cross-reference (external) database.

- db_name:

  [cross-reference](https://www.uniprot.org/database/) (external
  database) name.

- reviewed:

  Logical: (Optional) If TRUE, only returns "UniProtKB/Swiss-Prot"
  (reviewed) entries; If FALSE, only returns TrEMBL (un-reviewed)
  entries.

- isoform:

  Numeric: (Optional) you have two options:

  - 0: Exclude isoforms.

  - 1: Return isoforms only.

  see: [Alternative
  products](https://www.uniprot.org/help/alternative_products)

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

List which each element is a UniProt entity that correspond to your
supplied cross-reference database name and ID.

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/proteins/{dbtype}:{dbid}"

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
